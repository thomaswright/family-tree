module SSet = {
  include Belt.Set.String

  let toggle = (s, k) =>
    s->Belt.Set.String.has(k) ? s->Belt.Set.String.remove(k) : s->Belt.Set.String.add(k)
}

module SMap = Belt.Map.String

module Utils = {
  let addToMapArray = (a, k, x) => {
    a->Belt.Map.String.update(k, v => {
      switch v {
      | Some(children) => Some([...children, x])
      | None => Some([x])
      }
    })
  }

  let reactMap = (arr, f) => arr->Array.map(f)->React.array

  let mapOrNull = (o, f) => o->Option.mapOr(React.null, f)

  let mapReduceWithIndex = (arr, (mi, ri), f) => {
    let (mapResult, _) = arr->Array.reduceWithIndex((mi, ri), ((m, r), c, i) => {
      let (m2, r2) = f(m, r, c, i)

      (m2, r2)
    })
    mapResult
  }
}

open Utils

module Icons = {
  module Plus = {
    @module("react-icons/fi") @react.component
    external make: unit => React.element = "FiPlus"
  }
  module Minus = {
    @module("react-icons/fi") @react.component
    external make: unit => React.element = "FiMinus"
  }
}

type relation = Blood(string, string) | InLaw

type personDetail = {
  id: string,
  name: string,
  relation: relation,
  spouse: string,
  children: array<(string, string)>,
  coparents: Belt.Map.String.t<array<string>>,
  anc: array<string>,
  des: array<string>,
}

let getDetails = (input, from) => {
  // Todo: maybe update the relation formatter to
  // consider if one or other isn't found
  // and classify as something other than "InLaw"
  // This could be extended to just classify
  // InLaw as nodes not found descended from root
  let baseDetails = x =>
    x->Array.map((p: Types.person): personDetail => {
      id: p.id,
      name: p.name,
      relation: switch (p.mother, p.father) {
      | (Some(m), Some(f)) =>
        switch (x->Array.find(v => v.id == m), x->Array.find(v => v.id == m)) {
        | (Some(_), Some(_)) => Blood(m, f)
        | _ => InLaw
        }
      | _ => InLaw
      },
      spouse: p.spouse,
      coparents: SMap.empty,
      children: [],
      anc: [],
      des: [],
    })

  let addLineage = (basePeople, ur) => {
    let rec recFun = (id, idAnc, people: array<personDetail>) => {
      let idChildren = people->Belt.Array.keepMap(p => {
        switch p.relation {
        | Blood(m, f) => m == id ? Some(p.id, f) : f == id ? Some(p.id, m) : None
        | InLaw => None
        }
      })

      let idLineage = idAnc->Array.concat([id])

      let (des, newDetails) = idChildren->Array.reduce(([], people), (
        (desAcc, peopleAcc),
        (curChildId, _withId),
      ) => {
        let (recDes, recPeople) = recFun(curChildId, idLineage, peopleAcc)
        (Array.concat(desAcc, recDes), recPeople)
      })

      (
        des->Array.concat([id]),
        newDetails->Array.map(d =>
          d.id == id
            ? {
                ...d,
                children: idChildren,
                des,
                anc: idAnc,
              }
            : d
        ),
      )
    }

    let (_, result) = recFun(ur, [], basePeople)
    result
  }

  let addInLaws = people => {
    people->Array.map(p => {
      p.relation == InLaw
        ? {
            let (children, des) = people->Array.reduce(([], []), ((children, des), c) => {
              switch c.relation {
              | Blood(m, f) =>
                m == p.id ? Some(c.id, f, c.des) : f == p.id ? Some(c.id, m, c.des) : None
              | InLaw => None
              }
              ->Option.map(
                ((child, with, childsDes)) => {
                  (
                    Array.concat(children, [(child, with)]),
                    Array.concat([...des, child], childsDes),
                  )
                },
              )
              ->Option.getOr((children, des))
            })
            {
              ...p,
              children,
              des,
            }
          }
        : p
    })
  }

  let addCoparents = people => {
    people->Array.map(p => {
      let coparents = people->Array.reduce(SMap.empty, (a, c) => {
        switch c.relation {
        | InLaw => a
        | Blood(m, f) => m == p.id || f == p.id ? a->addToMapArray(f == p.id ? m : f, c.id) : a
        }
      })

      {
        ...p,
        coparents,
      }
    })
  }

  input->baseDetails->addLineage(from)->addInLaws->addCoparents
}

let isBlood = p =>
  switch p.relation {
  | Blood(_, _) => true
  | InLaw => false
  }

let getById = (details: array<personDetail>, id) => details->Array.find(v => v.id == id)

// These are blood relatives because that's how we iterate
let getGenGroupings = details => {
  let genGrouping =
    details
    ->Array.reduce(Array.make(~length=100, []), (a, c) => {
      a->Array.mapWithIndex((v, i) => {
        c.relation == InLaw
          ? c.children
            ->Array.get(0)
            ->Option.flatMap(
              ((childId, _)) => {
                getById(details, childId)
              },
            )
            ->Option.flatMap(d => d.anc->Array.length - 1 == i ? Some([...v, c]) : None)
            ->Option.getOr(v)
          : i == c.anc->Array.length
          ? [...v, c]
          : v
      })
    })
    ->Array.filter(gen => gen->Array.length > 0)
    ->Array.mapWithIndex((gen, i) => i == 0 ? gen : gen->Array.filter(p => p->isBlood))

  let desGrouping = {
    let (result, _) =
      genGrouping
      ->Array.toReversed
      ->Array.reduce(([], []), ((acc, last), c) => {
        let new = last->Array.concat(c)

        ([...acc, new], new)
      })

    result->Array.toReversed
  }

  (genGrouping, desGrouping)
}

@react.component
let make = (~familyTreeData: array<Types.person>, ~rootId: option<string>) => {
  // if rootId is None, will use first entry in familyTreeData as root
  let rootNode =
    rootId
    ->Option.mapOr(familyTreeData->Array.get(0)->Option.map(f => f.id), r => r->Some)
    ->Option.getOr("")

  let (details, genGrouping, desGrouping) = React.useMemo0(() => {
    let details = getDetails(familyTreeData, rootNode)
    let (genGrouping, desGrouping) = getGenGroupings(details)

    (details, genGrouping, desGrouping)
  })

  let (collapsed: SSet.t, setCollapsed) = React.useState(() => {
    SSet.empty
  })

  React.useEffect0(() => {
    None
  })

  let rec renderTree = id => {
    let renderChildren = children =>
      collapsed->SSet.has(id)
        ? React.null
        : children->reactMap(child => {
            renderTree(child)
          })

    getById(details, id)->mapOrNull(d => {
      let hasDes = d.des->Array.length > 0
      let nonSpouseCoparents =
        d.coparents
        ->SMap.remove(d.spouse)
        ->SMap.toArray

      let onClickBlood = e => {
        e->ReactEvent.Mouse.stopPropagation
        setCollapsed(v => {
          hasDes ? v->SSet.toggle(id) : v
        })
      }
      <div className="">
        <div className="flex flex-row">
          <div
            onClick={onClickBlood}
            className={[
              "bg-primary-200 m-1 px-1.5 py-0.5 w-40 rounded border border-primary-400",
              nonSpouseCoparents->Array.length > 0 ? "mb-0 rounded-b-none  border-b-0" : "",
            ]->Array.join(" ")}>
            <div className={""}> {d.name->React.string} </div>
            {getById(details, d.spouse)->mapOrNull(s => {
              <div className={"text-cyan-700 "}> {s.name->React.string} </div>
            })}
          </div>
          <div>
            {collapsed->SSet.has(id) && hasDes
              ? <div
                  className={`w-10 h-10 cursor-pointer flex flex-row items-center justify-center m-1 border border-primary-400 bg-primary-200 rounded-full text-2xl font-normal`}
                  onClick={e => {
                    e->ReactEvent.Mouse.stopPropagation
                    setCollapsed(v => {
                      hasDes ? v->SSet.toggle(id) : v
                    })
                  }}>
                  <Icons.Plus />
                </div>
              : React.null}
            {d.coparents
            ->SMap.get(d.spouse)
            ->mapOrNull(childrenWithCurrentSpouse => {
              renderChildren(childrenWithCurrentSpouse)
            })}
          </div>
        </div>
        {nonSpouseCoparents->reactMap(((coparent, children)) => {
          <div className="flex flex-row">
            <div
              onClick={onClickBlood}
              className={[
                "bg-primary-200 m-1 px-1 py-1 w-40 rounded border border-primary-400",
                nonSpouseCoparents->Array.length > 0
                  ? "mt-0 rounded-t-none border-t-primary-300"
                  : "",
              ]->Array.join(" ")}>
              <div className="text-sm"> {"with"->React.string} </div>
              {getById(details, coparent)->mapOrNull(
                p => {
                  <div className={"text-cyan-700"}> {p.name->React.string} </div>
                },
              )}
            </div>
            <div> {renderChildren(children)} </div>
          </div>
        })}
      </div>
    })
  }

  // width is scaled with number of gens by gen width to prevent
  // jumping when gens collapsed
  <div
    style={{width: (genGrouping->Array.length * 180)->Int.toString ++ "px"}}
    className={["p-4 flex-1 text-primary-900"]->Array.join(" ")}>
    <div className="flex flex-row px-2">
      <div className="w-40 mx-1 " />
      {genGrouping
      ->Array.slice(~start=0, ~end=-1)
      ->mapReduceWithIndex(([], false), (m, prevCollapsed, gen, i) => {
        prevCollapsed
          ? (m, true)
          : {
              // This logic is kind of cursed, but idk how to do it otherways:
              // If the parents are all collapsed, we show it as collapsed
              // unless the parents anc themselves are all collapsed
              // in which case we dont show the button at all

              let allCollapsed = gen->Array.every(p => collapsed->SSet.has(p.id))

              let desIds =
                desGrouping
                ->Array.getUnsafe(i)
                ->Array.map(x => x.id)
                ->SSet.fromArray

              let parentsInGen = gen->Array.filter(p => p.children->Array.length > 0)

              let parentsCollapsed = parentsInGen->Array.every(p => collapsed->SSet.has(p.id))

              let parentAncCollapsed =
                parentsInGen->Array.every(p => p.anc->Array.some(x => collapsed->SSet.has(x)))

              let isCollapsed = allCollapsed || parentsCollapsed

              let result = parentAncCollapsed
                ? React.null
                : <div className="w-40 mx-1 ">
                    <div
                      className={`w-10 h-10 cursor-pointer flex flex-row items-center justify-center border border-primary-400 bg-primary-200 rounded-full text-2xl font-normal`}
                      onClick={e => {
                        e->ReactEvent.Mouse.stopPropagation
                        setCollapsed(v => {
                          isCollapsed ? v->SSet.diff(desIds) : v->SSet.union(desIds)
                        })
                      }}>
                      {isCollapsed ? <Icons.Plus /> : <Icons.Minus />}
                    </div>
                  </div>

              (m->Array.concat([result]), prevCollapsed || isCollapsed)
            }
      })
      ->React.array}
    </div>
    <div className="p-2 flex flex-row font-bold w-fit"> {renderTree("geo1")} </div>
  </div>
}
