// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Core__Array from "@rescript/core/src/Core__Array.res.mjs";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as Belt_SetString from "rescript/lib/es6/belt_SetString.js";
import * as Fi from "react-icons/fi";
import * as JsxRuntime from "react/jsx-runtime";

function toggle(s, k) {
  if (Belt_SetString.has(s, k)) {
    return Belt_SetString.remove(s, k);
  } else {
    return Belt_SetString.add(s, k);
  }
}

var SSet = {
  empty: undefined,
  fromArray: Belt_SetString.fromArray,
  fromSortedArrayUnsafe: Belt_SetString.fromSortedArrayUnsafe,
  isEmpty: Belt_SetString.isEmpty,
  has: Belt_SetString.has,
  add: Belt_SetString.add,
  mergeMany: Belt_SetString.mergeMany,
  remove: Belt_SetString.remove,
  removeMany: Belt_SetString.removeMany,
  union: Belt_SetString.union,
  intersect: Belt_SetString.intersect,
  diff: Belt_SetString.diff,
  subset: Belt_SetString.subset,
  cmp: Belt_SetString.cmp,
  eq: Belt_SetString.eq,
  forEachU: Belt_SetString.forEachU,
  forEach: Belt_SetString.forEach,
  reduceU: Belt_SetString.reduceU,
  reduce: Belt_SetString.reduce,
  everyU: Belt_SetString.everyU,
  every: Belt_SetString.every,
  someU: Belt_SetString.someU,
  some: Belt_SetString.some,
  keepU: Belt_SetString.keepU,
  keep: Belt_SetString.keep,
  partitionU: Belt_SetString.partitionU,
  partition: Belt_SetString.partition,
  size: Belt_SetString.size,
  toList: Belt_SetString.toList,
  toArray: Belt_SetString.toArray,
  minimum: Belt_SetString.minimum,
  minUndefined: Belt_SetString.minUndefined,
  maximum: Belt_SetString.maximum,
  maxUndefined: Belt_SetString.maxUndefined,
  get: Belt_SetString.get,
  getUndefined: Belt_SetString.getUndefined,
  getExn: Belt_SetString.getExn,
  split: Belt_SetString.split,
  checkInvariantInternal: Belt_SetString.checkInvariantInternal,
  toggle: toggle
};

function addToMapArray(a, k, x) {
  return Belt_MapString.update(a, k, (function (v) {
                if (v !== undefined) {
                  return Belt_Array.concatMany([
                              v,
                              [x]
                            ]);
                } else {
                  return [x];
                }
              }));
}

function reactMap(arr, f) {
  return arr.map(f);
}

function mapOrNull(o, f) {
  return Core__Option.mapOr(o, null, f);
}

function mapReduceWithIndex(arr, param, f) {
  return Core__Array.reduceWithIndex(arr, [
                param[0],
                param[1]
              ], (function (param, c, i) {
                  var match = f(param[0], param[1], c, i);
                  return [
                          match[0],
                          match[1]
                        ];
                }))[0];
}

var Utils = {
  addToMapArray: addToMapArray,
  reactMap: reactMap,
  mapOrNull: mapOrNull,
  mapReduceWithIndex: mapReduceWithIndex
};

var Plus = {};

var Minus = {};

var Icons = {
  Plus: Plus,
  Minus: Minus
};

function getDetails(input, from) {
  var baseDetails = function (x) {
    return x.map(function (p) {
                var match = p.mother;
                var match$1 = p.father;
                var tmp;
                if (match !== undefined && match$1 !== undefined) {
                  var match$2 = x.find(function (v) {
                        return v.id === match;
                      });
                  var match$3 = x.find(function (v) {
                        return v.id === match;
                      });
                  tmp = match$2 !== undefined && match$3 !== undefined ? ({
                        TAG: "Blood",
                        _0: match,
                        _1: match$1
                      }) : "InLaw";
                } else {
                  tmp = "InLaw";
                }
                return {
                        id: p.id,
                        name: p.name,
                        relation: tmp,
                        spouse: p.spouse,
                        children: [],
                        coparents: undefined,
                        anc: [],
                        des: []
                      };
              });
  };
  var addLineage = function (basePeople, ur) {
    var recFun = function (id, idAnc, people) {
      var idChildren = Belt_Array.keepMap(people, (function (p) {
              var match = p.relation;
              if (typeof match !== "object") {
                return ;
              }
              var f = match._1;
              var m = match._0;
              if (m === id) {
                return [
                        p.id,
                        f
                      ];
              } else if (f === id) {
                return [
                        p.id,
                        m
                      ];
              } else {
                return ;
              }
            }));
      var idLineage = idAnc.concat([id]);
      var match = Core__Array.reduce(idChildren, [
            [],
            people
          ], (function (param, param$1) {
              var match = recFun(param$1[0], idLineage, param[1]);
              return [
                      param[0].concat(match[0]),
                      match[1]
                    ];
            }));
      var des = match[0];
      return [
              des.concat([id]),
              match[1].map(function (d) {
                    if (d.id === id) {
                      return {
                              id: d.id,
                              name: d.name,
                              relation: d.relation,
                              spouse: d.spouse,
                              children: idChildren,
                              coparents: d.coparents,
                              anc: idAnc,
                              des: des
                            };
                    } else {
                      return d;
                    }
                  })
            ];
    };
    return recFun(ur, [], basePeople)[1];
  };
  var addInLaws = function (people) {
    return people.map(function (p) {
                if (p.relation !== "InLaw") {
                  return p;
                }
                var match = Core__Array.reduce(people, [
                      [],
                      []
                    ], (function (param, c) {
                        var des = param[1];
                        var children = param[0];
                        var match = c.relation;
                        var tmp;
                        if (typeof match !== "object") {
                          tmp = undefined;
                        } else {
                          var f = match._1;
                          var m = match._0;
                          tmp = m === p.id ? [
                              c.id,
                              f,
                              c.des
                            ] : (
                              f === p.id ? [
                                  c.id,
                                  m,
                                  c.des
                                ] : undefined
                            );
                        }
                        return Core__Option.getOr(Core__Option.map(tmp, (function (param) {
                                          var child = param[0];
                                          return [
                                                  children.concat([[
                                                          child,
                                                          param[1]
                                                        ]]),
                                                  Belt_Array.concatMany([
                                                          des,
                                                          [child]
                                                        ]).concat(param[2])
                                                ];
                                        })), [
                                    children,
                                    des
                                  ]);
                      }));
                return {
                        id: p.id,
                        name: p.name,
                        relation: p.relation,
                        spouse: p.spouse,
                        children: match[0],
                        coparents: p.coparents,
                        anc: p.anc,
                        des: match[1]
                      };
              });
  };
  var people = addInLaws(addLineage(baseDetails(input), from));
  return people.map(function (p) {
              var coparents = Core__Array.reduce(people, undefined, (function (a, c) {
                      var match = c.relation;
                      if (typeof match !== "object") {
                        return a;
                      }
                      var f = match._1;
                      var m = match._0;
                      if (m === p.id || f === p.id) {
                        return addToMapArray(a, f === p.id ? m : f, c.id);
                      } else {
                        return a;
                      }
                    }));
              return {
                      id: p.id,
                      name: p.name,
                      relation: p.relation,
                      spouse: p.spouse,
                      children: p.children,
                      coparents: coparents,
                      anc: p.anc,
                      des: p.des
                    };
            });
}

function isBlood(p) {
  var match = p.relation;
  if (typeof match !== "object") {
    return false;
  } else {
    return true;
  }
}

function getById(details, id) {
  return details.find(function (v) {
              return v.id === id;
            });
}

function getGenGroupings(details) {
  var genGrouping = Core__Array.reduce(details, Core__Array.make(100, []), (function (a, c) {
              return a.map(function (v, i) {
                          if (c.relation === "InLaw") {
                            return Core__Option.getOr(Core__Option.flatMap(Core__Option.flatMap(c.children[0], (function (param) {
                                                  return getById(details, param[0]);
                                                })), (function (d) {
                                              if ((d.anc.length - 1 | 0) === i) {
                                                return Belt_Array.concatMany([
                                                            v,
                                                            [c]
                                                          ]);
                                              }
                                              
                                            })), v);
                          } else if (i === c.anc.length) {
                            return Belt_Array.concatMany([
                                        v,
                                        [c]
                                      ]);
                          } else {
                            return v;
                          }
                        });
            })).filter(function (gen) {
          return gen.length > 0;
        }).map(function (gen, i) {
        if (i === 0) {
          return gen;
        } else {
          return gen.filter(function (p) {
                      return isBlood(p);
                    });
        }
      });
  var match = Core__Array.reduce(genGrouping.toReversed(), [
        [],
        []
      ], (function (param, c) {
          var $$new = param[1].concat(c);
          return [
                  Belt_Array.concatMany([
                        param[0],
                        [$$new]
                      ]),
                  $$new
                ];
        }));
  var desGrouping = match[0].toReversed();
  return [
          genGrouping,
          desGrouping
        ];
}

function FamilyTree(props) {
  var familyTreeData = props.familyTreeData;
  var rootNode = Core__Option.getOr(Core__Option.mapOr(props.rootId, Core__Option.map(familyTreeData[0], (function (f) {
                  return f.id;
                })), (function (r) {
              return r;
            })), "");
  var match = React.useMemo((function () {
          var details = getDetails(familyTreeData, rootNode);
          var match = getGenGroupings(details);
          return [
                  details,
                  match[0],
                  match[1]
                ];
        }), []);
  var desGrouping = match[2];
  var genGrouping = match[1];
  var details = match[0];
  var match$1 = React.useState(function () {
        
      });
  var setCollapsed = match$1[1];
  var collapsed = match$1[0];
  React.useEffect((function () {
          
        }), []);
  var renderTree = function (id) {
    var renderChildren = function (children) {
      if (Belt_SetString.has(collapsed, id)) {
        return null;
      } else {
        return children.map(function (child) {
                    return renderTree(child);
                  });
      }
    };
    return mapOrNull(getById(details, id), (function (d) {
                  var hasDes = d.des.length > 0;
                  var nonSpouseCoparents = Belt_MapString.toArray(Belt_MapString.remove(d.coparents, d.spouse));
                  var onClickBlood = function (e) {
                    e.stopPropagation();
                    setCollapsed(function (v) {
                          if (hasDes) {
                            return toggle(v, id);
                          } else {
                            return v;
                          }
                        });
                  };
                  return JsxRuntime.jsxs("div", {
                              children: [
                                JsxRuntime.jsxs("div", {
                                      children: [
                                        JsxRuntime.jsxs("div", {
                                              children: [
                                                JsxRuntime.jsx("div", {
                                                      children: d.name,
                                                      className: ""
                                                    }),
                                                mapOrNull(getById(details, d.spouse), (function (s) {
                                                        return JsxRuntime.jsx("div", {
                                                                    children: s.name,
                                                                    className: "text-cyan-700 "
                                                                  });
                                                      }))
                                              ],
                                              className: [
                                                  "bg-primary-200 m-1 px-1.5 py-0.5 w-40 rounded border border-primary-400",
                                                  nonSpouseCoparents.length > 0 ? "mb-0 rounded-b-none  border-b-0" : ""
                                                ].join(" "),
                                              onClick: onClickBlood
                                            }),
                                        JsxRuntime.jsxs("div", {
                                              children: [
                                                Belt_SetString.has(collapsed, id) && hasDes ? JsxRuntime.jsx("div", {
                                                        children: JsxRuntime.jsx(Fi.FiPlus, {}),
                                                        className: "w-10 h-10 cursor-pointer flex flex-row items-center justify-center m-1 border border-primary-400 bg-primary-200 rounded-full text-2xl font-normal",
                                                        onClick: (function (e) {
                                                            e.stopPropagation();
                                                            setCollapsed(function (v) {
                                                                  if (hasDes) {
                                                                    return toggle(v, id);
                                                                  } else {
                                                                    return v;
                                                                  }
                                                                });
                                                          })
                                                      }) : null,
                                                mapOrNull(Belt_MapString.get(d.coparents, d.spouse), (function (childrenWithCurrentSpouse) {
                                                        return renderChildren(childrenWithCurrentSpouse);
                                                      }))
                                              ]
                                            })
                                      ],
                                      className: "flex flex-row"
                                    }),
                                nonSpouseCoparents.map(function (param) {
                                      return JsxRuntime.jsxs("div", {
                                                  children: [
                                                    JsxRuntime.jsxs("div", {
                                                          children: [
                                                            JsxRuntime.jsx("div", {
                                                                  children: "with",
                                                                  className: "text-sm"
                                                                }),
                                                            mapOrNull(getById(details, param[0]), (function (p) {
                                                                    return JsxRuntime.jsx("div", {
                                                                                children: p.name,
                                                                                className: "text-cyan-700"
                                                                              });
                                                                  }))
                                                          ],
                                                          className: [
                                                              "bg-primary-200 m-1 px-1 py-1 w-40 rounded border border-primary-400",
                                                              nonSpouseCoparents.length > 0 ? "mt-0 rounded-t-none border-t-primary-300" : ""
                                                            ].join(" "),
                                                          onClick: onClickBlood
                                                        }),
                                                    JsxRuntime.jsx("div", {
                                                          children: renderChildren(param[1])
                                                        })
                                                  ],
                                                  className: "flex flex-row"
                                                });
                                    })
                              ],
                              className: ""
                            });
                }));
  };
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsxs("div", {
                      children: [
                        JsxRuntime.jsx("div", {
                              className: "w-40 mx-1 "
                            }),
                        mapReduceWithIndex(genGrouping.slice(0, -1), [
                              [],
                              false
                            ], (function (m, prevCollapsed, gen, i) {
                                if (prevCollapsed) {
                                  return [
                                          m,
                                          true
                                        ];
                                }
                                var allCollapsed = gen.every(function (p) {
                                      return Belt_SetString.has(collapsed, p.id);
                                    });
                                var desIds = Belt_SetString.fromArray(desGrouping[i].map(function (x) {
                                          return x.id;
                                        }));
                                var parentsInGen = gen.filter(function (p) {
                                      return p.children.length > 0;
                                    });
                                var parentsCollapsed = parentsInGen.every(function (p) {
                                      return Belt_SetString.has(collapsed, p.id);
                                    });
                                var parentAncCollapsed = parentsInGen.every(function (p) {
                                      return p.anc.some(function (x) {
                                                  return Belt_SetString.has(collapsed, x);
                                                });
                                    });
                                var isCollapsed = allCollapsed || parentsCollapsed;
                                var result = parentAncCollapsed ? null : JsxRuntime.jsx("div", {
                                        children: JsxRuntime.jsx("div", {
                                              children: isCollapsed ? JsxRuntime.jsx(Fi.FiPlus, {}) : JsxRuntime.jsx(Fi.FiMinus, {}),
                                              className: "w-10 h-10 cursor-pointer flex flex-row items-center justify-center border border-primary-400 bg-primary-200 rounded-full text-2xl font-normal",
                                              onClick: (function (e) {
                                                  e.stopPropagation();
                                                  setCollapsed(function (v) {
                                                        if (isCollapsed) {
                                                          return Belt_SetString.diff(v, desIds);
                                                        } else {
                                                          return Belt_SetString.union(v, desIds);
                                                        }
                                                      });
                                                })
                                            }),
                                        className: "w-40 mx-1 "
                                      });
                                return [
                                        m.concat([result]),
                                        prevCollapsed || isCollapsed
                                      ];
                              }))
                      ],
                      className: "flex flex-row px-2"
                    }),
                JsxRuntime.jsx("div", {
                      children: renderTree("geo1"),
                      className: "p-2 flex flex-row font-bold w-fit"
                    })
              ],
              className: ["p-4 flex-1 text-primary-900"].join(" "),
              style: {
                width: Math.imul(genGrouping.length, 180).toString() + "px"
              }
            });
}

var SMap;

var make = FamilyTree;

export {
  SSet ,
  SMap ,
  Utils ,
  Icons ,
  getDetails ,
  isBlood ,
  getById ,
  getGenGroupings ,
  make ,
}
/* react Not a pure module */
