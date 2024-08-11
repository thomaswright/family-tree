module JsonUpload = {
  @module("./upload.jsx") @react.component
  external make: (~setJsonData: array<Types.person> => unit) => React.element = "JsonUpload"
}

// Todo: decode data

let specDef = `{
  id: string,
  name: string,
  mother: id || undefined,
  father: id || undefined,
  spouse: id || undefined,
}
`
@module("./assets/family-tree.png") external imgUrl: string = "default"

@react.component
let make = () => {
  let (jsonData, setJsonData) = React.useState(() => None)
  // let (rootId, setRootId) = React.useState(() => "")

  let decodeData = v => {
    setJsonData(_ => v->Some)
  }

  <div className="py-6">
    <div className="flex flex-col lg:flex-row ">
      <div className="flex-none">
        <div className="px-6 text-2xl font-black mb-2 flex flex-row gap-2">
          <img src={imgUrl} width={"32"} />
          {"Family Tree"->React.string}
        </div>
        // <button className="bg-blue-400"> {"Button"->React.string} </button>
        <div className="px-6">
          <JsonUpload setJsonData={v => v->decodeData} />
        </div>
      </div>
      <div className={"flex-none text-sm  px-6 flex flex-col gap-2 py-2"}>
        <div> {"The data is a json file - an array of the following object."->React.string} </div>
        <div> {"The first element will be regarded as the root of the tree."->React.string} </div>
        <div
          className="font-mono font-medium text-xs w-fit whitespace-pre bg-gray-100 rounded border px-2 py-1 mt-1">
          {specDef->React.string}
        </div>
      </div>
    </div>
    {jsonData->Option.mapOr(React.null, jsonData_ => {
      <div>
        // <div className="px-6">
        //   <div className="font-bold"> {"Root Id"->React.string} </div>
        //   <input
        //     id={"rootFamilyId"}
        //     className={"rounded border border-slate-700"}
        //     type_="text"
        //     value={rootId}
        //     onChange={e => {
        //       let value = (e->ReactEvent.Form.target)["value"]
        //       setRootId(_ => value)
        //     }}
        //   />
        // </div>
        <FamilyTree familyTreeData={jsonData_} rootId={None} />
      </div>
    })}
  </div>
}
