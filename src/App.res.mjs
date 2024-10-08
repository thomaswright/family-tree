// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as FamilyTree from "./FamilyTree.res.mjs";
import * as UploadJsx from "./upload.jsx";
import * as Core__Option from "@rescript/core/src/Core__Option.res.mjs";
import * as JsxRuntime from "react/jsx-runtime";
import FamilyTreePng from "./assets/family-tree.png";

var make = UploadJsx.JsonUpload;

var imgUrl = FamilyTreePng;

function App(props) {
  var match = React.useState(function () {
        
      });
  var setJsonData = match[1];
  return JsxRuntime.jsxs("div", {
              children: [
                JsxRuntime.jsxs("div", {
                      children: [
                        JsxRuntime.jsxs("div", {
                              children: [
                                JsxRuntime.jsxs("div", {
                                      children: [
                                        JsxRuntime.jsx("img", {
                                              src: imgUrl,
                                              width: "32"
                                            }),
                                        "Family Tree"
                                      ],
                                      className: "px-6 text-2xl font-black mb-2 flex flex-row gap-2"
                                    }),
                                JsxRuntime.jsx("div", {
                                      children: JsxRuntime.jsx(make, {
                                            setJsonData: (function (v) {
                                                setJsonData(function (param) {
                                                      return v;
                                                    });
                                              })
                                          }),
                                      className: "px-6"
                                    })
                              ],
                              className: "flex-none"
                            }),
                        JsxRuntime.jsxs("div", {
                              children: [
                                JsxRuntime.jsx("div", {
                                      children: "The data is a json file - an array of the following object."
                                    }),
                                JsxRuntime.jsx("div", {
                                      children: "The first element will be regarded as the root of the tree."
                                    }),
                                JsxRuntime.jsx("div", {
                                      children: "{\n  id: string,\n  name: string,\n  mother: id || undefined,\n  father: id || undefined,\n  spouse: id || undefined,\n}\n",
                                      className: "font-mono font-medium text-xs w-fit whitespace-pre bg-gray-100 rounded border px-2 py-1 mt-1"
                                    })
                              ],
                              className: "flex-none text-sm  px-6 flex flex-col gap-2 py-2"
                            })
                      ],
                      className: "flex flex-col lg:flex-row "
                    }),
                Core__Option.mapOr(match[0], null, (function (jsonData_) {
                        return JsxRuntime.jsx("div", {
                                    children: JsxRuntime.jsx(FamilyTree.make, {
                                          familyTreeData: jsonData_,
                                          rootId: undefined
                                        })
                                  });
                      }))
              ],
              className: "py-6"
            });
}

var make$1 = App;

export {
  make$1 as make,
}
/* make Not a pure module */
