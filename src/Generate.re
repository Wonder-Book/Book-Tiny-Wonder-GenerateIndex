open Node;

open GlobExtend;

open GenerateType;

let _writeToIndexFile = (destDir, content) => {
  let destFilePath = Path.join([|destDir, "Index.re"|]);
  Fs.writeFileAsUtf8Sync(destFilePath, content);
  content;
};

let _buildModuleContent = (fileName, dataList) => {
  dataList
  |> List.fold_left(
       (content, functionName) =>
         content ++ {j|let $functionName = $fileName.$functionName;\n\n|j},
       "",
     );
};

let _buildContent = fileDataList =>
  fileDataList
  |> List.fold_left(
       (content, (moduleName, fileName, dataList)) => {
         let moduleContent = _buildModuleContent(fileName, dataList);

         {j|$content

      module $moduleName{
        $moduleContent
      }
      |j};
       },
       "",
     )
  |> (
    content =>
      content |> Js.String.slice(~from=0, ~to_=Js.String.length(content) - 2)
  );

let _findPublicFunctionList = (code: string) => {
  let regex = [%re {|/^let\s+([a-zA-Z][\w\d]+)\s+\=\s+/mg|}];
  let break = ref(false);
  let resultList = ref([]);
  while (! break^) {
    switch (regex |> Js.Re.exec(code)) {
    | None => break := true
    | Some(result) =>
      switch (Js.Nullable.to_opt(Js.Re.captures(result)[1])) {
      | None => ()
      | Some(result) => resultList := [result, ...resultList^]
      }
    };
  };
  resultList^;
};

let generate =
    (
      globCwd: string,
      rootDir: string,
      sourceFileGlobArr: array(string),
      destDir: string,
      config,
    ) => {
  let excludeList = config##exclude |> Array.to_list;
  let replaceAPIModuleNameFunc =
    config##replaceAPIModuleNameFunc
    |> Js.Option.getWithDefault(moduleName =>
         moduleName |> Js.String.replace("JsAPI", "")
       );

  sourceFileGlobArr
  |> Array.to_list
  |> List.fold_left(
       (fileDataList, filePath) => {
         let fileName = Path.basename_ext(filePath, ".re");
         [
           syncWithConfig(Path.join([|rootDir, filePath|]), {"cwd": globCwd})
           |> Array.to_list
           |> List.filter(filePath =>
                excludeList
                |> List.filter(exclude =>
                     filePath |> Js.String.includes(exclude)
                   )
                |> List.length === 0
              )
           |> List.map(filePath =>
                (
                  Path.basename_ext(filePath, ".re")
                  |> replaceAPIModuleNameFunc,
                  Path.basename_ext(filePath, ".re"),
                  Fs.readFileAsUtf8Sync(filePath) |> _findPublicFunctionList,
                )
              ),
           ...fileDataList,
         ];
       },
       [],
     )
  |> List.flatten
  |> _buildContent
  |> _writeToIndexFile(destDir);
};