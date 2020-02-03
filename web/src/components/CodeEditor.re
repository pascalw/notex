[%bs.raw {|require('codemirror/lib/codemirror.css')|}];
[%bs.raw {|require('codemirror/theme/material.css')|}];
open Utils.Import;

type cmMode = {
  name: string,
  install: unit => Js.Promise.t(unit),
};

let mode = (name, install) => {
  {name, install};
};

module SupportedLanguageMap = Belt.Map.String;

let supportedLanguages: SupportedLanguageMap.t(cmMode) =
  SupportedLanguageMap.(
    empty
    ->set("go", mode("Go", () => import("codemirror/mode/ruby/ruby")))
    ->set("python", mode("Python", () => import("codemirror/mode/python/python")))
    ->set("css", mode("CSS", () => import("codemirror/mode/css/css")))
    ->set("clojure", mode("Clojure", () => import("codemirror/mode/clojure/clojure")))
    ->set("gherkin", mode("Gherkin", () => import("codemirror/mode/gherkin/gherkin")))
    ->set("dart", mode("Dart", () => import("codemirror/mode/dart/dart")))
    ->set("shell", mode("Shell", () => import("codemirror/mode/shell/shell")))
    ->set("markdown", mode("Markdown", () => import("codemirror/mode/markdown/markdown")))
    ->set("perl", mode("Perl", () => import("codemirror/mode/perl/perl")))
    ->set("rust", mode("Rust", () => import("codemirror/mode/rust/rust")))
    ->set("rst", mode("reStructuredText", () => import("codemirror/mode/rst/rst")))
    ->set("erlang", mode("Erlang", () => import("codemirror/mode/erlang/erlang")))
    ->set("elixir", mode("Elixir", () => import("codemirror-mode-elixir")))
    ->set("elm", mode("Elm", () => import("codemirror/mode/elm/elm")))
    ->set("crystal", mode("Crystal", () => import("codemirror/mode/crystal/crystal")))
    ->set("html", mode("HTML", () => import("codemirror/mode/htmlmixed/htmlmixed")))
    ->set("haskell", mode("Haskell", () => import("codemirror/mode/haskell/haskell")))
    ->set("php", mode("PHP", () => import("codemirror/mode/php/php")))
    ->set("lua", mode("Lua", () => import("codemirror/mode/lua/lua")))
    ->set("xml", mode("XML", () => import("codemirror/mode/xml/xml")))
    ->set(
        "powershell",
        mode("Powershell", () => import("codemirror/mode/powershell/powershell")),
      )
    ->set("swift", mode("Swift", () => import("codemirror/mode/swift/swift")))
    ->set("yaml", mode("YAML", () => import("codemirror/mode/yaml/yaml")))
    ->set("groovy", mode("Groovy", () => import("codemirror/mode/groovy/groovy")))
    ->set("java", mode("Java", () => import("codemirror/mode/clike/clike")))
    ->set(
        "javascript",
        mode("JavaScript", () => import("codemirror/mode/javascript/javascript")),
      )
    ->set("sass", mode("Sass", () => import("codemirror/mode/sass/sass")))
    ->set("ruby", mode("Ruby", () => import("codemirror/mode/ruby/ruby")))
    ->set("sql", mode("SQL", () => import("codemirror/mode/sql/sql")))
  );

[@bs.module "react-codemirror2"] external codeMirrorReact: ReasonReact.reactClass = "Controlled";

let code = (block: Data.contentBlock) =>
  switch (block.content) {
  | Data.CodeContent(code, _language) => code
  | _ => Js.Exn.raiseError("Unsupported content block")
  };

let language = (block: Data.contentBlock) =>
  switch (block.content) {
  | Data.CodeContent(_code, language) when language != "" => language
  | _ => "unknown"
  };

module CodeMirror = {
  module InputField = {
    type t;
    [@bs.send] external blur: t => unit = "";
  };

  module Editor = {
    type t;
    [@bs.send] external setOption: (t, string, string) => unit = "";
    [@bs.send] external getInputField: t => InputField.t = "";
  };

  [@bs.deriving abstract]
  type cmOptions = {
    mode: string,
    theme: string,
    lineNumbers: bool,
    lineWrapping: bool,
  };

  [@bs.deriving abstract]
  type jsProps = {
    value: string,
    options: cmOptions,
    onBeforeChange: (string, string, string) => unit,
    editorDidMount: Editor.t => unit,
    onKeyDown: (Editor.t, ReactEvent.Keyboard.t) => unit,
  };

  let theme =
    switch (AppState.getTheme()) {
    | Some(AppState.Theme.Dark) => "material"
    | _ => "default"
    };

  let options = cmOptions(~theme, ~lineNumbers=true, ~lineWrapping=true);

  let make =
      (~mode: string, ~code: string, ~editorDidMount, ~onBeforeChange, ~onKeyDown, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=codeMirrorReact,
      ~props=
        jsProps(
          ~value=code,
          ~options=options(~mode),
          ~onBeforeChange,
          ~editorDidMount,
          ~onKeyDown,
        ),
      children,
    );
};

module CodeMirrorWrapper = {
  type state = {editor: option(CodeMirror.Editor.t)};

  type action =
    | SetEditor(CodeMirror.Editor.t);

  let onKeyDown = (editor, event) =>
    if (ReactEvent.Keyboard.key(event) == "Escape") {
      editor->CodeMirror.Editor.getInputField->CodeMirror.InputField.blur;
    };

  let mapMode = language =>
    switch (language) {
    | "sql" => "text/x-sql"
    | "php" => "text/x-php"
    | "java" => "text/x-java"
    | "html" => "text/html"
    | _ => language
    };

  let loadMode = (contentBlock, editor) => {
    let language = language(contentBlock);
    let mode = SupportedLanguageMap.getExn(supportedLanguages, language);

    mode.install()
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.tapOk(_ => CodeMirror.Editor.setOption(editor, "mode", mapMode(language)));
  };

  let component = ReasonReact.reducerComponent("CodeMirrorWrapper");
  let make = (~contentBlock: Data.contentBlock, ~onChange, _children) => {
    ...component,
    initialState: () => {editor: None},
    reducer: (action: action, _state: state) =>
      switch (action) {
      | SetEditor(editor) => ReasonReact.Update({editor: Some(editor)})
      },
    didUpdate: oldAndNewSelf =>
      switch (oldAndNewSelf.newSelf.state.editor) {
      | None => ()
      | Some(editor) => loadMode(contentBlock, editor) |> ignore
      },
    render: self => {
      let code = code(contentBlock);
      let language = language(contentBlock);

      <CodeMirror
        mode={mapMode(language)}
        code
        onBeforeChange=onChange
        editorDidMount={editor => self.send(SetEditor(editor))}
        onKeyDown
      />;
    },
  };
};

let typeOptions = () =>
  <optgroup label="Code">
    {SupportedLanguageMap.toList(supportedLanguages)
     ->Belt.List.map(((id, mode)) =>
         <option key=id value=id> {ReasonReact.string(mode.name)} </option>
       )
     ->Belt.List.toArray
     ->ReasonReact.array}
  </optgroup>;

let component = ReasonReact.statelessComponent("CodeEditor");
let make = (~contentBlock: Data.contentBlock, ~onChange, _children) => {
  let onChange = (_, _, value) => onChange(value);

  {...component, render: _self => <CodeMirrorWrapper contentBlock onChange />};
};
