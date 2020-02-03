exception UnknownContentType(string);

type resource = {
  id: string,
  type_: string,
};

type changes = {
  notebooks: list(Data.notebook),
  notes: list(Data.note),
  contentBlocks: list(Data.contentBlock),
};

type apiResponse = {
  revision: string,
  changes,
  deletions: list(resource),
};

module JsonCoders = {
  let encodeNotebook = (notebook: Data.notebook) =>
    Json.Encode.(
      object_([
        ("id", string(notebook.id)),
        ("title", string(notebook.title)),
        ("createdAt", date(notebook.createdAt)),
        ("updatedAt", date(notebook.updatedAt)),
        ("revision", nullable(string, notebook.revision)),
      ])
    );

  let decodeNotebook = (json): Data.notebook =>
    Json.Decode.{
      id: json |> field("id", string),
      title: json |> field("title", string),
      createdAt: json |> field("createdAt", date),
      updatedAt: json |> field("updatedAt", date),
      revision: json |> optional(field("revision", string)),
    };

  let encodeNote = (note: Data.note) =>
    Json.Encode.(
      object_([
        ("id", string(note.id)),
        ("notebookId", string(note.notebookId)),
        ("title", string(note.title)),
        ("tags", jsonArray(note.tags |> List.map(string) |> Array.of_list)),
        ("createdAt", date(note.createdAt)),
        ("updatedAt", date(note.updatedAt)),
        ("revision", nullable(string, note.revision)),
      ])
    );

  let decodeNote = (json): Data.note =>
    Json.Decode.{
      id: json |> field("id", string),
      notebookId: json |> field("notebookId", string),
      title: json |> field("title", string),
      tags: json |> field("tags", list(string)),
      createdAt: json |> field("createdAt", date),
      updatedAt: json |> field("updatedAt", date),
      revision: json |> optional(field("revision", string)),
    };

  let textContent = json => {
    let text = json |> Json.Decode.field("text", Json.Decode.string);
    Data.TextContent(RichText.fromString(text));
  };

  let codeContent = json => {
    let code = json |> Json.Decode.field("code", Json.Decode.string);
    let language = json |> Json.Decode.field("language", Json.Decode.string);

    Data.CodeContent(code, language);
  };

  let decodeContent = json => {
    let type_ = json |> Json.Decode.field("type", Json.Decode.string);

    switch (type_) {
    | "text" => json |> Json.Decode.field("data", textContent)
    | "code" => json |> Json.Decode.field("data", codeContent)
    | other => UnknownContentType(other)->raise
    };
  };

  let decodeContentBlock = (json): Data.contentBlock =>
    Json.Decode.{
      id: json |> field("id", string),
      noteId: json |> field("noteId", string),
      content: json |> field("content", decodeContent),
      createdAt: json |> field("createdAt", date),
      updatedAt: json |> field("updatedAt", date),
      revision: json |> optional(field("revision", string)),
    };

  let encodeContentBlock = (contentBlock: Data.contentBlock) => {
    let type_ = (content: Data.content) =>
      switch (content) {
      | Data.TextContent(_text) => "text"
      | Data.CodeContent(_code, _language) => "code"
      };

    let data = (content: Data.content) =>
      switch (content) {
      | TextContent(richText) =>
        Json.Encode.(object_([("text", string(RichText.toString(richText)))]))
      | CodeContent(code, language) =>
        Json.Encode.(object_([("code", string(code)), ("language", string(language))]))
      };

    let content = (content: Data.content) =>
      Json.Encode.(object_([("type", string(type_(content))), ("data", data(content))]));

    Json.Encode.(
      object_([
        ("id", string(contentBlock.id)),
        ("noteId", string(contentBlock.noteId)),
        ("content", content(contentBlock.content)),
        ("createdAt", date(contentBlock.createdAt)),
        ("updatedAt", date(contentBlock.updatedAt)),
        ("revision", nullable(string, contentBlock.revision)),
      ])
    );
  };

  let decodeChanges = json =>
    Json.Decode.{
      notebooks: json |> field("notebooks", list(decodeNotebook)),
      notes: json |> field("notes", list(decodeNote)),
      contentBlocks: json |> field("contentBlocks", list(decodeContentBlock)),
    };

  let decodeResource = json =>
    Json.Decode.{id: json |> field("id", string), type_: json |> field("type", string)};

  let decodeChangesResponse = json =>
    Json.Decode.{
      revision: json |> field("revision", string),
      changes: json |> field("changes", decodeChanges),
      deletions: json |> field("deletions", list(decodeResource)),
    };
};

let fetchUrl = (revision: option(string)) =>
  switch (revision) {
  | Some(revision) => "/api/data?since_revision=" ++ revision
  | None => "/api/data"
  };

let authHeader = token => {"Authorization": "bearer " ++ token};

let headers = () => {
  "Content-Type": "application/json",
  "Authorization": "bearer " ++ Belt.Option.getExn(Auth.getToken()),
};

let toResult =
    (mapper: Fetch.Response.t => Js.Promise.t('b), promise)
    : Promise.t(Belt.Result.t('b, Js.Promise.error)) =>
  promise
  |> Js.Promise.then_(response =>
       if (!Fetch.Response.ok(response)) {
         Js.Promise.reject(
           Js.Exn.raiseError("Request failed with " ++ Fetch.Response.statusText(response)),
         );
       } else {
         Js.Promise.resolve(response);
       }
     )
  |> Js.Promise.then_(mapper)
  |> Promise.Js.fromBsPromise
  |> Promise.Js.toResult;

let toJsonResult = (mapper: Js.Json.t => 'a, promise) =>
  (promise |> toResult(Fetch.Response.json))->Promise.mapOk(mapper);

let fetchChanges =
    (revision: option(string)): Promise.t(Belt.Result.t(apiResponse, Js.Promise.error)) =>
  Fetch.fetchWithInit(
    fetchUrl(revision),
    Fetch.RequestInit.make(~method_=Get, ~headers=Fetch.HeadersInit.make(headers()), ()),
  )
  |> toJsonResult(JsonCoders.decodeChangesResponse);

let createNote = (note: Data.note): Promise.t(Belt.Result.t(Data.note, Js.Promise.error)) => {
  let json = JsonCoders.encodeNote(note);

  Fetch.fetchWithInit(
    "/api/notes",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeNote);
};

let createNotebook = (notebook: Data.notebook) => {
  let json = JsonCoders.encodeNotebook(notebook);

  Fetch.fetchWithInit(
    "/api/notebooks",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeNotebook);
};

let updateNotebook = (notebook: Data.notebook) => {
  let json = JsonCoders.encodeNotebook(notebook);

  Fetch.fetchWithInit(
    "/api/notebooks/" ++ notebook.id,
    Fetch.RequestInit.make(
      ~method_=Put,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeNotebook);
};

let deleteNotebook =
    (notebookId: string): Promise.t(Belt.Result.t(Fetch.Response.t, Js.Promise.error)) =>
  Fetch.fetchWithInit(
    "/api/notebooks/" ++ notebookId,
    Fetch.RequestInit.make(~method_=Delete, ~headers=Fetch.HeadersInit.make(headers()), ()),
  )
  |> toResult(Js.Promise.resolve);

let updateNote = (note: Data.note) => {
  let json = JsonCoders.encodeNote(note);

  Fetch.fetchWithInit(
    "/api/notes/" ++ note.id,
    Fetch.RequestInit.make(
      ~method_=Put,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeNote);
};

let deleteNote = (noteId: string) =>
  Fetch.fetchWithInit(
    "/api/notes/" ++ noteId,
    Fetch.RequestInit.make(~method_=Delete, ~headers=Fetch.HeadersInit.make(headers()), ()),
  )
  |> toResult(Js.Promise.resolve);

let createContentBlock = (contentBlock: Data.contentBlock) => {
  let json = JsonCoders.encodeContentBlock(contentBlock);

  Fetch.fetchWithInit(
    "/api/content_blocks",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeContentBlock);
};

let updateContentBlock = (contentBlock: Data.contentBlock) => {
  let json = JsonCoders.encodeContentBlock(contentBlock);

  Fetch.fetchWithInit(
    "/api/content_blocks/" ++ contentBlock.id,
    Fetch.RequestInit.make(
      ~method_=Put,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(json)),
      ~headers=Fetch.HeadersInit.make(headers()),
      (),
    ),
  )
  |> toJsonResult(JsonCoders.decodeContentBlock);
};

let checkAuth = token =>
  Fetch.fetchWithInit(
    "/api/auth",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~headers=Fetch.HeadersInit.make(authHeader(token)),
      (),
    ),
  )
  |> toResult(Js.Promise.resolve);
