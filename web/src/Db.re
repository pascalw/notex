open Belt.Result;
type result('a) = Belt.Result.t('a, Js.Promise.error);

type listener = unit => unit;

module JsonCoders = {
  /* Notebooks */
  let decodeNotebook = (json): Data.notebook =>
    Json.Decode.{
      id: json |> field("id", string),
      title: json |> field("title", string),
      createdAt: json |> field("createdAt", date),
      updatedAt: json |> field("updatedAt", date),
      revision: json |> optional(field("revision", string)),
    };

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

  /* Notes */
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

  /* ContentBlocks */
  let decodeContentBlock = (json): Data.contentBlock => {
    let textContent = json => {
      let text = json |> Json.Decode.field("text", Json.Decode.string);
      Data.TextContent(RichText.fromString(text));
    };

    let codeContent = json => {
      let code = json |> Json.Decode.field("code", Json.Decode.string);
      let language = json |> Json.Decode.field("language", Json.Decode.string);

      Data.CodeContent(code, language);
    };

    let content = json => {
      let type_ = json |> Json.Decode.field("type", Json.Decode.string);

      switch (type_) {
      | "text" => json |> Json.Decode.field("data", textContent)
      | "code" => json |> Json.Decode.field("data", codeContent)
      | _other => raise(Not_found)
      };
    };

    Json.Decode.{
      id: json |> field("id", string),
      noteId: json |> field("noteId", string),
      content: json |> field("content", content),
      createdAt: json |> field("createdAt", date),
      updatedAt: json |> field("updatedAt", date),
      revision: json |> optional(field("revision", string)),
    };
  };

  let encodeContentBlock = (contentBlock: Data.contentBlock) => {
    let type_ = content =>
      switch (content) {
      | Data.TextContent(_text) => "text"
      | Data.CodeContent(_code, _language) => "code"
      };

    let data = content =>
      switch (content) {
      | Data.TextContent(richText) =>
        Json.Encode.(object_([("text", string(RichText.toString(richText)))]))
      | Data.CodeContent(code, language) =>
        Json.Encode.(object_([("code", string(code)), ("language", string(language))]))
      };

    let content = content =>
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
};

let notebooksStore = "notebooks";
let notesStore = "notes";
let contentBlocksStore = "contentBlocks";

let iDb: ref(option(IndexedDB.DB.t)) = ref(None);
let initDb = () => {
  let currentVersion = 2;
  IndexedDB.open_(
    "pragma",
    currentVersion,
    upgradeDb => {
      let oldVersion = IndexedDB.UpgradeDb.oldVersion(upgradeDb);
      DbMigrations.runMigrations(upgradeDb, oldVersion, currentVersion);
    },
  )
  ->Promise.Js.fromBsPromise
  ->Promise.Js.toResult;
};

let dbPromise = () =>
  switch (iDb^) {
  | None =>
    initDb()
    ->Promise.map(result => {
         let db = Belt.Result.getExn(result);

         iDb := Some(db);
         db;
       })
  | Some(db) => Promise.resolved(db)
  };

let toOptionPromise = (jsPromise: Js.Promise.t(option('a))) =>
  jsPromise
  ->Promise.Js.fromBsPromise
  ->Promise.Js.map(
       fun
       | None => None
       | Some(value) => Some(value),
     )
  ->Promise.Js.catch(error => {
       Js.Console.error2("Db promise failed: ", error);
       Promise.resolved(None);
     });

let awaitTransactionPromise = (transaction: IndexedDB.Transaction.t) =>
  IndexedDB.Transaction.complete(transaction)
  ->Promise.Js.fromBsPromise
  ->Promise.Js.toResult;

let listeners: ref(list(listener)) = ref([]);

let subscribe = listener => listeners := [listener, ...listeners^];

let unsubscribe = listener => listeners := Belt.List.keep(listeners^, l => l !== listener);

let getNotes = (notebookId: string) =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notesStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notesStore)
         ->ObjectStore.index("forNotebook")
         ->IndexedDB.Index.getAllByKey(notebookId)
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.map(
              fun
              | Ok(array) => array->Belt.List.fromArray->Belt.List.map(JsonCoders.decodeNote)
              | Error(_) => [],
            )
       )
     );

let getRecentNotes = limit =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         (DB.transaction(db, notesStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notesStore)
         ->ObjectStore.index("byUpdatedAt")
         ->IndexedDB.Index.openCursor(Cursor.Prev)
         |> Js.Promise.then_(Cursor.take(_, limit, [||])))
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.map(
              fun
              | Ok(array) => array->Belt.List.fromArray->Belt.List.map(JsonCoders.decodeNote)
              | Error(_) => [],
            )
       )
     );

let getRecentNotesCount = () =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notesStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notesStore)
         ->ObjectStore.index("byUpdatedAt")
         ->IndexedDB.Index.count
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.map(Belt.Result.getWithDefault(_, 0))
       )
     );

let countNotes = (notebookId: string) =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notesStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notesStore)
         ->ObjectStore.index("forNotebook")
         ->IndexedDB.Index.countByKey(notebookId)
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.map(Belt.Result.getWithDefault(_, 0))
       )
     );

let getNote = (noteId: string) =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notesStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notesStore)
         ->ObjectStore.get(noteId)
         ->toOptionPromise
         ->Promise.map(v => Belt.Option.map(v, JsonCoders.decodeNote))
       )
     );

let getNotebooks = () =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notebooksStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notebooksStore)
         ->ObjectStore.getAll
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.flatMap(
              fun
              | Ok(array) =>
                array
                ->Belt.List.fromArray
                ->Belt.List.map(JsonCoders.decodeNotebook)
                ->Belt.List.map(n => countNotes(n.id)->Promise.map(count => (n, count)))
                ->Promise.all
              | Error(_) => Promise.resolved([]),
            )
       )
     );

let getNotebook = (notebookId: string) =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, notebooksStore, Transaction.ReadOnly)
         ->Transaction.objectStore(notebooksStore)
         ->ObjectStore.get(notebookId)
         ->toOptionPromise
         ->Promise.map(v => Belt.Option.map(v, JsonCoders.decodeNotebook))
       )
     );

let getContentBlocks = noteId =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, contentBlocksStore, Transaction.ReadOnly)
         ->Transaction.objectStore(contentBlocksStore)
         ->ObjectStore.index("forNote")
         ->IndexedDB.Index.getAllByKey(noteId)
         ->Promise.Js.fromBsPromise
         ->Promise.Js.toResult
         ->Promise.map(
              fun
              | Ok(array) =>
                array->Belt.List.fromArray->Belt.List.map(JsonCoders.decodeContentBlock)
              | Error(_) => [],
            )
       )
     );

let getContentBlock = blockId =>
  dbPromise()
  ->Promise.flatMap(db =>
       IndexedDB.(
         DB.transaction(db, contentBlocksStore, Transaction.ReadOnly)
         ->Transaction.objectStore(contentBlocksStore)
         ->ObjectStore.get(blockId)
         ->toOptionPromise
         ->Promise.map(v => Belt.Option.map(v, JsonCoders.decodeContentBlock))
       )
     );

let addNotebook = notebook =>
  IndexedDB.(
    dbPromise()
    ->Promise.flatMap(db => {
         let tx = DB.transaction(db, notebooksStore, Transaction.ReadWrite);
         let data = JsonCoders.encodeNotebook(notebook);

         Transaction.objectStore(tx, notebooksStore)->ObjectStore.put(data)->ignore;

         awaitTransactionPromise(tx);
       })
  );

let addNote = note =>
  IndexedDB.(
    dbPromise()
    ->Promise.flatMap(db => {
         let tx = DB.transaction(db, notesStore, Transaction.ReadWrite);
         let data = JsonCoders.encodeNote(note);

         Transaction.objectStore(tx, notesStore)->ObjectStore.put(data)->ignore;

         awaitTransactionPromise(tx);
       })
  );

let addContentBlock = block =>
  IndexedDB.(
    dbPromise()
    ->Promise.flatMap(db => {
         let tx = DB.transaction(db, contentBlocksStore, Transaction.ReadWrite);
         let data = JsonCoders.encodeContentBlock(block);

         Transaction.objectStore(tx, contentBlocksStore)->ObjectStore.put(data)->ignore;

         awaitTransactionPromise(tx);
       })
  );

let createNote = (notebookId: string) => {
  let now = Js.Date.fromFloat(Js.Date.now());

  let note: Data.note = {
    id: Utils.generateId(),
    notebookId,
    title: "Untitled note",
    tags: [],
    createdAt: now,
    updatedAt: now,
    revision: None,
  };

  let contentBlock: Data.contentBlock = {
    id: Utils.generateId(),
    noteId: note.id,
    content: Data.TextContent(RichText.create()),
    createdAt: now,
    updatedAt: now,
    revision: None,
  };

  Promise.all([addNote(note), addContentBlock(contentBlock)])
  ->Promise.map(_ => {
       /* FIXME: only push when succesful */
       DataSync.pushNewNote(note);
       DataSync.pushNewContentBlock(contentBlock);

       Ok((note, contentBlock));
     });
};

let createNotebook = notebook =>
  addNotebook(notebook)
  ->Promise.map(_result => {
       /* FIXME: only push when create is succesfull */
       DataSync.pushNewNotebook(notebook) |> ignore;
       Ok(notebook);
     });

let updateContentBlock = (contentBlock: Data.contentBlock, ~sync=true, ()) =>
  addContentBlock(contentBlock)
  ->Promise.map(result => {
       /* FIXME: only push when create is succesfull */
       if (sync) {
         DataSync.pushContentBlock(contentBlock);
       };

       result;
     });

let updateNote = (note: Data.note, ~sync=true, ()) =>
  addNote(note)
  ->Promise.map(result => {
       /* FIXME: only push when create is succesfull */
       if (sync) {
         DataSync.pushNoteChange(note);
       };

       result;
     });

let updateNotebook = (notebook: Data.notebook, ~sync=true, ()) =>
  addNotebook(notebook)
  ->Promise.map(result => {
       /* FIXME: only push when create is succesfull */
       if (sync) {
         DataSync.pushNotebookChange(notebook);
       };

       result;
     });

let deleteNotebook = (notebookId: string, ~sync=true, ()) =>
  dbPromise()
  ->Promise.flatMap(db => {
       open IndexedDB;
       let tx = DB.transaction(db, notebooksStore, Transaction.ReadWrite);

       Transaction.objectStore(tx, notebooksStore)
       ->ObjectStore.delete(notebookId)
       ->Promise.Js.fromBsPromise
       ->Promise.Js.toResult
     })
  ->Promise.map(_result => {
       /* FIXME: only push when succesfull */
       if (sync) {
         DataSync.pushNotebookDelete(notebookId);
       };

       Ok();
     });

let deleteNote = (noteId: string, ~sync=true, ()) =>
  dbPromise()
  ->Promise.flatMap(db => {
       open IndexedDB;
       let tx = DB.transaction(db, notesStore, Transaction.ReadWrite);

       Transaction.objectStore(tx, notesStore)
       ->ObjectStore.delete(noteId)
       ->Promise.Js.fromBsPromise
       ->Promise.Js.toResult
     })
  ->Promise.map(_result => {
       /* FIXME: only push when succesfull */
       if (sync) {
         DataSync.pushNoteDelete(noteId);
       };

       Ok();
     });

let deleteContentBlock = (contentBlockId: string) =>
  dbPromise()
  ->Promise.flatMap(db => {
       open IndexedDB;
       let tx = DB.transaction(db, contentBlocksStore, Transaction.ReadWrite);

       Transaction.objectStore(tx, contentBlocksStore)
       ->ObjectStore.delete(contentBlockId)
       ->Promise.Js.fromBsPromise
       ->Promise.Js.toResult
     })
  ->Promise.map(_result
       /* FIXME: only push when succesfull */
       => Ok());

let insertRevision = (revision: string) =>
  LocalStorage.setItem("pragma-revision", revision)->Promise.resolved;

let getRevision = () => LocalStorage.getItem("pragma-revision")->Promise.resolved;

let withNotification = fn => {
  let result = fn();
  Belt.List.forEach(listeners^, l => l());

  result;
};

let withPromiseNotification = promise =>
  promise->Promise.get(_ => Belt.List.forEach(listeners^, l => l()));

let clear = () => {
  IndexedDB.delete("pragma") |> ignore;
  LocalStorage.clear();
};

let touchNote = noteId => {
  let now = Js.Date.fromFloat(Js.Date.now());

  getNote(noteId)
  ->Promise.mapSome((note: Data.note) => {...note, updatedAt: now})
  ->Promise.flatMap(maybeNote =>
       switch (maybeNote) {
       | None => Promise.resolved(Belt.Result.Error())
       | Some(note) => updateNote(note, ())->Promise.map(Results.mapError(_, _ => ()))
       }
     );
};
