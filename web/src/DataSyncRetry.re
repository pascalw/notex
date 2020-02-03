let optionToResult = option =>
  switch (option) {
  | None => Belt.Result.Error()
  | Some(value) => Belt.Result.Ok(value)
  };

let getPendingChanges = () => {
  let changeIds = DataSyncPersistence.getStoredChangeIds();

  let promises =
    Belt.List.map(
      changeIds,
      changeId => {
        let change =
          switch (Js.String.split(":", changeId)) {
          | [|"contentBlock", "updated", id|] =>
            ContentBlocks.get(id)->Promise.mapSome(cb => DataSync.ContentBlockUpdated(cb))

          | [|"contentBlock", "created", id|] =>
            ContentBlocks.get(id)->Promise.mapSome(cb => DataSync.ContentBlockCreated(cb))

          | [|"note", "created", id|] =>
            Notes.get(id)->Promise.mapSome(note => DataSync.NoteCreated(note))

          | [|"note", "updated", id|] =>
            Notes.get(id)->Promise.mapSome(note => DataSync.NoteUpdated(note))

          | [|"note", "deleted", id|] => Promise.resolved(Some(DataSync.NoteDeleted(id)))

          | [|"notebook", "created", id|] =>
            Notebooks.get(id)->Promise.mapSome(notebook => DataSync.NotebookCreated(notebook))

          | [|"notebook", "updated", id|] =>
            Notebooks.get(id)->Promise.mapSome(notebook => DataSync.NotebookUpdated(notebook))

          | [|"notebook", "deleted", id|] =>
            Promise.resolved(Some(DataSync.NotebookDeleted(id)))

          | _ =>
            Js.Console.error2("Unknown change id:", changeId);
            Promise.resolved(None);
          };

        Promise.mapSome(change, change => {DataSync.id: changeId, change});
      },
    );

  promises->Promise.all->Promise.map(result => Belt.List.keepMap(result, item => item));
};
