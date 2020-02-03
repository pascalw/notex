open Belt;

let upsertContentBlock = (contentBlock: Data.contentBlock) =>
  ContentBlocks.get(contentBlock.id)
  ->Promise.flatMap(storedContentBlock =>
       switch (storedContentBlock) {
       | None => ContentBlocks.add(contentBlock)
       | Some(_contentBlock) => ContentBlocks.update(contentBlock, ~sync=false, ())
       }
     );

let upsertNote = (note: Data.note) =>
  Notes.get(note.id)
  ->Promise.flatMap(storedNote =>
       switch (storedNote) {
       | None => Notes.add(note)
       | Some(_note) => Notes.update(note, ~sync=false, ())
       }
     );

let upsertNotebook = (notebook: Data.notebook) =>
  Notebooks.get(notebook.id)
  ->Promise.flatMap(storedNotebook =>
       switch (storedNotebook) {
       | None => Notebooks.add(notebook)
       | Some(_note) => Notebooks.update(notebook, ~sync=false, ())
       }
     );

let run = () =>
  Db.getRevision()
  ->Promise.flatMap(Api.fetchChanges)
  ->Promise.get((result: Belt.Result.t(Api.apiResponse, _)) =>
       switch (result) {
       | Result.Ok(result) =>
         let revision = result.revision;
         Db.insertRevision(revision) |> ignore;

         let notebookResults = result.changes.notebooks->List.map(upsertNotebook);
         let noteResults = result.changes.notes->List.map(upsertNote);
         let contentBlockResults = result.changes.contentBlocks->List.map(upsertContentBlock);
         let deletionResults =
           result.deletions
           ->List.map(deletedResource =>
               switch (deletedResource.type_) {
               | "notebook" => Notebooks.delete(deletedResource.id, ~sync=false, ())
               | "note" => Notes.delete(deletedResource.id, ~sync=false, ())
               | "contentBlock" => ContentBlocks.delete(deletedResource.id)
               | type_ => Js.Exn.raiseError("Unsupported deletion type: " ++ type_)
               }
             );

         let promise =
           Promise.all(
             List.concatMany([|
               notebookResults,
               noteResults,
               contentBlockResults,
               deletionResults,
             |]),
           );
         Db.withPromiseNotification(promise);
       | Result.Error(reason) => Js.Console.error2("Failed to fetch changes", reason)
       }
     );
