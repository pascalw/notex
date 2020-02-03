let fromNote = noteId => Db.getContentBlocks(noteId);
let get = id => Db.getContentBlock(id);
let add = block => Db.addContentBlock(block);

let update = (block, ~sync=true, ()) => {
  let now = Js.Date.fromFloat(Js.Date.now());

  Db.updateContentBlock({...block, updatedAt: now}, ~sync, ())
  ->Promise.tapOk(_ =>
       if (sync) {
         Db.touchNote(block.noteId) |> ignore;
       }
     );
};

let updateContentType = (block: Data.contentBlock, newContentType) => {
  let newContent =
    switch (block.content, newContentType) {
    | (Data.TextContent(text), "code") =>
      Data.CodeContent(Utils.htmlToText(RichText.toString(text)), "")
    | (Data.CodeContent(code, _), "text") =>
      Data.TextContent(Utils.textToHtml(code) |> RichText.fromString)
    | (Data.TextContent(_), "text") => block.content
    | (Data.CodeContent(_, _), "code") => block.content
    | _ => Js.Exn.raiseError("Unsupported content type change")
    };

  {...block, content: newContent};
};

let updateCodeLanguage = (block: Data.contentBlock, language) =>
  switch (block.content) {
  | Data.CodeContent(code, _) => {...block, content: Data.CodeContent(code, language)}
  | _ => Js.Exn.raiseError("Unsupported contentblock")
  };

let delete = id => Db.deleteContentBlock(id);

DataSync.setContentBlockSyncedListener(block =>
  Db.updateContentBlock(block, ~sync=false, ())->Promise.map(_ => ())
); /* FIXME: error handling? */
