let fromNotebook = notebookId => Db.getNotes(notebookId);
let get = id => Db.getNote(id);
let add = note => Db.addNote(note);
let create = (notebookId: string) => Db.createNote(notebookId);

let update = (note: Data.note, ~sync=true, ()) => {
  let now = Js.Date.fromFloat(Js.Date.now());
  Db.updateNote({...note, updatedAt: now}, ~sync, ());
};

let delete = (noteId: string, ~sync=true, ()) => Db.deleteNote(noteId, ~sync, ());

DataSync.setNoteSyncedListener(note =>
  Db.updateNote(note, ~sync=false, ())->Promise.map(_ => ())
); /* FIXME: error handling? */
