let all = () => Db.getNotebooks();
let get = id => Db.getNotebook(id);
let add = notebooks => Db.addNotebooks(notebooks);
let create = () => Db.createNotebook();
let update = (notebook: Data.notebook, ~sync=true, ()) =>
  Db.updateNotebook(notebook, ~sync, ());
let delete = (id, ~sync=true, ()) => Db.deleteNotebook(id, ~sync, ());

DataSync.setNotebookSyncedListener(notebook =>
  update(notebook, ~sync=false, ())
);
