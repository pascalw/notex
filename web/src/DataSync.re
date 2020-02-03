type changeValue =
  | ContentBlockCreated(Data.contentBlock)
  | ContentBlockUpdated(Data.contentBlock)
  | NoteCreated(Data.note)
  | NoteUpdated(Data.note)
  | NotebookCreated(Data.notebook)
  | NotebookUpdated(Data.notebook)
  | NotebookDeleted(string)
  | NoteDeleted(string);

type change = {
  id: string,
  change: changeValue,
};

let pendingChanges: ref(list(change)) = ref([]);
let retryQueue: ref(list(change)) = ref([]);

type syncedListener('a) = 'a => Promise.t(unit);

let noteSyncedListener: ref(option(syncedListener(Data.note))) = ref(None);
let notebookSyncedListener: ref(option(syncedListener(Data.notebook))) = ref(None);
let contentBlockSyncedListener: ref(option(syncedListener(Data.contentBlock))) = ref(None);

let setNoteSyncedListener = listener => noteSyncedListener := Some(listener);

let setNotebookSyncedListener = listener => notebookSyncedListener := Some(listener);

let setContentBlockSyncedListener = listener => contentBlockSyncedListener := Some(listener);

let notifyListener = (listener, resource) =>
  (
    switch (listener^) {
    | None => Promise.resolved()
    | Some(listener) => listener(resource)
    }
  )
  ->Promise.map(v => Belt.Result.Ok(v));

let notifyNoteSyncedListener = notifyListener(noteSyncedListener);
let notifyNotebookSyncedListener = notifyListener(notebookSyncedListener);
let notifyContentBlockSyncedListener = notifyListener(contentBlockSyncedListener);

type pendingChangesListener = int => unit;
let pendingChangesListener: ref(option(pendingChangesListener)) = ref(None);

let setPendingChangesListener = listener => pendingChangesListener := Some(listener);

let removePendingChangesListener = () => pendingChangesListener := None;

let notifyPendingChangesListener = pendingChanges => {
  /* FIXME: pendingChanges can currently contain duplicates because it consists of pending changes
     + the retry queue. */
  let uniqueCount =
    Belt.Set.String.fromArray(Belt.List.toArray(pendingChanges))->Belt.Set.String.size;

  switch (pendingChangesListener^) {
  | None => ()
  | Some(listener) => listener(uniqueCount)
  };
};

let syncChange = change =>
  switch (change.change) {
  | NoteCreated(note) => Api.createNote(note)->Promise.flatMapOk(notifyNoteSyncedListener)
  | NoteUpdated(note) => Api.updateNote(note)->Promise.flatMapOk(notifyNoteSyncedListener)
  | ContentBlockCreated(contentBlock) =>
    Api.createContentBlock(contentBlock)->Promise.flatMapOk(notifyContentBlockSyncedListener)
  | ContentBlockUpdated(contentBlock) =>
    Api.updateContentBlock(contentBlock)->Promise.flatMapOk(notifyContentBlockSyncedListener)
  | NotebookCreated(notebook) =>
    Api.createNotebook(notebook)->Promise.flatMapOk(notifyNotebookSyncedListener)
  | NotebookUpdated(notebook) =>
    Api.updateNotebook(notebook)->Promise.flatMapOk(notifyNotebookSyncedListener)
  | NotebookDeleted(notebookId) => Api.deleteNotebook(notebookId)->Promise.mapOk(ignore)
  | NoteDeleted(noteId) => Api.deleteNote(noteId)->Promise.mapOk(ignore)
  };

let storePendingChanges = () => {
  let pendingChangeIds =
    (pendingChanges^)->Belt.List.concat(retryQueue^)->Belt.List.map(change => change.id);

  DataSyncPersistence.store(pendingChangeIds);
  notifyPendingChangesListener(pendingChangeIds);
};

let removePendingChange = change => {
  pendingChanges := Belt.List.keep(pendingChanges^, pendingChange => pendingChange !== change);

  storePendingChanges();
};

let pushChangeToQueue = (queue, change) => {
  queue :=
    (queue^)
    ->Belt.List.keep(pendingChange => pendingChange.id != change.id)
    ->Belt.List.concat([change]);

  storePendingChanges();
};

let pushChange = change => pushChangeToQueue(pendingChanges, change);

let pushContentBlock = (contentBlock: Data.contentBlock) => {
  let id = "contentBlock:updated:" ++ contentBlock.id;
  let change = {id, change: ContentBlockUpdated(contentBlock)};

  pushChange(change);
};

let pushNewContentBlock = (contentBlock: Data.contentBlock) => {
  let id = "contentBlock:created:" ++ contentBlock.id;
  let change = {id, change: ContentBlockCreated(contentBlock)};

  pushChange(change);
};

let pushNewNote = (note: Data.note) => {
  let id = "note:created:" ++ note.id;
  let change = {id, change: NoteCreated(note)};

  pushChange(change);
};

let pushNoteChange = (note: Data.note) => {
  let id = "note:updated:" ++ note.id;
  let change = {id, change: NoteUpdated(note)};

  pushChange(change);
};

let pushNewNotebook = (notebook: Data.notebook) => {
  let id = "notebook:created:" ++ notebook.id;
  let change = {id, change: NotebookCreated(notebook)};

  pushChange(change);
};

let pushNotebookChange = (notebook: Data.notebook) => {
  let id = "notebook:updated:" ++ notebook.id;
  let change = {id, change: NotebookUpdated(notebook)};

  pushChange(change);
};

let pushNotebookDelete = (notebookId: string) => {
  let id = "notebook:deleted:" ++ notebookId;
  let change = {id, change: NotebookDeleted(notebookId)};

  pushChange(change);
};

let pushNoteDelete = (noteId: string) => {
  let id = "note:deleted:" ++ noteId;
  let change = {id, change: NoteDeleted(noteId)};

  pushChange(change);
};

let rec syncPendingChanges = onComplete => {
  let nextChange = Belt.List.take(pendingChanges^, 1);

  switch (nextChange) {
  | Some([change]) =>
    syncChange(change)
    ->Promise.get(result => {
         if (Belt.Result.isError(result)) {
           Js.Console.error2("Error syncing: ", result);
           pushChangeToQueue(retryQueue, change);
         };

         removePendingChange(change);
         syncPendingChanges(onComplete);
       })
  | _ => onComplete()
  };
};

let retryFailed = () => {
  Belt.List.forEach(retryQueue^, pushChange);
  retryQueue := [];
};

let start = persistedChanges => {
  Belt.List.forEach(persistedChanges, pushChange);

  let rec onComplete = () =>
    Js.Global.setTimeout(() => syncPendingChanges(onComplete), 3_000) |> ignore;

  syncPendingChanges(onComplete);
  Js.Global.setInterval(retryFailed, 10_000) |> ignore;
};
