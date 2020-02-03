module NoteCollection = UiTypes.NoteCollection.Collection;
module CollectionKind = UiTypes.NoteCollection.CollectionKind;

type state = {editingTitleId: option(string)};

type action =
  | EditTitle(option(string));

let component = ReasonReact.reducerComponent("Notebooks");
let make =
    (
      ~dispatch,
      ~notebooks: list((Data.notebook, int)),
      ~noteCollections: list(NoteCollection.t),
      ~selectedCollection: option(string),
      ~hidden,
      _children,
    ) => {
  let createNotebook = (send, _) => {
    let notebook = Data.newNotebook();

    let promise =
      Notebooks.create(notebook)
      ->Promise.tapOk(_ => {
           let collection = UiTypes.NoteCollection.fromNotebook(notebook);
           dispatch(NoteManagementContainer.SelectNotebook(collection));
           send(EditTitle(Some(notebook.id)));
         });

    Db.withPromiseNotification(promise);
  };

  let getCollection =
      (kind: CollectionKind.t, collections: list(NoteCollection.t)): option(NoteCollection.t) => {
    Utils.find(collections, c => c.kind == kind);
  };

  let renderCollection: option(NoteCollection.t) => ReasonReact.reactElement =
    fun
    | Some({kind: CollectionKind.Recents} as collection) when collection.noteCount > 1 => {
        let id = NoteCollection.id(collection);
        let title = NoteCollection.name(collection);
        let icon =
          switch (collection.kind) {
          | CollectionKind.Recents => Icon.Recent
          };

        <ListView.Item
          key=id
          selected={Some(id) == selectedCollection}
          onClick={_ =>
            dispatch(
              NoteManagementContainer.SelectNotebook(
                UiTypes.NoteCollection.fromCollection(collection),
              ),
            )
          }>
          <ListView.ItemContent title count={collection.noteCount} icon />
        </ListView.Item>;
      }
    | Some(_)
    | None => ReasonReact.null;

  {
    ...component,
    initialState: () => {editingTitleId: None},
    reducer: (action: action, _state: state) =>
      switch (action) {
      | EditTitle(id) => ReasonReact.Update({editingTitleId: id})
      },
    render: self => {
      let renderListItemContent = (notebook: Data.notebook, noteCount: int) =>
        if (Some(notebook.id) == self.state.editingTitleId) {
          <p>
            <NotebookTitleEditor
              value={notebook.title}
              onComplete={title => {
                let updatedNotebook = {...notebook, title};

                self.send(EditTitle(None));
                dispatch(NoteManagementContainer.UpdateNotebook(updatedNotebook));
              }}
            />
          </p>;
        } else {
          <ListView.ItemContent title={notebook.title} count=noteCount />;
        };

      <ListView hidden>
        <ListView.ItemContainer>
          {CollectionKind.Recents->getCollection(noteCollections)->renderCollection}
        </ListView.ItemContainer>
        /* spacer */
        <ListView.ItemContainer>
          {Belt.List.map(notebooks, ((notebook, noteCount)) =>
             <ListView.Item
               key={notebook.id}
               selected={Some(notebook.id) == selectedCollection}
               onClick={_ =>
                 dispatch(
                   NoteManagementContainer.SelectNotebook(
                     UiTypes.NoteCollection.fromNotebook(notebook),
                   ),
                 )
               }
               onDoubleClick={_ => self.send(EditTitle(Some(notebook.id)))}
               onLongpress={_ =>
                 if (WindowRe.confirm(
                       "Are you sure you want to delete this notebook?",
                       Webapi.Dom.window,
                     )) {
                   dispatch(NoteManagementContainer.DeleteNotebook(notebook));
                 }
               }>
               {renderListItemContent(notebook, noteCount)}
             </ListView.Item>
           )
           |> Belt.List.toArray
           |> ReasonReact.array}
        </ListView.ItemContainer>
        <ListView.Footer>
          <SyncStateContainer />
          <AddButton onClick={createNotebook(self.send)} />
        </ListView.Footer>
      </ListView>;
    },
  };
};
