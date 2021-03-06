[@bs.module] external styles: Js.Dict.t(string) = "./index.scss";
let style = name => Js.Dict.get(styles, name)->Belt.Option.getExn;

let isAuthedUser = () => Auth.getToken() |> Belt.Option.isSome;
type viewMode =
  | SingleColumn
  | TwoColumn
  | ThreeColumn;

type state = {viewMode};

type action =
  | Render
  | ToggleViewMode;

module Main = {
  let component = ReasonReact.statelessComponent("Main");
  let make = children => {
    ...component,
    render: _self => {
      let theme =
        switch (AppState.getTheme()) {
        | Some(AppState.Theme.Light) => "light"
        | Some(AppState.Theme.Dark) => "dark"
        | _ => ""
        };

      ReactDOMRe.createElementVariadic(
        "main",
        ~props=ReactDOMRe.objToDOMProps({"data-theme": theme, "className": style("main")}),
        children,
      );
    },
  };
};

let component = ReasonReact.reducerComponent("App");
let make = _children => {
  ...component,
  initialState: () => {viewMode: ThreeColumn},
  didMount: self => {
    Mousetrap.bind("command+alt+v", _e => self.send(ToggleViewMode));

    self.onUnmount(() => Mousetrap.unbind("command+alt+v"));
  },
  reducer: (action: action, state: state) =>
    switch (action) {
    | ToggleViewMode =>
      let nextMode =
        switch (state.viewMode) {
        | SingleColumn => ThreeColumn
        | TwoColumn => SingleColumn
        | ThreeColumn => TwoColumn
        };

      ReasonReact.Update({viewMode: nextMode});
    | Render => ReasonReact.Update(state)
    },
  render: self => {
    let editingNote = (notes: list(Data.note), selectedNoteId) =>
      Belt.Option.flatMap(selectedNoteId, selectedNoteId =>
        Utils.find(notes, n => n.id == selectedNoteId)
      );

    let selectedCollection = NoteManagementContainer.selectedCollectionFromState;

    isAuthedUser() ?
      <>
        <NoteManagementContainer>
          ...{(state, dispatch) =>
            <Main>
              <div className={style("columns")}>
                <NotebooksContainer
                  notebooks={state.notebooks}
                  noteCollections={state.noteCollections}
                  selectedCollection={state.selectedCollection}
                  hidden={self.state.viewMode != ThreeColumn}
                  dispatch
                />
                <NotesContainer
                  notes={state.notes}
                  selectedNote={state.selectedNote}
                  selectedNoteCollection={selectedCollection(state)}
                  hidden={self.state.viewMode == SingleColumn}
                  dispatch
                />
                <NoteEditorContainer
                  note={editingNote(state.notes, state.selectedNote)}
                  contentBlocks={state.contentBlocks}
                  dispatch
                />
              </div>
            </Main>
          }
        </NoteManagementContainer>
        <Toast.Container />
      </> :
      <OnboardingContainer onCompleted={() => self.send(Render)} />;
  },
};
