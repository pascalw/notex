[@bs.module] external styles: Js.Dict.t(string) = "./LoginContainer.scss";
let style = name => Js.Dict.get(styles, name)->Belt.Option.getExn;

module PasswordField = {
  let component = ReasonReact.statelessComponent("Login.PasswordField");
  let make = (~authFailure, ~inputRef, _children) => {
    ...component,
    render: _self => {
      let className =
        switch (authFailure) {
        | true => Utils.classnames([|style("passwordField"), style("passwordFieldError")|])
        | _ => style("passwordField")
        };
      <>
        {authFailure ?
           <p className={style("passwordErrorText")}>
             {ReasonReact.string("Uh oh, that's not correct. Try again.")}
           </p> :
           ReasonReact.null}
        <input
          type_="password"
          placeholder="Password"
          className
          required=true
          autoFocus=true
          ref=inputRef
        />
      </>;
    },
  };
};

type phase =
  | Start
  | Authenticating
  | AuthenticationFailure
  | AuthSuccesful;

type state = {
  phase,
  passwordInput: ref(option(Dom.element)),
};

type action =
  | Proceed(phase);

let setPasswordInputRef = (theRef, {ReasonReact.state}) =>
  state.passwordInput := Js.Nullable.toOption(theRef);

let passwordInputValue = state =>
  switch (state.passwordInput^) {
  | Some(el) =>
    Webapi.Dom.Element.asHtmlElement(el) |> Belt.Option.getExn |> Webapi.Dom.HtmlElement.value
  | _ => ""
  };

let component = ReasonReact.reducerComponent("LoginContainer");
let make = (~onLoggedIn, _children) => {
  ...component,
  initialState: () => {phase: Start, passwordInput: ref(None)},
  reducer: (action: action, state: state) =>
    switch (action) {
    | Proceed(phase) => ReasonReact.Update({...state, phase})
    },
  render: self => {
    let isState = state => self.state.phase == state;

    let onFormSubmit = e => {
      ReactEvent.Form.preventDefault(e);
      self.send(Proceed(Authenticating));

      let password = passwordInputValue(self.state);
      Auth.checkToken(Api.checkAuth, password)
      ->Promise.get(result =>
           switch (result) {
           | Belt.Result.Ok(_) =>
             self.send(Proceed(AuthSuccesful));
             Js.Global.setTimeout(onLoggedIn, 500) |> ignore;
           | Belt.Result.Error(_) => self.send(Proceed(AuthenticationFailure))
           }
         );
    };

    <div className={style("container")}>
      <h1> {ReasonReact.string("Hello and welcome to Pragma!")} </h1>
      <h2> {ReasonReact.string("The open-source personal note-taking app.")} </h2>
      <form onSubmit=onFormSubmit className={style("form")}>
        <PasswordField
          authFailure={isState(AuthenticationFailure)}
          inputRef={self.handle(setPasswordInputRef)}
        />
        <button type_="submit" disabled={isState(Authenticating)}>
          {(
             switch (self.state.phase) {
             | Authenticating => "Logging In..."
             | AuthSuccesful => "Logged In."
             | _ => "Log In"
             }
           )
           |> ReasonReact.string}
        </button>
      </form>
      <p className={style("docs")}>
        {ReasonReact.string("See ")}
        <a href="https://github.com/pascalw/pragma" target="_blank">
          {ReasonReact.string("the docs")}
        </a>
        {ReasonReact.string(" for more information.")}
      </p>
    </div>;
  },
};
