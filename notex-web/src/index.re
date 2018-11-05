[%bs.raw {|require('draft-js/dist/Draft.css')|}];

[@bs.module "./registerServiceWorker"]
external registerServiceWorker: (unit => unit) => unit = "default";

Auth.check();
ReactDOMRe.renderToElementWithId(<App />, "root");

if (Utils.hot) {
  Utils.accept();
};

registerServiceWorker(() =>
  Toast.show("New version available.", "Update", () =>
    LocationRe.reload(Webapi.Dom.location)
  )
);

DbSync.run();
DataSyncRetry.getPendingChanges()->Future.get(DataSync.start);
