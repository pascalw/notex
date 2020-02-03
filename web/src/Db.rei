type result('a) = Belt.Result.t('a, Js.Promise.error);
type listener = unit => unit;

let subscribe: listener => unit;
let unsubscribe: listener => unit;

let addNotebook: Data.notebook => Promise.t(result(unit));
let addNote: Data.note => Promise.t(result(unit));

let createNote: string => Promise.t(result((Data.note, Data.contentBlock)));
let createNotebook: Data.notebook => Promise.t(result(Data.notebook));

let getNote: string => Promise.t(option(Data.note));
let getNotes: string => Promise.t(list(Data.note));

let getRecentNotes: (int) => Promise.t(list(Data.note));
let getRecentNotesCount: unit => Promise.t(int);

let getNotebooks: unit => Promise.t(list((Data.notebook, int)));
let getNotebook: string => Promise.t(option(Data.notebook));

let getContentBlocks: string => Promise.t(list(Data.contentBlock));
let getContentBlock: string => Promise.t(option(Data.contentBlock));
let addContentBlock: Data.contentBlock => Promise.t(result(unit));

let updateContentBlock: (Data.contentBlock, ~sync: bool=?, unit) => Promise.t(result(unit));
let updateNote: (Data.note, ~sync:bool=?, unit) => Promise.t(result(unit));
let updateNotebook: (Data.notebook, ~sync:bool=?, unit) => Promise.t(result(unit));

let touchNote: string => Promise.t(Belt.Result.t(unit, unit));

let deleteNotebook: (string, ~sync: bool=?, unit) => Promise.t(result(unit));
let deleteNote: (string, ~sync: bool=?, unit) => Promise.t(result(unit));
let deleteContentBlock: (string) => Promise.t(result(unit));

let withNotification: (unit => 'a) => 'a;
let withPromiseNotification: Promise.t('a) => unit;

let insertRevision: string => Promise.t(unit);
let getRevision: unit => Promise.t(option(string));
let clear: unit => unit;
