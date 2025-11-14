open Tylr_core;

module State = State;
module Font = Font;

type hist = list((string, string));

type t = {
  editor: int,
  buffer: Buffer.t,
  // history: History.t,
  font: Font.t,
  hist,
};

let cutoff = (==);

let init = {
  editor: 0,
  buffer: Buffer.empty,
  // history: History.empty,
  font: Font.init,
  hist: [],
};

let init_from_store = _ => {
  init;
  // {
  //   ...init,
  //   // zipper: Store.load_syntax(0),
  // };
};
