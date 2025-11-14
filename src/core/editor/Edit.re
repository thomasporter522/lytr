open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
// open Stds;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // | Tab(Dir.t)
  | Move(Move.t)
  // | Select(Select.t)
  | Insert(string)
  | Delete(Dir.t);

let perform = (a: t, b: Buffer.t): option(Buffer.t) => {
  // Effects.reset();
  switch (a) {
  // | Tab(d) => Tab.perform(d, b)
  | Move(a) => Move.perform(a, b)
  // | Select(a) => Select.perform(a, b)
  | Insert(s) => Some(Modify.insert(s, b))
  | Delete(d) => Modify.delete(d, b)
  };
};
