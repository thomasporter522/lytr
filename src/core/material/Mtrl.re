// open Sexplib.Std;
// open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('s, 'g, 't) =
    | Space('s)
    | Grout('g)
    | Tile('t);
};
include Base;

let space = s => Space(s);
let grout = g => Grout(g);
let tile = t => Tile(t);

let is_space =
  fun
  | Space(_) => true
  | Grout(_)
  | Tile(_) => false;
let is_grout =
  fun
  | Grout(_) => true
  | Space(_)
  | Tile(_) => false;
let is_tile =
  fun
  | Tile(_) => true
  | Space(_)
  | Grout(_) => false;

let map = (~space, ~grout, ~tile) =>
  fun
  | Space(s) => Space(space(s))
  | Grout(g) => Grout(grout(g))
  | Tile(t) => Tile(tile(t));
