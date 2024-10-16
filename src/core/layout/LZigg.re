include Zigg.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Zigg.Base.t(Block.t);

let flatten = (zigg: t) =>
  Block.hcats([
    LSlope.Up.flatten(zigg.up),
    LWald.flatten(~flatten=LCell.flatten, zigg.top),
    LSlope.Dn.flatten(zigg.dn),
  ]);
