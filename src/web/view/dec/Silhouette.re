open Virtual_dom.Vdom;

module T = Token;
open Tylr_core;
module L = Layout;

module Style = {
  type t =
    | Space
    | Inner
    | Outer;
  let to_str =
    fun
    | Space => "outer"
    | Inner => "inner"
    | Outer => "outer";
};

module Inner = {
  module Profile = {
    type t = {
      style: Style.t,
      state: L.State.t,
      block: Block.t,
    };
    let mk = (~style, ~state, block) => {style, state, block};
  };

  let tips = (t: Token.t): Tip.s =>
    switch (t.mtrl) {
    | Mtrl.Tile((_, mold)) =>
      Tip.(
        Mold.is_null(~side=L, mold) ? Conv : Conc,
        Mold.is_null(~side=R, mold) ? Conv : Conc,
      )
    | Grout((_, tips)) => tips
    | Space(_) => (Conv, Conv)
    };

  let blur =
    Util.Nodes.filter(
      ~attrs=[Attr.id("silhouette-inner-blur")],
      [
        Node.create_svg(
          "feGaussianBlur",
          ~attrs=[
            Attr.create("in", "SourceGraphic"),
            Attr.create("stdDeviation", "0.03"),
          ],
          [],
        ),
      ],
    );

  let mk = (~font, p: Profile.t): list(Node.t) => {
    Block.flatten(p.block)
    |> Chain.fold_left_map(
         line =>
           line
           |> List.fold_left_map(
                (state: L.State.t, tok) => {
                  let sil =
                    T.mk_silhouette(
                      ~font,
                      ~inner=
                        switch (p.style) {
                        | Inner => true
                        | _ => false
                        },
                      state.loc,
                      Token.length(tok),
                      tips(tok),
                    );
                  let state =
                    L.State.map(Loc.shift(Token.length(tok)), state);
                  (state, sil);
                },
                p.state,
              ),
         (state, n, line) => {
           let state = L.State.return(state, n);
           let (state, sils) =
             line
             |> List.fold_left_map(
                  (state: L.State.t, tok) => {
                    let sil =
                      T.mk_silhouette(
                        ~font,
                        ~inner=
                          switch (p.style) {
                          | Inner => true
                          | _ => false
                          },
                        state.loc,
                        Token.length(tok),
                        tips(tok),
                      );
                    let state =
                      L.State.map(Loc.shift(Token.length(tok)), state);
                    (state, sil);
                  },
                  state,
                );
           (state, (), sils);
         },
       )
    |> snd
    |> Chain.loops
    |> List.concat;
  };
};

module Outer = {
  module Profile = {
    open Util.Svgs;
    type t = list(Rect.t);
    let point_of_loc = (loc: Loc.t) =>
      Point.{x: Float.of_int(loc.col), y: Float.of_int(loc.row)};
    let mk = (~state: L.State.t, block: Block.t) => {
      Block.flatten(block)
      |> Chain.fold_left_map(
           line => {
             let min = point_of_loc(state.loc);
             let len = Block.Line.len(line);
             let s = L.State.map(Loc.shift(len), state);
             (s, Rect.{min, width: Float.of_int(len), height: 1.});
           },
           (state, ind, line) => {
             let state = L.State.return(state, ind);
             let min = point_of_loc(state.loc);
             let len = Block.Line.len(line);
             let s = L.State.map(Loc.shift(len), state);
             (s, (), Rect.{min, width: Float.of_int(len), height: 1.});
           },
         )
      |> snd
      |> fst;
    };
  };

  let mk = (~font, p: Profile.t) => {
    p
    |> Util.Svgs.OrthogonalPolygon.mk(~corner_radii=(0.1, 0.1))
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["silhouette", "outer"])
    |> Stds.Lists.single
    |> Box.mk(~font, ~loc=Loc.zero);
  };

  let blur =
    Util.Nodes.filter(
      ~attrs=[Attr.id("silhouette-outer-blur")],
      [
        Node.create_svg(
          "feGaussianBlur",
          ~attrs=[
            Attr.create("in", "SourceGraphic"),
            Attr.create("stdDeviation", "0.1"),
          ],
          [],
        ),
      ],
    );
};
