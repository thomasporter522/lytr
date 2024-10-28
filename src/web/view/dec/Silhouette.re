open Virtual_dom.Vdom;
open Stds;

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
      is_space: bool,
      state: L.State.t,
      block: Block.t,
    };
    let mk = (~is_space, ~state, block) => {is_space, state, block};
  };

  let tips = (t: Token.t): option(Tip.s) =>
    switch (t.mtrl) {
    | Mtrl.Tile((_, mold)) =>
      Some(
        Tip.(
          Mold.is_null(~side=L, mold) ? Conv : Conc,
          Mold.is_null(~side=R, mold) ? Conv : Conc,
        ),
      )
    | Grout((_, tips)) => Some(tips)
    | Space(_) => None
    };

  let blur =
    Util.Nodes.filter(
      ~attrs=[Attr.id("silhouette-inner-blur")],
      [
        Node.create_svg(
          "feGaussianBlur",
          ~attrs=[
            Attr.create("in", "SourceGraphic"),
            Attr.create("stdDeviation", "0.01"),
          ],
          [],
        ),
      ],
    );

  // magic number used to align with scaling transform applied to e
  let h_pad = 0.2;
  let v_trunc = T.v_trunc -. 0.1;

  let mk_glue = (~font, loc: Loc.t, len: int): Node.t => {
    Util.Svgs.Path.[
      m(~x=0, ~y=0) |> cmdfudge(~x=-. h_pad, ~y=T.v_trunc),
      h(~x=len) |> cmdfudge(~x=h_pad),
      v(~y=0) |> cmdfudge(~y=-. T.v_trunc),
      h(~x=0) |> cmdfudge(~x=-. h_pad),
      v(~y=0) |> cmdfudge(~y=T.v_trunc),
    ]
    |> Util.Svgs.Path.view
    |> Util.Nodes.add_classes(["silhouette", "glue"])
    |> Stds.Lists.single
    |> Box.mk(~font, ~loc);
  };

  let mk_ind = (~font, loc: Loc.t, len: int) =>
    if (len <= 0) {
      None;
    } else {
      Util.Svgs.Path.[
        m(~x=0, ~y=0) |> cmdfudge(~x=-. h_pad, ~y=v_trunc),
        h(~x=len) |> cmdfudge(~x=h_pad),
        v(~y=1) |> cmdfudge(~y=-. v_trunc),
        h(~x=0) |> cmdfudge(~x=-. h_pad),
        v(~y=0) |> cmdfudge(~y=v_trunc),
      ]
      |> Util.Svgs.Path.view
      |> Util.Nodes.add_classes(["silhouette", "inner"])
      |> Stds.Lists.single
      |> Box.mk(~font, ~loc)
      |> Option.some;
    };

  let mk_tok = (~font, loc: Loc.t, tok: Token.t) =>
    switch (tok.mtrl) {
    // ignore empty space tokens left over from line splitting
    | Space(_) when Token.length(tok) == 0 => None
    | _ =>
      let len = Token.length(tok);
      // magic number used to scale up tip size to align with h_pad and shortened
      // v_trunc
      let c = 1.2825;
      let path =
        switch (tips(tok)) {
        | None =>
          Util.Svgs.Path.[
            m(~x=0, ~y=0) |> cmdfudge(~x=-. h_pad, ~y=v_trunc),
            h(~x=len) |> cmdfudge(~x=h_pad),
            v(~y=1) |> cmdfudge(~y=-. v_trunc),
            h(~x=0) |> cmdfudge(~x=-. h_pad),
            v(~y=0) |> cmdfudge(~y=v_trunc),
          ]
        | Some((l, r)) =>
          Util.Svgs.[
            Path.[
              m(~x=0, ~y=0) |> cmdfudge(~x=-. h_pad, ~y=v_trunc),
              h(~x=len) |> cmdfudge(~x=h_pad),
            ],
            Path.scale(c, T.tip(r)),
            Path.[h(~x=0) |> cmdfudge(~x=-. h_pad)],
            Path.scale(-. c, T.tip(l)),
          ]
          |> List.flatten
        };
      let clss =
        switch (tok.mtrl) {
        | Space(_) => ["silhouette", "space"]
        | Grout(_)
        | Tile(_) => ["silhouette", "inner"]
        };
      Util.Svgs.Path.view(path)
      |> Util.Nodes.add_classes(clss)
      |> Stds.Lists.single
      |> Box.mk(~font, ~loc)
      |> Option.some;
    };

  let mk = (~font, p: Profile.t): list(Node.t) => {
    p.is_space
      ? []
      : Block.flatten(p.block)
        |> Chain.fold_left_map(
             line =>
               line
               |> List.fold_left_map(
                    (state: L.State.t, tok) => {
                      let len = Token.length(tok);
                      let sil =
                        Option.to_list(mk_tok(~font, state.loc, tok));
                      let state = L.State.map(Loc.shift(len), state);
                      (state, sil);
                    },
                    p.state,
                  )
               |> Tuples.map_snd(List.concat),
             (s, n, line) => {
               let state = L.State.return(s, n);
               let loc = Loc.shift(- n, state.loc);
               let glue_sil = {
                 let intersection =
                   min(s.loc.col - loc.col, n + Block.Line.len(line));
                 mk_glue(~font, loc, intersection);
               };
               let ind_sil = Option.to_list(mk_ind(~font, loc, n));
               let (state, sils) =
                 line
                 |> List.fold_left_map(
                      (state: L.State.t, tok) => {
                        let len = Token.length(tok);
                        let sil = mk_tok(~font, state.loc, tok);
                        let state = L.State.map(Loc.shift(len), state);
                        (state, Option.to_list(sil));
                      },
                      state,
                    )
                 |> Tuples.map_snd(List.concat);
               (state, [glue_sil, ...ind_sil], sils);
             },
           )
        |> snd
        |> Chain.to_list(Fun.id, Fun.id)
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
             let rect =
               Rect.{min, width: Float.of_int(len), height: 1.}
               |> Rect.pad(~x=0.55, ~y=0.075);
             (s, rect);
           },
           (state, ind, line) => {
             let state = L.State.return(state, 0);
             let min = point_of_loc(state.loc);
             let width = ind + Block.Line.len(line);
             let s = L.State.map(Loc.shift(width), state);
             let rect =
               Rect.{min, width: Float.of_int(width), height: 1.}
               |> Rect.pad(~x=0.55, ~y=0.075);
             (s, (), rect);
           },
         )
      |> snd
      |> fst;
    };
  };

  let mk = (~font, p: Profile.t) => {
    p
    |> Util.Svgs.OrthogonalPolygon.mk(~corner_radii=(0.7, 0.3))
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
            Attr.create("stdDeviation", "0.01"),
          ],
          [],
        ),
      ],
    );
};
