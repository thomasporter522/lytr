open Stds;

module Tok = Token;
module T = Terr;
module W = Wald;
module M = Meld;
open Tylr_core;

module L = Layout;

module Sil = {
  type t = {
    // total selection silhouette
    outer: Silhouette.Outer.Profile.t,
    // parsed inner silhouettes
    inner: list(Silhouette.Inner.Profile.t),
  };
};

module Profile = {
  type t = {
    l: option(M.Profile.t),
    up: list(T.Profile.t),
    top: W.Profile.t,
    dn: list(T.Profile.t),
    r: option(M.Profile.t),
    sil: Sil.t,
  };

  // let tokens = ({up, top, dn, _}: t) =>
  //   List.concat([
  //     List.concat_map(T.Profile.tokens, up),
  //     fst(top),
  //     List.concat_map(T.Profile.tokens, dn),
  //   ]);
  // let cells = ({up, top, dn, _}: t) =>
  //   List.concat_map(T.Profile.cells, up)
  //   @ snd(top)
  //   @ List.concat_map(T.Profile.cells, dn);

  let mk =
      (
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null: (bool, bool),
        // indices into the slopes indicating which have delim-matching counterparts
        // in the surrounding context. indices assume slopes in top-down order. -1
        // is used to indicate that the top wald matches. matching info is needed
        // for proper layout traversal
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        ~rolled: ((int, Rel.t(_)), (int, Rel.t(_))),
        zigg: LZigg.t,
      ) => {
    let outer_sil = Silhouette.Outer.Profile.mk(~state, LZigg.flatten(zigg));

    let (up_len, dn_len) = List.(length(zigg.up), length(zigg.dn));
    let l_bound =
      switch (zigg.up) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) => up_len - 2
      | _ => up_len - 1
      };
    let r_bound =
      switch (zigg.dn) {
      | [t, ..._] when Mtrl.is_space(LTerr.sort(t)) => dn_len - 2
      | _ => dn_len - 1
      };

    let (r_l, r_r) = rolled;
    let (rolled_l, rolled_z, rolled_r) =
      LZigg.roll(~l=fst(r_l), ~r=fst(r_r), zigg);

    let (state, (m_l, m_l_sil)) =
      switch (rolled_l.meld) {
      | None => (state, (None, []))
      | Some(m) =>
        let sil =
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(m)),
            ~state,
            LMeld.flatten(~flatten=LCell.flatten, m),
          );
        let (state, p) = M.Profile.mk(~sil=true, ~whole, ~state, m);
        (state, (Some(p), [sil]));
      };
    let (state, (up, up_sil)) =
      // reverse to get top-down index which matches eqs
      List.rev(rolled_z.up)
      |> List.mapi((i, t) => (i, t))
      // drop rolled terraces
      // |> Lists.take_while(~f=((i, _)) => i < up_len - fst(rolled))
      // reverse back to go left to right
      |> List.rev
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i >= l_bound && fst(null);
             let eq = List.mem(i, eqs_l);

             let (p_r, cell) = LCell.depad(terr.cell, ~side=R);
             let terr = {...terr, cell};

             let sil =
               Silhouette.Inner.Profile.mk(
                 ~is_space=Mtrl.is_space(LTerr.sort(terr)),
                 ~state,
                 LTerr.L.flatten(terr),
               );
             let (state, p) =
               T.Profile.mk_l(~sil=true, ~whole, ~state, ~eq, ~null, terr);

             let state = L.State.jump_cell(state, ~over=p_r);

             (state, (p, sil));
           },
           state,
         )
      |> Tuples.map_snd(List.split);
    let (state, (top, top_sil)) = {
      let null = (
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.up)
        && fst(null),
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.dn)
        && snd(null),
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      let sil =
        Silhouette.Inner.Profile.mk(
          ~is_space=Mtrl.is_space(LWald.sort(rolled_z.top)),
          ~state,
          LWald.flatten(~flatten=LCell.flatten, rolled_z.top),
        );
      let (state, p) =
        W.Profile.mk(~sil=true, ~whole, ~state, ~null, ~eq, rolled_z.top);
      (state, (p, sil));
    };
    let (state, (dn, dn_sil)) =
      // reverse to get top-down index which matches eqs
      List.rev(rolled_z.dn)
      |> List.mapi((i, t) => (i, t))
      // drop rolled terraces
      // |> Lists.take_while(~f=((i, _)) => i < dn_len - snd(rolled))
      |> List.fold_left_map(
           (state, (i, terr: LTerr.t)) => {
             let null = i >= r_bound && snd(null);
             let eq = List.mem(i, eqs_r);

             let (p_l, cell) = LCell.depad(~side=L, terr.cell);
             let terr = {...terr, cell};

             let state = L.State.jump_cell(state, ~over=p_l);

             let sil =
               Silhouette.Inner.Profile.mk(
                 ~is_space=Mtrl.is_space(LTerr.sort(terr)),
                 ~state,
                 LTerr.R.flatten(terr),
               );
             let (state, p) =
               T.Profile.mk_r(~sil=true, ~whole, ~state, ~eq, ~null, terr);

             (state, (p, sil));
           },
           state,
         )
      |> Tuples.map_snd(List.split);
    let (_state, (m_r, m_r_sil)) =
      switch (rolled_r.meld) {
      | None => (state, (None, []))
      | Some(m) =>
        let sil =
          Silhouette.Inner.Profile.mk(
            ~is_space=Mtrl.is_space(LMeld.sort(m)),
            ~state,
            LMeld.flatten(~flatten=LCell.flatten, m),
          );
        let (state, p) = M.Profile.mk(~sil=true, ~whole, ~state, m);
        (state, (Some(p), [sil]));
      };
    let sil =
      Sil.{
        outer: outer_sil,
        inner: List.concat([m_l_sil, up_sil, [top_sil], dn_sil, m_r_sil]),
      };
    {l: m_l, up, top, dn, r: m_r, sil};
  };
};

let mk = (~font, p: Profile.t) =>
  List.concat([
    [Silhouette.Outer.mk(~font, p.sil.outer)],
    List.concat_map(Silhouette.Inner.mk(~font), p.sil.inner),
    p.l |> Option.map(M.mk(~font)) |> Option.value(~default=[]),
    List.concat_map(T.mk(~font), p.up),
    W.mk(~font, p.top),
    List.concat_map(T.mk(~font), p.dn),
    p.r |> Option.map(M.mk(~font)) |> Option.value(~default=[]),
  ]);
