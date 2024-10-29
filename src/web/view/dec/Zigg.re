module Tok = Token;
module T = Terr;
module W = Wald;
open Tylr_core;

module L = Layout;

module Profile = {
  type t = {
    up: list((T.Profile.t, Silhouette.Inner.Profile.t)),
    top: (W.Profile.t, Silhouette.Inner.Profile.t),
    dn: list((T.Profile.t, Silhouette.Inner.Profile.t)),
    // total silhouette
    sil: Silhouette.Outer.Profile.t,
  };

  let tokens = ({up, top, dn, _}: t) =>
    List.concat_map(((t, _)) => T.Profile.tokens(t), up)
    @ fst(fst(top))
    @ List.concat_map(((t, _)) => T.Profile.tokens(t), dn);
  let cells = ({up, top, dn, _}: t) =>
    List.concat_map(((t, _)) => T.Profile.cells(t), up)
    @ snd(fst(top))
    @ List.concat_map(((t, _)) => T.Profile.cells(t), dn);
  let silhouettes = ({up, top, dn, _}: t) =>
    List.map(snd, up) @ [snd(top)] @ List.map(snd, dn);

  let mk =
      (
        ~whole: LCell.t,
        ~state: L.State.t,
        ~null: (bool, bool),
        // indices into the slopes indicating which have delim-matching counterparts
        // in the surrounding context. indices assume slopes in top-down order. -1
        // is used to indicate that the top wald matches
        ~eqs as (eqs_l, eqs_r): (list(int), list(int)),
        zigg: LZigg.t,
      ) => {
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
    let sil = Silhouette.Outer.Profile.mk(~state, LZigg.flatten(zigg));
    let (state, up) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.up)
      |> List.mapi((i, t) => (i, t))
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
         );
    let (state, top) = {
      let null = (
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.up)
        && fst(null),
        List.for_all(t => Mtrl.is_space(LTerr.sort(t)), zigg.dn)
        && snd(null),
      );
      let eq = List.(mem(-1, eqs_l), mem(-1, eqs_r));
      let sil =
        Silhouette.Inner.Profile.mk(
          ~is_space=Mtrl.is_space(LWald.sort(zigg.top)),
          ~state,
          LWald.flatten(~flatten=LCell.flatten, zigg.top),
        );
      let (state, p) =
        W.Profile.mk(~sil=true, ~whole, ~state, ~null, ~eq, zigg.top);
      (state, (p, sil));
    };
    let (_, dn) =
      // reverse to get top-down index which matches eqs
      List.rev(zigg.dn)
      |> List.mapi((i, t) => (i, t))
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
         );
    {up, top, dn, sil};
  };
};

let mk = (~font, p: Profile.t) =>
  [Silhouette.Outer.mk(~font, p.sil)]
  @ List.concat_map(Silhouette.Inner.mk(~font), Profile.silhouettes(p))
  @ List.map(Tok.mk(~font), Profile.tokens(p))
  @ List.map(Child.mk(~font), Profile.cells(p));
