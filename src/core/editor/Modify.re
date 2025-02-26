open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Stds;

module Change = {
  module Src = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      // the original mtrl of the deleted token
      | Deleted(Mtrl.T.t)
      // the original mtrls of neighboring tokens that the insertion extended
      | Inserted(option(Mtrl.T.t), option(Mtrl.T.t));
    let ins = (~l=?, ~r=?, ()) => Inserted(l, r);
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    tok: Token.Unmolded.t,
    src: Src.t,
  };
  let mk = (~src=Src.ins(), tok) => {tok, src};
  let map = (f, {tok, src}: t) => {tok: f(tok), src};
};

module Changes = {
  // a list of changes along with interleaved cells for holding normalized cursors
  type t = Chain.t(Cell.t, Change.t);
};

let restore_and_normalize_cursor = (n: int, cs: list(Change.t)): Changes.t => {
  cs
  // restore cursor position
  |> Lists.folding_map(
       ~init=0,
       ~f=(num_chars, c: Change.t) => {
         let m = num_chars + Utf8.length(c.tok.text);
         let put_cursor =
           m >= n
             ? Token.put_cursor(Point(Caret.focus(n - num_chars))) : Fun.id;
         (m, Change.map(put_cursor, c));
       },
     )
  // normalize the cursors by popping off any carets at the token edges
  // and storing them instead in neighboring cells, the final result being a
  // chain of cell-loops (either empty or with a caret) and token-links
  |> Lists.fold_right(~init=Chain.unit(Cell.dirty), ~f=(c: Change.t, acc) =>
       switch (c.tok.marks) {
       | Some(Point({path: 0, _})) =>
         Chain.link(
           Cell.point(~dirty=true, Focus),
           Change.map(Token.clear_marks, c),
           acc,
         )
       | Some(Point({path: n, _})) when n == Utf8.length(c.tok.text) =>
         acc
         |> Chain.map_hd(Fun.const(Cell.point(~dirty=true, Focus)))
         |> Chain.link(Cell.dirty, Change.map(Token.clear_marks, c))
       | _ => Chain.link(Cell.dirty, c, acc)
       }
     );
};

let relabel = (s: string, z: Zipper.t): Choice.t((Changes.t, Ctx.t)) => {
  let (cur_site, ctx) = Zipper.cursor_site(z);
  switch (Option.get(Cursor.get_point(cur_site))) {
  | Within(tok) =>
    let (l, r) = Token.(affix(~side=L, tok), affix(~side=R, tok));
    let labeled = Labeler.label(l ++ s ++ r);
    let cs =
      switch (labeled) {
      | [lsr_] => Change.[{tok: lsr_, src: Src.ins(~l=tok.mtrl, ())}]
      | [l, s, r] => List.map(Change.mk, [l, s, r])
      | [tok_l, tok_r] =>
        tok_l.text == l ++ s
          ? Change.[mk(~src=Src.ins(~l=tok.mtrl, ()), tok_l), mk(tok_r)]
          : Change.[mk(tok_l), mk(~src=Src.ins(~r=tok.mtrl, ()), tok_r)]
      | _ => assert(false)
      };
    let cs = restore_and_normalize_cursor(Utf8.length(l ++ s), cs);
    Choice.one((cs, ctx));
  | Between =>
    let (l, ctx_sans_l) = Ctx.pull(~from=L, ctx);
    let (r, ctx_sans_r) = Ctx.pull(~from=R, ctx);
    let (_, ctx_sans_lr) = Ctx.pull(~from=R, ctx_sans_l);
    let no_merge = {
      let cs =
        Labeler.label(s)
        |> List.map(Change.mk)
        |> restore_and_normalize_cursor(Utf8.length(s));
      Choice.one((cs, ctx));
    };
    let merged_l =
      switch (l) {
      // | Node({mtrl: Tile((Const(_, _, c), _)) as mtrl, text, _})
      //     when String.starts_with(~prefix=text ++ s, c) =>
      //   let cs =
      //     Change.[mk(~src=Src.ins(~l=mtrl, ()), tok)]
      //     |> restore_and_normalize_cursor(Utf8.length(l ++ s));
      //   Choice.one((cs, ctx_sans_l));
      | Root
      | Node({mtrl: Space(White(_)) | Grout(_), _}) => Choice.nil
      // | Node({mtrl: Tile(_), text: "", _}) => Choice.nil
      | Node({id, mtrl, text: l, _}) =>
        switch (Labeler.single(~id, l ++ s)) {
        | None => Choice.nil
        | Some(tok) =>
          let cs =
            Change.[mk(~src=Src.ins(~l=mtrl, ()), tok)]
            |> restore_and_normalize_cursor(Utf8.length(l ++ s));
          Choice.one((cs, ctx_sans_l));
        }
      };
    let merged_r =
      switch (r) {
      | Root
      | Node({mtrl: Space(White(_)) | Grout(_), _}) => Choice.nil
      | Node({id, mtrl, text: r, _}) =>
        switch (Labeler.single(~id, s ++ r)) {
        | None => Choice.nil
        | Some(tok) =>
          let cs =
            Change.[mk(~src=Src.ins(~r=mtrl, ()), tok)]
            |> restore_and_normalize_cursor(Utf8.length(s));
          Choice.one((cs, ctx_sans_r));
        }
      };
    let merged_lr =
      switch (l, r) {
      | (Root | Node({mtrl: Space(White(_)) | Grout(_), _}), _)
      | (_, Root | Node({mtrl: Space(White(_)) | Grout(_), _})) => Choice.nil
      | (
          Node({mtrl: mtrl_l, text: l, id, _}),
          Node({mtrl: mtrl_r, text: r, _}),
        ) =>
        switch (Labeler.single(~id, l ++ s ++ r)) {
        | None => Choice.nil
        | Some(tok) =>
          let cs =
            Change.[mk(~src=Src.ins(~l=mtrl_l, ~r=mtrl_r, ()), tok)]
            |> restore_and_normalize_cursor(Utf8.length(l ++ s));
          Choice.one((cs, ctx_sans_lr));
        }
      };
    Choice.prefers([merged_lr, merged_l, merged_r, no_merge]);
  };
};

// None means token was removed. Some(ctx) means token was molded (or deferred and
// tagged as an unmolded space), ctx includes the molded token.
// let mold =
//     (ctx: Ctx.t, ~fill=Cell.dirty, tok: Token.Unmolded.t)
//     : Result.t(Ctx.t, Cell.t) => {
//   open Result.Syntax;
//   // P.log("--- Modify.mold");
//   // P.show("ctx", Ctx.show(ctx));
//   // P.show("fill", Cell.show(fill));
//   // P.show("tok", Token.Unmolded.show(tok));
//   let ((l, r), rest) = Ctx.unlink_stacks(ctx);
//   // Grouter.dbg := true;
//   let+ (tok, grouted, l) = Molder.mold(l, ~fill, tok);
//   // Grouter.dbg := false;
//   // P.log("--- Modify.mold/success");
//   // P.show("tok", Token.show(tok));
//   // P.show("grouted", Grouted.show(grouted));
//   // P.show("stack", Stack.show(l));
//   let connected = Stack.connect(Effects.insert(tok), grouted, l);
//   // P.show("connected", Stack.show(connected));
//   connected.bound == l.bound
//     ? Ctx.link_stacks((connected, r), rest)
//     : Ctx.map_hd(
//         Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
//         rest,
//       );
// };

let rec remold = (~fill=Cell.dirty, ctx: Ctx.t): (Grouted.t, Ctx.t) => {
  // P.log("--- Modify.remold");
  // P.show("fill", Cell.show(fill));
  // P.show("ctx", Ctx.show(ctx));
  let ((l, r), tl) = Ctx.unlink_stacks(ctx);
  switch (Molder.remold(~fill, (l, r))) {
  | Error((fill, (l', r'))) =>
    // remold error means something in r melded onto the bound of l, breaking their
    // bidelimited container, so we need to add the suffix of the next stack frame
    // in tl to the remolding queue
    // P.log("--- Modify.remold/error");
    // P.show("fill", Cell.show(fill));
    // P.show("(l', r')", Stack.Frame.show((l', r')));
    tl
    |> Ctx.map_hd(Frame.Open.cat(Stack.(to_slope(l'), to_slope(r'))))
    |> remold(~fill)
  | Ok((dn, fill)) =>
    // P.log("--- Modify.remold/done");
    // P.show("dn", Slope.Dn.show(dn));
    // P.show("fill", Cell.show(fill));
    let bounds = (l.bound, r.bound);
    // Melder.debug := true;
    let grouted = Melder.complete_bounded(~bounds, ~onto=L, dn, ~fill);
    // Melder.debug := false;
    // P.show("completed", Cell.show(cell));
    let hd = ({...l, slope: []}, {...r, slope: []});
    let ctx = Ctx.link_stacks(hd, tl);
    (grouted, ctx);
  };
};

// let remold_ = (~restrict_obligs, ~fill, ctx: Ctx.t) => {
//   let remolded = remold(~fill, ctx);
//   // P.log("--- meld_remold/not redundant");
//   // P.show("tok", Token.show(tok));
//   // P.show("ctx", Ctx.show(ctx));
//   // P.show("remolded", Grouted.show(fst(remolded)));
//   // P.show("remolded ctx", Ctx.show(snd(remolded)));
//   // P.show("effects", Fmt.(to_to_string(list(Effects.pp), Effects.log^)));
//   switch (tok.mtrl) {
//   | Tile((lbl, _))
//       // delay expansion if obligations more severe than holes
//       when
//         !expanding
//         && !Label.is_instant(lbl)
//         && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
//         // this is necessary when deleting delims to empty ghosts
//         && !Token.is_empty(tok)
//         // only delay expansion for tokens followed by caret (#126)
//         && Option.(is_some(tok.marks) || is_some(next.marks.cursor)) =>
//     None
//   | _ => Some(remolded)
//   };
// };

let finalize = (remolded: Grouted.t, ctx: Ctx.t): Zipper.t => {
  // P.log("--- Modify.finalize_");
  // P.show("remolded", Grouted.show(remolded));
  // P.show("ctx", Ctx.show(ctx));
  let repadded =
    remolded
    |> Chain.rev
    |> Chain.map_link(Delim.tok)
    |> Chain.consnoc(
         ~hd=Ctx.face(~side=L, ctx),
         ~ft=Ctx.face(~side=R, ctx),
       )
    |> Chain.map_linked((l, (sw, c), r) => {
         let repadded = Linter.repad(~l, c, ~r);
         (sw, {...repadded, marks: Cell.Marks.flush(repadded.marks)});
       })
    |> Chain.unconsnoc_exn
    |> (((_, c, _)) => c)
    |> Chain.map_link(Delim.unwrap);
  // P.show("repadded", Grouted.show(repadded));
  let (pre, (_, cur), suf) =
    repadded
    |> Chain.find_unzip_loop(((_, c: Cell.t)) =>
         Option.is_some(c.marks.cursor)
       )
    |> Options.get_fail("bug: lost cursor");
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  let (l, r) = Stack.(connect_affix(pre, l), connect_affix(suf, r));
  let ctx = Ctx.link_stacks((l, r), rest);
  // P.show("flushed", Cell.show(c));
  Zipper.unzip_exn(cur, ~ctx);
};

// let try_move = (s: string, z: Zipper.t) => {
//   let (face, ctx) = Ctx.pull(~from=R, z.ctx);
//   switch (s, face, Ctx.face(~side=R, ctx)) {
//   | (" ", Node(tok), _) when tok.text == " " || Mtrl.is_grout(tok.mtrl) =>
//     Move.perform(Step(H(R)), z)
//   | ("\n", Node(tok), Node(next)) when tok.text == "\n" && next.text == "" =>
//     Move.perform(Step(H(R)), z)
//   | _ => None
//   };
// };

// let extend = (~side=Dir.R, s: string, tok: Token.t) =>
//   switch (tok.mtrl) {
//   | Space(Unmolded) =>
//     // this path may need extra guards, currently always succeeds at extending
//     // unmolded token
//     let (l, r) =
//       Token.split_text(tok)
//       |> Option.map(((l, _, r)) => (l, r))
//       |> Option.value(
//            ~default=Dir.pick(side, (("", tok.text), (tok.text, ""))),
//          );
//     let text = l ++ s ++ r;
//     let n = Utf8.length(l ++ s);
//     Labeler.single(text)
//     |> Option.map(_ => {...tok, text})
//     |> Option.map(tok =>
//          n >= Token.length(tok)
//            ? Token.clear_marks(tok)
//            : Token.put_cursor(Point(Caret.focus(n)), tok)
//        );
//   | Space(_)
//   | Grout(_) => None
//   | Tile((lbl, _)) =>
//     let (l, r) =
//       Token.split_text(tok)
//       |> Option.map(((l, _, r)) => (l, r))
//       |> Option.value(
//            ~default=Dir.pick(side, (("", tok.text), (tok.text, ""))),
//          );
//     let text = l ++ s ++ r;
//     switch (Labeler.label(text)) {
//     | [t] when Token.Unmolded.has_lbl(lbl, t) =>
//       let n = Utf8.length(l ++ s);
//       let extended = {...tok, text};
//       extended
//       |> (
//         // extending doesn't require zipping (which takes place in other
//         // insertion paths while remolding), which handles cursor normalization,
//         // so need to do some manual cursor normalization here
//         n >= Token.length(extended)
//           ? Token.clear_marks : Token.put_cursor(Point(Caret.focus(n)))
//       )
//       |> Option.some;
//     | _ => None
//     };
//   };
// let try_extend = (s: string, z: Zipper.t): option(Zipper.t) => {
//   open Options.Syntax;
//   // P.log("--- Modify.try_extend");
//   let* () = Options.of_bool(!Strings.is_empty(s));
//   // P.log("not empty");
//   let (sites, ctx) = Zipper.cursor_site(z);
//   let* site = Cursor.get_point(sites);
//   // P.log("cursor site is point");
//   let+ (extended, ctx) =
//     switch (site) {
//     | Within(tok) =>
//       let+ extended = extend(~side=R, s, tok);
//       (extended, ctx);
//     | Between =>
//       let/ () = {
//         // P.log("--- Modify.try_extend/Between/trying left");
//         let (face, ctx) = Ctx.pull(~from=L, ctx);
//         let* tok = Delim.is_tok(face);
//         // P.log("is tok");
//         let+ extended = extend(~side=R, s, tok);
//         // P.log("extended");
//         (extended, ctx);
//       };
//       // P.log("--- Modify.try_extend/Between/trying right");
//       let (face, ctx) = Ctx.pull(~from=R, ctx);
//       let* tok = Delim.is_tok(face);
//       // P.log("is tok");
//       let+ extended = extend(~side=L, s, tok);
//       // P.log("extended");
//       (extended, ctx);
//     };
//   ctx
//   |> Ctx.push(~onto=L, extended)
//   |> (Option.is_some(extended.marks) ? Ctx.push(~onto=R, extended) : Fun.id)
//   |> Zipper.mk;
// };

let put_edge = (~hand=Caret.Hand.Focus, side: Dir.t, tok: Token.t) =>
  switch (side) {
  | L => Token.put_cursor(Point(Caret.mk(hand, 0)), tok)
  | R => Token.put_cursor(Point(Caret.mk(hand, Token.length(tok))), tok)
  };
let add_edge = (~hand=Caret.Hand.Focus, side: Dir.t, tok: Token.t) =>
  switch (side) {
  | L => Token.add_mark(Caret.mk(hand, 0), tok)
  | R => Token.add_mark(Caret.mk(hand, Token.length(tok)), tok)
  };

let delete_toks = (d: Dir.t, toks: list(Token.t)): Changes.t => {
  let n = List.length(toks);
  let car = Cell.point(~dirty=true, Focus);
  toks
  // first, clear text of selected tokens within selection bounds and mark
  // either the first or last token with the final cursor position
  |> List.mapi((i, tok) =>
       if (i == 0 && i == n - 1) {
         // single-token selection
         // note: affixes empty if token completely selected
         // (assuming edge carets have been temporarily non-normally placed on toks)
         let (l, r) = Token.(affix(~side=L, tok), affix(~side=R, tok));
         let text = Token.is_const(tok) ? l : l ++ r;
         {...tok, text}
         |> Token.put_cursor(Point(Step.Caret.focus(Utf8.length(l))));
       } else if (i == 0) {
         let l = Token.affix(~side=L, tok);
         let car = Step.Caret.focus(Utf8.length(l));
         {...tok, text: l}
         |> Token.(d == L ? put_cursor(Point(car)) : clear_marks);
       } else if (i == n - 1) {
         let r = Token.affix(~side=R, tok);
         {...tok, text: r}
         |> (d == R ? put_edge(r == "" ? R : L) : Token.clear_marks);
       } else {
         Token.clear_marks({...tok, text: ""});
       }
     )
  // next, normalize the cursors by popping off any carets at the token edges
  // and storing them instead in neighboring cells, the final result being a
  // chain of cell-loops (either empty or with a caret) and token-links
  |> Lists.fold_right(
       ~init=Chain.unit(Cell.dirty),
       ~f=(tok, c) => {
         let (l, tok, r) = Token.pop_end_carets(tok);
         c
         |> Chain.map_hd(r == None ? Fun.id : Fun.const(car))
         |> (
           switch (tok.mtrl) {
           | Space(_)
           | Grout(_) when tok.text == "" =>
             Chain.map_hd(
               l == None
                 ? Fun.id
                 : car
                   |> Cell.map_marks(Cell.Marks.mark_degrouted([]))
                   |> Fun.const,
             )
           | _ => Chain.link(l == None ? Cell.dirty : car, tok)
           }
         );
       },
     )
  // finally, unmold the tokens (only relabeling the last token)
  |> Chain.mapi_link((i, tok: Token.t) =>
       Change.mk(
         ~src=Deleted(tok.mtrl),
         Labeler.unmold(~relabel=i - 1 / 2 == n - 1, tok),
       )
     );
  // |> Chain.mapi_link(i => Labeler.unmold(~relabel=i - 1 / 2 == n - 1))
};

let meld =
    (tok: Token.t, ~fill=Cell.dirty, ctx: Ctx.t)
    : option(Result.t(Ctx.t, Cell.t)) => {
  open Options.Syntax;
  let ((l, r), rest) = Ctx.unlink_stacks(ctx);
  let+ (grouted, l) =
    Melder.push(tok, ~fill, l, ~onto=L, ~repair=Molder.remold);
  let is_redundant =
    tok.text == ""
    && (
      Token.is_complete(tok)
      || Grouted.is_neq(grouted)
      || Option.is_some(Grouted.is_eq(grouted))
      && l.slope == []
    );
  if (is_redundant) {
    // todo: maybe need to return result here and emit the fill?
    // probably dropping otherwise. this is akin to molder error case.
    // tho molder only does this for the final melded result, does that matter?
    Effects.remove(tok);
    Error(Cell.mark_degrouted(fill, ~side=R));
  } else {
    let connected = Stack.connect(Effects.insert(tok), grouted, l);
    let ctx =
      connected.bound == l.bound
        ? Ctx.link_stacks((connected, r), rest)
        : Ctx.map_hd(
            Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
            rest,
          );
    Ok(ctx);
  };
};

// maybe rename expandable
let expand = (tok: Token.t): option(Token.Unmolded.t) =>
  switch (tok.mtrl) {
  | Space(White(_))
  | Grout(_) => None
  | Space(Unmolded) =>
    open Options.Syntax;
    let* labeled = Labeler.single(tok.text);
    switch (labeled.mtrl) {
    | Space(_)
    | Grout(_) => None
    | Tile(_) => Some(labeled)
    };
  // none for already expanded
  | Tile((Const(_), _)) => None
  | Tile(_) =>
    open Options.Syntax;
    let* labeled = Labeler.single(~id=tok.id, tok.text);
    Token.Unmolded.expands(labeled);
  };

let is_instant = (tok: Token.t) =>
  switch (tok.mtrl) {
  | Tile((lbl, _)) => Label.is_instant(lbl)
  | _ => false
  };

let try_move = (c: Change.t, ctx: Ctx.t) => {
  // P.log("--- Modify.try_move");
  let (face, ctx) = Ctx.pull(~from=R, ctx);
  // P.show("face", Delim.show(face));
  // P.show("ctx", Ctx.show(ctx));
  switch (c.tok.text, face, Ctx.face(~side=R, ctx)) {
  | (" ", Node(tok), _) when tok.text == " " || Mtrl.is_grout(tok.mtrl) =>
    // P.log("--- Modify.try_move/moved over space");
    Ctx.push_opt(~onto=L, tok, ~fill=Cell.dirty, ctx)
  | ("\n", Node(tok), Node(next)) when tok.text == "\n" && next.text == "" =>
    // P.log("--- Modify.try_move/moved over newline");
    Ctx.push_opt(~onto=L, tok, ~fill=Cell.dirty, ctx)
  | _ => None
  };
};

let apply_change =
    (~followed_by_cursor, c: Change.t, ~fill, ctx: Ctx.t)
    : Choice.t(Options.Thunk.t((Result.t(Ctx.t, Cell.t), bool, bool))) => {
  open Options.Syntax;
  // P.log("--- Modify.apply_change");
  // P.show("c", Change.show(c));
  // P.show("fill", Cell.show(fill));
  // P.show("ctx", Ctx.show(ctx));

  // first, possibly expand the left face of ctx
  let expanded =
    switch (c.src) {
    | Deleted(_)
    | Inserted(Some(_), _) => Choice.nil
    | Inserted(None, _) =>
      let (let$) = (o, f) =>
        switch (o) {
        | None => Choice.nil
        | Some(x) => f(x)
        };
      let (face, rest) = Ctx.pull(~from=L, ctx);
      let$ tok = Delim.is_tok(face);
      let$ expanded = expand(tok);
      Molder.candidates(expanded)
      |> List.map((candidate, ()) =>
           meld(candidate, rest)
           |> Options.bind(~f=Result.to_option)
           |> Option.map(melded => (melded, true))
         )
      |> Choice.any;
    };
  let (ctx, expanded) =
    Choice.prefer(expanded, One(Options.Thunk.some((ctx, false))))
    |> Oblig.Delta.min_choice
    |> Option.get;
  // P.show("expanded", Ctx.show(ctx));

  // next, check if the change is a space or newline and if it can be interpreted as
  // movement over the following space or newline
  let- () =
    try_move(c, ctx)
    |> Option.map(ctx =>
         Choice.one(Options.Thunk.some((Ok(ctx), false, false)))
       );

  // enumerate molded candidates
  let restrict_obligs = followed_by_cursor || Option.is_some(c.tok.marks);
  let candidates =
    switch (c.src) {
    | Deleted(mtrl) =>
      // expecting empty ghost tile
      // retain original mtrl
      Choice.One((Token.map(Fun.const(mtrl), c.tok), false))
    | Inserted(None, None) =>
      // include unmolded at same preference as molded
      Molder.candidates(c.tok)
      |> List.map(tok => (tok, !is_instant(tok) && restrict_obligs))
      |> Choice.any
    | Inserted(Some(l), _) =>
      // prefer original mold
      Choice.prefer(
        switch (l, c.tok.mtrl) {
        | (Tile((lbl, _)), Tile(lbls)) when List.mem(lbl, lbls) =>
          One((Token.map(Fun.const(l), c.tok), false))
        | (Space(Unmolded), _) =>
          One((Token.map(Fun.const(l), c.tok), false))
        | _ => Nil
        },
        Molder.candidates(c.tok)
        |> List.map(tok => (tok, !is_instant(tok) && restrict_obligs))
        |> Choice.any,
      )
    | Inserted(_, Some(r)) =>
      // prefer original mold
      Choice.prefer(
        switch (r, c.tok.mtrl) {
        | (Tile((lbl, _)), Tile(lbls)) when List.mem(lbl, lbls) =>
          One((Token.map(Fun.const(r), c.tok), false))
        | (Space(Unmolded), _) =>
          One((Token.map(Fun.const(r), c.tok), false))
        | _ => Nil
        },
        Molder.candidates(c.tok)
        |> List.map(tok => (tok, !is_instant(tok) && restrict_obligs))
        |> Choice.any,
      )
    };

  Choice.prefer(candidates, One((Token.Unmolded.defer(c.tok), false)))
  |> Choice.map(((candidate, restrict_obligs), ()) => {
       //  P.log("--- Modify.apply_change/candidate");
       //  P.show("candidate", Token.show(candidate));
       //  P.show("restrict_obligs", string_of_bool(restrict_obligs));
       meld(candidate, ~fill, ctx)
       |> Option.map(melded => (melded, expanded, restrict_obligs))
     });
};
// let apply_choose =
//     (~followed_by_cursor, c: Change.t, ~fill, ctx: Ctx.t)
//     : (Result.t(Ctx.t, Cell.t), bool) => {
//   apply_change(~followed_by_cursor, c, ~fill, ctx)
//   |> Oblig.Delta.min_choice
//   |> Options.get_fail("todo: fall back to unmolded");
// };

let process_changed =
    (
      ~prev_ctx,
      ~change: Change.t,
      ~next_fill,
      changed: Result.t(Ctx.t, Cell.t),
    ) =>
  switch (changed) {
  | Error(fill) =>
    // removed empty token
    let next_fill =
      Cell.mark_ends_dirty(Cell.Space.merge(fill, next_fill))
      |> Cell.mark_end_ungrouted(~side=R);
    (prev_ctx, next_fill);
  | Ok(ctx) =>
    //  P.show("-- Modify.insert_toks/tok/Ok ctx", Ctx.show(ctx));
    let (face, rest) = Ctx.pull(~from=L, ctx);
    switch (face, next_fill.marks.cursor) {
    // if molded token is longer than original, then move cursor out of
    // next_fill and into molded token at the end of its text
    | (Node(molded), Some(Point({hand, path: []})))
        when Token.length(molded) > Token.Unmolded.length(change.tok) =>
      let marks = {...next_fill.marks, cursor: None};
      let next_fill = {...next_fill, marks};
      let molded =
        Token.put_cursor(
          Point(Caret.mk(hand, Token.Unmolded.length(change.tok))),
          molded,
        );
      let ctx = Ctx.push(~onto=L, molded, ~fill=Cell.dirty, rest);
      (ctx, next_fill);
    | _ => (ctx, next_fill)
    };
  };

let apply_leading_changes =
    (cs: Changes.t, ctx: Ctx.t): ((Cell.t, Ctx.t), bool) =>
  cs
  |> Chain.fold_left(
       fill => ((ctx, fill), false),
       (((ctx, fill), expanded), c, next_fill: Cell.t) => {
         let followed_by_cursor = Option.is_some(next_fill.marks.cursor);
         let (melded, expanded', _) =
           apply_change(~followed_by_cursor, c, ~fill, ctx)
           |> Oblig.Delta.min_choice
           |> Options.get_fail("todo: fall back to unmolded");
         (
           process_changed(~prev_ctx=ctx, ~change=c, ~next_fill, melded),
           expanded || expanded',
         );
       },
     )
  |> Tuples.map_fst(Tuples.swap);

let apply_last_change =
    (~next: Cell.t, c: Change.t, ~fill: Cell.t, ctx: Ctx.t)
    : Choice.t(Options.Thunk.t(((Ctx.t, Cell.t), bool, bool))) => {
  open Choice.Syntax;
  let followed_by_cursor = Option.is_some(next.marks.cursor);
  let+ changed = apply_change(~followed_by_cursor, c, ~fill, ctx);
  () => {
    open Options.Syntax;
    let+ (melded, expanded, restrict_obligs) = changed();
    let (ctx, fill) =
      process_changed(~prev_ctx=ctx, ~change=c, ~next_fill=next, melded);
    ((ctx, fill), expanded, restrict_obligs);
  };
};

let apply_changes =
    (changes: Changes.t, ctx: Ctx.t)
    : Choice.t(Options.Thunk.t(((Ctx.t, Cell.t), bool, bool))) =>
  switch (Chain.(unlink(rev(changes)))) {
  | Error(fill) => Choice.one(() => Some(((ctx, fill), false, false)))
  | Ok((last_fill, last_change, leading_changes)) =>
    let ((fill, ctx), expanded) =
      apply_leading_changes(Chain.rev(leading_changes), ctx);
    // P.log("--- Modify.apply_changes/applied leading changes");
    // P.show("ctx", Ctx.show(ctx));
    // P.show("fill", Cell.show(fill));
    // P.show("expanded", string_of_bool(expanded));
    apply_last_change(~next=last_fill, last_change, ~fill, ctx)
    |> Choice.map((changed, ()) => {
         open Options.Syntax;
         let+ ((ctx, fill), expanded', restrict_obligs) = changed();
         ((ctx, fill), expanded || expanded', restrict_obligs);
       });
  };

// let apply_changes =
//     (cs: Changes.t, ctx: Ctx.t)
//     : (Cell.t, Choice.t(Options.Thunk.t((Ctx.t, bool)))) =>
//   cs
//   |> Chain.fold_left(
//        fill => (Choice.One(Options.Thunk.some((ctx, false))), fill),
//        ((changed, fill), c, next_fill: Cell.t) => {
//          let (melded, _restrict_obligs) =
//            Oblig.Delta.min_choice(changed)
//            |> Options.get_fail("todo: fall back to unmolded");
//          let (ctx, fill) =
//           switch (melded) {
//           | Ok(ctx) =>  x
//           | Error(prev_fill) =>
//             // removed redundant token
//             let fill =
//              Cell.mark_ends_dirty(Cell.Space.merge(prev_fill, fill))
//              |> Cell.mark_end_ungrouted(~side=R);

//           };
//         let followed_by_cursor = Option.is_some(next_fill.marks.cursor);
//          (apply_change(~followed_by_cursor, c, ~fill, ctx), next_fill);
//        },
//      )
//   |> Tuples.swap;

// mold each token against the ctx, using each preceding cell as its fill, and
// return the total ctx and the final remaining fill to be used when subsequently
// remolding
// let insert_toks =
//     (toks: Chain.t(Cell.t, Token.Unmolded.t), ctx: Ctx.t): (Ctx.t, Cell.t) => {
//   toks
//   |> Chain.fold_left(
//        fill => (ctx, fill),
//        ((ctx, fill), tok, next_fill) => {
//          //  P.log("--- Modify.insert_toks/tok");
//          //  P.show("tok", Token.Unmolded.show(tok));
//          //  P.show("fill", Cell.show(fill));
//          //  P.show("ctx", Ctx.show(ctx));
//          switch (mold(ctx, ~fill, tok)) {
//          | Ok(ctx) =>
//            //  P.show("-- Modify.insert_toks/tok/Ok ctx", Ctx.show(ctx));
//            let (face, rest) = Ctx.pull(~from=L, ctx);
//            switch (face, next_fill.marks.cursor) {
//            // if molded token is longer than original, then move cursor out of
//            // next_fill and into molded token at the end of its text
//            | (Node(molded), Some(Point({hand, path: []})))
//                when Token.length(molded) > Token.Unmolded.length(tok) =>
//              let marks = {...next_fill.marks, cursor: None};
//              let next_fill = {...next_fill, marks};
//              let molded =
//                Token.put_cursor(
//                  Point(Caret.mk(hand, Token.Unmolded.length(tok))),
//                  molded,
//                );
//              let ctx = Ctx.push(~onto=L, molded, ~fill=Cell.dirty, rest);
//              (ctx, next_fill);
//            | _ => (ctx, next_fill)
//            };
//          | Error(fill) =>
//            //  P.log("--- Modify.insert_toks/tok/Error removed");
//          }
//        },
//      );
// };

// let meld_remold =
//     (~expanding=false, prev, tok: Token.t, next, ctx: Ctx.t)
//     : option((Grouted.t, Ctx.t)) => {
//   open Options.Syntax;
//   // P.log("--- Modify.meld_remold");
//   // P.show("prev", Cell.show(prev));
//   // P.sexp("tok", Token.sexp_of_t(tok));
//   // P.show("next", Cell.show(next));
//   // P.show("ctx", Ctx.show(ctx));
//   let ((l, r), rest) = Ctx.unlink_stacks(ctx);
//   let* (grouted, l) =
//     Melder.push(tok, ~fill=prev, l, ~onto=L, ~repair=Molder.remold);
//   // P.log("--- Modify.meld_remold/pushed");
//   // P.show("grouted", Grouted.show(grouted));
//   // P.show("l", Stack.show(l));
//   let is_redundant =
//     tok.text == ""
//     && (
//       Token.is_complete(tok)
//       || Grouted.is_neq(grouted)
//       || Option.is_some(Grouted.is_eq(grouted))
//       && l.slope == []
//     );
//   if (is_redundant) {
//     Effects.remove(tok);
//     let fill = Cell.Space.merge(prev, ~fill=Cell.degrouted, next);
//     Some(remold(~fill, ctx));
//   } else {
//     let connected = Stack.connect(Effects.insert(tok), grouted, l);
//     let ctx =
//       connected.bound == l.bound
//         ? Ctx.link_stacks((connected, r), rest)
//         : Ctx.map_hd(
//             Frame.Open.cat(Stack.(to_slope(connected), to_slope(r))),
//             rest,
//           );
//     // todo: generalize this to other expansion triggers
//     let ctx =
//       expanding
//         ? ctx |> Ctx.push(~onto=L, Token.space()) |> Ctx.trim_space(~side=R)
//         : ctx;
//     let remolded = remold(~fill=next, ctx);
//     // P.log("--- meld_remold/not redundant");
//     // P.show("tok", Token.show(tok));
//     // P.show("ctx", Ctx.show(ctx));
//     // P.show("remolded", Grouted.show(fst(remolded)));
//     // P.show("remolded ctx", Ctx.show(snd(remolded)));
//     // P.show("effects", Fmt.(to_to_string(list(Effects.pp), Effects.log^)));
//     switch (tok.mtrl) {
//     | Tile((lbl, _))
//         // delay expansion if obligations more severe than holes
//         when
//           !expanding
//           && !Label.is_instant(lbl)
//           && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
//           // this is necessary when deleting delims to empty ghosts
//           && !Token.is_empty(tok)
//           // only delay expansion for tokens followed by caret (#126)
//           && Option.(is_some(tok.marks) || is_some(next.marks.cursor)) =>
//       None
//     | _ => Some(remolded)
//     };
//   };
// };

// let expand_remold =
//     (tok: Token.Unmolded.t, ~fill, ctx: Ctx.t)
//     : (Token.t, (Grouted.t, Ctx.t)) => {
//   switch (
//     Molder.candidates(tok)
//     |> Oblig.Delta.minimize(tok =>
//          meld_remold(~expanding=true, Cell.dirty, tok, fill, ctx)
//          |> Option.map(r => (tok, r))
//        )
//   ) {
//   | Some(r) => r
//   | None =>
//     let tok = Token.Unmolded.defer(tok);
//     let r =
//       meld_remold(~expanding=true, Cell.dirty, tok, fill, ctx)
//       |> Options.get_fail(
//            "bug: at least deferred candidate should have succeeded",
//          );
//     (tok, r);
//   };
// };

// let try_expand = (s: string, z: Zipper.t): option(Zipper.t) => {
//   open Options.Syntax;
//   let effects = Effects.log^;
//   let* () = Options.of_bool(String.starts_with(~prefix=" ", s));
//   // todo: check if in middle of token
//   let (face, rest) = Ctx.pull(~from=L, z.ctx);
//   let* tok = Delim.is_tok(face);
//   // if expandable, consider all expandable const labels
//   let* expanded = expand(tok);
//   let (molded, (remolded, ctx)) =
//     expand_remold(expanded, ~fill=Cell.point(~dirty=true, Focus), rest);
//   molded == tok
//     ? {
//       Effects.log := effects;
//       None;
//     }
//     : return(finalize(remolded, ctx));
// };

// let mold_remold =
//     (prev, tok: Token.Unmolded.t, next, ctx: Ctx.t): (Grouted.t, Ctx.t) => {
//   open Options.Syntax;
//   // P.log("--- Modify.mold_remold");
//   // P.show("prev", Cell.show(prev));
//   // P.show("tok", Token.Unmolded.show(tok));
//   // P.show("next", Cell.show(next));
//   // P.show("ctx", Ctx.show(ctx));
//   let- () =
//     Molder.candidates(tok)
//     @ (tok.text == "" ? [] : [Token.Unmolded.defer(tok)])
//     |> Oblig.Delta.minimize(tok => meld_remold(prev, tok, next, ctx));
//   assert(tok.text == "");
//   let fill = Cell.Space.merge(prev, ~fill=Cell.degrouted, next);
//   remold(~fill, ctx);
// };

// let insert_remold =
//     (toks: Chain.t(Cell.t, Token.Unmolded.t), ctx: Ctx.t)
//     : (Grouted.t, Ctx.t) => {
//   // P.log("--- Modify.insert_remold");
//   // P.show("ctx", Ctx.show(ctx));
//   switch (Chain.(unlink(rev(toks)))) {
//   | Error(cell) => remold(~fill=cell, ctx)
//   | Ok((next, tok, toks)) =>
//     // P.log("--- Modify.insert_remold/Ok");
//     // P.show("next", Cell.show(next));
//     // P.show("tok", Token.Unmolded.show(tok));
//     // P.show("toks", Chain.show(Cell.pp, Token.Unmolded.pp, toks));
//     let (ctx, prev) = insert_toks(Chain.rev(toks), ctx);
//     // P.show("inserted toks ctx", Ctx.show(ctx));
//     // P.show("prev", Cell.show(prev));
//     mold_remold(prev, tok, next, ctx);
//   };
// };

// let apply_changes_and_remold =
//     (changes: Changes.t, ctx: Ctx.t): Choice.t(Options.Thunk.t((Grouted.t, Ctx.t))) => {
//   switch (Chain.(unlink(rev(changes)))) {
//   | Error(fill) => Choice.one(() => Some(remold(~fill, ctx)))
//   | Ok((last_fill, last_change, leading_changes)) =>
//     open Choice.Syntax;
//     let (fill, ctx) = apply_leading_changes(Chain.rev(leading_changes), ctx);
//     let+ changed = apply_last_change(~next=last_fill, last_change, ~fill, ctx);
//     () => {
//       open Options.Syntax;
//       let* ((ctx, fill), restrict_obligs) = changed();
//       let (remolded, ctx) = remold(~fill, ctx);
//       restrict_obligs && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
//         ? None : Some((remolded, ctx));
//     };
//   };
// };

// let apply_remold =
//     (changes: Choice.t((Changes.t, Ctx.t))): (Grouted.t, Ctx.t) => {
//   let changed = {
//     open Choice.Syntax;
//     let* (changes, ctx) = changes;
//     let (fill, changed) = apply_changes(changes, ctx);
//     let+ changed = changed;
//     () => {
//       open Options.Syntax;
//       let* (ctx, restrict_obligs) = changed();
//       let (remolded, ctx) = remold(~fill, ctx);
//       restrict_obligs && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
//         ? None : Some((remolded, ctx));
//     };
//   };
//   changed
//   |> Oblig.Delta.min_choice
//   |> Options.get_fail("bug: failed to apply changes");
// };

let apply_remold = (changes, ctx) => {
  open Choice.Syntax;
  let+ changed = apply_changes(changes, ctx);
  () => {
    open Options.Syntax;
    let* ((ctx, fill), expanded, restrict_obligs) = changed();
    // P.log("--- Modify.apply_remold/changed");
    // P.show("ctx", Ctx.show(ctx));
    // P.show("fill", Cell.show(fill));
    // P.show("expanded", string_of_bool(expanded));
    // P.show("restrict_obligs", string_of_bool(restrict_obligs));
    let (remolded, ctx) = remold(~fill, ctx);
    // P.show("effects", Fmt.(to_to_string(list(Effects.pp), Effects.log^)));
    // P.show("delta", Oblig.Delta.show(Oblig.Delta.of_effects(Effects.log^)));
    !expanded
    && restrict_obligs
    && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
      ? None : Some((remolded, ctx));
  };
};

// delete_sel clears the textual content of the current selection (doing nothing if
// the selection is empty). this entails dropping all of the zigg's cells and
// remelding the zigg's tokens as empty ghosts onto (the left side of) the ctx. in
// the case of tokens at the ends of the selection that are split by the selection
// boundaries, the selection-external affixes of those tokens are preserved.
let delete_sel = (d: Dir.t, z: Zipper.t): Zipper.t => {
  // P.log("--- Modify.delete_sel");
  Mode.set(Deleting(d));
  switch (z.cur) {
  | Point(_) => z
  | Select(sel) =>
    // prune ctx of any duplicated tokens
    let (sites, ctx) = Zipper.cursor_site(z);
    let (l, r) = Option.get(Cursor.get_select(sites));
    let deleted_toks =
      Zigg.tokens(sel.range)
      |> (
        l == Between
          ? Lists.map_hd(add_edge(~hand=sel.focus == L ? Focus : Anchor, L))
          : Fun.id
      )
      |> (
        r == Between
          ? Lists.map_ft(add_edge(~hand=sel.focus == R ? Focus : Anchor, R))
          : Fun.id
      )
      |> delete_toks(d);
    // P.log("--- Modify.delete_sel/Select");
    // P.show("ctx sans sites", Ctx.show(ctx));
    // P.show("site l", Zipper.Site.show(l));
    // P.show("site r", Zipper.Site.show(r));
    // P.show(
    //   "deleted_toks",
    //   Chain.show(Cell.pp, Token.Unmolded.pp, deleted_toks),
    // );
    let (remolded, ctx) =
      apply_remold(deleted_toks, ctx)
      |> Oblig.Delta.min_choice
      |> Options.get_fail("bug: failed to apply delete changes");
    finalize(remolded, ctx);
  };
};

let try_truncate = (z: Zipper.t) => {
  switch (z.cur) {
  | Point(_) => None
  | Select(sel) =>
    // P.log("--- Modify.try_truncate");
    // prune ctx of any duplicated tokens
    let (sites, ctx) = Zipper.cursor_site(z);
    let (l, r) = Option.get(Cursor.get_select(sites));
    // temporarily denormalize cursor for more convenient processing
    switch (
      Zigg.tokens(sel.range)
      |> (
        l == Between
          ? Lists.map_hd(add_edge(~hand=sel.focus == L ? Focus : Anchor, L))
          : Fun.id
      )
      |> (
        r == Between
          ? Lists.map_ft(add_edge(~hand=sel.focus == R ? Focus : Anchor, R))
          : Fun.id
      )
    ) {
    | [tok] =>
      switch (Token.split_text(tok)) {
      | Some((l, _, "")) when !Strings.is_empty(l) =>
        // P.log("--- Modify.try_truncate/success");
        let tok = {
          ...tok,
          text: l,
          // renormalize cursor
          marks: Some(Point(Caret.focus(Utf8.length(l)))),
        };
        ctx
        |> Ctx.push(~onto=L, tok)
        |> (Token.is_complete(tok) ? Fun.id : Ctx.push(~onto=R, tok))
        |> Zipper.mk
        |> Option.some;
      | _ => None
      }
    | _ => None
    };
  };
};

let delete = (d: Dir.t, z: Zipper.t) => {
  open Options.Syntax;
  // P.log("--- Modify.delete");
  // P.show("z", Zipper.show(z));
  Mode.set(Deleting(d));
  let+ z =
    Cursor.is_point(z.cur) ? Select.hstep(~char=true, d, z) : return(z);
  let- () = try_truncate(z);
  // P.log("didn't truncate");
  // P.show("selected", Zipper.show(z));
  delete_sel(d, z);
};

// let insert = (s: string, z: Zipper.t) => {
//   open Options.Syntax;
//   let z = delete_sel(L, z);

//   Mode.set(Inserting(s));
//   // P.log("--- Modify.insert");
//   let- () = try_expand(s, z);
//   // P.log("didn't expand");
//   let- () = try_move(s, z);
//   // P.log("didn't move");
//   let- () = try_extend(s, z);
//   // P.log("didn't extend");

//   let (remolded, ctx) =
//     relabel(s, z)
//     |> Oblig.Delta.minimize(((toks, ctx)) =>
//          Some(insert_remold(toks, ctx))
//        )
//     |> Option.get;
//   finalize(remolded, ctx);
// };

let insert = (s: string, z: Zipper.t) => {
  // open Options.Syntax;
  let z = delete_sel(L, z);
  // let- () = try_move(s, z);
  Mode.set(Inserting(s));
  let (remolded, ctx) =
    relabel(s, z)
    |> Choice.bind(Funs.uncurry(apply_remold))
    // |> Choice.map((changed, ()) => {
    //   let* ((ctx, fill), restrict_obligs) = changed();
    //   let (remolded, ctx) = remold(~fill, ctx);
    //   restrict_obligs && Oblig.Delta.(not_hole(of_effects(Effects.log^)))
    //     ? None : Some((remolded, ctx));
    // })
    |> Oblig.Delta.min_choice
    |> Options.get_fail("bug: failed to apply insert changes");
  finalize(remolded, ctx);
};
