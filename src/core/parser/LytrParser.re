open LytrGrammar;

type shard_mode =
  | ShardsSecondary
  | ShardsObstructive;

let shardmode: shard_mode = ShardsSecondary;

// lists that grow on the right
// used so that the left-right order in this code matches
// the left-right order of the parsed object code
type listr('a) =
  | Nil
  | Cons(listr('a), 'a);

let sing = a => Cons(Nil, a);

/* Helper functions for right-cons lists */
let rec appendr = (l1: listr('a), l2: listr('a)): listr('a) =>
  switch (l2) {
  | Nil => l1
  | Cons(l2_rest, a) => Cons(appendr(l1, l2_rest), a)
  };

let rec mapr = (f: 'a => 'b, l: listr('a)): listr('b) =>
  switch (l) {
  | Nil => Nil
  | Cons(rest, a) => Cons(mapr(f, rest), f(a))
  };

type unform =
  | Secondary(secondary_token)
  | Shard(primary_token);

// syntax forms along with with secondary syntax and unmatched tokens (shards)
type sharded('a) =
  | Unform(unform)
  | Form('a);

// a "work in progress" matched syntactic form
type partial_form =
  | Head(primary_token)
  | Match(partial_form, listr(sharded(partial_form)), primary_token);

let rec head_of_partial_form = (pf: partial_form): primary_token =>
  switch (pf) {
  | Head(t) => t
  | Match(f, _is, _t) => head_of_partial_form(f)
  };

let face_of_partial_form = (pf: partial_form): primary_token =>
  switch (pf) {
  | Head(t) => t
  | Match(_f, _is, t) => t
  };

/* matching phase */
// In this phase, matching relationships are established between tokens.
// Precedence is not brought into play yet. The main function is [match_parse]

let rec shatter_partial_form =
        (pf: partial_form): listr(sharded(partial_form)) =>
  switch (pf) {
  | Head(te) => sing(Unform(Shard(te)))
  | Match(f, is, te) =>
    appendr(shatter_partial_form(f), Cons(is, Unform(Shard(te))))
  };

// this flattens a partial form that appeared on the stack
// it is called when a match has been found that spans over the form,
// and as such such the partial form has no further chance to be extended.
// So if it's complete, it stays itself. Otherwise, it is shattered into
// its constitutents.

let flatten_partial_form = (f: partial_form): listr(sharded(partial_form)) =>
  is_valid_end(face_of_partial_form(f))
    ? sing(Form(f)) : shatter_partial_form(f);

// flattening a stack involved flattening all partial forms on the stack

let rec flatten =
        (spfs: list(sharded(partial_form))): listr(sharded(partial_form)) =>
  switch (spfs) {
  | [] => Nil
  | [Unform(u), ...s] => appendr(sing(Unform(u)), flatten(s))
  | [Form(f), ...s] => appendr(flatten_partial_form(f), flatten(s))
  };

type match_stack_result =
  | NoMatch
  | Match(listr(sharded(partial_form)));

// This is where a token searches for a match on the stack.
// It also keeps an accumulator of the part of the stack that has
// been skipped over during this search.
// If a match is found, the skipped part is "flattened" and
// placed between the new betrothed couple in the new extended partial form.

let rec match_stack =
        (
          s: listr(sharded(partial_form)),
          t: primary_token,
          s_skipped: list(sharded(partial_form)),
        )
        : match_stack_result =>
  switch (s) {
  | Nil => NoMatch
  | Cons(rest, Unform(u)) =>
    match_stack(rest, t, [Unform(u), ...s_skipped])
  | Cons(rest, Form(f)) =>
    switch (match_token(face_of_partial_form(f), t)) {
    | NoMatch => match_stack(rest, t, [Form(f), ...s_skipped])
    | MatchMorph(t') =>
      Match(Cons(rest, Form(Match(f, flatten(s_skipped), t'))))
    | Match => Match(Cons(rest, Form(Match(f, flatten(s_skipped), t))))
    }
  };

// The stack is a list of partial forms (or shards or secondary)
// Tokens are pushed on one after another onto the right
// Tokens look for a match on the stack.
// If one is not found, it becomes a shard.
// If one is found, it fuses to that form, rolling up the intermediate stack segment.

let match_push =
    (s: listr(sharded(partial_form)), t: token)
    : listr(sharded(partial_form)) =>
  switch (t) {
  | Secondary(se) => Cons(s, Unform(Secondary(se)))
  | Primary(t) =>
    switch (match_stack(s, t, [])) {
    | Match(s_prime) => s_prime
    | NoMatch =>
      switch (is_valid_start(t)) {
      | false => Cons(s, Unform(Shard(t)))
      | true => Cons(s, Form(Head(t)))
      }
    }
  };

let rec match_pushes =
        (s: listr(sharded(partial_form)), ts: list(token))
        : listr(sharded(partial_form)) =>
  switch (ts) {
  | [] => s
  | [t, ...rest] => match_pushes(match_push(s, t), rest)
  };

let match_parse = (ts: list(token)): listr(sharded(partial_form)) => {
  let tokens = [Primary(BOF), ...List.append(ts, [Primary(EOF)])];
  let result = match_pushes(Nil, tokens);
  switch (result) {
  | Cons(Nil, Form(Match(Head(BOF), is, EOF))) => is
  | _ => failwith("impossible matching")
  };
};

/* operatorize phase */

// In this phase, operator precedence is used to give matched forms
// their left and right children: their open children, as opposed to
// their closed children which lie between matched tokens of the form.

// It is a "shift-reduce-roll" parser, with that last option indicating
// that neither of the compared faces can take the other as a child,
// and that the two pieces must simply stay at arms length in a
// "multiterm" - a list of terms that occupy the same closed child position.

// type secondaries = listr(secondary_token);
type unforms = listr(unform);

// a complete matched syntactic form
type closed_form =
  | Head(primary_token)
  | Match(closed_form, listr(sharded(open_form)), primary_token)

// a complete matched syntactic form with (possible) children on the left and right
and open_form =
  | OForm(
      option(open_form),
      unforms,
      closed_form,
      unforms,
      option(open_form),
    );

// a complete matched syntactic form with a (possible) child on the left
type half_open_form =
  | HOForm(option(open_form), unforms, closed_form, unforms);

let rec head_of = (cf: closed_form): primary_token =>
  switch (cf) {
  | Head(t) => t
  | Match(f, _is, _t) => head_of(f)
  };

let face_of_form = (cf: closed_form): primary_token =>
  switch (cf) {
  | Head(t) => t
  | Match(_f, _is, t) => t
  };

let face_of_half_open_form = (hof: half_open_form): primary_token =>
  switch (hof) {
  | HOForm(_, _, f, _) => face_of_form(f)
  };

type compare_tokens_result =
  | Shift // first thing wants the second as a child
  | Reduce // second thing wants the first as a child
  | Roll; // neither can be the other's child
// Fuse // for associative operators?

/* Precondition: t1 ends a form and t2 starts a form */
let compare_tokens =
    (t1: primary_token, t2: primary_token): compare_tokens_result => {
  switch (snd(prec(t1)), fst(prec(t2))) {
  | (Precedence(p1), Precedence(p2)) when p1 < p2 => Shift
  | (Precedence(p1), Precedence(p2)) when p1 > p2 => Reduce
  | (Precedence(_), Precedence(_)) => failwith("precedence collision")
  | (Uninterested, Precedence(_)) => Reduce
  | (Precedence(_), Uninterested) => Shift
  | (Uninterested, Uninterested) => Roll
  | (Interior, _)
  | (_, Interior) => failwith("precondition violated")
  };
};

/* need only consider when t starts a form */
let wants_left_child = (t: primary_token): bool =>
  switch (prec(t)) {
  | (Interior, _)
  | (Uninterested, _) => false
  | (Precedence(_), _) => true
  };

type op_state =
  | OS(listr(sharded(open_form)), listr(half_open_form));

let rec op_state_roll =
        (s: op_state, acc: option(open_form)): listr(sharded(open_form)) => {
  switch (s, acc) {
  | (OS(fs, Nil), None) => fs
  | (OS(fs, Nil), Some(acc)) => Cons(fs, Form(acc))
  | (OS(fs, Cons(s, HOForm(l, se1, f, se2))), None) =>
    let se2' = mapr(f => Unform(f), se2);
    let acc' = Some(OForm(l, se1, f, Nil, None));
    let rolled = op_state_roll(OS(fs, s), acc');
    appendr(rolled, se2');
  | (OS(fs, Cons(s, HOForm(l, se1, f, se2))), Some(acc)) =>
    op_state_roll(OS(fs, s), Some(OForm(l, se1, f, se2, Some(acc))))
  };
};

let rec op_push_form =
        (
          os: op_state,
          acc: option(open_form),
          se_acc: listr(unform),
          f: closed_form,
        )
        : op_state => {
  switch (os) {
  | OS(fs, Nil) =>
    switch (acc, wants_left_child(head_of(f))) {
    | (None, _) => OS(fs, sing(HOForm(None, Nil, f, Nil)))
    | (Some(acc), true) => OS(fs, sing(HOForm(Some(acc), se_acc, f, Nil)))
    | (Some(_), false) => failwith("I'm curious whether this is possible")
    // OS(Cons(fs, Form(acc)), sing(HOForm(None, Nil, f, Nil)))
    }
  | OS(fs, Cons(s, face)) =>
    switch (compare_tokens(face_of_half_open_form(face), head_of(f))) {
    | Shift => OS(fs, Cons(Cons(s, face), HOForm(acc, se_acc, f, Nil)))
    | Reduce =>
      switch (face, acc) {
      | (HOForm(l, se1, f1_inner, se2), None) =>
        let acc' = Some(OForm(l, se1, f1_inner, Nil, None));
        op_push_form(OS(fs, s), acc', appendr(se2, se_acc), f);
      | (HOForm(l, se1, f1_inner, se2), Some(acc)) =>
        let acc' = Some(OForm(l, se1, f1_inner, se2, Some(acc)));
        op_push_form(OS(fs, s), acc', se_acc, f);
      }
    | Roll =>
      let se_acc' = mapr(f => Unform(f), se_acc);
      let completed' = appendr(op_state_roll(os, acc), se_acc');
      OS(completed', sing(HOForm(None, Nil, f, Nil)));
    }
  };
};

let op_push = (os: op_state, f: sharded(closed_form)): op_state =>
  switch (shardmode) {
  | ShardsSecondary =>
    switch (os, f) {
    | (OS(completed, Nil), Unform(u)) =>
      OS(Cons(completed, Unform(u)), Nil)
    | (OS(completed, Cons(fs, HOForm(a, b, c, ses))), Unform(u)) =>
      OS(completed, Cons(fs, HOForm(a, b, c, Cons(ses, u))))
    | (os, Form(f)) => op_push_form(os, None, Nil, f)
    }
  | ShardsObstructive =>
    switch (os, f) {
    | (OS(completed, Nil), Unform(Secondary(se))) =>
      OS(Cons(completed, Unform(Secondary(se))), Nil)
    | (
        OS(completed, Cons(fs, HOForm(a, b, c, ses))),
        Unform(Secondary(se)),
      ) =>
      OS(completed, Cons(fs, HOForm(a, b, c, Cons(ses, Secondary(se)))))
    | (os, Unform(Shard(s))) =>
      OS(Cons(op_state_roll(os, None), Unform(Shard(s))), Nil)
    | (os, Form(f)) => op_push_form(os, None, Nil, f)
    }
  };

let rec close_partial_form = (f: partial_form): closed_form =>
  switch (f) {
  | Head(t) => Head(t)
  | Match(f, fs, t) => Match(close_partial_form(f), operatorize(fs), t)
  }

and close_sharded_partial_form =
    (f: sharded(partial_form)): sharded(closed_form) =>
  switch (f) {
  | Unform(u) => Unform(u)
  | Form(f) => Form(close_partial_form(f))
  }

and op_pushes = (s: op_state, fs: listr(sharded(partial_form))): op_state => {
  switch (fs) {
  | Nil => s
  | Cons(fs, f) => op_push(op_pushes(s, fs), close_sharded_partial_form(f))
  };
}

and operatorize =
    (fs: listr(sharded(partial_form))): listr(sharded(open_form)) =>
  op_state_roll(op_pushes(OS(Nil, Nil), fs), None);
