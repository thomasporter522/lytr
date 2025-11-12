open LytrGrammar;

type listr('a) =
  | Nil
  | Cons(listr('a), 'a);

let sing = a => Cons(Nil, a);

/* Helper functions for right-cons lists */
let rec append = (l1: listr('a), l2: listr('a)): listr('a) =>
  switch (l2) {
  | Nil => l1
  | Cons(l2_rest, a) => Cons(append(l1, l2_rest), a)
  };

let rec map_r = (f: 'a => 'b, l: listr('a)): listr('b) =>
  switch (l) {
  | Nil => Nil
  | Cons(rest, a) => Cons(map_r(f, rest), f(a))
  };

type sharded('a) =
  | Secondary(secondary_token)
  | Shard(primary_token)
  | Form('a);

type partial_form =
  | Head(primary_token)
  | Match(partial_form, listr(sharded(partial_form)), primary_token);

type closed_form =
  | Head(primary_token)
  | Match(closed_form, listr(sharded(open_form)), primary_token)

and open_form =
  | OForm(option(open_form), closed_form, option(open_form))

and half_open_form =
  | HOForm(option(open_form), closed_form);

let rec head_of = (cf: closed_form): primary_token =>
  switch (cf) {
  | Head(t) => t
  | Match(f, _is, _t) => head_of(f)
  };

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

let face_of_form = (cf: closed_form): primary_token =>
  switch (cf) {
  | Head(t) => t
  | Match(_f, _is, t) => t
  };

let face_of_half_open_form = (hof: half_open_form): primary_token =>
  switch (hof) {
  | HOForm(_, f) => face_of_form(f)
  };

/* matching phase */

let rec shatter_partial_form =
        (pf: partial_form): listr(sharded(partial_form)) =>
  switch (pf) {
  | Head(te) => sing(Shard(te))
  | Match(f, is, te) =>
    append(shatter_partial_form(f), Cons(is, Shard(te)))
  };

let flatten_partial_form = (f: partial_form): listr(sharded(partial_form)) =>
  is_valid_end(face_of_partial_form(f))
    ? sing(Form(f)) : shatter_partial_form(f);

let rec flatten =
        (spfs: list(sharded(partial_form))): listr(sharded(partial_form)) =>
  switch (spfs) {
  | [] => Nil
  | [Secondary(se), ...s] => append(sing(Secondary(se)), flatten(s))
  | [Shard(t), ...s] => append(sing(Shard(t)), flatten(s))
  | [Form(f), ...s] => append(flatten_partial_form(f), flatten(s))
  };

type match_stack_result =
  | NoMatch
  | Match(listr(sharded(partial_form)));

let rec match_stack =
        (
          s: listr(sharded(partial_form)),
          t: primary_token,
          s_prime: list(sharded(partial_form)),
        )
        : match_stack_result =>
  switch (s) {
  | Nil => NoMatch
  | Cons(rest, Secondary(s)) =>
    match_stack(rest, t, [Secondary(s), ...s_prime])
  | Cons(rest, Shard(t_prime)) =>
    match_stack(rest, t, [Shard(t_prime), ...s_prime])
  | Cons(rest, Form(f)) =>
    switch (match_token(face_of_partial_form(f), t)) {
    | NoMatch => match_stack(rest, t, [Form(f), ...s_prime])
    | Match => Match(Cons(rest, Form(Match(f, flatten(s_prime), t))))
    }
  };

let match_stack_init =
    (s: listr(sharded(partial_form)), t: primary_token): match_stack_result =>
  match_stack(s, t, []);

let match_push =
    (s: listr(sharded(partial_form)), t: token)
    : listr(sharded(partial_form)) =>
  switch (t) {
  | Secondary(se) => Cons(s, Secondary(se))
  | Primary(t) =>
    switch (match_stack_init(s, t)) {
    | Match(s_prime) => s_prime
    | NoMatch =>
      switch (is_valid_start(t)) {
      | false => Cons(s, Shard(t))
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

type compare_tokens_result =
  | Shift // first thing wants the second as a child
  | Reduce // second thing wants the first as a child
  | Roll; // neither can be the other's child
// Fuse // for associative operators

/* Precondition: t1 ends a form and t2 starts a form */
let compare_tokens =
    (t1: primary_token, t2: primary_token): compare_tokens_result => {
  let (_, prec1_r) = prec(t1);
  let (prec2_l, _) = prec(t2);
  switch (prec1_r, prec2_l) {
  | (Precedence(p1), Precedence(p2)) =>
    if (p1 < p2) {
      Shift;
    } else if (p1 > p2) {
      Reduce;
    } else {
      // Roll?
      failwith("impossible: precedence collision");
    }
  | (Uninterested, Precedence(_)) => Reduce
  | (Precedence(_), Uninterested) => Shift
  | (Uninterested, Uninterested) => Roll
  | (Interior, _)
  | (_, Interior) => failwith("impossible: precondition violated")
  };
};

type wants_left_child_result =
  | Yes
  | No;

/* need only consider when t starts a form */
let wants_left_child = (t: primary_token): wants_left_child_result =>
  switch (prec(t)) {
  | (Interior, _)
  | (Uninterested, _) => No
  | (Precedence(_), _) => Yes
  };

type op_state =
  | OS(listr(open_form), list(half_open_form));

let rec op_state_roll =
        (os: op_state, acc: option(open_form)): listr(open_form) =>
  switch (os, acc) {
  | (OS(fs, []), None) => fs
  | (OS(fs, []), Some(acc)) => Cons(fs, acc)
  | (OS(fs, [HOForm(l, f), ...s]), acc) =>
    op_state_roll(OS(fs, s), Some(OForm(l, f, acc)))
  };

/* the top "with" clause might be unnecessary if acc is not nothing */
let rec op_parse =
        (os: op_state, acc: option(open_form), f: closed_form): op_state =>
  switch (os) {
  | OS(fs, []) =>
    switch (wants_left_child(head_of(f)), acc) {
    | (Yes, _) => OS(fs, [HOForm(acc, f)])
    | (No, None) => OS(fs, [HOForm(None, f)])
    | (No, Some(acc)) => OS(Cons(fs, acc), [HOForm(None, f)])
    }
  | OS(fs, [f1, ...s]) =>
    switch (compare_tokens(face_of_half_open_form(f1), head_of(f))) {
    | Shift => OS(fs, [HOForm(acc, f), f1, ...s])
    | Reduce =>
      switch (f1) {
      | HOForm(l, f1_inner) =>
        op_parse(OS(fs, s), Some(OForm(l, f1_inner, acc)), f)
      }
    | Roll =>
      OS(op_state_roll(OS(fs, [f1, ...s]), acc), [HOForm(None, f)])
    }
  };

type sharded_op_state =
  | SOS(listr(sharded(open_form)), op_state);

let sharded_op_state_roll =
    (sos: sharded_op_state): listr(sharded(open_form)) =>
  switch (sos) {
  | SOS(fs, s) =>
    append(fs, map_r(form => Form(form), op_state_roll(s, None)))
  };

let rec close_partial_form = (pf: partial_form): closed_form =>
  switch (pf) {
  | Head(t) => Head(t)
  | Match(f, fs, t) => Match(close_partial_form(f), operatorize(fs), t)
  }

and close_sharded_partial_form =
    (spf: sharded(partial_form)): sharded(closed_form) =>
  switch (spf) {
  | Secondary(s) => Secondary(s)
  | Shard(t) => Shard(t)
  | Form(f) => Form(close_partial_form(f))
  }

and op_parse_step =
    (s: sharded_op_state, scf: sharded(closed_form)): sharded_op_state =>
  switch (scf) {
  | Secondary(se) =>
    SOS(Cons(sharded_op_state_roll(s), Secondary(se)), OS(Nil, []))
  | Shard(t) => SOS(Cons(sharded_op_state_roll(s), Shard(t)), OS(Nil, []))
  | Form(f) =>
    switch (s) {
    | SOS(fs, os) => SOS(fs, op_parse(os, None, f))
    }
  }

and op_parse_steps =
    (s: sharded_op_state, spfs: listr(sharded(partial_form)))
    : sharded_op_state =>
  switch (spfs) {
  | Nil => s
  | Cons(l, f) =>
    op_parse_step(op_parse_steps(s, l), close_sharded_partial_form(f))
  }

and operatorize =
    (fs: listr(sharded(partial_form))): listr(sharded(open_form)) =>
  sharded_op_state_roll(op_parse_steps(SOS(Nil, OS(Nil, [])), fs));
