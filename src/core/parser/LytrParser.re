open LytrGrammar;

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

// syntax forms along with with secondary syntax and unmatched tokens (shards)
type sharded('a) =
  | Secondary(secondary_token)
  | Shard(primary_token)
  | Form('a);

// a "work in progress" matched syntactic form
type partial_form =
  | Head(primary_token)
  | Match(partial_form, listr(sharded(partial_form)), primary_token);


// a complete matched syntactic form
type closed_form =
  | Head(primary_token)
  | Match(closed_form, listr(sharded(open_form)), primary_token)

// a complete matched syntactic form with (possible) children on the left and right
and open_form =
  | OForm(option(open_form), closed_form, option(open_form))

// a complete matched syntactic form with a (possible) child on the left
type half_open_form =
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
// In this phase, matching relationships are established between tokens. 
// Precedence is not brought into play yet. The main function is [match_parse]

let rec shatter_partial_form =
        (pf: partial_form): listr(sharded(partial_form)) =>
  switch (pf) {
  | Head(te) => sing(Shard(te))
  | Match(f, is, te) =>
    appendr(shatter_partial_form(f), Cons(is, Shard(te)))
  };

let appendr_secondary =
    (secondary: listr(secondary_token), l: listr(sharded('a))) => {
  appendr(mapr(se => Secondary(se), secondary), l);
};

// this flattens a partial form that appeared on the stack
// it is called when a match has been found that spans over the form,
// and as such such the partial form has no further chance to be extended. 
// So if it's complete, it stays itself. Otherwise, it is shattered into 
// its constitutents. 

let flatten_partial_form =
    (secondary: listr(secondary_token), f: partial_form)
    : listr(sharded(partial_form)) =>
  is_valid_end(face_of_partial_form(f))
    ? sing(Form(f)) : appendr_secondary(secondary, shatter_partial_form(f));

// flattening a stack involved flattening all partial forms on the stack

let rec flatten =
        (
          secondary: listr(secondary_token),
          spfs: list(sharded(partial_form)),
        )
        : listr(sharded(partial_form)) =>
  switch (spfs) {
  | [] => Nil
  | [Secondary(se), ...s] => flatten(Cons(secondary, se), s)
  | [Shard(t), ...s] =>
    appendr_secondary(secondary, appendr(sing(Shard(t)), flatten(Nil, s)))
  | [Form(f), ...s] =>
    appendr(flatten_partial_form(secondary, f), flatten(Nil, s))
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
  | Cons(rest, Secondary(s)) =>
    match_stack(rest, t, [Secondary(s), ...s_skipped])
  | Cons(rest, Shard(t_prime)) =>
    match_stack(rest, t, [Shard(t_prime), ...s_skipped])
  | Cons(rest, Form(f)) =>
    switch (match_token(face_of_partial_form(f), t)) {
    | NoMatch => match_stack(rest, t, [Form(f), ...s_skipped])
    | Match => Match(Cons(rest, Form(Match(f, flatten(Nil, s_skipped), t))))
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
  | Secondary(se) => Cons(s, Secondary(se))
  | Primary(t) =>
    switch (match_stack(s, t, [])) {
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

// In this phase, operator precedence is used to give matched forms 
// their left and right children: their open children, as opposed to 
// their closed children which lie between matched tokens of the form. 

// It is a "shift-reduce-roll" parser, with that last option indicating 
// that neither of the compared faces can take the other as a child, 
// and that the two pieces must simply stay at arms length in a 
// "multiterm" - a list of terms that occupy the same closed child position. 

type compare_tokens_result =
  | Shift // first thing wants the second as a child
  | Reduce // second thing wants the first as a child
  | Roll; // neither can be the other's child
// Fuse // for associative operators?

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
    appendr(fs, mapr(form => Form(form), op_state_roll(s, None)))
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
