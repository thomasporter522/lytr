open Virtual_dom.Vdom;
open Tylr_core;

/* Cursor position types for mapping character indices to AST positions */
type cursor_position =
  | TokenStart(int) /* at the beginning of a token */
  | TokenEnd(int) /* at the end of a token */
  | TokenMiddle(int, int) /* within token (token_id, offset) */
  | Hole(int) /* at a hole location */
  | Between(int, int) /* between tokens (left_token_id, right_token_id) */
  | BeforeAll /* before first token */
  | AfterAll; /* after last token */

/* Position mapping entry - maps character range to cursor position */
type position_entry = {
  char_start: int,
  char_end: int,
  position: cursor_position,
  text: string /* the actual text at this position */
};

/* Position map for resolving cursor positions */
type position_map = list(position_entry);

/* Build position map by walking through the syntax tree during rendering */
let rec build_position_map =
        (original_text: string, terms: LytrParser.terms): position_map => {
  let rendered_pos = ref(0);
  let original_pos = ref(0);
  let token_id = ref(0);
  let position_entries = ref([]);

  /* Walk through terms and track positions */
  let rec walk_terms = (terms: LytrParser.terms): unit => {
    switch (terms) {
    | LytrParser.Nil => ()
    | LytrParser.Cons(rest, sharded) =>
      walk_terms(rest);
      /* Add grout between terms if not first */
      if (rest != LytrParser.Nil) {
        rendered_pos := rendered_pos^ + 1; /* grout symbol */
      };
      walk_sharded(sharded);
    };
  }

  and walk_sharded = (sharded: LytrParser.sharded(LytrParser.term)): unit => {
    switch (sharded) {
    | LytrParser.Shard(token) =>
      let token_text = LytrToken.string_of_token(token);
      add_token_entry(token_text);
    | LytrParser.Form(term) => walk_term(term)
    };
  }

  and walk_term = (term: LytrParser.term): unit => {
    switch (term) {
    | LytrParser.Parens(inner_terms) =>
      add_token_entry("(");
      walk_terms(inner_terms);
      add_token_entry(")");
    | LytrParser.Times(left, right) =>
      walk_child(left);
      add_token_entry("*");
      walk_child(right);
    | LytrParser.Negative(child) =>
      add_token_entry("-");
      walk_child(child);
    | LytrParser.Minus(left, right) =>
      walk_child(left);
      add_token_entry("-");
      walk_child(right);
    | LytrParser.Atom(atom) =>
      let text = LytrToken.string_of_atom(atom);
      add_token_entry(text);
    | LytrParser.DEBUG => add_token_entry("DEBUG")
    };
  }

  and walk_child = (child: LytrParser.child): unit => {
    switch (child) {
    | LytrParser.Hole => rendered_pos := rendered_pos^ + 1 /* hole symbol */
    | LytrParser.Term(term) => walk_term(term)
    };
  }

  and add_token_entry = (token_text: string): unit => {
    /* Skip whitespace in original text to find token position */
    while (original_pos^ < String.length(original_text)
           && (
             original_text.[original_pos^] == ' '
             || original_text.[original_pos^] == '\t'
             || original_text.[original_pos^] == '\n'
           )) {
      incr(original_pos);
    };

    let current_id = token_id^;
    incr(token_id);

    let entry = {
      char_start: original_pos^,
      char_end: original_pos^ + String.length(token_text),
      position: TokenStart(current_id),
      text: token_text,
    };

    position_entries := [entry, ...position_entries^];
    original_pos := original_pos^ + String.length(token_text);
    rendered_pos := rendered_pos^ + String.length(token_text);
  };

  walk_terms(terms);
  List.rev(position_entries^);
}

/* Extract text representation from a term */
and extract_term_text = (term: LytrParser.term): string => {
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    "(" ++ terms_to_text(inner_terms) ++ ")"
  | LytrParser.Times(left, right) =>
    child_to_text(left) ++ "*" ++ child_to_text(right)
  | LytrParser.Negative(child) => "-" ++ child_to_text(child)
  | LytrParser.Minus(left, right) =>
    child_to_text(left) ++ "-" ++ child_to_text(right)
  | LytrParser.Atom(atom) => LytrToken.string_of_atom(atom)
  | LytrParser.DEBUG => "DEBUG"
  };
}

and child_to_text = (child: LytrParser.child): string => {
  switch (child) {
  | LytrParser.Hole => "?"
  | LytrParser.Term(term) => extract_term_text(term)
  };
}

and terms_to_text = (terms: LytrParser.terms): string => {
  switch (terms) {
  | LytrParser.Nil => ""
  | LytrParser.Cons(rest, sharded) =>
    let rest_text = terms_to_text(rest);
    let current_text =
      switch (sharded) {
      | LytrParser.Shard(token) => LytrToken.string_of_token(token)
      | LytrParser.Form(term) => extract_term_text(term)
      };
    rest_text ++ current_text;
  };
};

/* Resolve cursor position from character index */
let resolve_cursor_position =
    (char_index: int, position_map: position_map): cursor_position => {
  let rec find_position = (remaining_map: position_map): cursor_position => {
    switch (remaining_map) {
    | [] => AfterAll
    | [entry, ...rest] =>
      if (char_index < entry.char_start) {
        BeforeAll;
      } else if (char_index >= entry.char_start && char_index < entry.char_end) {
        let offset = char_index - entry.char_start;
        /* Extract token ID from entry.position */
        let token_id =
          switch (entry.position) {
          | TokenStart(id) => id
          | TokenEnd(id) => id
          | TokenMiddle(id, _) => id
          | Hole(id) => id
          | Between(left, _) => left
          | BeforeAll => 0
          | AfterAll => 0
          };
        if (offset == 0) {
          entry.position;
        } else if (offset == String.length(entry.text)) {
          TokenEnd(token_id);
        } else {
          TokenMiddle(token_id, offset);
        };
      } else {
        find_position(rest);
      }
    };
  };
  find_position(position_map);
};

/* Phase 2: Layout Integration and Cursor Rendering */

/* Convert cursor position to rendered layout position */
let cursor_position_to_layout_info =
    (pos: cursor_position, position_map: position_map): option(Loc.t) => {
  /* Find the corresponding entry in position map */
  let rec find_entry = (entries: position_map): option(position_entry) => {
    switch (entries) {
    | [] => None
    | [entry, ...rest] =>
      let entry_matches =
        switch (pos, entry.position) {
        | (TokenStart(id1), TokenStart(id2)) => id1 == id2
        | (TokenEnd(id1), TokenStart(id2)) => id1 == id2
        | (TokenMiddle(id1, _), TokenStart(id2)) => id1 == id2
        | _ => false
        };
      if (entry_matches) {
        Some(entry);
      } else {
        find_entry(rest);
      };
    };
  };

  switch (find_entry(position_map)) {
  | None =>
    /* Fallback for unmapped positions */
    switch (pos) {
    | BeforeAll =>
      Some({
        row: 0,
        col: 0,
      })
    | AfterAll =>
      Some({
        row: 0,
        col: 50,
      })
    | _ =>
      Some({
        row: 0,
        col: 10,
      })
    }
  | Some(entry) =>
    /* Use the character position from original text as column */
    let col =
      switch (pos) {
      | TokenStart(_) => entry.char_start
      | TokenEnd(_) => entry.char_end
      | TokenMiddle(_, offset) => entry.char_start + offset
      | _ => entry.char_start
      };
    Some({
      row: 0,
      col,
    });
  };
};

/* Create cursor decoration using inline positioning */
let mk_cursor_at_position =
    (~position_map: position_map, ~cursor_pos: cursor_position): Node.t => {
  /* Use position info to determine cursor type and position */
  let cursor_class =
    switch (cursor_pos) {
    | TokenStart(_) => "cursor-token-start"
    | TokenEnd(_) => "cursor-token-end"
    | TokenMiddle(_, _) => "cursor-token-middle"
    | _ => "cursor-default"
    };

  /* Use position_map to determine positioning (simplified for now) */
  let _ = List.length(position_map); /* Use position_map to avoid warning */

  /* Create an inline cursor that gets positioned during rendering */
  Node.span(
    ~attrs=[
      Attr.classes(["cursor-indicator", "lytr-cursor", cursor_class]),
      Attr.create(
        "style",
        "position: relative; display: inline-block; width: 0;",
      ),
    ],
    [Node.text("│")] /* Use a thicker vertical bar */
  );
};

/* Create styled view nodes that mimic the rich token system */
let mk_styled_token = (~text, ~classes, ()) => {
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", ...classes])],
    [Node.text(text)],
  );
};

let mk_operator_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["operator", "tile"], ());

let mk_paren_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["paren", "tile"], ());

let mk_atom_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["atom", "tile"], ());

let mk_unlexed_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["atom", "tile", "unlexed"], ());

let mk_grout = (~font, ()) => {
  /* Create a simple inline grout symbol without complex positioning */
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", "hole", "lytr-grout"])],
    [
      Node.text(Util.Unicode.nbsp),
      Dec.Box.mk(
        ~font,
        ~loc={
          row: 0,
          col: 0,
        },
        [
          Dec.Token.hexagon(
            Dec.Token.Style.{
              sort: Sort.root,
              shape: (Tip.Conc, Tip.Conc),
              sil: false,
            },
            1,
          ),
        ],
      ),
    ],
  );
};

let mk_hole = (~font, ()) => {
  /* Create a simple inline grout symbol without complex positioning */
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", "hole", "lytr-grout"])],
    [
      Node.text(Util.Unicode.nbsp),
      Dec.Box.mk(
        ~font,
        ~loc={
          row: 0,
          col: 0,
        },
        [
          Dec.Token.hexagon(
            Dec.Token.Style.{
              sort: Sort.root,
              shape: (Tip.Conv, Tip.Conv),
              sil: false,
            },
            1,
          ),
        ],
      ),
    ],
  );
};

let mk_error_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["error", "grout"], ());

/* Helper function to intersperse grout between terms */
let rec intersperse_grout = (~font, nodes: list(Node.t)): list(Node.t) =>
  switch (nodes) {
  | [] => []
  | [single] => [single]
  | [first, ...rest] => [
      first,
      mk_grout(~font, ()),
      ...intersperse_grout(~font, rest),
    ]
  };

/* Convert LytrParser terms to styled nodes WITHOUT grout interspersion */
let rec view_lytr_terms_rich_no_grout =
        (~font, terms: LytrParser.terms): list(Node.t) =>
  switch (terms) {
  | LytrParser.Nil => []
  | LytrParser.Cons(rest, sharded) =>
    view_lytr_terms_rich_no_grout(~font, rest)
    @ [view_lytr_sharded_rich(~font, sharded)]
  }

/* Convert LytrParser terms to styled nodes WITH grout interspersion */
and view_lytr_terms_rich = (~font, terms: LytrParser.terms): list(Node.t) => {
  let nodes = view_lytr_terms_rich_no_grout(~font, terms);
  if (List.length(nodes) == 0) {
    [mk_hole(~font, ())];
  } else {
    intersperse_grout(~font, nodes);
  };
}

and view_lytr_sharded_rich =
    (~font, sharded: LytrParser.sharded(LytrParser.term)): Node.t =>
  switch (sharded) {
  | LytrParser.Shard(token) =>
    let text = LytrToken.string_of_token(token);
    mk_error_token(~text, ());
  | LytrParser.Form(term) => view_lytr_term_rich(~font, term)
  }

and view_lytr_term_rich = (~font, term: LytrParser.term): Node.t =>
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-parens")],
      [mk_paren_token(~text="(", ())]
      @ view_lytr_terms_rich(~font, inner_terms)
      @ [mk_paren_token(~text=")", ())],
    )
  | LytrParser.Times(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-times")],
      [
        view_lytr_child_rich(~font, left),
        mk_operator_token(~text="*", ()),
        view_lytr_child_rich(~font, right),
      ],
    )
  | LytrParser.Negative(child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-negative")],
      [
        mk_operator_token(~text="-", ()),
        view_lytr_child_rich(~font, child),
      ],
    )
  | LytrParser.Minus(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-minus")],
      [
        view_lytr_child_rich(~font, left),
        mk_operator_token(~text="-", ()),
        view_lytr_child_rich(~font, right),
      ],
    )
  | LytrParser.Atom(atom) =>
    switch (atom) {
    | Unlexed(s) => mk_unlexed_token(~text=s, ())
    | _ =>
      let text = LytrToken.string_of_atom(atom);
      mk_atom_token(~text, ());
    }

  | LytrParser.DEBUG => mk_error_token(~text="DEBUG", ())
  }

and view_lytr_child_rich = (~font, child: LytrParser.child): Node.t =>
  switch (child) {
  | LytrParser.Hole => mk_hole(~font, ())
  | LytrParser.Term(term) => view_lytr_term_rich(~font, term)
  };

/* Rich view function using styled tokens */
let view_lytr_text_rich = (~font, terms: LytrParser.terms): Node.t => {
  let styled_line = view_lytr_terms_rich(~font, terms);
  Node.div(
    ~attrs=[Attr.classes(["block", "lytr-block"])],
    [Node.span(~attrs=[Attr.class_("line")], styled_line)],
  );
};

/* Fallback simple view function */
let rec view_lytr_terms = (~font, terms: LytrParser.terms): list(Node.t) =>
  switch (terms) {
  | LytrParser.Nil => []
  | LytrParser.Cons(rest, sharded) =>
    view_lytr_terms(~font, rest) @ [view_lytr_sharded(~font, sharded)]
  }

and view_lytr_sharded =
    (~font, sharded: LytrParser.sharded(LytrParser.term)): Node.t =>
  switch (sharded) {
  | LytrParser.Shard(token) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-shard")],
      [Node.text("💥" ++ LytrToken.string_of_token(token) ++ "💥")],
    )
  | LytrParser.Form(term) => view_lytr_term(~font, term)
  }

and view_lytr_term = (~font, term: LytrParser.term): Node.t =>
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-parens")],
      [Node.text("(")]
      @ view_lytr_terms(~font, inner_terms)
      @ [Node.text(")")],
    )
  | LytrParser.Times(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-times")],
      [
        view_lytr_child(~font, left),
        Node.text("*"),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Negative(child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-negative")],
      [Node.text("-"), view_lytr_child(~font, child)],
    )
  | LytrParser.Minus(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-minus")],
      [
        view_lytr_child(~font, left),
        Node.text("-"),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Atom(atom) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-atom")],
      [Node.text(LytrToken.string_of_atom(atom))],
    )
  | LytrParser.DEBUG =>
    Node.span(~attrs=[Attr.class_("lytr-debug")], [Node.text("DEBUG")])
  }

and view_lytr_child = (~font, child: LytrParser.child): Node.t =>
  switch (child) {
  | LytrParser.Hole =>
    Node.span(~attrs=[Attr.class_("lytr-hole")], [Node.text("?")])
  | LytrParser.Term(term) => view_lytr_term(~font, term)
  };

/* Main view function - use rich version by default */
let view_lytr_text = (~font, terms: LytrParser.terms): Node.t =>
  view_lytr_text_rich(~font, terms);

/* Phase 3: Cursor-aware view function */

/* Main view function with cursor support */
let view_lytr_text_with_cursor =
    (
      ~font,
      ~cursor_index: option(int),
      ~original_text: string,
      terms: LytrParser.terms,
    )
    : Node.t => {
  let styled_line = view_lytr_terms_rich(~font, terms);

  let cursor_node =
    switch (cursor_index) {
    | None => Node.text("")
    | Some(index) =>
      let position_map = build_position_map(original_text, terms);
      let cursor_pos = resolve_cursor_position(index, position_map);
      mk_cursor_at_position(~position_map, ~cursor_pos);
    };

  Node.div(
    ~attrs=[Attr.classes(["block", "lytr-block", "with-cursor"])],
    [Node.span(~attrs=[Attr.class_("line")], styled_line), cursor_node],
  );
};

/* Enhanced version that works with the existing Page.re cursor system */
let view_lytr_text_enhanced =
    (
      ~font,
      ~cursor_index: option(int),
      ~original_text: string,
      terms: LytrParser.terms,
    )
    : (Node.t, list(Node.t)) => {
  let text_view = view_lytr_text_rich(~font, terms);

  let cursor_decorations =
    switch (cursor_index) {
    | None => []
    | Some(index) =>
      let position_map = build_position_map(original_text, terms);
      let cursor_pos = resolve_cursor_position(index, position_map);
      [mk_cursor_at_position(~position_map, ~cursor_pos)];
    };

  (text_view, cursor_decorations);
};

/* Simplified cursor-aware view function */
let view_lytr_text_with_inline_cursor =
    (
      ~font,
      ~cursor_index: option(int),
      ~original_text: string,
      terms: LytrParser.terms,
    )
    : Node.t => {
  switch (cursor_index) {
  | None => view_lytr_text_rich(~font, terms)
  | Some(target_index) =>
    /* Use the existing rich view and add cursor as overlay */
    let styled_line = view_lytr_terms_rich(~font, terms);

    /* Create a simple cursor positioned based on the target index */
    /* Use a simple character width estimation based on original text length */
    let char_width =
      if (String.length(original_text) > 0) {
        8;
      } else {
        8;
      };
    let cursor_node =
      Node.span(
        ~attrs=[
          Attr.classes(["cursor-indicator", "lytr-cursor"]),
          Attr.create(
            "style",
            Printf.sprintf(
              "left: %dpx; position: absolute;",
              target_index * char_width,
            ),
          ),
        ],
        [Node.text("│")],
      );

    Node.div(
      ~attrs=[Attr.classes(["block", "lytr-block", "with-cursor"])],
      [Node.span(~attrs=[Attr.class_("line")], styled_line), cursor_node],
    );
  };
};

/* Main view function with cursor support - for use in Page.re */
let view_lytr_text_with_cursor_support =
    (
      ~font,
      ~cursor_index: option(int)=?,
      ~original_text: string="",
      terms: LytrParser.terms,
    )
    : Node.t =>
  switch (cursor_index) {
  | None => view_lytr_text_rich(~font, terms)
  | Some(_) =>
    view_lytr_text_with_inline_cursor(
      ~font,
      ~cursor_index,
      ~original_text,
      terms,
    )
  };

/* Cursor-aware rendering - tracks positions during rendering */
type render_context = {
  font: Model.Font.t,
  original_text: string,
  cursor_index: option(int),
  mutable rendered_pos: int,
  mutable original_pos: int,
  mutable cursor_node: option(Node.t),
};

let create_render_context =
    (~font, ~original_text, ~cursor_index): render_context => {
  {
    font,
    original_text,
    cursor_index,
    rendered_pos: 0,
    original_pos: 0,
    cursor_node: None,
  };
};

/* Check if cursor should be placed at current position */
let check_cursor_position = (ctx: render_context): option(Node.t) => {
  switch (ctx.cursor_index) {
  | None => None
  | Some(target_index) =>
    if (ctx.original_pos == target_index) {
      Some(
        Node.span(
          ~attrs=[
            Attr.classes(["cursor-indicator", "lytr-cursor"]),
            Attr.create(
              "style",
              "position: relative; display: inline-block;",
            ),
          ],
          [Node.text("│")],
        ),
      );
    } else {
      None;
    }
  };
};

/* Render a token with cursor tracking */
let render_token_with_cursor =
    (ctx: render_context, text: string, classes: list(string)): Node.t => {
  /* Skip whitespace in original text */
  while (ctx.original_pos < String.length(ctx.original_text)
         && (
           ctx.original_text.[ctx.original_pos] == ' '
           || ctx.original_text.[ctx.original_pos] == '\t'
           || ctx.original_text.[ctx.original_pos] == '\n'
         )) {
    ctx.original_pos = ctx.original_pos + 1;
  };

  /* Check for cursor at start of token */
  let cursor_before = check_cursor_position(ctx);

  /* Create the token */
  let token_node = mk_styled_token(~text, ~classes, ());

  /* Update positions */
  ctx.original_pos = ctx.original_pos + String.length(text);
  ctx.rendered_pos = ctx.rendered_pos + String.length(text);

  /* Check for cursor at end of token */
  let cursor_after = check_cursor_position(ctx);

  /* Combine token with any cursors */
  let nodes =
    [
      switch (cursor_before) {
      | None => []
      | Some(c) => [c]
      },
      [token_node],
      switch (cursor_after) {
      | None => []
      | Some(c) => [c]
      },
    ]
    |> List.flatten;

  Node.span(~attrs=[], nodes);
};

/* Cursor-aware rendering for terms */
let rec view_lytr_terms_cursor_aware =
        (ctx: render_context, terms: LytrParser.terms): list(Node.t) => {
  switch (terms) {
  | LytrParser.Nil => []
  | LytrParser.Cons(rest, sharded) =>
    view_lytr_terms_cursor_aware(ctx, rest)
    @ [view_lytr_sharded_cursor_aware(ctx, sharded)]
  };
}

and view_lytr_sharded_cursor_aware =
    (ctx: render_context, sharded: LytrParser.sharded(LytrParser.term))
    : Node.t =>
  switch (sharded) {
  | LytrParser.Shard(token) =>
    let text = LytrToken.string_of_token(token);
    mk_error_token(~text, ());
  | LytrParser.Form(term) => view_lytr_term_cursor_aware(ctx, term)
  }

and view_lytr_term_cursor_aware =
    (ctx: render_context, term: LytrParser.term): Node.t =>
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-parens")],
      [mk_paren_token(~text="(", ())]
      @ view_lytr_terms_cursor_aware(ctx, inner_terms)
      @ [mk_paren_token(~text=")", ())],
    )
  | LytrParser.Times(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-times")],
      [
        view_lytr_child_cursor_aware(ctx, left),
        mk_operator_token(~text="*", ()),
        view_lytr_child_cursor_aware(ctx, right),
      ],
    )
  | LytrParser.Negative(child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-negative")],
      [
        mk_operator_token(~text="-", ()),
        view_lytr_child_cursor_aware(ctx, child),
      ],
    )
  | LytrParser.Minus(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-minus")],
      [
        view_lytr_child_cursor_aware(ctx, left),
        mk_operator_token(~text="-", ()),
        view_lytr_child_cursor_aware(ctx, right),
      ],
    )
  | LytrParser.Atom(atom) =>
    switch (atom) {
    | Unlexed(s) => mk_unlexed_token(~text=s, ())
    | _ =>
      let text = LytrToken.string_of_atom(atom);
      mk_atom_token(~text, ());
    }

  | LytrParser.DEBUG => mk_error_token(~text="DEBUG", ())
  }

and view_lytr_child_cursor_aware =
    (ctx: render_context, child: LytrParser.child): Node.t =>
  switch (child) {
  | LytrParser.Hole => mk_hole(~font=ctx.font, ())
  | LytrParser.Term(term) => view_lytr_term_cursor_aware(ctx, term)
  };
