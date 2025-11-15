open Virtual_dom.Vdom;
open Node;
open Tylr_core;

let editor_view = (model: Model.t) => {
  let text = model.buffer.text;
  let parsed = LytrAbstractor.go(LytrLexer.lex(text));
  let cursor = model.buffer.cursor;
  let len = String.length(text);
  let cursor_pos = max(0, min(cursor, len));

  // Split text at cursor position
  let (before_cursor, after_cursor) =
    if (cursor_pos == 0) {
      ("", text);
    } else if (cursor_pos >= len) {
      (text, "");
    } else {
      (
        String.sub(text, 0, cursor_pos),
        String.sub(text, cursor_pos, len - cursor_pos),
      );
    };

  let cursor_element =
    span(~attrs=[Attr.class_("cursor-indicator")], [Node.text("|")]);

  div(
    ~attrs=[Attr.id("code-container")],
    [
      Node.text(before_cursor),
      cursor_element,
      Node.text(after_cursor),
      Node.br(),
      LytrTerms.view_lytr_text(~font=model.font, parsed),
      Node.br(),
    ],
  );
};

let on_key = (~inject, ~model) => {
  let prevent_default_tab = evt =>
    Util.Key.key_tag(evt) == "Tab" ? [Effect.Prevent_default] : [];
  [
    Attr.on_keypress(_ => Effect.Prevent_default),
    Attr.on_keyup(evt =>
      Effect.Many(
        prevent_default_tab(evt)
        @ List.map(
            inject,
            Update.handle_key_event(Util.Key.mk(KeyUp, evt), ~model),
          ),
      )
    ),
    Attr.on_keydown(evt =>
      Effect.Many(
        prevent_default_tab(evt)
        @ List.map(
            inject,
            Update.handle_key_event(Util.Key.mk(KeyDown, evt), ~model),
          ),
      )
    ),
  ];
};

// let copy = (cur: Tylr_core.Zipper.Cursor.Base.t('tok)) =>
//   switch (Tylr_core.Zipper.selection_str(cur)) {
//   | None => ()
//   | Some(str) => Util.Dom.copy(str)
//   };

let get_goal = (~font: Model.Font.t, ~target_id, e): Tylr_core.Loc.t => {
  let rect = Util.Dom.get_elem_by_id(target_id)##getBoundingClientRect;
  let goal_x = float_of_int(e##.clientX);
  let goal_y = float_of_int(e##.clientY);
  {
    row: Float.to_int((goal_y -. rect##.top) /. font.row_height),
    col: Float.(to_int(round((goal_x -. rect##.left) /. font.col_width))),
  };
};

let modulo = (x, y) => {
  let result = x mod y;
  result >= 0 ? result : result + y;
};

let center_panel_view = (~inject, cur_idx, stored) => {
  let next_ed = (cur_idx + 1) mod stored;
  let prev_ed = modulo(cur_idx - 1, stored);
  let incr_ed = _ => inject(Update.Load(next_ed));
  let decr_ed = _ => inject(Update.Load(prev_ed));
  let s = Printf.sprintf("%d / %d", cur_idx + 1, stored);
  div(
    ~attrs=[Attr.id("editor-id")],
    [
      div(
        ~attrs=[Attr.class_("topbar-icon"), Attr.on_mousedown(decr_ed)],
        [Icons.back],
      ),
      div([text(s)]),
      div(
        ~attrs=[Attr.class_("topbar-icon"), Attr.on_mousedown(incr_ed)],
        [Icons.forward],
      ),
    ],
  );
};

let load_button = (~inject, idx) => {
  div(
    ~attrs=[
      Attr.id("editor-id"),
      Attr.class_("topbar-icon"),
      Attr.on_mousedown(_ =>
        Effect.sequence_as_sibling(
          inject(Update.Load(idx)), ~unless_stopped=() =>
          Effect.Stop_propagation
        )
      ),
    ],
    [Node.text(string_of_int(idx + 1))],
  );
};

let view = (~inject, model: Model.t) => {
  div(
    ~attrs=
      Attr.[
        id("page"),
        // necessary to make cell focusable
        // tabindex(0),
        on_blur(_ => {
          //Util.Dom.get_elem_by_id("page")##focus;
          Util.Dom.focus_clipboard_shim();
          Effect.Prevent_default;
        }),
        Attr.on_focus(_ => {
          Util.Dom.focus_clipboard_shim();
          Effect.Ignore;
        }),
        Attr.on_copy(_ => {
          Effect.Ignore
                 // copy(model.zipper.cur);
        }),
        Attr.on_cut(_ => {
          // copy(model.zipper.cur);
          inject(Update.PerformAction(Delete(L)))
        }),
        Attr.on_paste(evt => {
          Js_of_ocaml.Dom.preventDefault(evt);
          inject(Update.PerformAction(Insert(Util.Dom.paste(evt))));
        }),
        Attr.on_mousedown(evt => {
          Util.Dom.focus_clipboard_shim();
          let loc =
            get_goal(~font=model.font, ~target_id="code-container", evt);
          inject(PerformAction(Move(Jump(loc))));
        }),
        ...on_key(~inject, ~model),
      ],
    [
      FontSpecimen.view("font-specimen"),
      Util.Dom.clipboard_shim,
      editor_view(model),
    ],
  );
};
