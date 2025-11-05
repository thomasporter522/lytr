// todo: add options for skipping walds/melds and use in selection
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // single step
  | Step(Dir2.t)
  // skip to end
  | Skip(Dir2.t)
  // jump to absolute loc
  | Jump(Loc.t);

let unselect = (~toward as _=?, b: Buffer.t) =>
  // With simple buffer model, unselect just returns the buffer as-is
  // since there's no selection state to clear
  b;

// Helper to clamp cursor position within buffer bounds
let clamp_cursor = (cursor: int, text: string): int => {
  let len = String.length(text);
  max(0, min(cursor, len));
};

// Move cursor one character in the given direction
let hstep_char = (d: Dir.t, b: Buffer.t): option(Buffer.t) => {
  let new_cursor =
    switch (d) {
    | L => b.cursor - 1
    | R => b.cursor + 1
    };
  let clamped_cursor = clamp_cursor(new_cursor, b.text);
  clamped_cursor == b.cursor
    ? None  // No movement occurred
    : Some({
        ...b,
        cursor: clamped_cursor,
      });
};

let hstep = (d: Dir.t, b: Buffer.t): option(Buffer.t) => {
  hstep_char(d, b);
};
// let rec hstep_n = (n: int, z: Zipper.t): Zipper.t => {
//   let step = (d, z) =>
//     hstep(d, z) |> Options.get_exn(Invalid_argument("Move.hstep_n"));
//   switch (n) {
//   | _ when n < 0 => z |> step(L) |> hstep_n(n + 1)
//   | _ when n > 0 => z |> step(R) |> hstep_n(n - 1)
//   | _zero => z
//   };
// };

// Find the next/previous line in the buffer
let find_line_start =
    (text: string, from_pos: int, direction: Dir.t): option(int) => {
  let len = String.length(text);
  let pos = clamp_cursor(from_pos, text);

  switch (direction) {
  | L =>
    // Move to previous line
    let rec find_prev_newline = i =>
      if (i < 0) {
        Some
          (0); // Beginning of text
      } else if (text.[i] == '\n') {
        Some
          (i + 1); // Start of line after newline
      } else {
        find_prev_newline(i - 1);
      };

    // First, find the start of current line
    let current_line_start = {
      let rec find_current = i =>
        if (i <= 0) {
          0;
        } else if (text.[i - 1] == '\n') {
          i;
        } else {
          find_current(i - 1);
        };
      find_current(pos);
    };

    // Then find the previous line
    current_line_start > 0 ? find_prev_newline(current_line_start - 2) : None;

  | R =>
    // Move to next line
    let rec find_next_newline = i =>
      if (i >= len) {
        None; // End of text
      } else if (text.[i] == '\n') {
        Some
          (i + 1); // Start of next line
      } else {
        find_next_newline(i + 1);
      };
    find_next_newline(pos);
  };
};

let vstep = (~round_tok=?, ~save_anchor=false, d: Dir.t, b: Buffer.t) => {
  let _ = (round_tok, save_anchor); // Suppress unused warnings
  switch (find_line_start(b.text, b.cursor, d)) {
  | None => None // No movement possible
  | Some(new_pos) =>
    Some({
      ...b,
      cursor: new_pos,
    })
  };
};

// Find start/end of current line or start/end of document
let find_line_boundary = (text: string, cursor: int, d2: Dir2.t): int => {
  let len = String.length(text);
  let pos = clamp_cursor(cursor, text);

  switch (d2) {
  | H(L) =>
    // Start of current line
    let rec find_line_start = i =>
      if (i <= 0) {
        0;
      } else if (text.[i - 1] == '\n') {
        i;
      } else {
        find_line_start(i - 1);
      };
    find_line_start(pos);
  | H(R) =>
    // End of current line
    let rec find_line_end = i =>
      if (i >= len) {
        len;
      } else if (text.[i] == '\n') {
        i;
      } else {
        find_line_end(i + 1);
      };
    find_line_end(pos);
  | V(L) => 0 // Start of document
  | V(R) => len // End of document
  };
};

let skip =
    (~round_tok=?, ~save_anchor=false, d2: Dir2.t, b: Buffer.t)
    : option(Buffer.t) => {
  let _ = (round_tok, save_anchor); // Suppress unused warnings
  let new_cursor = find_line_boundary(b.text, b.cursor, d2);
  new_cursor == b.cursor
    ? None  // No movement
    : Some({
        ...b,
        cursor: new_cursor,
      });
};

// Convert Loc.t to simple buffer position (for now, just use a simple mapping)
let loc_to_pos = (loc: Loc.t, text: string): int => {
  // Simple implementation: treat loc as {row, col} and find position
  let len = String.length(text);
  let target_row = max(0, loc.row);
  let target_col = max(0, loc.col);

  let rec find_pos = (pos, current_row, current_col) =>
    if (pos >= len || current_row > target_row) {
      pos;
    } else if (current_row == target_row && current_col >= target_col) {
      pos;
    } else if (text.[pos] == '\n') {
      find_pos(pos + 1, current_row + 1, 0);
    } else {
      find_pos(pos + 1, current_row, current_col + 1);
    };

  clamp_cursor(find_pos(0, 0, 0), text);
};

let jump =
    (~round_tok=?, ~save_anchor=false, loc: Loc.t, b: Buffer.t)
    : option(Buffer.t) => {
  let _ = (round_tok, save_anchor); // Suppress unused warnings
  let new_cursor = loc_to_pos(loc, b.text);
  new_cursor == b.cursor
    ? None  // No movement
    : Some({
        ...b,
        cursor: new_cursor,
      });
};

// todo: need to return none in some more cases when no visible movement occurs
let perform =
  fun
  | Step(H(d)) => hstep(d)
  | Step(V(d)) => vstep(d)
  | Skip(d2) => skip(d2)
  | Jump(loc) => jump(loc);
