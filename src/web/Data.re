let t0_transcribe = {|let x = 1 in
x + 2|};

let t0_modify = {|let x = 1 in
let y = 2 in
x + y|};

let t1_transcribe = {|fun (center, p) ->
let (x1, y1) = center in
let (x2, y2) = p in
let r = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2)) in
circle(center, r)|};

let t1_modify = {|let dist =
fun (p1, p2) ->
let (x1, y1) = p1 in
let (x2, y2) = p2 in
sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
in
fun (center, p) ->
let r = dist(center, p) in
circle(center, r)|};

let t2_transcribe = {|fun (p1, p2) ->
let mark =
fun center ->
let r = 4 in
circle(center, r)
in
[mark(p1), line(p1, p2), mark(p2)]|};

let t2_modify = {|fun (square, p1, p2) ->
let mark =
fun center ->
if square then
let (x, y) = center in
rect(x - 2, y - 2, 4, 4)
else
let r = 4 in
circle(center, r)
in
[mark(p1), line(p1, p2), mark(p2)]|};

let t3_transcribe = {|shapes
|> map(rotate(pi / 4))
|> map(translate(6, 7))
|> filter(fun shape -> area(shape) < 50)
|> map(dilate(5))|};

let t3_modify = {|shapes
|> filter(fun shape -> area(shape) < 50)
|> map(dilate(5))
|> map(rotate(pi / 4))
|> map(translate(6, 7))|};

let emoji_paint = {|type Emoji = None + Smile + Laugh in
let Row = Int in
let Col = Int in
let Grid = [[Emoji]] in

let Model = (
  Grid,
  Emoji,
  [Emoji]
  )
in

let Action =
  StampEmoji(Row, Col)
  + ClearCell(Row, Col)
  + SelectEmoji(Emoji)
in

let init: Model = (
  [
    [None, None, None],
    [None,None,None],
    [None,None,None]
  ],
  Smile,
  [
    Smile,
    Laugh
  ]) in

let updateGrid: (Grid, Row, Col, Emoji) -> Grid =
  fun (grid, row, col, emoji) ->
    List.mapi(
      fun (i, r) ->
        if i == row
        then List.mapi(fun (j, c) -> if j == col then emoji else c, r)
        else r,
     grid
  ) in

let update: (Model, Action) -> Model =
    case action
    | SelectEmoji(emoji) =>
      (grid, emoji, emojiList)
    | StampEmoji(row, col) =>
      (updateGrid(grid, row, col, selectedEmoji), selectedEmoji, emojiList)
    | ClearCell(row, col) =>
      (updateGrid(grid, row, col, None), selectedEmoji, emojiList)
in
update(init, StampEmoji(1, 1))|};

//fun ((grid, selectedEmoji, emojiList), action) ->
