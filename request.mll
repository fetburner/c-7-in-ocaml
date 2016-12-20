{
type t =
  | Noop
  | Quit
  | Move of Board.t
}

let color = ['O' 'X']
let disc = color | '-'
let board =
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc
  disc disc disc disc disc disc disc disc

rule token = parse
  | "NOOP\r\n"
      { Noop }
  | "QUIT\r\n"
      { Quit }
  | "MOVE " (board as brd) " " (color as c) "\r\n"
      { Move (Board.of_string (Color.of_char c) brd) }
  | eof
      { raise End_of_file }
