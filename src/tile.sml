datatype direction
	= North
	| South
	| East
	| West
	| NorthWest
	| NorthEast
	| SouthWest
	| SouthEast

datatype openstatus
	= Open
	| Closed

type player = char
type monster = char

datatype actor
	= Player of player
	| Monster of monster

datatype furniture
	= Colidable of char
	| NonColidable of char

datatype tile
	= Floor
	| Wall of direction
	| Furniture of furniture
	| Door of openstatus

type spot = tile * actor option

fun
	  printTile (_, (SOME (Player _))) = #"F"
	| printTile (_, (SOME (Monster rep))) = rep
	| printTile (Floor, _) = #"."
	| printTile ((Wall North), _) = #"-"
	| printTile ((Wall East), _ ) = #"|"
	| printTile ((Door Closed), _) = #"+"
	| printTile ((Door Open), _) = #"="

(* Here * North * NorthEast * East * SouthEast * South * SouthWest * West * NorthWest *)
type boardSpot = spot * (spot * direction) list

type board = boardSpot list list

fun traverseBoard board f g = app (fn boardRow => ((app f boardRow); g ())) board

fun printBoard board = traverseBoard board (fn (spot, _) => print (implode [printTile spot])) (fn () => print "\n")

fun getLines file =
	let
		val line = case (TextIO.inputLine file) of
			  SOME x => x
			| NONE => ""
	in
		if (line = "")
		then
			[]
		else
			line::(getLines file)
	end

fun
	  splitLines [] = []
	| splitLines (x::lst) = (map (fn i => valOf(Int.fromString i)) (String.tokens (fn d => d = #",") (String.translate (fn c => if (c = #"\n") then "" else String.str c) x)))::(splitLines lst)

fun mkBoard lst = map (fn row => map (fn i => (((case i of
	  1 => Floor
	| 2 => Wall North
	| 3 => Wall East
	| 4 => Door Closed
	| 5 => Door Open
	), NONE), [])) row) lst

fun readBoard filename = 
	let
		val file = TextIO.openIn filename
		val lines = getLines file
		val splits = splitLines lines
		val board = mkBoard splits
	in
		board
	end

;

printBoard (readBoard "test.txt")

