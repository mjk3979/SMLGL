datatype direction
	= North
	| South
	| East
	| West
	| NorthWest
	| NorthEast
	| SouthWest
	| SouthEast

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
	| Door of direction

type spot = tile * actor option

fun
	  printTile (_, (SOME (Player _))) = #"F"
	| printTile (_, (SOME (Monster rep))) = rep
	| printTile (Floor, _) = #"."
	| printTile ((Wall North), _) = #"-"
	| printTile ((Wall East), _ ) = #"|";

(* Here * North * NorthEast * East * SouthEast * South * SouthWest * West * NorthWest *)
type boardSpot = spot * (spot * direction) list;

type board = boardSpot list list
