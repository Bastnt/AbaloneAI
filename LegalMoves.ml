let marge = [|5;6;7;8;9;8;7;6;5|]

type marble = E | B | W | O

type env = { 	mutable playerColor: marble;
				mutable opponentColor: marble;
				mutable rest: ((int * (int * int) list) list);
				mutable numP: int;
				mutable numO: int;
				mutable staticValue: int
			};; 

let global = { playerColor =W ; opponentColor= B; rest = []; numP = 0; numO = 0; staticValue=0 }

let test1 =  [|    [| W; W; W; W; W |];
                  [| W; W; W; W; W; W |];
                 [| E; E; W; W; W; E; E |];
                [| E; E; E; E; E; E; E; E|];
               [| E; E; E; E; E; E; E; E; E|];
                [| E; E; E; E; E; E; E; E|];
                 [| E; E; B; B; B; E; E |];
                  [| B; B; B; B; B; B |];
                   [| B; B; B; B; B |] |]


let test2 =  [|    [| E; E; E; E; E |];
                  [| E; E; E; E; E; E |];
                 [| E; E; E; E; E; E; E |];
                [| E; E; E; E; E; E; E; E|];
               [| E; E; E; E; E; W; W; B; E|];
                [| E; E; E; E; E; E; E; E|];
                 [| E; E; E; E; E; E; E |];
                  [| E; E; E; E; E; E |];
                   [| E; E; E; E; E |] |]

let opponent p = if(p = W) then B else W

let dumpB (t : marble array array) = 
    for i=1  to (Array.length marge) do
        Printf.printf "\n";
        let max = if(i<5) then (5-i) else (i-5) in
        for a=1 to max do
            Printf.printf " ";
        done;
        for j=1 to marge.(i-1) do
            match t.(i-1).(j-1) with
                | B -> Printf.printf "X "
                | W -> Printf.printf "O "
                | E -> Printf.printf "_ "
                | _ -> Printf.printf "? "
        done;
    done;
    Printf.printf "\n"

let rec dumpL l = 
  let aux2 (x,y) = Printf.printf "%d%d " x y in
  let aux (d, ll) = Printf.printf "%d " d; List.iter aux2 ll;Printf.printf "\n"; in
  List.iter aux l;Printf.printf "\n";;

(* ======================================================================================================================================
   Finding the legal moves
   ====================================================================================================================================== *)

(* Give the position of the next marble in a given direction (dir) *)
let nextMarble pos dir = 
    let (i,j) = pos in
        match dir with
            | 1 -> (i, j+1)
            | 2 -> if(i<4) then (i+1, j+1) else (i+1, j)
            | 3 -> if(i>=4) then (i+1, j-1) else (i+1, j)
            | 4 -> (i, j-1)
            | 5 -> if(i<=4) then (i-1, j-1) else (i-1, j)
            | 6 -> if(i>4) then (i-1, j+1) else (i-1, j)
            | _ -> Printf.printf "Wrong Direction: %d" dir; exit 1

(* is this Marble out ? *)
let isOut (i,j) = 
    if(i < 0 || i >= (Array.length marge)) then true
    else if(j < 0 || j >= marge.(i)) then true
    else false

let getMarble (i,j) (board : marble  array array) = if(isOut (i,j)) then O else board.(i).(j)

(* Give a list of all neighbours *)
let giveNeighbours pos = 
    let rec aux dir = 
        match dir with
            | 7 -> []
            | _ -> let m = (nextMarble pos dir) in
                    if(isOut m) then (aux (dir+1)) else (m,dir)::(aux (dir+1))
    in (aux 1)

(* Give all moves involving pushs with one marble (and collisions, ex: --> _OOOX_ = __OOOX) *)
let pushMoves pos board player = 
    let rec next p dir nPlayer nOpponent acc = 
        let m = (nextMarble p dir) in 
            match (getMarble m board) with
                | O when (nOpponent = 0)                    -> []
                | _ when (nPlayer > 3)                      -> []
                | c when (c = player && nOpponent > 0) -> []
                | c when (c = player)                  -> (next m dir (nPlayer+1) nOpponent (m::acc))
                | c when (c = (opponent player))                -> (next m dir nPlayer (nOpponent+1) acc)
                | E when (nPlayer > nOpponent)              ->  acc
                | O when (nPlayer > nOpponent)              ->  acc
                | _ -> []
        in let rec aux l = 
                match l with
                    | [] -> []
                    | (m, d)::t -> let c = (next pos d 1 0 [pos]) in if(c = []) then (aux t) else (d, c)::(aux t)
           in (aux (giveNeighbours pos))

(*Check if two balls are neighbour *)
let nextTo marble1 marble2 = 
    let rec aux dir = match dir with 
                        | 7 -> 7
                        | x -> if nextMarble marble1 x = marble2 then x else (aux (x+1))
    in  
    (aux 1)

	
(*Create the oneMoveList of the moves with the moves of one marble only *)
let oneMoveList (movesList : (int * (int * int) list) list) =
    global.rest <- [];
    let rec aux (l : (int * (int * int) list) list) =
        match l with
            | [] -> []
            | (dir,li)::tl when (List.length li) = 1 -> (dir,li)::(aux tl)
            | m::tl -> global.rest <- (m::(global.rest)); (aux tl)
    in (aux movesList)

let allPushMoves (board : marble array array) player = 
    let rec parseCol i j = if(j = 0) then [] 
                           else if(board.(i).(j-1) = player) then (pushMoves (i,j-1) board player)@(parseCol i (j-1)) 
                           else (parseCol i (j-1))
    in 
    let rec parseLine i = if(i = 0) then [] 
                          else let c = (parseCol (i-1) marge.(i-1)) in 
                                if(c = []) then (parseLine (i-1)) else c@(parseLine (i-1))
    in
    (parseLine (Array.length marge))


(*Deal with the result of allPushMoves to create all the slides moves, and return all of the moves (slide ones + push ones) *)
let legalMoves (board : marble array array) player =
    let rec contain elmt list =
        match list with
          | [] -> false
          | hd::tl when hd=elmt -> true
          | _::tl -> contain elmt tl
    in
    let rec aux (dir,[pos]) moves = 
        match moves with
          | [] -> []
          | (dir2,[pos2])::tl when dir = dir2 -> let relation = (nextTo pos pos2) in
                                                     if relation <> 7
                                                     then let pos3 = (nextMarble pos2 relation) in
                                                             if contain (dir,[pos3]) tl 
                                                             then [(dir,[pos;pos2;pos3]); (dir,[pos;pos2])]@(aux (dir,[pos]) tl)
                                                             else [(dir,[pos;pos2])]@(aux (dir,[pos]) tl)
                                                     else (aux (dir,[pos]) tl)
          | (_,[pos2])::tl -> aux (dir,[pos]) tl
          | _ -> Printf.printf "This case shouldn't happen, baka"; exit 1
    in
    (* Give all slide moves that involve more than one marble *)
    let rec generateSlideMoves listMove =
        match listMove with 
          | [] -> []
          | hd::tl -> (aux hd tl)@(generateSlideMoves tl)
    in 
    let pushs = (allPushMoves board player) in 
        let res = (oneMoveList pushs) in
            (global.rest)@(generateSlideMoves res)@res


(* ======================================================================================================================================
   Applying moves on a board
   ====================================================================================================================================== *)

(*Deal with the opponent marbles encountered in applyMove (see below) and return true if there is a marble out *)
let rec collapseMarble dir m (board : marble  array array) player = 
    let shift (i,j) (ni,nj) (board : marble  array array) =
        if((board.(i).(j)) = player) then
            false
        else 
			if((ni = -1) || (nj = -1)) then begin
				board.(i).(j) <- E;
				true
			end
			else begin 
				board.(i).(j) <- E;
				board.(ni).(nj) <- (opponent player);
				false
			end
			
    in
    let nm = (nextMarble m dir) in
        match (getMarble nm board) with
            | c when (c = player)          			-> (collapseMarble dir nm board player)
            | c when (c = (opponent player))   		-> (collapseMarble dir nm board player) || (shift m nm board)
            | E                                 	-> (shift m nm board)
            | _ (* O *)                          	-> (shift m (-1,-1) board)


							

(*Apply a move to the provided board and return true if there is a marble out
   A move = [(dir, [(x1,y1);(x2,y2);…])
            …] *)
let applyMove (board : marble array array) (dir, (duos : (int * int) list)) player = 
	let rec addbis (board : marble array array) (dir,duos) =
        match duos with 
          | [] -> ()
          | (x,y)::tl -> let (newx,newy) = (nextMarble (x,y) dir) in 
                           begin addbis board (dir,tl);
                            board.(newx).(newy) <- player end
	in
	let rec deletebis (board : marble array array) (dir,duos) =
        match duos with 
          | [] -> ()
          | (x,y)::tl ->  begin  deletebis board (dir,tl);
                            board.(x).(y) <- E end
	in
    let out = (collapseMarble dir (List.hd duos) board player)
    in
    begin
  	  (deletebis board (dir,duos));
      (addbis board (dir,duos));
      out
    end