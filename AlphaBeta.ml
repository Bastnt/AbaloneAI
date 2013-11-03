open LegalMoves;;

let finalNum = 8

let positionValues =    [|          [| -3 ; -2 ; -2 ; -2 ;-3 |];
                                 [| -2 ;  0 ;  0 ;  0 ;  0 ; -2 |];
                              [| -2 ;  0 ;  2 ;  2 ;  2 ;  0 ; -2 |];
                           [| -2 ;  0 ;  2 ;  3 ;  3 ;  2 ;  0 ; -2 |];
                        [| -3 ;  0 ;  2 ;  3 ;  3 ;  3 ;  2 ;  0 ; -3 |];
                           [| -2 ;  0 ;  2 ;  3 ;  3 ;  2 ;  0 ; -2 |];
                              [| -2 ;  0 ;  2 ;  2 ;  2 ;  0 ; -2 |];
                                 [| -2 ;  0 ;  0 ;  0 ;  0 ; -2 |];
                                    [| -3 ; -2 ; -2 ; -2 ; -3 |] |]

let expelBonus = 2000
let loseMalus = -1800

(* ======================================================================================================================================
   Tool functions out: bool, board: marble array array)
   ====================================================================================================================================== *)

let copyBoard (b: marble array array) = 
    let res = (Array.create (Array.length marge) [||]) in
		for i=0 to ((Array.length marge)-1) do
			res.(i) <- (Array.copy b.(i))
		done;
		res

let countMarbles (b: marble array array) = 
    global.numP <- 0;
    global.numO <- 0;
    for i=0 to ((Array.length marge)-1) do
        for j=0 to (marge.(i)-1) do
            if(b.(i).(j) = global.playerColor) then
                global.numP <- (global.numP + 1)
            else
                global.numO <- (global.numO + 1)
		done
	done

(*Return the index of the first maximum element found in the provided list *)

let findFirstMaximumIndex listo = 
    let rec aux l actualIndex maxIndex maxValue =
        match l with
          | []  -> maxIndex
          | hd::tl -> if hd > maxValue then (aux tl (actualIndex+1) actualIndex hd) else (aux tl (actualIndex+1) maxIndex maxValue)
    in
    aux listo 0 0 (List.hd listo)


(* ======================================================================================================================================
   AlphaBeta Algorithm functions
   ====================================================================================================================================== *)

(*Return the static evaluation of the provided board during the provided phase *)
let staticEvaluation (board : marble array array) outsO outsP =
	global.staticValue <- 0;
	for i=0 to ((Array.length marge)-1) do
		for j=0 to (marge.(i)-1) do
			let marble = board.(i).(j) in 
				if marble = global.playerColor then 
					global.staticValue <- global.staticValue + (positionValues.(i).(j))
				else if marble = global.opponentColor then 
					global.staticValue <- global.staticValue - (positionValues.(i).(j))
				else ()
		done;
	done;
    (global.staticValue + (outsP*loseMalus + outsO*expelBonus) + (100 * (global.numP- global.numO)))

(*Return a list of the possible board issues (Marble array array style) after one move of the provided player. The result is a tuple (list of moves , list of boards) *)
let boardChildren board player = 
    let rec aux (board : marble array array) movesList =
        match movesList with 
            | [] -> []
            | move::tl -> let nboard = (copyBoard board) in ((applyMove nboard move player),nboard)::(aux board tl)
    in
        (aux board (legalMoves board player))

let rec alphaBetaAlgo (board : marble array array) depth =
    let rec dealWithParent parent oO oP depth alpha beta player =
        let rec dealWithChildren children a b =
                match player with
                    (* MAX *)
                    | c when c=global.playerColor ->  (match children with
														| [] -> a
														| (out,cboard)::tl -> 
															if b <= a then
																a 
															else 
																(dealWithChildren tl (max (dealWithParent cboard (if out then oO+1 else oO) oP (depth-1) a b (opponent player)) a) b)
													   )
                    (* MIN *)
                    | _ -> (match children with
							| [] -> b
							| (out,cboard)::tl -> 
								if b <= a then 
									b 
								else
									(dealWithChildren tl a (min (dealWithParent cboard oO (if out then oP+1 else oP) (depth-1) a b (opponent player)) b))
							)
        in
        if(depth <= 0)                  then (staticEvaluation parent oO oP)
        else if(global.numP - oP <= finalNum)  then loseMalus (* We lose *)
        else if(global.numO - oO <= finalNum)  then expelBonus (* We win *)
        else                                 (dealWithChildren (boardChildren parent player) alpha beta)
    in

    let rec dealWithFirstChildren children depth = 
        match children with
        | [] -> []
        | (out,cboard)::tl -> (dealWithParent cboard (if out then 1 else 0) 0 (depth-1) (-9999) 9999 global.opponentColor)::(dealWithFirstChildren tl depth)
	
    in
        dealWithFirstChildren (boardChildren board global.playerColor) depth

let findNextMove (board : marble array array) depth = 
    (countMarbles board); 
    let res = (legalMoves board global.playerColor) in
    List.nth res ((findFirstMaximumIndex (alphaBetaAlgo board depth)))
	
