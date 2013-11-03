open LegalMoves
open AlphaBeta

(* Change the result of findBestMove (a move) in the expected format, then write it in result.txt or write it on the console output *)
let sendMove (dir,list) = 
    let rec aux positionList = 
        match positionList with
            | [] -> ""
            | (x,y)::tl -> Printf.sprintf " %d%d%s" x y (aux tl)
    in
    Printf.printf "%d%s" dir (aux list) (*the GUI needs a console output *)


let splitLine (str : string) =
    let answer = Array.make (String.length str) E in 
		begin
			for i=0 to ((String.length str)-1) do
				match str.[i] with
				 | '1' -> answer.(i) <- B
				 | '2' -> answer.(i) <- W
				 | '0' -> ()
				 | _ -> Printf.printf "Error in parsing board.txt Found unknown character (expecting 0, 1 or 2)"; exit 1
			done;
			answer
		end
			

let formatArgToDepth str = 
    match str with
      | "1" -> 1
      | "2" -> 2
      | "3" -> 3
      | "4" -> 4
      | "5" -> 5
      | "6" -> 6
      | "7" -> 7
      | "8" -> 8
      |  _  -> Printf.printf "The provided depth is missing or is not between 1 and 8, try again"; exit 1

let formatArgToPlayerColor c = 
    match c with
      | "1" -> global.playerColor <- B; global.opponentColor <- W;
      | "2" -> global.playerColor <- W; global.opponentColor <- B;
      | _   -> Printf.printf "Wrong playerColor: %s (expecting 1 or 2)" c; exit 1

let formatArgToBoard (arg : string array) a b = 
    let answer = Array.create (b-a+1) [||] in
		begin
			for i=a to b do
				answer.(i-a) <- (splitLine (arg.(i)))
			done;
			answer
		end
        
let _ = 
    (*let sw = System.Diagnostics.Stopwatch() in
    System.Console.ReadKey() |> ignore
    sw.Start()
    sendMove (findNextMove  test1 4)
    sendMove (findNextMove (formatInputToBoard "board.txt") (depthToString argv.[0]))
    dumpL (legalMoves test1 global.playerColor);
    sw.Stop()
    printfn "Execution time: %f" sw.Elapsed.TotalMilliseconds
    System.Console.ReadKey() |> ignore
    exit 0;*)
    if( (Array.length Sys.argv) = 1) then 
		begin
			Printf.printf "How to call :\n    %s depth(1-8) playerColor(1-2) board\nExample:\n%s 3 1 11111 111111 0011100 00000000 000000000 00000000 0022200 222222 22222\n" Sys.argv.(0) Sys.argv.(0); 
			exit 1;
		end;
    (formatArgToPlayerColor Sys.argv.(2));
    sendMove (findNextMove (formatArgToBoard Sys.argv 3 11) (formatArgToDepth Sys.argv.(1)));
    0 (* return an integer exit code *)