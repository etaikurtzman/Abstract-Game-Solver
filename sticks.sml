functor SticksFun(val N : int) :> GAME = struct
type state = int * Player.player 

fun invariant (i, p) = not (i < 0)
(* Abstraction Function *)
(* A(0, p) = player p has lost*)
(* A(n, p) where n > 0 = it is player p's turn and there are n sticks left *)


structure Move = struct
    datatype move = ONE | TWO | THREE
    fun invariant m = true
    (* A(ONE) = picking up 1 stick *)
    (* A(TWO) = picking up 2 sticks *)
    (* A(THREE) = picking up 3 sticks *) 
    exception Move
    
    fun visualize p = (fn (m) => "Player " ^ Player.unparse p ^ " takes " ^
                        (case m
                         of ONE => "1 stick"
                          | TWO => "2 sticks"
                          | THREE => "3 sticks"))

    fun prompt p = "How many sticks does player " ^ Player.unparse p 
                                                  ^ " pick up? "

    fun parse "1" = ONE
      | parse "2" = TWO
      | parse "3" = THREE
      | parse _ = raise Move
    end

fun initial P = (N, P)

fun whoseturn (i, p) = p 

fun makemove (ii, ip) = (fn (m) => let val (fi, fp)
                      = (case m
                           of Move.ONE => (ii - 1, Player.otherplayer ip)
                            | Move.TWO => (ii - 2, Player.otherplayer ip)
                            | Move.THREE => (ii - 3, Player.otherplayer ip))
                                   in if invariant (fi, fp) then (fi, fp)
                                      else raise Move.Move
                                   end) 
                            



fun outcome (i, p) = if i = 0 
                     then SOME (Player.WINS (Player.otherplayer p)) 
                     else NONE  

fun isOver (i, p) = i = 0

fun legalmoves (i, p) = if i = 0 then [] else
                        if i = 1 then [Move.ONE] else
                        if i = 2 then [Move.ONE, Move.TWO] 
                        else [Move.ONE, Move.TWO, Move.THREE]


(* stick visual takes an int and returns a string containing a visual 
representation of the sticks game with int sticks *)
fun stickvisual 0 = ""
  | stickvisual i = " |" ^ stickvisual (i - 1)
fun visualize (i, p) = 
    if i > 0
    then "Player " ^ Player.unparse p ^ " sees" ^ (stickvisual i) ^ "\n"
    else "Player " ^ Player.unparse p ^ " faces an empty table.\n"



end