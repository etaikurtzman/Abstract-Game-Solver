functor AgsFun (structure Game : GAME) :>
   AGS
     where type Game.Move.move = Game.Move.move
     and   type Game.state     = Game.state
= struct
  structure Game = Game
  type advice = { recommendation : Game.Move.move option
                , expectedOutcome : Player.outcome
                }
 
  fun advice state =    
    let val p = Game.whoseturn state
        val opp = Player.otherplayer p

        (*the following 6 functions are outlined in the spec *)
        fun extractOutcome { recommendation = r, expectedOutcome = oc } = oc
        fun postAdvice m = advice (Game.makemove state m)
        fun currPlayerWins oc = oc = Player.WINS p
        fun firstBetter (oc1, oc2) =
            if currPlayerWins oc1 then true else
            if currPlayerWins oc2 then false else
            if oc1 = Player.TIE then true else
            if oc2 = Player.TIE then false else
            if oc1 = Player.WINS opp then false else true
        fun compareAdvice (a1, a2) =
            if firstBetter (extractOutcome a1, extractOutcome a2)
            then a1 else a2
 
        fun bestAdvice [m] =
          { recommendation = SOME m,  
            expectedOutcome = extractOutcome (postAdvice m)}
          | bestAdvice (m :: ms) =
            let val postoutcome = extractOutcome (postAdvice m)
              in
              if currPlayerWins postoutcome
              then { recommendation = SOME m,
                     expectedOutcome = postoutcome}
              else let val bestrest = bestAdvice ms 
              in compareAdvice ({ recommendation = SOME m,
                                     expectedOutcome = postoutcome}
                                  , bestrest)
                   end
            end
          | bestAdvice [] = let exception BadInput
                          in raise BadInput
                          end
    in if Game.isOver state then
                                {recommendation = NONE,
                                 expectedOutcome = valOf (Game.outcome state)}
                              else bestAdvice (Game.legalmoves state)
    end
end

