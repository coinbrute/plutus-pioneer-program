-- this week focuses on state and state machines to mane state

-- think about a game between two players alice and bob
 -- they have two options 0 and 1
 -- depending on what they play one of them wins
 -- if the choices are the same alice wins
 -- if the choices are different bob wins

-- how would this work if they werent together?
 -- say alice sent 0 to bob as her move
 -- bob could open her move see it and respond with 1 and win

-- one option is to have alice commit to a choice and send a hash of that commit 
 -- so she could instead send the hash of 0 so ff27....
 -- so bob wouldn't know so he may send 0 
 -- then alice sends her actual choice of 0
 -- then bob checks it against the hash to ensure alice didn't cheat
 -- then alice would win
-- one big issue is after 2 rounds bob will know what the hashes are 

-- one way around this is instead of sending the hash is sending 0 with some arbitrary bytestring called a nonce
 -- so alice picks a nonce
 -- then she sends the hash of the nonce and her choice i.e 0
 -- now it wont always be the same hash
 -- then bob sends his guess
 -- then alice sends not only her choice to validate but also the nonce she chose at the beginning
 -- then bob can check the hash of alices nonce concatted with her choice
 -- if alice wins then alice claims her win
  -- if after a deadline is reached and alice doesn't reveal her win bob can claim the win
  -- this is true if bob wins as well
 -- if after a deadline is reached bob never interacts alice can claim back her stake 

-- this is what we will implement in EvenOdd.hs

-- then we will see with state machines how code can be clearer and more consise



-- next up is state machines which is essentially what we just went through building but just a different name. 
 -- it is a program or machine that tracks things through "state" and has some special statuses where they become final and cannot be changed once they reach this state
 -- so the events in a state machine can be though of as transitions and the things in the state machine are the states
  -- so in bob and alices example 
   -- the hash or the nonce hash is the state
   -- when bob or alice makes a move or reveals that is a transition
 -- the state machine is represented by a utxo sitting at the state machine script address
-- the state of the state machine is the datum of the utxo
-- the tranistion is a tx that consumes the current state using a redeemer that characterizes the transition and produces a new utxo at the same address where the datum now reflects the new state
-- support for state machines is in Plutus.Contract.StateMachine

