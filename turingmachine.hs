-- Author: Sean Sinnott

---------------------------
-- Part 1 --
---------------------------

type Tape = (String, Int)

type Configuration = (Int, Tape)

data Action = L | R | Y | N
              deriving Eq

type Transitions = (Int, Char) -> (Int, Char, Action)
                                 
data Result = Accept Tape | Reject Tape
              deriving Show


--getTapeValue t returns the current character value under the tape t's read write head			  
getTapeValue :: Tape -> Char
getTapeValue ([s],i) = error "no string entered"
getTapeValue (s,i) = s !! i

-- getNewState returns the integer value corresponding to the transition's next state (e.g. 2)
getNextState :: (Int, Char, Action) -> Int
getNextState (i,c,a) = i

-- getNextValue returns the character value corresponding to the transition's next character (e.g. 'a')
getNextValue :: (Int, Char, Action) -> Char
getNextValue (i,c,a) = c

-- getNextAction returns the Action value corresponding to the transition's next action (e.g. R)
getNextAction :: (Int, Char, Action) -> Action
getNextAction (i,c,a) = a

-- changeTapeValue i c s returns string s with s[i] now changed to c
changeTapeValue :: Int -> Char -> String -> String
changeTapeValue index newValue (x:xs)
    | index == 0 = newValue:xs
    | otherwise = x:changeTapeValue (index-1) newValue xs

-- run executes the Turing Machine	
run :: Configuration -> Transitions -> Result
run (confInt, (tapeString, tapeIndex)) trans
    | getNextAction (trans (confInt, (getTapeValue (tapeString, tapeIndex)))) == Y = Accept (tapeString, tapeIndex)
    | getNextAction (trans (confInt, (getTapeValue (tapeString, tapeIndex)))) == N = Reject (tapeString, tapeIndex)
    | getNextAction (trans (confInt, (getTapeValue (tapeString, tapeIndex)))) == R = run (newState, (newTapeString, tapeIndex+1)) trans
    | getNextAction (trans (confInt, (getTapeValue (tapeString, tapeIndex)))) == L = run (newState, (newTapeString, tapeIndex-1)) trans
      where newState = getNextState (trans (confInt, (getTapeValue (tapeString, tapeIndex))))
            newTapeString = changeTapeValue tapeIndex (getNextValue (trans (confInt, (getTapeValue (tapeString, tapeIndex))))) tapeString


---------------------------
-- Part 2 --
---------------------------

anbncn (s,c) = case (s,c) of
                  (0,' ') -> (0,' ',Y)
                  (0,'a') -> (1,'X',R)
                  (0,'b') -> (0,'b',N)
                  (0,'c') -> (0,'c',N)
                  (0,'X') -> (0,'X',N)
                  (0,'Y') -> (0,'Y',R)
                  (0,'Z') -> (0,'Z',R)
                  (1,' ') -> (1,' ',N)
                  (1,'a') -> (1,'a',R)
                  (1,'b') -> (2,'Y',R)
                  (1,'c') -> (1,'c',N)
                  (1,'X') -> (1,'X',N)
                  (1,'Y') -> (1,'Y',R)
                  (1,'Z') -> (1,'Z',N)
                  (2,' ') -> (2,' ',N)
                  (2,'a') -> (2,'a',N)
                  (2,'b') -> (2,'b',R)
                  (2,'c') -> (3,'Z',L)
                  (2,'X') -> (2,'X',N)
                  (2,'Y') -> (2,'Y',N)
                  (2,'Z') -> (2,'Z',R)
                  (3,' ') -> (3,' ',N)
                  (3,'a') -> (3,'a',L)
                  (3,'b') -> (3,'b',L)
                  (3,'c') -> (3,'c',N)
                  (3,'X') -> (0,'X',R)
                  (3,'Y') -> (3,'Y',L)
                  (3,'Z') -> (3,'Z',L)
                  _       -> error "No valid transition"

---------------------------
-- Part 3 --
---------------------------

palindrome (s,c) = case (s,c) of
                     (0,'a') -> (1,' ',R)
                     (0,'b') -> (2,' ',R)
                     (0,' ') -> (0,' ',Y)
                     (1,'a') -> (1,'a',R)
                     (1,'b') -> (1,'b',R)
                     (1,' ') -> (3,' ',L)
                     (2,'a') -> (2,'a',R)
                     (2,'b') -> (2,'b',R)
                     (2,' ') -> (4,' ',L)
                     (3,'a') -> (5,' ',L)
                     (3,'b') -> (3,'b',N)
                     (3,' ') -> (3,' ',Y)
                     (4,'a') -> (4,'a',N)
                     (4,'b') -> (5,' ',L)
                     (4,' ') -> (4,' ',Y)
                     (5,'a') -> (5,'a',L)
                     (5,'b') -> (5,'b',L)
                     (5,' ') -> (0,' ',R)
                     _       -> error "No valid transition"

---------------------------
-- Part 4 --
---------------------------

reverse (s,c) = case (s,c) of
                  (0,' ') -> (0,' ',Y)
                  (0,'a') -> (1,'x',R)
                  (0,'b') -> (2,'y',R)
                  (0,'x') -> (7,'x',R)
                  (0,'y') -> (7,'y',R)
                  (1,'a') -> (1,'a',R)
                  (1,'b') -> (1,'b',R)
                  (1,'x') -> (3,'x',L)
                  (1,'y') -> (3,'y',L)
                  (1,' ') -> (3,' ',L)
                  (2,'a') -> (2,'a',R)
                  (2,'b') -> (2,'b',R)
                  (2,'x') -> (4,'x',L)
                  (2,'y') -> (4,'y',L)
                  (2,' ') -> (4,' ',L)
                  (3,'a') -> (5,'x',L)
                  (3,'b') -> (6,'x',L)
                  (3,'x') -> (7,'x',R)
                  (3,'y') -> (7,'x',R)
                  (4,'a') -> (5,'y',L)
                  (4,'b') -> (6,'y',L)
                  (4,'x') -> (7,'y',R)
                  (4,'y') -> (7,'y',R)
                  (5,'a') -> (5,'a',L)
                  (5,'b') -> (5,'b',L)
                  (5,'x') -> (0,'x',R)
                  (5,'y') -> (0,'x',R)
                  (6,'a') -> (6,'a',L)
                  (6,'b') -> (6,'b',L)
                  (6,'x') -> (0,'y',R)
                  (6,'y') -> (0,'y',R)
                  (7,'x') -> (7,'x',R)
                  (7,'y') -> (7,'y',R)
                  (7,' ') -> (8,' ',L)
                  (8,'x') -> (8,'a',L)
                  (8,'y') -> (8,'b',L)
                  (8,'<') -> (8,'<',Y)
                  _       -> error "No valid transition"