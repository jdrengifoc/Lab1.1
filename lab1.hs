import FiniteAutomata

img2dfa :: ImagePixels -> FiniteAutomata ImagePixels Int
img2dfa img = aux 0 0 img fa
  where 
    fa :: FiniteAutomata ImagePixels Int
    fa = FA {
    states = Set.singleton img
    , symbols = imageAlphabet			-- alphabet
    , delta = delta'
    , q0 = img
    , f = Set.fromList [blackPx]
    } 
    delta' :: TransitionFunction ImagePixels Int
    delta' = Map.empty								-- En lugar de simbolos podemos usar enteros? SI
    					-- VERIFICAR! SI
		

aux :: Int -> Int -> ImagePixels -> FiniteAutomata ImagePixels Int -> FiniteAutomata ImagePixels
aux i j img fa@(FA st sy tf as es) = if (i == j && i + j /= 0) then fa else u	-- en que comienzan i y j? AGREGAR I + J DIFERENTE DE 0
  where dfa :: FiniteAutomata ImagePixels Int			-- Hay que usar i y j?
        dfa = FA {							-- fa@(FA st sy tf as es)  Aclarar esto. Lo inical: st sy tf as es
        states = Set.union st (Set.fromList imgP) 			-- :D more better
        , symbol = sy
        , delta = delta'
        , q0 = as
        , f = es
        } 
        delta' = 
          addTransition (img, 0, imgP!!0)	$			-- concatenar????????????????
          addTransition (img, 1, imgP!!1) $
          addTransition (img, 2, imgP!!2) $
          addTransition (img, 3, imgP!!3)  			        -- PUEDE HABER UN $
          tf							-- Verificar

        imgP = [split img k | k <- [0, 1, 2, 3]]
        i' = i + 1						-- Poner +4 o +1
        j' = j + 4						-- Poner un i
        fas = [aux i' j' image fa | image <- imgP]
        u = joinAu fas

--[Split image]
split :: ImagePixels -> Int -> ImagePixels
  | img 0 = List.take a (transpose (snd(Prelude.splitAt a img)))
  | img 1 = List.take a (transpose (fst(Prelude.splitAt a img)))
  | img 2 = List.drop a (transpose (fst(Prelude.splitAt a img))) 
  | img 3 = List.drop a (transpose (snd(Prelude.splitAt a img))) 
  where a = (length img)/2
	
joinAu :: [FiniteAutomata ImagePixels Int] -> Finite Automata ImagePixels Int

 
split :: ImagePixels -> Int -> ImagePixels
split img k = take (n) imagePixels
  where n = length.LISTA/2 
