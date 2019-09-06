import FiniteAutomata
import Codec.Picture
import Data.List as List
import Data.Set as Set
import Data.Map as Map

img2dfa :: ImagePixels -> FiniteAutomata ImagePixels Int
img2dfa img = aux 0 0 img fa
  where 
    fa :: FiniteAutomata ImagePixels Int
    fa = FA {
    states = Set.singleton img
    , symbols = imageAlphabet			-- alphabet
    , delta = delta'
    , q0 = img
    , f = Set.fromList [[[blackPx]]]
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
          , delta = addTransition (img, 0, imgP!!0) $			-- concatenar????????????????
                    addTransition (img, 1, imgP!!1) $
                    addTransition (img, 2, imgP!!2) $
                    addTransition (img, 3, imgP!!3) $ 			        -- PUEDE HABER UN $
                    tf	
          , q0 = as
          , f = es
          }     				
        imgP = [split img k | k <- [0, 1, 2, 3]]
        i' = i + 1						-- Poner +4 o +1
        j' = j + 4						-- Poner un i
        fas = [aux i' j' image dfa | image <- imgP]
        u = joinAu fas

--[Split image]
split :: ImagePixels -> Int -> ImagePixels
  | img 0 = List.take a (transpose (snd(Prelude.splitAt a img)))
  | img 1 = List.take a (transpose (fst(Prelude.splitAt a img)))
  | img 2 = List.drop a (transpose (fst(Prelude.splitAt a img))) 
  | img 3 = List.drop a (transpose (snd(Prelude.splitAt a img))) 
  where a = (length img)/2
	
joinAu :: [FiniteAutomata ImagePixels Int] -> FiniteAutomata ImagePixels Int            
joinAu fas = FA{
      states = Set.unions (List.map states fas)         
        , alphabet  = imageAlphabet
        , delta = delta' (delta (head fas)) (tail (List.map delta (tail fas)))
        , initialState = initialState (head fas)
        , acceptState  = Set.unions (List.map acceptState fas)
      }
      where 
        delta' :: TransitionFunction ImagePixels Int -> [TransitionFunction ImagePixels Int] -> TransitionFunction ImagePixels Int
        delta' tf [] = tf
        delta' d tfs = Map.union d (delta' (head tfs) (tail tfs))
	
createImage :: Int -> FiniteAutomata state symbol -> DynamicImage
	
--ImageRGB8 (generateImage func int int) 
--createImage n dfa = Image PixelRGB8(generateImage (returnPixel x y dfa) 

coordenates :: Int -> Int -> ImagePixels -> [Int]
coordenates x y img
  | length img == 1 = s
  | otherwise =
    if(x < l) then
      if(y < l) then
        s ++ [1] ++ (coords x y (splitList img 1))
      else
        s ++ [0] ++ (coords x (y-l) (splitList img 0))
    else 
      if(y < l) then
        s ++ [3] ++ (coords (x-l) y (splitList img 3))
      else
        s ++ [2] ++ (coords (x-l) (y-l) (splitList img 2))
  where s :: [Ord]
        s = []
	l = :: Int
        l = (length  img `div` 2)
	
returnPixel:: Int -> Int -> FiniteAutomata ImagePixels Int -> PixelRGB8
returnPixel x y dfa
 | Set.member [[blackPx]] (returnPixel2 (coords x y (initialState dfa)) (delta dfa) ((delta dfa)!initialState dfa) (initialState dfa) dfa) = blackPx
 | otherwise = whitePx

 --generateCoords :: Int -> [(Int,Int)]
--generateCoords n
--    | n>0 = cartProd [0..(n-1)] [0..(n-1)]
--    | otherwise = []

--cartProd :: [Int] -> [Int] -> [(Int,Int)]
--cartProd xs ys = [(x,y) | x <- xs, y <- ys]
 
