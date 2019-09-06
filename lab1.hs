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
    , alphabet = imageAlphabet
    , delta = delta'
    , initialState = img
    , acceptState = Set.fromList [[[blackPx]]]
    } 
    delta' :: TransitionFunction ImagePixels Int
    delta' = Map.empty

aux :: Int -> Int -> ImagePixels -> FiniteAutomata ImagePixels Int -> FiniteAutomata ImagePixels Int
aux i j img fa@(FA st sy tf as es) = if (i == j && i + j /= 0) then fa else u
  where dfa :: FiniteAutomata ImagePixels Int
        dfa = FA {
          states = Set.union st (Set.fromList imgP)
          , alphabet = sy
          , delta = addTransition (img, 0, imgP!!0) $
                    addTransition (img, 1, imgP!!1) $
                    addTransition (img, 2, imgP!!2) $
                    addTransition (img, 3, imgP!!3) $
                    tf	
          , initialState = as
          , acceptState = es
          }     				
        imgP = [splitList img k | k <- [0, 1, 2, 3]]
        i' = i + 1						-- Poner +4 o +1
        j' = j + 4						-- Poner un i
        fas = [aux i' j' image dfa | image <- imgP]
        u = joinAu fas

splitList ::  [[a]] -> Int -> [[a]]
splitList l a
  | length l == 1 = l
  | a==1 = [Prelude.take (length l `div` 2) x | x <- lT]
  | a==3 = [Prelude.drop (length l `div` 2) x | x <- lT]
  | a==0 = [Prelude.take (length l `div` 2) x | x <- lD]
  | a==2 = [Prelude.drop (length l `div` 2) x | x <- lD]
      where
       lT= Prelude.take (length l `div` 2) l
       lD= Prelude.drop (length l `div` 2) l  
	
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
	
-- createImage :: Int -> FiniteAutomata state symbol -> DynamicImage
	
--ImageRGB8 (generateImage func int int) 
--createImage n dfa = Image PixelRGB8(generateImage (returnPixel x y dfa) 

coordenates :: Int -> Int -> ImagePixels -> [Int]
coordenates x y img
  | length img == 1 = s
  | otherwise =
    if(x < l) then
      if(y < l) then
        s ++ [1] ++ (coordenates x y (splitList img 1))
      else
        s ++ [0] ++ (coordenates x (y-l) (splitList img 0))
    else 
      if(y < l) then
        s ++ [3] ++ (coordenates (x-l) y (splitList img 3))
      else
        s ++ [2] ++ (coordenates (x-l) (y-l) (splitList img 2))
  where s :: [a]
        s = []
	l :: Int
        l = (length  img `div` 2)
	
returnPixel:: Int -> Int -> FiniteAutomata ImagePixels Int -> PixelRGB8
returnPixel x y dfa
 | Set.member [[blackPx]] (returnPixel2 (coordenates x y (initialState dfa)) (delta dfa) ((delta dfa)!initialState dfa) (initialState dfa) dfa) = blackPx
 | otherwise = whitePx


returnPixel2 ::(Eq k,Eq a, Eq b) => [Int] -> Map k a ->Map Int b-> k -> ImagePixels -> FiniteAutomata ImagePixels Int-> b
returnPixel2 ad m n img dfa
 | length ad==1 = n!(head ad)
 | otherwise = returnPixel2 (tail ad) m n (n!(head ad)) dfa 
 where n= m!img

--generateCoords :: Int -> [(Int,Int)]
--generateCoords n
--    | n>0 = cartProd [0..(n-1)] [0..(n-1)]
--    | otherwise = []

--cartProd :: [Int] -> [Int] -> [(Int,Int)]
--cartProd xs ys = [(x,y) | x <- xs, y <- ys]
