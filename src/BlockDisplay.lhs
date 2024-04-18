> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-} 
> module BlockDisplay where
> import Data.Ratio
> import Data.List
> data OutputLine = Ascender Int | Base | Descender Int deriving (Eq, Ord, Show)
> data HorizontalAlignment = LeftAligned | CentredHorizontally deriving (Show)
> data VerticalAlignment = Top | CentredVertically | Bottom deriving (Show)
> data HBlockSeparator = NoSeparator | NSpaces Int | Delimeter String deriving (Show)
> data HorizontalBlock = HorizontalBlock HBlockSeparator [BlockElement] deriving (Show)
> data VerticalBlock = VerticalBlock {
>                         internal_blocks_alignment :: HorizontalAlignment,
>                         blocks :: [BlockElement],
>                         block_alignment :: VerticalAlignment } deriving (Show)
> data BlockElement = OrdinaryText String | Superscript String | Fraction String String
>    | VBlock VerticalBlock
>    | HBlock HorizontalBlock
>    | Bordered BlockElement  deriving (Show)
> -- I should be able to recreate Fraction as VerticalBlock elements
>
> hthen :: BlockElement -> BlockElement -> BlockElement
> hthen b1 b2 = HBlock (HorizontalBlock NoSeparator [b1,b2])
> 
> hblockSeparatorString :: HBlockSeparator -> String
> hblockSeparatorString NoSeparator = ""
> hblockSeparatorString (NSpaces n) = nSpaces n
> hblockSeparatorString (Delimeter d) = d
>
> verticalBlockHeight :: VerticalBlock -> Int
> verticalBlockHeight (VerticalBlock {blocks=bs}) =
>   (length bs) + (sum [numberOfAscenderLines b | b <- bs]) + (sum [numberOfDescenderLines b | b <- bs])
>
> verticalBlockWidth :: VerticalBlock -> Int
> verticalBlockWidth (VerticalBlock {blocks=bs}) = maximum [blockWidth b | b <- bs]
>
> -- top line is line 0
> lineCountFromTop :: BlockElement -> OutputLine -> Int
> lineCountFromTop be (Ascender n) = (ascenders - n)
>  where ascenders = numberOfAscenderLines be
> lineCountFromTop be (Base) = ascenders 
>  where ascenders = numberOfAscenderLines be
> lineCountFromTop be (Descender n) = ascenders + n
>  where ascenders = numberOfAscenderLines be
>
> verticalBlockLineRequirements :: [BlockElement] -> [(BlockElement, OutputLine)]
> verticalBlockLineRequirements [] = []
> verticalBlockLineRequirements (be:bes) = [(be,a) | a <- allOutputLines [be]] ++ verticalBlockLineRequirements bes
>
> halign :: HorizontalAlignment -> Int -> String -> String
> halign LeftAligned n s = s ++ (nSpaces (n - (length s)))
> halign CentredHorizontally n s = centreInSpaceOfLength n s
>
> verticalBlockLineRender :: VerticalBlock -> OutputLine -> String
> verticalBlockLineRender (v@(VerticalBlock {blocks=bes, internal_blocks_alignment=iba})) lineloc
>  | isBlank lineloc (VBlock v) = nSpaces (blockWidth (VBlock v))
>  | otherwise = halign iba (blockWidth (VBlock v)) element_string
>   where zs = verticalBlockLineRequirements bes
>         outs = allOutputLines [VBlock v]
>         zipped = zip zs outs
>         relevant = [(be, their_line) | ((be,their_line),outer_line) <- zipped, outer_line == lineloc]
>         (target_element, line_to_trigger) = head relevant
>         element_string = renderBlockElementAt line_to_trigger target_element
>
> blockWidth :: BlockElement -> Int
> blockWidth (OrdinaryText s) = length s
> blockWidth (Superscript s) = length s
> blockWidth (Fraction s t) = maximum [3, length s, length t]
> blockWidth (Bordered x) = 6 + (blockWidth x)
> blockWidth (VBlock vb) = verticalBlockWidth vb
> blockWidth (HBlock (HorizontalBlock NoSeparator bes)) = sum [blockWidth b | b <- bes]
> blockWidth (HBlock (HorizontalBlock (NSpaces n) bes)) = (sum [blockWidth b | b <- bes]) + (n * ((length bes) - 1))
> blockWidth (HBlock (HorizontalBlock (Delimeter d) bes)) = (sum [blockWidth b | b <- bes]) + ((length d) * ((length bes) - 1))
> 
>
> numberOfAscenderLines :: BlockElement -> Int
> numberOfAscenderLines (OrdinaryText _) = 0
> numberOfAscenderLines (Superscript _) = 1
> numberOfAscenderLines (Fraction _ _) = 1
> numberOfAscenderLines (Bordered x) = 1 + numberOfAscenderLines x 
> numberOfAscenderLines (VBlock vb) = case (block_alignment vb) of
>    Top -> 0
>    CentredVertically -> ((verticalBlockHeight vb)-1) `div` 2
>    Bottom -> (verticalBlockHeight vb) - 1
> numberOfAscenderLines (HBlock (HorizontalBlock _ bes)) = maximum [numberOfAscenderLines b | b <- bes]
>
> numberOfDescenderLines :: BlockElement -> Int
> numberOfDescenderLines (OrdinaryText _) = 0
> numberOfDescenderLines (Superscript _) = 0
> numberOfDescenderLines (Fraction _ _) = 1
> numberOfDescenderLines (Bordered x) = 1 + numberOfDescenderLines x 
> numberOfDescenderLines (VBlock vb) = case (block_alignment vb) of
>    Top -> (verticalBlockHeight vb) - 1
>    CentredVertically -> (((verticalBlockHeight vb)-1) `div` 2) + (((verticalBlockHeight vb)-1) `mod` 2)
>    Bottom -> 0
> numberOfDescenderLines (HBlock (HorizontalBlock _ bes)) = maximum [numberOfDescenderLines b | b <- bes] 
>   
> hasAscenderN :: Int -> BlockElement -> Bool
> hasAscenderN n b = (numberOfAscenderLines b) >= n
> hasDescenderN :: Int -> BlockElement -> Bool
> hasDescenderN n b = (numberOfDescenderLines b) >= n
> isBlank :: OutputLine -> BlockElement -> Bool
> isBlank Base _ = False
> isBlank (Ascender n) b = not (hasAscenderN n b)
> isBlank (Descender n) b = not (hasDescenderN n b)
>
> nSpaces :: Int -> String
> nSpaces n = take n (repeat ' ')
>
> renderBlockElementAt :: OutputLine -> BlockElement -> String
> renderBlockElementAt Base (OrdinaryText s) = s
> 
> renderBlockElementAt (Ascender 1) (Superscript s) = s
> renderBlockElementAt Base (Superscript s) = nSpaces (length s)
> 
> renderBlockElementAt (Ascender 1) (f@(Fraction s t)) = centreInSpaceOfLength (blockWidth f) s
> renderBlockElementAt Base (f@(Fraction s t)) = take (blockWidth f) (repeat '-')
> renderBlockElementAt (Descender 1) (f@(Fraction s t)) = centreInSpaceOfLength (blockWidth f) t
>
> renderBlockElementAt lineloc (VBlock vb) = verticalBlockLineRender vb lineloc
> 
> renderBlockElementAt (Ascender n) (Bordered be)
>   | numberOfAscenderLines be == n - 1  = " +-" ++ (take (blockWidth be) (repeat '-')) ++ "-+ "
>   | numberOfAscenderLines be < n - 1 = nSpaces (blockWidth (Bordered be))
>   | otherwise = " | " ++ (renderBlockElementAt (Ascender n) be) ++ " | "
> renderBlockElementAt Base (Bordered be) = " | " ++ (renderBlockElementAt Base be) ++ " | "
> renderBlockElementAt (Descender n) (Bordered be)
>   | numberOfDescenderLines be == n - 1 = " +-" ++ (take (blockWidth be) (repeat '-')) ++ "-+ "
>   | numberOfDescenderLines be < n - 1 = nSpaces (blockWidth (Bordered be)) 
>   | otherwise = " | " ++ (renderBlockElementAt (Descender n) be) ++ " | "
>
> renderBlockElementAt lineloc (HBlock (HorizontalBlock sep bes)) =
>   (concat (intersperse (hblockSeparatorString sep) [renderBlockElementAt lineloc b | b <- bes]))
>
> -- final fallback
> renderBlockElementAt _ thing = nSpaces (blockWidth thing)
>
> allOutputLines :: [BlockElement] -> [OutputLine]
> allOutputLines bes = [Ascender x | x <- reverse [1 .. (maxAscender)]] ++ 
>                         [Base] ++
>                         [Descender x | x <- [1 .. (maxDescender)]]
>   where maxAscender = maximum [numberOfAscenderLines b | b <- bes ]
>         maxDescender =  maximum [numberOfDescenderLines b | b <- bes ]
>
> -- We don't get the right length if the string is longer than the padding
> centreInSpaceOfLength :: Int -> String -> String
> centreInSpaceOfLength n s = left_spaces ++ s ++ right_space
>   where spaces_to_add = n - length s
>         left_spaces = nSpaces (spaces_to_add `div` 2)
>         extra_space = nSpaces (spaces_to_add `mod` 2)
>         right_space = left_spaces ++ extra_space
>
> render :: BlockElement -> String
> render block = (concat (intersperse "\n" ([ renderBlockElementAt lineloc block | lineloc <- allOutputLines [block]]))) ++ "\n"
>
> 
> class BlockDisplay a where
>   blockDisplay :: a -> BlockElement
>   blockDisplayList :: HBlockSeparator -> [a] -> BlockElement
>   blockDisplayList separator contents = HBlock (HorizontalBlock separator [blockDisplay c | c <- contents])
>
> instance (Integral a, Show a) => BlockDisplay (Ratio a) where
>   blockDisplay x
>     | denominator x == 1 = OrdinaryText (show (numerator x))
>     | otherwise = Fraction (show (numerator x)) (show (denominator x))
>
> instance BlockDisplay String where
>   blockDisplay x = OrdinaryText x
> instance BlockDisplay Int where
>   blockDisplay x = OrdinaryText (show x)
> instance BlockDisplay Integer where
>   blockDisplay x = OrdinaryText (show x)
