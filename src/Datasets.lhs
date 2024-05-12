> module Datasets where
> import Data.List
> -- A is the type of the row names. Often Int, sometimes String
> -- B is the type of the column names. Often String, otherwise something enumerable
> -- C is the type of a feature. Really, I should make this abstract as well, but
> --   how often do you want to have heterogenous feature types. (Answer: really often, actually)
> -- D is the type of the target.
>
> data Dataset a b c d = Dataset {
>   rowNames :: [a],
>   featureNames :: [b],
>   extractFeature :: b -> [c], -- I think this is a bad idea. We should use a hashtable.
>   extractTarget :: [d]
> }
> instance (Show a, Show b, Show c, Show d) => Show (Dataset a b c d) where
>   show (Dataset {rowNames = r, featureNames = fnames, extractFeature=ef, extractTarget=et}) =
>     (intercalate "\n" [
>         (show fname) ++ ": " ++ (intercalate ", " [(show name) ++ "=" ++ (show fval) | (name,fval) <- zip r (ef fname)]) |
>          fname <- fnames]) ++ "\nTarget: " ++ 
>     (intercalate ", " [(show name) ++ "=" ++ (show t) | (name, t) <- zip r et])
> 
> extractFeatureAndTarget :: Dataset a b c d -> b -> [(c,d)]
> extractFeatureAndTarget = -- Existing function definition
>
> selectListElements :: [a] -> [Bool] -> [a]
> selectListElements [] [] = []
> selectListElements _ [] = error "selectListElements called with a too-short list of bools"
> selectListElements [] _ = error "selectListElements called with a too-short list of values"
> selectListElements (v:vs) (d:ds)
>   | d = v : (selectListElements vs ds)
>   | otherwise = (selectListElements vs ds)
>
> invertedDeciderList :: [Bool] -> [Bool]
> invertedDeciderList bs = [not b | b <- bs]
>
> selectRowsDataset :: Dataset a b c d -> [Bool] -> Dataset a b c d
> selectRowsDataset (Dataset {featureNames = f, extractFeature = ef, extractTarget = et, rowNames = rn}) bs =
>    Dataset {featureNames = f, extractFeature = ef', extractTarget = et', rowNames = rn'}
>  where
>    ef' feature = selectListElements (ef feature) bs
>    et' = selectListElements et bs
>    rn' = selectListElements rn bs
>
> splitBasedOnRowSelection :: Dataset a b c d -> b -> (c -> Bool) -> (Dataset a b c d, Dataset a b c d)
> splitBasedOnRowSelection dataset column filterfunc =
>  (selectRowsDataset dataset selection, selectRowsDataset dataset unselection)
>   where feature = (extractFeature dataset) column
>         selection = [filterfunc f | f <- feature]
>         unselection = invertedDeciderList selection
>
