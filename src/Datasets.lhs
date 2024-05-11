> module Datasets where
> import Data.List
> -- B is the type of the column names. Often String, otherwise something enumerable
> -- C is the type of a feature. Really, I should make this abstract as well, but
> --   how often do you want to have heterogenous feature types. (Answer: really often, actually)
> -- D is the type of the target.
>
> data Dataset b c d = Dataset {
>   featureNames :: [b],
>   extractFeature :: b -> [c],
>   extractTarget :: [d]
> }
> extractFeatureAndTarget :: Dataset b c d -> b -> [(c,d)]
> extractFeatureAndTarget dataset idx = zip ((extractFeature dataset) idx) ((extractTarget dataset))
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
> selectRowsDataset :: Dataset b c d -> [Bool] -> Dataset b c d
> selectRowsDataset (Dataset {featureNames = f, extractFeature = ef, extractTarget = et}) bs =
>    Dataset {featureNames = f, extractFeature = ef', extractTarget = et'}
>  where
>    ef' feature = selectListElements (ef feature) bs
>    et' = selectListElements et bs
>
> splitBasedOnRowSelection :: Dataset b c d -> b -> (c -> Bool) -> (Dataset b c d, Dataset b c d)
> splitBasedOnRowSelection dataset column filterfunc =
>  (selectRowsDataset dataset selection, selectRowsDataset dataset unselection)
>   where feature = (extractFeature dataset) column
>         selection = [filterfunc f | f <- feature]
>         unselection = invertedDeciderList selection
>
>
