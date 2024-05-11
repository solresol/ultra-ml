> module DecisionTree where
> import RegionSlices
> import Datasets
> import UltrametricCalculator
> import Data.Maybe
> data DecisionTreeTraining a b c =
>     DecisionTreeTrainingLeaf { exemplar :: c }
>   | DecisionTreeTrainingBranch {
>                          -- decision_centre :: a,
>                          -- decision_radius :: Double,
>                          -- I need to fix the regionIdToRegionFunc so that I get that data back
>                          in_vs_out_func :: a -> Bool,
>                          which_feature :: b,
>                          training_score :: Double,
>                          what_to_do_if_in :: DecisionTreeTraining a b c,
>                          what_to_do_if_out :: DecisionTreeTraining a b c }
>   | UnfinishedTrainingBlob {
>       unprocessed :: Dataset b a c
>       }
>                            
> train_decision_tree :: Eq b => Dataset b c d -> (UltrametricCalculator a) -> (DecisionTreeTraining a b c)
> train_decision_tree training_data calc =
>   DecisionTreeTrainingBranch {
>      in_vs_out_func = splittingfunc,
>      which_feature = feature,
>      training_score = lowest_score,
>      what_to_do_if_in = UnfinishedTrainingBlob { unprocessed = in_data },
>      what_to_do_if_out = UnfinishedTrainingBlob { unprocessed = out_data }
>      }
>    where features = featureNames training_data
>          column_slices = [(f,extractFeatureAndTarget training_data f) | f <- features]
>          ultraregions = [(f,treeify calc column_slice) | (f,column_slice) <- column_slices ]
>          scorer = partitionScorer SumOfDistances calc
>          partitions = [(f,optimalPartition tree scorer) | (f,tree) <- ultraregions]
>          lowest_score = minimum [k | (f,(r,k)) <- partitions ]
>          (feature, region_id) = head [(f,r) | (f,(r,k)) <- partitions, k == lowest_score]
>          ultraregion_from_that_feature = head [t | (f,t) <- ultraregions, f == feature]
>          splittingfunc = fromJust (regionIdToRegionFunc calc ultraregion_from_that_feature region_id)
>          (in_data, out_data) = splitBasedOnRowSelection training_data feature splittingfunc
>          
>
