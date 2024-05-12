> module DecisionTree where
> import RegionSlices
> import Datasets
> import UltrametricCalculator
> import Data.Maybe

a ... the type of a row name
b ... the type of a column name
c ... the type of a feature
d ... the type of a target (a prediction)

 
> data DecisionTreeTraining a b c d =
>     DecisionTreeTrainingLeaf { exemplar :: d }
>   | DecisionTreeTrainingBranch {
>                          -- decision_centre :: a,
>                          -- decision_radius :: Double,
>                          -- I need to fix the regionIdToRegionFunc so that I get that data back
>                          in_vs_out_func :: c -> Bool,
>                          which_feature :: b,
>                          training_score :: Double,
>                          what_to_do_if_in :: DecisionTreeTraining a b c d,
>                          what_to_do_if_out :: DecisionTreeTraining a b c d }
>   | UnfinishedTrainingBlob {
>       unprocessed :: Dataset a b c d
>       }
> -- I need a way of visualising it.
> -- I need a way of pickling it.
>
> data DecisionTreeProcessingStep b c d =
>        ScoreColumnSplit b     -- What's the best split in feature a?
>      | CreateInVsOutDataset
>      | ResumeUnfinishedTraining
>

b ... the type of the column names
c ... the type of a feature
d ... the type of a target


> -- Add type signature to train_decision_tree function
> train_decision_tree training_data featurecalc targetcalc =
>   return (DecisionTreeTrainingBranch {
>      in_vs_out_func = splittingfunc,
>      which_feature = feature,
>      training_score = lowest_score,
>      what_to_do_if_in = UnfinishedTrainingBlob { unprocessed = in_data },
>      what_to_do_if_out = UnfinishedTrainingBlob { unprocessed = out_data }
>      })
>    where features = featureNames training_data
>          column_slices = [(f,extractFeatureAndTarget training_data f) | f <- features]
>          ultraregions = [(f,treeify featurecalc column_slice) | (f,column_slice) <- column_slices ]
>          scorer = partitionScorer SumOfDistances targetcalc
>          partitions = [(f,optimalPartition tree scorer) | (f,tree) <- ultraregions]
>          lowest_score = minimum [k | (f,(r,k)) <- partitions ]
>          (feature, region_id) = head [(f,r) | (f,(r,k)) <- partitions, k == lowest_score]
>          ultraregion_from_that_feature = head [t | (f,t) <- ultraregions, f == feature]
>          splittingfunc = fromJust (regionIdToRegionFunc featurecalc ultraregion_from_that_feature region_id)
>          (in_data, out_data) = splitBasedOnRowSelection training_data feature splittingfunc
>          
>
