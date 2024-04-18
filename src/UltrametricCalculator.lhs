> module UltrametricCalculator where
> data UltrametricCalculator a = UltrametricCalculator (a -> a -> Double)
> -- This has to satisfy the ultrametric triangle inequality, otherwise
> -- distance branching won't make sense
>
