> module RegionSelection where
> import RegionSlices
> import Polynomial
> import Data.Ratio
> import BlockDisplay
>
> driver = RegionSlices.treeify (Polynomial.polyOverRationalsDegree) [
>           ((Polynomial [1%2,1%3]), (Polynomial [1%1])),
>           ((Polynomial [6%1,5%1,4%1]), (Polynomial [1%1, 2%1])),
>           ((Polynomial [6%1,3%1,0%1]), (Polynomial [2%1, 3%1])),
>           ((Polynomial [1%2,2%3]),     (Polynomial [1%1, 1%2]))
>         ]
> driver_regions = extractRegions driver
> driver_all_payloads = recursivePayloadExtract driver

