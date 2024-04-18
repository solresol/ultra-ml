> module RegionSelection where
> import RegionSlices
> import Polynomial
> import Data.Ratio
>
> driver = RegionSlices.treeify (Polynomial.polyOverRationalsDegree) [
>           ((Polynomial [1%2,2%3]),"poly1"),
>           ((Polynomial [1%2,1%3]), "poly2"),
>           ((Polynomial [6%1,5%1,4%1]), "poly3")]
