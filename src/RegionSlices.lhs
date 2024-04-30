> module RegionSlices where
> import UltrametricCalculator
> import BlockDisplay
> 
> data RegionId = Anonymous | RegionId Int deriving (Eq, Ord)
> instance Show RegionId where
>   show Anonymous = "(Region without id)"
>   show (RegionId n) = "Region #" ++ (show n)
> data RegionIdPool = RegionIdPool { next_unused :: Int }
> newRegionIdPool :: RegionIdPool
> newRegionIdPool = RegionIdPool {next_unused = 1}
> allocateRegionId :: RegionIdPool -> (RegionId, RegionIdPool)
> allocateRegionId (RegionIdPool {next_unused=n}) = ((RegionId n), RegionIdPool {next_unused=(n+1) })
>
> data UltrametricRegion a b = Point {
>    point_id :: RegionId,
>    point :: a,
>    payloads :: [b]
>   } | Circle {
>    circle_id :: RegionId,
>    centre :: a,
>    radius :: Double,
>    subregions :: [UltrametricRegion a b]
>    -- invariants:
>    -- - all subregions have a smaller radius than the current one
>    -- - the subregions are equidistant from each other
>  -- 5, 30, 130, 55
>
>  } | NullRegion
>
> instance (BlockDisplay a, BlockDisplay b) => BlockDisplay (UltrametricRegion a b) where
>  blockDisplay (Point {point_id=pid, point=p, payloads=ps}) =
>    Bordered (
>      HBlock (HorizontalBlock NoSeparator [
>           OrdinaryText (show (pid)),
>           OrdinaryText " Point = ",
>           blockDisplay p,
>           OrdinaryText " Payload = [",
>           HBlock (HorizontalBlock (Delimiter "; ") [blockDisplay px | px <- ps]),
>           OrdinaryText "]"
>           ]))
>  blockDisplay NullRegion = Bordered (OrdinaryText "Null region")
>  blockDisplay (Circle {circle_id=cid, centre=c, radius=r, subregions=ss}) =
>     Bordered (
>       VBlock (VerticalBlock { internal_blocks_alignment=LeftAligned, block_alignment=Top,
>         blocks=[(HBlock (HorizontalBlock NoSeparator [
>                    OrdinaryText (show cid),
>                    OrdinaryText " Centre = ",
>                    (blockDisplay c),
>                    OrdinaryText "; Radius = ",
>                    OrdinaryText (show r)
>                   ])),
>                 (HBlock (HorizontalBlock NoSeparator [
>                    OrdinaryText "Subregions: ",
>                    VBlock (VerticalBlock { internal_blocks_alignment=LeftAligned, block_alignment=Top,
>                          blocks=[Bordered (blockDisplay s) | s <- ss] } ) ] ) ) ]  } ) )
>
> instance (BlockDisplay a, BlockDisplay b) => Show (UltrametricRegion a b) where
>   show ur = render (blockDisplay ur)
>
> getRegionId :: UltrametricRegion a b -> RegionId
> getRegionId NullRegion = Anonymous
> getRegionId (Point {point_id=p}) = p
> getRegionId (Circle {circle_id=i}) = i
>
> isAnonymousRegion :: UltrametricRegion a b -> Bool
> isAnonymousRegion u = case (getRegionId u) of
>     Anonymous -> True
>     _ -> False
> 
> isNullRegion NullRegion = True
> isNullRegion _ = False
>

data Separation = Undefined | Equidistant Double | Multidistance [Double]

 separation :: (a -> a -> Double) -> UltrametricRegion a b -> UltrametricRegion a b -> Maybe Double
 separation _ NullRegion _ = Undefined
 separation _ _ NullRegion = Undefined
 separation f (Point {point=p}) (Point {point=q}) = Equidistant (f p q)
 separation f (Point {point=p}) (Circle {centre=c}) =  Equidistant (f p c)
 separation f (Circle {centre=d}) (Circle {centre=c}) = Equidistant (f d c)  
 separation f  (Circle {centre=c}) (Point {point=p}) = Equidistant (f c p)
 separationCheck :: (a -> a -> Double) -> [UltrametricRegion a b] -> Separation
 separationCheck f [] = Undefined
 separationCheck f [_] = Undefined 
 separationCheck f [u1,u2] = separation u1 u2
 

sanitcheck' needs to check that all the subregions are equidistant
from each other

> sanitycheck' ::  (a -> a -> Double) -> UltrametricRegion a b -> Bool
> sanitycheck' _ (p@(Point {point=_})) = True
> sanitycheck' _ NullRegion = True
> sanitycheck' f (Circle {centre=c, radius=r, subregions=ss})
>  | any isNullRegion ss = error "There is a null region inside a Circle"
>  | not (all issmaller ss) = error "Not all subregions are smaller than the radius we of this ultrametric region"
>  | not (all (sanitycheck' f) ss) = error "Not all subregions passed the sanitycheck (although how we got back here is anyone's guess)"
>  | otherwise = True
>  where issmaller (Point {point=_}) = True
>        issmaller (Circle {radius=t}) = t < r
>
> sanitychecked :: (a -> a -> Double) -> UltrametricRegion a b -> UltrametricRegion a b
> sanitychecked f ur
>   | sanitycheck' f ur = ur
>   | otherwise = error "Sanity check failed"
>        
>
> data Containment = Inside | OutsideEquidistant Double |
>                    OutsideMinMaxRange Double Double |
>                    Noncontainer deriving Eq
> isOutside :: Containment -> Bool
> isOutside Inside = False
> isOutside _ = True
>
> isEquidistantOutside :: Containment -> Bool
> isEquidistantOutside (OutsideEquidistant _) = True
> isEquidistantOutside _ = False
>
> equidistantLength :: Containment -> Double
> equidistantLength (OutsideEquidistant k) = k
> equidistantLength _ = error "Asked to extract equidistant length from something that wasn't"
>
> isInside :: Containment -> Bool
> isInside Inside = True
> isInside _ = False
> 
> 
> containment :: (a -> a -> Double) -> a -> UltrametricRegion a b -> Containment
> containment f x (Point {point=p})
>   | f x p == 0.0 = Inside
>   | otherwise = OutsideEquidistant (f x p)
> containment f x (Circle {centre=c, radius=r})
>   | f x c <= r = Inside
>   | otherwise = OutsideEquidistant (f x c)
> containment _ _ NullRegion = Noncontainer
>
> containment_reduction :: [Containment] -> Containment
> containment_reduction [] = Noncontainer
> containment_reduction (Inside:_) = Inside
> containment_reduction (Noncontainer:xs) = containment_reduction xs
> containment_reduction ((OutsideEquidistant x):((OutsideEquidistant y):xs))
>   | x < y = containment_reduction ((OutsideMinMaxRange x y):xs)
>   | y < x = containment_reduction ((OutsideMinMaxRange y x):xs)
>   | x == y = containment_reduction ((OutsideEquidistant x):xs)
> containment_reduction ((OutsideEquidistant x):((Inside):_)) = Inside
> containment_reduction ((OutsideMinMaxRange x y):((Inside):_)) = Inside
>    -- that *should* be impossible
> containment_reduction ((OutsideEquidistant x):((Noncontainer):_)) = OutsideEquidistant x
> containment_reduction ((OutsideMinMaxRange x y):((Noncontainer):_)) = OutsideMinMaxRange x y
> containment_reduction [OutsideEquidistant x] = OutsideEquidistant x
> containment_reduction [OutsideMinMaxRange x y] = OutsideMinMaxRange x y

> containments :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> Containment
> containments f x regions = containment_reduction ([containment f x r | r <- regions])
>
> 
>
> outsideAll :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> Bool
> outsideAll f x rs = isOutside (containments f x rs)
>

This really shouldn't happen...

> insideMultiple :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> Bool
> insideMultiple f t regions = length (insiders f t regions) > 1
>
> insiders :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> [UltrametricRegion a b]
> insiders f t regions = [r | r <- regions, isInside (containment f t r)]
> outsiders :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> [UltrametricRegion a b]
> outsiders f t regions = [r | r <- regions, isOutside (containment f t r)]
> partitionByClosest :: (a -> a -> Double) -> a -> [UltrametricRegion a b] -> (UltrametricRegion a b, [UltrametricRegion a b])
> partitionByClosest f x (ur: urs) = partitionByClosest' urs distance (ur,[])
>  where partitionByClosest' [] _ k = k
>        partitionByClosest' (z:zs) d (k,ks)
>         | equidistantLength (containment f x z) < d = partitionByClosest' zs d (z,k:ks)
>         | otherwise = partitionByClosest' zs d (k,z:ks)
>        distance = equidistantLength (containment f x ur)
> 
>
> addToRegion :: (a -> a -> Double) -> a -> b -> UltrametricRegion a b -> UltrametricRegion a b
> addToRegion _ x y NullRegion = makePoint x y
> addToRegion f _ y (p@(Point {payloads = ps})) = p { payloads = y : ps }
> addToRegion f x y region
>  | insideMultiple f x  (subregions region) = error "inside multiple regions"
>  | length (insiders f x (subregions region)) == 1 =
>      region {subregions = [(addToRegion f x y (head (insiders f x (subregions region))))] ++ (outsiders f x (subregions region)) }
>  | isEquidistantOutside (containments f x (subregions region))
>     = region { subregions = (makePoint x y) : (subregions region ) }
>  | otherwise = region {subregions = (treeInsert f x y near) : far }
>      where (near, far) = partitionByClosest f x (subregions region)
>
> makePoint :: a -> b -> UltrametricRegion a b
> makePoint x y = Point { point=x, payloads=[y], point_id=Anonymous }
>
> treeify' :: (UltrametricCalculator a) -> [(a,b)] -> UltrametricRegion a b -> UltrametricRegion a b
> treeify' _ [] current_tree = current_tree
> treeify' (UltrametricCalculator uc) ((position,payload):remainder) current_tree =
>    treeify' (UltrametricCalculator uc) remainder (treeInsert uc position payload current_tree)
>
>
> treeInsert :: (a -> a -> Double) -> a -> b -> UltrametricRegion a b -> UltrametricRegion a b
> treeInsert _ position payload NullRegion = makePoint position payload
> treeInsert uc position payload (Point {point=pt, payloads=pl, point_id=Anonymous })
>   | uc position pt == 0.0 = Point {point=pt, payloads=payload:pl, point_id=Anonymous}
>   | otherwise = Circle {centre=pt, radius=uc position pt, circle_id=Anonymous,
>                         subregions=[Point {point=pt, payloads=pl, point_id=Anonymous},
>                                     makePoint position payload]}
> treeInsert uc position payload (region@(Circle {centre=c, radius=r, subregions=srs}))
>   | uc position c <= r = sanitychecked uc (addToRegion uc position payload region)
>   | otherwise = Circle { centre=position,
>                          radius=uc position c,
>                          subregions=[region, makePoint position payload],
>                          circle_id = Anonymous
> }
>
> assignRegionIDsToList :: RegionIdPool -> [UltrametricRegion a b] -> ([UltrametricRegion a b],RegionIdPool)
> assignRegionIDsToList rids [] = ([],rids)
> assignRegionIDsToList rids (u:us) = (u':us', rids'')
>   where (u', rids') = assignRegionIDs rids u
>         (us', rids'') = assignRegionIDsToList rids' us 
>
> assignRegionIDs :: RegionIdPool -> UltrametricRegion a b -> (UltrametricRegion a b,RegionIdPool)
> assignRegionIDs pool (p@(Point {}))
>   | isAnonymousRegion p = ((p { point_id = rid }), pool')
>   | otherwise = (p, pool)
>       where (rid, pool') = allocateRegionId pool
> assignRegionIDs pool (c@(Circle {subregions=s}))
>   | isAnonymousRegion c = ((c { circle_id = rid, subregions=s' }), pool'')
>   | otherwise = (c { subregions=s'}, pool')
>       where (rid, pool') = allocateRegionId pool
>             (s', pool'') = assignRegionIDsToList pool' s
>             -- if the region has an ID, we allocate one and then forget it
> assignRegionIDs pool (NullRegion) = (NullRegion, pool)
 
And this is what we've been building up to...

> treeify :: (UltrametricCalculator a) -> [(a,b)] -> UltrametricRegion a b
> treeify uc d = labelled
>   where unlabelled = treeify' uc d NullRegion
>         (labelled, pool') = assignRegionIDs (newRegionIdPool) unlabelled
>

> extractRegions :: UltrametricRegion a b -> [RegionId]
> extractRegions (Point {point_id=r}) = [r]
> extractRegions (Circle {circle_id=r, subregions=ss}) =
>     r : (concat ([extractRegions s | s <- ss]))
> extractRegions NullRegion = []
>    -- the only thing we want to do when we extractRegions is to keep track
>    -- of them for later, so the Anonymous region ID is useless
>    -- I would feel much happier if Id'd and Anonymous Regions were differnt types
>
> recursivePayloadExtract :: UltrametricRegion a b -> [b]
> recursivePayloadExtract (Point {payloads=ps}) = ps
> recursivePayloadExtract (Circle {subregions=ss}) =
>     concat ([recursivePayloadExtract s | s <- ss])
> recursivePayloadExtract NullRegion = []
>
> data PayloadPartition b = PayloadPartition { in_region :: [b], out_of_region :: [b] }
>    deriving (Show)
> instance (BlockDisplay b) => BlockDisplay (PayloadPartition b)
>   where blockDisplay (PayloadPartition { in_region=ins, out_of_region=outs }) =
>            VBlock (VerticalBlock {
>                       internal_blocks_alignment=LeftAligned,
>                       block_alignment=Top,
>                       blocks=[in_block, out_block] })
>            where in_block = (HBlock (
>                               HorizontalBlock (NSpaces 1) [OrdinaryText "IN  = ", Bordered (blockDisplayList (Delimiter " | ") ins)]))
>                  out_block =  (HBlock (
>                               HorizontalBlock (NSpaces 1) [OrdinaryText "OUT = ", Bordered (blockDisplayList (Delimiter " | ") outs)]))
> 
> emptyPayloadPartition = PayloadPartition { in_region=[], out_of_region=[] }
>
> mergePayloadPartitionPair :: PayloadPartition a -> PayloadPartition a-> PayloadPartition a
> mergePayloadPartitionPair (PayloadPartition {in_region=in1, out_of_region=out1}) (PayloadPartition {in_region=in2, out_of_region=out2}) =
>    PayloadPartition {in_region=in1 ++ in2, out_of_region=out1 ++ out2 }
>
> 
> mergePayloadPartitions :: [PayloadPartition a] -> PayloadPartition a
> mergePayloadPartitions ps = foldl mergePayloadPartitionPair emptyPayloadPartition ps
> 
> payloadPartitionByRegionId :: RegionId -> UltrametricRegion a b -> PayloadPartition b
> payloadPartitionByRegionId region_id (Point {point_id=r, payloads=ps})
>   | region_id == r = PayloadPartition { in_region=ps, out_of_region = [] }
>   | otherwise = PayloadPartition { in_region=[], out_of_region = ps }
> payloadPartitionByRegionId region_id (c@(Circle {circle_id=c_id, subregions=ss}))
>   | region_id == c_id = PayloadPartition { in_region=recursivePayloadExtract c,
>                                         out_of_region = [] }
>   | otherwise = mergePayloadPartitions [payloadPartitionByRegionId region_id s | s <- ss]
> payloadPartitionByRegionId region_id NullRegion = emptyPayloadPartition
>       
