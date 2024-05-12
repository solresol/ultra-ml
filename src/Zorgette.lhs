> module Zorgette where
> import Datasets
> import Padic
> import BlockDisplay
> import DecisionTree
> data ZorgetteLoot = Robot1Loot | Robot2Loot | Robot3Loot deriving (Eq, Ord, Enum, Show)
> data ZorgetteRequest = Chestnut_n_02
>                      | Hornbeam_n_01
>                      | Hop_hornbeam_n_01
>                      | Beech_n_01
>                      | Necklace_tree_n_01
>                      | Hackberry_n_01
>                      | Locust_tree_n_01
>                      | Angiospermous_tree_n_01
>                      | Bonsai_n_01
>                      | Incense_tree_n_01 deriving (Eq, Ord, Enum, Show)
> zp = padic_int 409
> zplist = map zp
zorgette :: Dataset ZorgetteRequest ZorgetteLoot Padic Padic
zorgette = -- Existing function definition
> zorgette = Dataset {
>   rowNames = [Chestnut_n_02  .. Incense_tree_n_01 ],
>   featureNames = [Robot1Loot, Robot2Loot],
>   extractFeature = \x -> (case (x) of
>       Robot1Loot -> zplist [273116748704467022682724613459,
>                             117240465583858939981595536269,
>                             117109477110648344954115595868,
>                             55675883174879277066023547799,
>                             68643742022728184786537647498,
>                             116847500164227154899155715066,
>                             120646165887334410696073986695,
>                             375942700174784119254477828244,
>                             110167088030486808497678754615,
>                             224912990562968052570106545891]
>       Robot2Loot -> zplist [326691034247600388922020237468,
>                             63666180040725573742299912260,
>                             63535191567514978714819971859,
>                             162824454261146009544614795817,
>                             122218027565861551025833271507,
>                             63273214621093788659860091057,
>                             227794736973601143174665234713,
>                             2840359835158918966262076532658,
>                             56592802487353442258383130606,
>                             171338705019834686330810921882]),
>   extractTarget = zplist [45991216075942090948,
>                           655934845482986543017862842,
>                           1573780139196323304716,
>                           396171205890659683677595416,
>                           800684989475070496403917474,
>                           1285764896971742062431186,
>                           5762476220082796694,
>                           394573092415095127486114211,
>                           106017242436927074913158021,
>                           99579452998956312316]
> }


