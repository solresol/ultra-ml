> module Main where
> import Zorgette
> import DecisionTree
> import Padic
>
> main :: IO ()
> main = do
>   putStrLn "Starting"
>   putStrLn (show zorgette)
>   dectree <- train_decision_tree zorgette padic_distance_calculator padic_distance_calculator
>   putStrLn "Ending"
