{-# LANGUAGE OverloadedStrings          #-}

module Utils.Utils where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

orLeft :: a -> Maybe b -> Either a b
orLeft leftValue mValue =
    case mValue of
        Just value -> Right value
        _ -> Left leftValue
