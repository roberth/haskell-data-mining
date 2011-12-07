{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Mining.Classification.Class where

class Classifier classifier attributes label 
                       | classifier -> attributes label where
  classify :: classifier -> attributes -> label
