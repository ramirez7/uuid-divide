{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.UUID.NewModule where

newFunction :: () -> () -> ()
newFunction _ _ = ()

otherFunction :: () -> ()
otherFunction _ = ()

newtype NewType = NewType () deriving (Show)

-- TODO: Write an orphan and it'll get down to completely 0



