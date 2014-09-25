{-# LANGUAGE MultiParamTypeClasses #-}

module Put (Put (put)) where

class Put a b where put :: a -> b -> b
