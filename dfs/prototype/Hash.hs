{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hash where

import qualified Control.Monad (liftM)
import qualified Data.Map
       (Map, insert, insertWith, lookup, delete, findWithDefault)
import qualified Data.Maybe (isNothing)
import qualified Data.Set (Set, insert, member, delete)

class Store s a where
      has :: s -> a -> Bool
      has s = not . Data.Maybe.isNothing . get s
      get :: s -> a -> Maybe a
      get s x = if has s x then Just x else Nothing

      put :: s -> a -> s

instance Eq a => Store [a] a where
  put = flip (:)
  has = flip elem

instance Ord a => Store (Data.Set.Set a) a where
  put = flip Data.Set.insert
  has = flip Data.Set.member

instance Ord a => Store (Data.Map.Map a b) (a, b) where
  put = flip (uncurry Data.Map.insert)
  get m (key, _) = Control.Monad.liftM ((,) key) (Data.Map.lookup key m)

class Delete s a where
  delete :: s -> a -> s

instance Eq a => Delete [a] a where
  delete [] _ = []
  delete (y:ys) x = if x == y then ys else y : delete ys x

instance Ord a => Delete (Data.Set.Set a) a where
  delete = flip Data.Set.delete

instance (Ord a, Eq b) => Delete (Data.Map.Map a b) (a, b) where
  delete m (key, x) | x == y = Data.Map.delete key m
    where Just y = Data.Map.lookup key m

class Replace s a where
  replace :: s -> a -> a -> a

data TakeNew = TakeNew
data TakeOld = TakeOld
data Concat = Concat

instance Replace TakeNew a where replace _ new _ = new
instance Replace TakeOld a where replace _ _ old = old
instance Replace Concat [a] where replace _ = (++)

class Weight w a b | a -> b where
  weight :: w -> a -> b

instance (Weight w a b, Ord b) => Replace w a where
  replace w new old
    | weight w new > weight w old = new
    | otherwise = old

class Map s k v where
  store :: s -> k -> v -> s
  retrieve :: s -> k -> [v]

  storeL :: s -> [(k,v)] -> s
  storeL s ((k,v):kvs) = storeL (store s k v) kvs
  storeL s [] = s

instance Eq k => Map [(k,v)] k v where
  store l k v = (k, v) : l
  retrieve ((kx,kv):kvs) k
           | k == kx = kv : retrieve kvs k
           | otherwise = retrieve kvs k
  retrieve [] _ = []

{- overlapping
instance Ord k => Map (Data.Map.Map k [v]) k v where
  store m k v = Data.Map.insertWith (flip (++)) k [v] m
  retrieve = flip (Data.Map.findWithDefault [])
-}

class Hash a where hash :: a -> Int

instance Hash Int where hash = id

instance Hash a => Hash (a, Int) where
  hash (x, m) = flip mod m $ hash x

instance (Replace r [v], Hash a) => Map (r, Data.Map.Map Int [v]) a v where
  store (r,m) k v = (r, Data.Map.insertWith (replace r) (hash k) [v] m)
  retrieve (_,m) k = Data.Map.findWithDefault [] (hash k) m

------------------------------------------------------------------------------

data Tree a = Tree a [Tree a]
     deriving Show

bin :: Int -> Tree Int
bin 0 = Tree 1 []
bin n = let childs@[Tree ls _, Tree rs _] = [bin (pred n), bin (pred n)]
        in Tree (succ $ ls + rs) childs
