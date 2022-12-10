module Common.Tree
  ( Tree,
    empty,
    insert,
    member,
    keys,
    elems,
    lookup,
    length,
    isEmpty,
    subtree,
    subtrees,
    fromList,
    isLeaf,
  )
where

import Data.Map (Map, partition)
import qualified Data.Map as Map (elems, empty, insert, keys, lookup, member, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (length, lookup)
import qualified Prelude (length)

data Node k a = Leaf a | Branch (Tree k a) deriving (Eq, Show)

newtype Tree k a = Tree (Map k (Node k a)) deriving (Eq, Show)

empty :: Tree k a
empty = Tree Map.empty

isEmpty :: Tree k a -> Bool
isEmpty (Tree tree) = null tree

insert :: (Ord k) => [k] -> a -> Tree k a -> Tree k a
insert [] _ _ = error "insert: no key specified"
insert [key] element (Tree tree) = Tree (Map.insert key (Leaf element) tree)
insert (key : path) element tree@(Tree sTree) = Tree (Map.insert key newBranch sTree)
  where
    newBranch = Branch . insert path element . subtree [key] $ tree

member :: (Ord k) => [k] -> Tree k a -> Bool
member [] _ = False
member [key] (Tree tree) = Map.member key tree
member (key : path) (Tree tree) = maybe False (member path . fromMaybe empty . subtreeOf) . Map.lookup key $ tree

keys :: (Ord k) => Tree k a -> [[k]]
keys (Tree tree) =
  ((map (: []) $ Map.keys leafs) ++)
    . concatMap (\(k, v) -> maybe [] (map ([k] ++) . keys) . subtreeOf $ v)
    . Map.toList
    $ branches
  where
    (leafs, branches) = partition isLeaf tree

elems :: Tree k a -> [a]
elems (Tree tree) = concatMap elemsOf . Map.elems $ tree

elemsOf :: Node k a -> [a]
elemsOf (Leaf x) = [x]
elemsOf (Branch tree) = elems tree

lookup :: (Ord k) => [k] -> Tree k a -> Maybe a
lookup [] _ = Nothing
lookup [key] (Tree tree) = maybe Nothing elemOf . Map.lookup key $ tree
lookup (key : path) (Tree tree) = maybe Nothing (lookup path) . maybe Nothing subtreeOf . Map.lookup key $ tree

elemOf :: Node k a -> Maybe a
elemOf (Leaf x) = Just x
elemOf (Branch _) = Nothing

length :: Tree k a -> Int
length (Tree tree) = Prelude.length leafs + (sum . map length . mapMaybe subtreeOf . Map.elems $ branches)
  where
    (leafs, branches) = partition isLeaf tree

subtree :: (Ord k) => [k] -> Tree k a -> Tree k a
subtree [] _ = error "subtree: no key specified"
subtree [key] (Tree tree) = fromMaybe empty . maybe Nothing subtreeOf . Map.lookup key $ tree
subtree (key : path) (Tree tree) = maybe empty (subtree path) . maybe Nothing subtreeOf . Map.lookup key $ tree

subtrees :: Tree k a -> [Tree k a]
subtrees tree@(Tree sTree) = (tree :) . concatMap subtrees . mapMaybe subtreeOf . Map.elems $ sTree

subtreeOf :: Node k a -> Maybe (Tree k a)
subtreeOf (Leaf _) = Nothing
subtreeOf (Branch tree) = Just tree

isLeaf :: Node k a -> Bool
isLeaf (Leaf _) = True
isLeaf (Branch _) = False

fromList :: (Ord k) => [([k], a)] -> Tree k a
fromList = foldl (\tree (k, v) -> insert k v tree) empty
