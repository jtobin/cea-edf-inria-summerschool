module Trie where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Data.Maybe

-- A trie is a tree storing elements with a common prefix in the same node.
data Trie k v      = Node (Maybe v) (Map.Map k (Trie k v))               -- Think of the v's as optional labels
type TrieSet k     = Trie k ()                                           -- A variant with no labels

lookup :: Ord k => [k] -> Trie k v -> Maybe v                           
lookup []     (Node v _)  = v                                           
lookup (k:ks) (Node _ cs) = case Map.lookup k cs of                     
                                Nothing -> Nothing                      
                                Just t  -> lookup ks t                  

empty :: Trie k v
empty = Node Nothing Map.empty

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert []     v (Node _ m)   = Node (Just v) m                                              
insert (k:ks) v (Node v0 cs) = case Map.lookup k cs of                                      
                                Nothing -> Node v0 (Map.insert k (insert ks v empty) cs)    
                                Just t  -> Node v0 (Map.insert k (insert ks v t)     cs)    

fromList :: Ord k => [([k], a)] -> Trie k a
fromList []           = empty
fromList ((ks, v):xs) = insert ks v (fromList xs)

fromList' :: Ord k => [[k]] -> TrieSet k        -- FIXME too lazy
fromList' []     = empty
fromList' (k:ks) = insert k () (fromList' ks)

notMember' :: Ord k => [k] -> TrieSet k -> Bool
notMember' ks t = isNothing (lookup ks t) 

elem' :: Ord k => [k] -> TrieSet k -> Bool
elem' ks t = not $ notMember' ks t

-- Testing

testList0 = ["hello", "he-man", "hello ma'am", "hello sir", "to", "this", "that"]
testTrie0 = fromList' testList0



