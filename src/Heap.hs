module Heap (insert, 
             size, 
             deleteMax, 
             getMax, 
             fromList, 
             toList, 
             isEmpty,
             empty,
             singleton,
             member,
             Heap (..) ) where


data Heap a = Empty | Node a Int (Heap a) (Heap a) 


-- Insert an element to the heap.   
insert :: Ord a => a -> Heap a -> Heap a
insert x Empty     = Node x 1 Empty Empty
insert x (Node t n l r)
    | x >  t && size l > size r    = Node x (n + 1) l (insert t r)
    | x >  t                       = Node x (n + 1) (insert t l) r 
    | x <= t && size l > size r    = Node t (n + 1) l (insert x r) 
    | x <= t                       = Node t (n + 1) (insert x l) r  


-- Checks if a heap is empty. 
isEmpty :: Ord a => Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False 


-- Gives the empty heap. 
empty :: Ord a => Heap a
empty = Empty 


-- Creates a heap from a list. 
fromList :: Ord a => [a] -> Heap a
fromList ls = foldr insert Empty ls      


-- Checks if an element is a member of the heap. 
member :: Ord a => a -> Heap a -> Bool
member x Empty         = False
member x (Node t _ l r) 
    | x >= t           = x == t 
    | otherwise        = member x l || member x r 


-- Turns the heap into a list. 
toList :: Ord a => Heap a -> [a]
toList Empty          = []
toList (Node t _ l r) = (t : toList l) ++ (toList r)


-- Deletes an element from a heap which doesn't have any 
-- smaller elements in a left or right heap. The mentioned
-- element together with the new heap are returned.  
deleteBottom :: Ord a => Heap a -> (a, Heap a)
deleteBottom (Node t _ Empty Empty) = (t, Empty)
deleteBottom (Node t n l r)
    | size l < size r         = (x1, Node t (n - 1) l r')  
    | otherwise               = (x2, Node t (n - 1) l' r)
    where
    (x1, r')   = deleteBottom r 
    (x2, l')   = deleteBottom l 


-- Delete the largest element in the heap. The largest element
-- together with the new heap are returned. 
-- Doesn't check for non-emptiness, therefore unsafe.  
deleteMax :: Ord a => Heap a -> (a, Heap a)
deleteMax (Node t _ Empty Empty) = (t, Empty) 
deleteMax (Node t n l r)         = (t, merge x l' r') 
    where
    (x, Node _ _ l' r')   = deleteBottom (Node t n l r)


-- Create a heap from a single element. 
singleton :: Ord a => a -> Heap a
singleton x = insert x Empty


-- An element x and two heaps A and B for which holds:
--    |size A - size B| <= 1. 
-- Returns a new heap where x, A and B are glued together. 
merge :: Ord a => a -> Heap a -> Heap a -> Heap a
merge x Empty Empty      = singleton x
merge x (Node t _ Empty Empty) Empty 
    | x < t              = Node t 2 (singleton x) Empty 
    | otherwise          = Node x 2 (singleton t) Empty   
merge x Empty (Node t _ Empty Empty)  
    | x < t              = Node t 2 Empty (singleton x)  
    | otherwise          = Node x 2 Empty (singleton t)
merge x (Node t n l r) (Node t' n' l' r')
    | x >= t && x >= t'  = Node x new_n (Node t n l r) (Node t' n' l' r')
    | t >= x && t >= t'  = Node t new_n (merge x l r) (Node t' n' l' r') 
    | otherwise          = Node t' new_n (Node t n l r) (merge x l' r') 
    where
    new_n   = 1 + n + n' 


-- Returns the number of elements in a heap. 
size :: Ord a => Heap a -> Int
size Empty              = 0
size (Node _ n _ _)     = n 


-- Get the largest element in a non-empty heap.
-- Doesn't check for non-emptiness, therefore unsafe.  
getMax :: Ord a => Heap a -> a
getMax (Node t _ _ _) = t


instance (Show a) => Show (Heap a) where
    show tr = ((++) "\n" . unlines . snd . toLines) tr

        where

        toLines :: (Show a) => Heap a -> (Int, [String])
        toLines Empty                   = (0, [""])
        toLines (Node t _ Empty Empty)  = (0, [" " ++ show t])
        toLines (Node t _ l r)          = (ln + 1, lv_new ++ [" *"] ++ [" " ++ show t] ++ [" *"] ++ rv_new)
            where
            (il, lv)    = toLines l
            (ir, rv)    = toLines r
            ln          = length lv
            rn          = length rv
            lv_sub      = (replicate il "        ") ++ [" *******"] ++ (replicate (ln - il) " *      ")
            rv_sub      = (replicate ir " *      ") ++ [" *******"] ++ (replicate (rn - ir) "        ")
            lv_new      = zipWith (++) lv_sub lv
            rv_new      = zipWith (++) rv_sub rv