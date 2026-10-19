module Satellite

import Data.Fin
import Data.List
import Data.Maybe

public export
data Tree : Type where
     Leaf : Tree
     Branch : Tree -> Char -> Tree -> Tree

walk : List Char -> List Char -> Maybe (Tree, List Char)
walk rem Nil = Just (Leaf, rem)
walk Nil (_ :: _) = Nothing
walk (x :: xs) inorder
  = do i <- findIndex (x == ) inorder
       let (l, (p :: r)) = splitAt (finToNat i) inorder | (_, Nil) => Nothing
       (tree1, rem1) <- walk xs l
       (tree2, rem2) <- walk rem1 r
       Just (Branch tree1 p tree2, rem2)

export
treeFromTraversals : List Char -> List Char -> Maybe Tree
treeFromTraversals preorder inorder 
  = let x = length preorder == (length $ nub preorder)
        y = length inorder  == (length $ nub inorder)
     in do _ <- toMaybe (x && y) Void
           (tree, rem) <- walk preorder inorder
           toMaybe (isNil rem) tree