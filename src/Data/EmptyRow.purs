module Data.EmptyRow
  ( class EmptyRow
  , emptyRow
  ) where

import Type.Row (RProxy(RProxy))

class EmptyRow (f :: # Type -> Type) where
  emptyRow :: f ()

instance emptyRowRecord :: EmptyRow Record where
  emptyRow = {}

instance emptyRowRProxy :: EmptyRow RProxy where
  emptyRow = RProxy
