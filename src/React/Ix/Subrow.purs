module React.Ix.Subrow where

class Subrow (r :: # Type) (s :: # Type)
instance srInst :: Union r t s => Subrow r s
