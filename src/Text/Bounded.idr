module Text.Bounded

import public Core.FC

%default total

public export
record WithBounds ty where
  constructor MkBounded
  val : ty
  isIrrelevant : Bool
  start : FilePos
  end : FilePos

export
boundToFC : FileName -> WithBounds t -> FC
boundToFC fname b = MkFC fname (start b) (end b)

export
Eq ty => Eq (WithBounds ty) where
  (MkBounded val ir s e) == (MkBounded val' ir' s' e') =
    val == val' && ir == ir' && s == s' && e == e'

export
Show ty => Show (WithBounds ty) where
  showPrec d (MkBounded val ir s e) =
    showCon d "MkBounded" (concat [showArg ir, showArg val, showArg s, showArg e])

export
Functor WithBounds where
  map f (MkBounded val ir s e) = MkBounded (f val) ir s e

export
Foldable WithBounds where
  foldr c n b = c b.val n

export
Traversable WithBounds where
  traverse f (MkBounded v a b c) = (\ v => MkBounded v a b c) <$> f v

export
irrelevantBounds : ty -> WithBounds ty
irrelevantBounds x = MkBounded x True irrelevant irrelevant

export
removeIrrelevance : WithBounds ty -> WithBounds ty
removeIrrelevance (MkBounded val ir s e) = MkBounded val True s e

export
mergeBounds : WithBounds ty -> WithBounds ty' -> WithBounds ty'
mergeBounds (MkBounded _ True _ _) (MkBounded val True _ _) = irrelevantBounds val
mergeBounds (MkBounded _ True _ _) b2 = b2
mergeBounds b1 (MkBounded val True _ _) = const val <$> b1
mergeBounds b1 b2 =
  let u = min (start b1) (start b2)
      l = max (end b1) (end b2) in
      MkBounded b2.val False u l

export
joinBounds : WithBounds (WithBounds ty) -> WithBounds ty
joinBounds b = mergeBounds b b.val
