module Core.FC

import Text.PrettyPrint.Prettyprinter

%default total

------------------------------------------------------------------------
-- Types

public export
record FilePos where
  constructor MkFilePos
  line : Int -- 0-based
  col : Int  -- 0-based

export
Show FilePos where
  showPrec d (MkFilePos l c) =
    showCon d "MkFilePos" (concat [showArg l, showArg c])

export
Eq FilePos where
  (MkFilePos l c) == (MkFilePos l' c') = (l, c) == (l', c')

export
Ord FilePos where
  compare (MkFilePos l c) (MkFilePos l' c') = compare (l, c) (l', c')

export
zeroFilePos : FilePos
zeroFilePos = (MkFilePos 0 0)

export
irrelevant : FilePos
irrelevant = MkFilePos (-1) (-1)

showPos : FilePos -> String
showPos (MkFilePos l c) = show (l + 1) ++ ":" ++ show (c + 1)

public export
FileName : Type
FileName = String

||| A file context is a filename together with starting and ending positions
public export
data FC = MkFC FileName FilePos FilePos
        | EmptyFC

------------------------------------------------------------------------
-- Projections

export
file : FC -> FileName
file (MkFC fn _ _) = fn
file EmptyFC = ""

export
startPos : FC -> FilePos
startPos (MkFC _ s _) = s
startPos EmptyFC = zeroFilePos

export
endPos : FC -> FilePos
endPos (MkFC _ _ e) = e
endPos EmptyFC = zeroFilePos

------------------------------------------------------------------------
-- Predicates

-- Return whether a given file position is within the file context (assuming we're
-- in the right file)
export
within : FilePos -> FC -> Bool
within fp (MkFC _ start end)
   = fp >= start && fp <= end
within _ _ = False

-- Return whether a given line is on the same line as the file context (assuming
-- we're in the right file)
export
onLine : Int -> FC -> Bool
onLine x (MkFC _ start end)
   = x >= start.line && x <= end.line
onLine _ _ = False

------------------------------------------------------------------------
-- Constant values

export
emptyFC : FC
emptyFC = EmptyFC

export
toplevelFC : FC
toplevelFC = MkFC "(toplevel)" zeroFilePos zeroFilePos

------------------------------------------------------------------------
-- Basic operations
export
mergeFC : FC -> FC -> Maybe FC
mergeFC (MkFC fname1 start1 end1) (MkFC fname2 start2 end2) =
  if fname1 == fname2
  then Just $ MkFC fname1 (min start1 start2) (max end1 end2)
  else Nothing
mergeFC _ _ = Nothing


%name FC fc

------------------------------------------------------------------------
-- Instances

export
Eq FC where
  (==) (MkFC n s e) (MkFC n' s' e') = n == n' && s == s' && e == e'
  (==) EmptyFC EmptyFC = True
  (==) _ _ = False

export
Show FC where
  show loc = file loc ++ ":" ++
             showPos (startPos loc) ++ "--" ++
             showPos (endPos loc)

export
Pretty FC where
  pretty loc = pretty (file loc) <+> colon
                 <+> prettyPos (startPos loc) <+> pretty "--"
                 <+> prettyPos (endPos loc)
    where
      prettyPos : FilePos -> Doc ann
      prettyPos (MkFilePos l c) = pretty (l + 1) <+> colon <+> pretty (c + 1)
