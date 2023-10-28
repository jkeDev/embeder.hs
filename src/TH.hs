module TH where

import Control.Lens.Internal.FieldTH (LensRules (_fieldToDef))
import Control.Lens.TH
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (Name, mkName, nameBase)

makeExternalLens :: Name -> DecsQ
makeExternalLens =
  makeLensesWith $
    lensRules
      { _fieldToDef = \_ _ n ->
          case nameBase n of
            x : xs -> [TopName . mkName $ x : xs]
            _ -> []
      }
