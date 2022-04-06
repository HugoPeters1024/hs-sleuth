module CoreLangUtils exposing (..)

import Core.Generated.Types exposing (..)
import Set

isInfixOperator : String -> Bool
isInfixOperator inp = 
    let symbols = Set.fromList (String.toList "!$%&*+./<=>?@\\^-~#")
    in String.all (\c -> Set.member c symbols) inp

coreBindBndr : CoreBind -> CoreId
coreBindBndr (NonRec id _) = id

coreBindBndrName : CoreBind -> String
coreBindBndrName (NonRec id _) = id.name

coreBindBndrUnique : CoreBind -> Int
coreBindBndrUnique (NonRec id _) = id.unique

coreBindBndrUniqueTag : CoreBind -> String
coreBindBndrUniqueTag (NonRec id _) = id.uniquetag
