module CoreLang.Utils where

import CoreLang.Types

coreLangBindBndr :: CoreBind -> CoreId
coreLangBindBndr (NonRec b _) = b
