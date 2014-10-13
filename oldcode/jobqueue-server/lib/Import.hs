module Import where

import           Data.Default
import           Yesod.Default.Util
import           Language.Haskell.TH


widgetFile :: FilePath -> ExpQ
widgetFile = widgetFileReload def
