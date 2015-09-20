import Control.Lens
import Data.Map as Map
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Debianize
import Debian.Relation (BinPkgName(..), Relation(Rel), SrcPkgName(..))

main :: IO ()
main = performDebianization $ do
         seereasonDefaults
         (debInfo . sourceFormat) .= Native3
         (debInfo . control . homepage) ~= Just "http://hackage.haskell.org/package/seereason-base"
         (debInfo . executable) %= Map.insert (BinPkgName "geni-client")
                                              (InstallFile {execName = "geni-client",
                                                            sourceDir = Nothing, destDir = Nothing,
                                                            destName = "geni-client"})
