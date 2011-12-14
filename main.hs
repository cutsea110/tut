import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withTut)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withTut