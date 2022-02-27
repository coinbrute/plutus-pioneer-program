module Main
    ( main
    ) where
-- can be run with cabal test
import qualified Spec.Model
import qualified Spec.ModelWithClose
import qualified Spec.Trace
import qualified Spec.TraceWithClose
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.TraceWithClose.tests
    , Spec.Model.tests
    , Spec.ModelWithClose.tests
    ]
