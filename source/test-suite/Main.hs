import qualified Test.Hspec as Hspec

main :: IO ()
main =
  Hspec.hspec . Hspec.describe "Monadoc" $ Hspec.it "needs tests" Hspec.pending
