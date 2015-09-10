-- Spec.hs

import Pear.Operator
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Integer parser" $ do
    it "should parse '2 + (3 * 4)' properly" do
             
