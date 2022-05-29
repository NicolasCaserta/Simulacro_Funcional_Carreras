module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
    correrDeterminadoTiempo 10 camaro `shouldBe` UnAuto {color="Blnaco",velocidad=10,distanciaRecorrida=110}