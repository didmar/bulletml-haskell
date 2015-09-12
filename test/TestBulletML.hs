{-# LANGUAGE TemplateHaskell #-}
module TestBulletML (
  spec
) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Control.Arrow (arr, returnA)
import Control.Category ((<<<), (>>>))
import Data.Tree.NTree.TypeDefs (NTree)
import Debug.Trace (trace)
import Text.XML.HXT.Arrow.XmlArrow (ArrowXml)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (IOStateArrow)
import Text.XML.HXT.Core (constA, deep,
                          getAttrValue,
                          getChildren, getText,
                          hasName, isA, isElem,
                          listA, no, orElse,
                          readDocument, readString,
                          runLA, runX,
                          withRemoveWS,
                          withSubstDTDEntities,
                          withValidate, xread, yes)
import Text.XML.HXT.DOM.TypeDefs (XNode, XmlTree)

import BulletML                             -- (BulletML(BulletML), Orientation(ONone), Speed(Speed), SpeedOrAccelType(SOATRelative), ActionContent(ACActionRef), Reference(Reference), getBulletML, getSpeed, getActionOrRef)
import Test.Hspec

deg2rad :: Angle -> Double
deg2rad a = fromIntegral a / 180.0 * pi
angleToUnitVector :: Angle -> (Double, Double)
angleToUnitVector a = (cos a', sin a') where a' = deg2rad a

traceShow :: Show a => a -> a
traceShow x = trace (show x) x

--parsedWith :: String -> a -> b
s `parsedWith` f = runLA (xread >>> f) s

spec :: Spec
spec = do
   describe "getBulletML" $ do
     it "empty bulletml" $ do
        "<bulletml></bulletml>" `parsedWith` getBulletML `shouldBe` [BulletML ONone []]
   describe "getSpeed" $ do
     it "relative speed" $ do
        "<speed type='relative'>6</speed>" `parsedWith` getSpeed `shouldBe` [Speed SOATRelative 6]
   describe "getActionContent" $ do
     it "actionRef" $ do
        "<actionRef label='toto'><param>1</param><param>2</param></actionRef>" `parsedWith` getActionContent `shouldBe` [(ACActionRef (Reference "toto" [1.0, 2.0]))]

allClose a b = 1e-15 > abs (a-b)

--prop_angleToUnitVector_0   = let (x,y) = angleToUnitVector   0 in allClose x 1.0    && allClose y 0.0
--prop_angleToUnitVector_90  = let (x,y) = angleToUnitVector  90 in allClose x 0.0    && allClose y 1.0
--prop_angleToUnitVector_180 = let (x,y) = angleToUnitVector 180 in allClose x (-1.0) && allClose y  0.0
--prop_angleToUnitVector_270 = let (x,y) = angleToUnitVector 270 in allClose x 0.0    && allClose y (-1.0)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec
