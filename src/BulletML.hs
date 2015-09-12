{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module BulletML (
    Label
  , Orientation(..)
  , Definition(..)
  , BulletML(..)
  , Bullet(..)
  , ActionOrRef(..)
  , Action(..)
  , ActionContent(..)
  , Fire(..)
  , BulletOrRef(..)
  , ChangeDir(..)
  , Termination
  , ChangeSpeed
  , Accel(..)
  , Wait(..)
  , Repeat(..)
  , Times
  , Direction(..)
  , DirType(..)
  , Angle
  , Speed(..)
  , SpeedOrAccelType(..)
  , SpeedValue
  , Horizontal(..)
  , AccelValue
  , Vertical(..)
  , Reference(..)
  , parseXMLFile
  -- for testing
  , getBulletML
  , getSpeed
  , getActionOrRef
  , getActionContent
) where

import Control.Arrow (arr, returnA)
import Control.Category ((<<<), (>>>))
import Text.XML.HXT.Arrow.XmlArrow (ArrowXml)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (IOStateArrow)
import Text.XML.HXT.Core (constA, deep, 
                          getAttrValue,
                          getChildren, getText,
                          hasName, isElem,
                          listA, no, yes, orElse,
                          readDocument,
                          withRemoveWS,
                          withSubstDTDEntities,
                          withValidate)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Expression (Expression, Param, parseExpr)

type Label = String
-- Defines the BulletML's body
data Orientation = ONone | OVertical | OHorizontal deriving (Show, Eq)
data Definition = DefBullet Bullet | DefAction Action | DefFire Fire deriving (Show, Eq)
data BulletML = BulletML Orientation [Definition] deriving (Show, Eq)
-- Defines attributes of a bullet
data Bullet = Bullet Label (Maybe Direction) (Maybe Speed) (Maybe ActionOrRef) deriving (Show, Eq)
data ActionOrRef = AORAction Action | AORRef Reference deriving (Show, Eq)
-- Defines the action of bullet
-- parameters may be provided if the action is refered to from an ActionRef
data Action = Action Label [ActionContent] [Param] deriving (Show, Eq)
data ActionContent = ACRepeat Repeat | ACFire Fire | ACFireRef Reference | ACChangeSpeed ChangeSpeed | ACChangeDir ChangeDir | ACAccel Accel | ACWait Wait | ACVanish | ACAction Action | ACActionRef Reference deriving (Show, Eq)
-- Fires a bullet
data Fire = Fire Label (Maybe Direction) (Maybe Speed) BulletOrRef deriving (Show, Eq)
data BulletOrRef = BORBullet Bullet | BORRef Reference deriving (Show, Eq)
-- Changes the direction of bullet
data ChangeDir = ChangeDir Direction Termination deriving (Show, Eq)
type Termination = Int -- number of frames
-- Changes the speed of bullet
data ChangeSpeed = ChangeSpeed Speed Termination deriving (Show, Eq)
-- Accelerates a bullet
data Accel = Accel (Maybe Horizontal) (Maybe Vertical) Termination deriving (Show, Eq)
-- Waits for a number of frames
data Wait = Wait Expression deriving (Show, Eq)
-- Repeats an action a number of times
data Repeat = Repeat Times ActionOrRef deriving (Show, Eq)
type Times = Int
-- Specifies a direction
-- "aim" type means that angle is relative to the direction to my ship (The direction to my ship is 0, clockwise).
-- "absolute" type means that angle is the absolute value (12 o'clock is 0, clockwise).
-- "relative" type means that angle is relative to the direction of this bullet (0 means that the direction of this fire and the direction of the bullet are the same).
-- "sequence" type means that angle is relative to the direction of the previous fire (0 means that the direction of this fire and the direction of the previous fire are the same).
-- TODO when using DTAim there is no angle, Angle should not be mandatory
data Direction = Direction DirType Angle deriving (Show, Eq)
data DirType = DTAim | DTAbsolute | DTRelative | DTSequence deriving (Show, Eq)
type Angle = Int -- degrees
-- Specifies a speed (default SpeedOrAccelType is SOATAbsolute)
data Speed = Speed SpeedOrAccelType SpeedValue deriving (Show, Eq)
data SpeedOrAccelType = SOATAbsolute | SOATRelative | SOATSequence deriving (Show, Eq)
type SpeedValue = Double -- unit ??
-- Specifies the acceleration in a horizontal line
data Horizontal = Horizontal SpeedOrAccelType AccelValue deriving (Show, Eq)
type AccelValue = Double -- unit ??
-- Specifies the acceleration in a horizontal line
data Vertical = Vertical SpeedOrAccelType AccelValue deriving (Show, Eq)
-- Reference to a bullet, an action or a fire
data Reference = Reference Label [Param] deriving (Show, Eq)

atTagDeep :: ArrowXml a => String -> a XmlTree XmlTree
atTagDeep tag = deep (isElem >>> hasName tag)

atTag :: ArrowXml a => String -> a XmlTree XmlTree
--atTag tag = getChildren >>> isElem >>> hasName tag
atTag = atTagDeep

maybeArr :: ArrowXml a => a b c -> a b (Maybe c)
maybeArr f = (f >>> arr Just) `orElse` constA Nothing

readSpeed :: String -> String -> Speed
readSpeed soat speed = Speed (readSOAT soat) (read speed)

readSOAT :: String -> SpeedOrAccelType
readSOAT soat = case soat of
    "absolute" -> SOATAbsolute
    "relative" -> SOATRelative
    "sequence" -> SOATSequence
    ""         -> SOATAbsolute
    _          -> error "error while parsing speed"

getSpeed :: ArrowXml cat => cat XmlTree Speed
getSpeed = atTag "speed" >>>
  proc x -> do
    soat <- getAttrValue "type" -< x
    speed <- getText <<< getChildren -< x
    returnA -< readSpeed soat speed

readOrientation :: String -> Orientation
readOrientation "" = ONone
readOrientation "none" = ONone
readOrientation "vertical" = OVertical
readOrientation "horizontal" = OHorizontal
readOrientation _ = error "Invalid orientation"

readDirType :: String -> DirType
readDirType ""         = DTAim
readDirType "aim"      = DTAim
readDirType "absolute" = DTAbsolute
readDirType "relative" = DTRelative
readDirType "sequence" = DTSequence
readDirType _ = error "Invalid direction type"

getDirection :: ArrowXml a => a XmlTree Direction
getDirection = atTag "direction" >>> proc x -> do
  dirType <- getAttrValue "type" -< x
  angle <- getText <<< getChildren -< x
  returnA -< Direction (readDirType dirType) (read angle)

getLabel :: ArrowXml a => a XmlTree String
getLabel = getAttrValue "label"

getBullet :: ArrowXml a => a XmlTree Bullet
getBullet = atTag "bullet" >>> proc x -> do
  label <- getLabel                -< x
  dir   <- maybeArr getDirection   -< x
  speed <- maybeArr getSpeed       -< x
  aor   <- maybeArr getActionOrRef -< x
  returnA -< Bullet label dir speed aor

getBulletOrRef :: ArrowXml a => a XmlTree BulletOrRef
getBulletOrRef = (getBullet >>> arr BORBullet) `orElse` (getReference >>> arr BORRef)

getFire :: ArrowXml a => a XmlTree Fire
getFire = atTag "fire" >>> proc x -> do
  label <- getLabel              -< x
  dir   <- maybeArr getDirection -< x
  speed <- maybeArr getSpeed     -< x
  bor   <- getBulletOrRef        -< x
  returnA -< Fire label dir speed bor

getTimes :: ArrowXml a => a XmlTree Times
getTimes = atTag "times" >>> proc x -> do
  times <- getText <<< getChildren -< x
  returnA -< read times

getActionOrRef :: ArrowXml a => a XmlTree ActionOrRef
getActionOrRef = (getAction >>> arr AORAction) `orElse` (getActionReference >>> arr AORRef)

getRepeat :: ArrowXml a => a XmlTree Repeat
getRepeat = atTag "repeat" >>> proc x -> do
  times <- getTimes -< x
  aor   <- getActionOrRef -< x
  returnA -< Repeat times aor

getChangeSpeed :: ArrowXml a => a XmlTree ChangeSpeed
getChangeSpeed = atTag "changeSpeed" >>> proc x -> do
  speed <- getSpeed -< x
  term <- getTermination -< x
  returnA -< ChangeSpeed speed term

getChangeDir :: ArrowXml a => a XmlTree ChangeDir
getChangeDir = atTag "changeDirection" >>> proc x -> do
  dir <- getDirection -< x
  term <- getTermination -< x
  returnA -< ChangeDir dir term

getHorizontal :: ArrowXml a => a XmlTree Horizontal
getHorizontal = atTag "horizontal" >>> proc x -> do
  soat <- arr readSOAT <<< getAttrValue "type" -< x
  accel <- arr read <<< getText -< x
  returnA -< Horizontal soat accel

getVertical :: ArrowXml a => a XmlTree Vertical
getVertical = atTag "vertical" >>> proc x -> do
  soat <- arr readSOAT <<< getAttrValue "type" -< x
  accel <- arr read <<< getText -< x
  returnA -< Vertical soat accel

getTermination :: ArrowXml a => a XmlTree Termination
getTermination = atTag "term" >>> getText >>> arr read

getAccel :: ArrowXml a => a XmlTree Accel
getAccel = atTag "accel" >>> proc x -> do
  dir   <- maybeArr getHorizontal -< x
  speed <- maybeArr getVertical   -< x
  term  <- getTermination -< x
  returnA -< Accel dir speed term

getParam :: ArrowXml a => a XmlTree Param
getParam = atTag "param" >>> getChildren >>> getText >>> arr read

getFireReference :: ArrowXml a => a XmlTree Reference
getFireReference = atTag "fireRef" >>> getReference

getActionReference :: ArrowXml a => a XmlTree Reference
getActionReference = atTag "actionRef" >>> getReference

getReference :: ArrowXml a => a XmlTree Reference
getReference = proc x -> do
   label <- getLabel -< x
   params <- listA getParam -< x
   returnA -< Reference label params

getWait :: ArrowXml a => a XmlTree Wait
getWait = atTag "wait" >>> proc x -> do
  expression <- getText <<< getChildren -< x
  returnA -< Wait (parseExpr expression)

getActionContent :: ArrowXml a => a XmlTree ActionContent
getActionContent = (getRepeat      >>> arr ACRepeat)
          `orElse` (getFire        >>> arr ACFire)
          `orElse` (getFireReference   >>> arr ACFireRef)
          `orElse` (getChangeSpeed >>> arr ACChangeSpeed)
          `orElse` (getChangeDir   >>> arr ACChangeDir)
          `orElse` (getAccel       >>> arr ACAccel)
          `orElse` (getWait        >>> arr ACWait)
          `orElse` (getAction      >>> arr ACAction)
          `orElse` (getActionReference   >>> arr ACActionRef)
          `orElse` (isElem >>> hasName "vanish" >>> constA ACVanish)

getAction :: ArrowXml a => a XmlTree Action
getAction = atTag "action" >>> proc x -> do
  label <- getLabel -< x
  contents <- listA getActionContent <<< getChildren -< x
  returnA -< Action label contents []

getBulletML :: ArrowXml a => a XmlTree BulletML
getBulletML = atTagDeep "bulletml" >>> proc x -> do
  orientation <- getAttrValue "type" -< x
  actions <- listA getAction -< x
  bullets <- listA getBullet -< x
  fires   <- listA getFire   -< x
  returnA -< BulletML (readOrientation orientation) (map DefAction actions ++ map DefBullet bullets ++ map DefFire fires)

parseXMLFile :: String -> IOStateArrow s b XmlTree
parseXMLFile = readDocument [ withValidate no
                            , withRemoveWS yes  -- throw away formating WS
                            , withSubstDTDEntities no
                            ]
