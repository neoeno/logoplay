module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Canvas (CANVAS, Context2D, beginPath, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setStrokeStyle, stroke)
import Math (sin, cos, pi)
import Partial.Unsafe (unsafePartial)

data CanvasCommand
  = BeginPath
  | MoveTo Number Number
  | LineTo Number Number
  | ClosePath
  | Stroke

data TurtleCommand
  = Forward Number
  | Rotate Number

data Position = Position Number Number Number

draw :: forall eff. CanvasCommand -> Context2D -> Eff ( canvas :: CANVAS | eff ) Context2D
draw (BeginPath) ctx = beginPath ctx
draw (MoveTo x y) ctx = moveTo ctx x y
draw (LineTo x y) ctx = lineTo ctx x y
draw (ClosePath) ctx = closePath ctx
draw (Stroke) ctx = stroke ctx

turtleCommandToCanvasCommand :: Position -> TurtleCommand -> Array CanvasCommand
turtleCommandToCanvasCommand (Position x y r) (Forward n) =
  let (Position nx ny nr) = turtleTransformPosition (Position x y r) (Forward n)
  in [ BeginPath, MoveTo x y, LineTo nx ny, ClosePath, Stroke ]
turtleCommandToCanvasCommand (Position x y r) (Rotate n) = [ ]

turtleTransformPosition :: Position -> TurtleCommand -> Position
turtleTransformPosition (Position x y r) (Forward n) = Position
  (x + (n * (sin r)))
  (y + (n * (cos r)))
  r
turtleTransformPosition (Position x y r) (Rotate n) = Position x y (r + n)

turtleFold :: Position -> Array TurtleCommand -> Array CanvasCommand
turtleFold pos turtleCommands = case uncons turtleCommands of
  Just { head: turtleCommand, tail: remainingTurtleCommands } ->
    (turtleCommandToCanvasCommand pos turtleCommand) <> turtleFold (turtleTransformPosition pos turtleCommand) remainingTurtleCommands
  Nothing -> []

runInstructions :: forall eff. Eff ( canvas :: CANVAS | eff ) Context2D -> Array CanvasCommand -> Eff ( canvas :: CANVAS | eff ) Context2D
runInstructions ctx commands = case uncons commands of
  Just { head: command, tail: restCommands } -> do
    rCtx <- ctx
    let newCtx = draw command rCtx
    runInstructions newCtx restCommands
  Nothing -> ctx

toRadians :: Number -> Number
toRadians degrees = degrees * (pi / 180.0)

main :: forall e. Eff (canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  ctx2 <- setStrokeStyle "#000000" ctx
  runInstructions (pure ctx2) (turtleFold (Position 100.0 100.0 0.0)
    [ Forward 180.0, Rotate (toRadians 72.0),
      Forward 180.0, Rotate (toRadians 72.0),
      Forward 180.0, Rotate (toRadians 72.0),
      Forward 180.0, Rotate (toRadians 72.0),
      Forward 180.0 ])
