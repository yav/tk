module GUI.Timer (
  Timers,
  empty,
  timer,
  update,
  cancel,
  pause,
  resume,

  Time,
  microSeconds,
  milliSeconds,
  seconds,

  Timer
) where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Int
import SFML.System.Time qualified as SFML

newtype Time = Time SFML.Time

microSeconds :: Int64 -> Time
microSeconds = Time

milliSeconds :: Int -> Time
milliSeconds = Time . SFML.milliseconds

seconds :: Float -> Time
seconds = Time . SFML.seconds


data Timers = Timers {
  nextTimer :: !Int,
  timers    :: !TimerQ,
  paused    :: !SFML.Time
}



empty :: Timers
empty = Timers { nextTimer = 0, timers = emptyQ, paused = 0 }

timer :: SFML.Time -> Bool -> Time -> Timers -> (Timer, Timers)
timer now per (Time len) ts =
  let nam = Timer (nextTimer ts)
      tim = TimerD {
              name = nam,
              deadline = now + len,
              period = if per then Just len else Nothing
            }
      nex = nextTimer ts + 1
      !ts1 = ts { nextTimer = nex, timers = enQ tim (timers ts) }
  in (nam, ts1)

update :: SFML.Time -> Timers -> ([Timer], Timers)
update now ts
  | paused ts > 0 = ([], ts)
  | otherwise =
    case getExpired now (timers ts) of
      ([], _) -> ([], ts)
      (ready,q1) ->
        let q = foldr addPeriodic q1 ready
        in (map name ready, ts { timers = q })
    where
    addPeriodic t q =
      case period t of
        Nothing -> q
        Just p ->
          let over = now - deadline t -- assumes we don't overshoot by more than the period
          in enQ t { deadline = now + (p - over) } q

cancel :: Timer -> Timers -> Timers
cancel n t = t { timers = removeTimer n (timers t) }

pause :: SFML.Time -> Timers -> Timers
pause now ts = ts { paused = now }

resume :: SFML.Time -> Timers -> Timers
resume now ts
  | p > 0 = ts { paused = 0, timers = adjustDeadlines (now - p) (timers ts) }
  | otherwise = ts
  where p = paused ts

-------------------------------------------------------------------------------

type TimerQ = Map SFML.Time [TimerD]

emptyQ :: TimerQ
emptyQ = mempty

removeTimer :: Timer -> TimerQ -> TimerQ
removeTimer n = fmap (filter ((n /=) . name))

getExpired :: SFML.Time -> TimerQ -> ([TimerD], TimerQ)
getExpired now q =
  case Map.spanAntitone ready q of
    (as,bs) -> (concat (Map.elems as), bs)
  where
  ready a = a <= now

enQ :: TimerD -> TimerQ -> TimerQ
enQ t = Map.insertWith (++) (deadline t) [t]

adjustDeadlines :: SFML.Time -> TimerQ -> TimerQ
adjustDeadlines d = Map.mapKeysMonotonic (d +)


-------------------------------------------------------------------------------

newtype Timer = Timer Int
  deriving (Eq,Ord,Show)

data TimerD = TimerD {
  deadline  :: !SFML.Time,
  name      :: !Timer,
  period    :: Maybe SFML.Time
}
