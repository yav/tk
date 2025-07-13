-- | Time and timer groups
module GUI.Timer (
  Timers,
  noTimers,
  timer,
  updateTimers,
  cancelTimers,
  pauseTimers,
  resumeTimers,

  Time,
  milliSeconds,
  seconds,

) where

import Data.IntMap(IntMap)
import Data.IntMap qualified as IntMap
import SFML.System.Time qualified as SFML


-- | Some amount of time.
newtype Time = Time Int
  deriving (Eq,Ord,Num,Show,Read)

-- | Amount of time in milliseconds.
milliSeconds :: Int -> Time
milliSeconds = Time

-- | Amount of time in seconds.
seconds :: Float -> Time
seconds = Time . SFML.asMilliseconds . SFML.seconds

-- | A collection of timers.
data Timers a = Timers {
  timers    :: !(TimerQ a),
  paused    :: !Time
}

type TimerQ a = IntMap [TimerD a]

data TimerD a = TimerD {
  deadline  :: !Time,
  period    :: Maybe Time,
  timerData :: a
}


-- | An empty collection of timers.
noTimers :: Timers a
noTimers = Timers { timers = emptyQ, paused = 0 }

-- | Add a new timer to the collection
timer ::
  Time      {- ^ The current time -} -> 
  Bool      {- ^ Is this timer periodic? -} ->
  Time      {- ^ Amount of time before the timer experies. -} ->
  a         {- ^ Data associated with the timer -} ->
  Timers a  {- ^ The timer collection to update -} ->
  Timers a
timer now per len dat ts = ts { timers = enQ tim (timers ts) }
  where
  tim = TimerD {
    deadline  = now + len,
    period    = if per then Just len else Nothing,
    timerData = dat
  }

-- | Update a set of timers.
updateTimers ::
  Time {- ^ Current time -} ->
  Timers a {- ^ Timer collection to update -} ->
  ([a], Timers a) {-^ Timers that expired, and updated collection -}
updateTimers now ts
  | paused ts > 0 = ([], ts)
  | otherwise =
    case getExpired now (timers ts) of
      ([], _) -> ([], ts)
      (ready,q1) ->
        let q = foldr addPeriodic q1 ready
        in (map timerData ready, ts { timers = q })
    where
    addPeriodic t q =
      case period t of
        Nothing -> q
        Just p ->
          let over = now - deadline t -- assumes we don't overshoot by more than the period
          in enQ t { deadline = now + (p - over) } q

-- | Cancel timers that satisfy a predicate.
cancelTimers :: (a -> Bool) -> Timers a -> Timers a
cancelTimers n t = t { timers = removeTimer n (timers t) }

-- | Pause all timers in the colleciton.
pauseTimers :: Time {- ^ Current time -} -> Timers a -> Timers a
pauseTimers now ts = ts { paused = now }

-- | Resume the timers in the collection.
resumeTimers :: Time {- ^ Current time -} -> Timers a -> Timers a
resumeTimers now ts
  | p > 0 = ts { paused = 0, timers = adjustDeadlines (now - p) (timers ts) }
  | otherwise = ts
  where p = paused ts

-------------------------------------------------------------------------------


emptyQ :: TimerQ a
emptyQ = mempty

removeTimer :: (a -> Bool) -> TimerQ a -> TimerQ a
removeTimer p = fmap (filter (p . timerData))

getExpired :: Time -> TimerQ a -> ([TimerD a], TimerQ a)
getExpired (Time now) q =
  case IntMap.spanAntitone ready q of
    (as,bs) -> (concat (IntMap.elems as), bs)
  where
  ready a = a <= now

enQ :: TimerD a -> TimerQ a -> TimerQ a
enQ t = IntMap.insertWith (++) ti [t]
  where Time ti = deadline t

adjustDeadlines :: Time -> TimerQ a -> TimerQ a
adjustDeadlines (Time d) = IntMap.mapKeysMonotonic (d +)


-------------------------------------------------------------------------------

