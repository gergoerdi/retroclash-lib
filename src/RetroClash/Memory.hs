module RetroClash.Memory
    ( RAM(..), ROM(..), Mem(..)
    , packRam
    , MemSpec(..)
    , memory
    ) where

import Clash.Prelude
import RetroClash.Utils
import Control.Arrow (second)

type RAM dom a d = Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d
type ROM dom a d = Signal dom a ->                              Signal dom d

data Mem dom a d
    = RAM (Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d)
    | ROM (Signal dom a ->                              Signal dom d)

data MemSpec dom a d
    = UpTo a (Mem dom a d) (MemSpec dom a d)
    | Default (Mem dom a d)

packRam :: (BitPack d) => RAM dom a (BitVector (BitSize d)) -> RAM dom a d
packRam ram addr = fmap unpack . ram addr . fmap (second pack <$>)

memory
    :: (Ord a, Num a, HiddenClockResetEnable dom, Show a)
    => MemSpec dom a d
    -> Signal dom a
    -> Signal dom (Maybe d)
    -> Signal dom d
memory mems addr wr = go mems 0 (pure True)
  where
    go mems base from = case mems of
        UpTo to mem mems ->
            let this = addr .< to
                here = from .&&. this
            in mux (register False this) (connect here mem) (go mems to (not <$> here))
        Default mem ->
            connect from mem
      where
        connect en mem = case mem of
            ROM rom -> rom addr'
            RAM ram -> ram addr' (packWrite <$> addr' <*> wr')
          where
            addr' = mux en (addr - pure base) 0
            wr' = mux en wr (pure Nothing)
