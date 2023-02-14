{-# LANGUAGE UnboxedTuples #-}

module Bytezap.Prim where

type Poke# = Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
