{-# LANGUAGE CPP #-}
-- | Compatibility for old GHC
module Data.Vector.Fixed.Compat
  (
#if MIN_VERSION_base(4,17,0)
  type(~)
#endif
  ) where
