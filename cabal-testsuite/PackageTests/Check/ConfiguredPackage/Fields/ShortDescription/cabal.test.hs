import Test.Cabal.Prelude

-- Description should be longer than synopsis.
main = cabalTest $
  fails $ cabal "check" []
