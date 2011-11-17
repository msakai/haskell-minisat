-----------------------------------------------------------------------------
-- |
-- Module      :  MiniSat2
-- Copyright   :  (c) Masahiro Sakai 2008
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A wrapper for MiniSat2
--
-----------------------------------------------------------------------------

module MiniSat2
    (
    -- * The @Solver@ type
      Solver
    , newSolver

    -- * Problem specification
    , Var
    , Lit (..)
    , Clause
    , newVar
    , newVar_
    , addClause

    -- * Solving
    , simplify
    , solve
    , solveUnderAssumption
    , okay

    -- * Variable mode
    , setPolarity
    , setDecisionVar

    -- * Read state
    , LBool
    , Valued (..)
    , nAssigns
    , nClauses
    , nLearnts
    , nVars
    --, vars

    -- * Extra results
    --, model

    -- * Mode of operation

    -- * Statistics
    ) where

import Foreign hiding (new)
import Foreign.C
import Foreign.Marshal.Utils hiding (new)
import Data.Array
import Data.Array.IO
import Control.Monad (liftM, (=<<))
import Control.Exception (bracket)

{--------------------------------------------------------------------
  The @Solver@ type
--------------------------------------------------------------------}

newtype Solver = Solver (ForeignPtr CSolver)

-- |Construct a new SAT solver.
newSolver :: IO Solver
newSolver =
    liftM Solver . newForeignPtr hsminisat_deleteSolver =<< hsminisat_newSolver

{--------------------------------------------------------------------
  Problem specification:
--------------------------------------------------------------------}

-- |Variable
newtype Var = Var CVar deriving (Show, Eq, Ord, Enum)

instance Ix Var where
    range (Var a, Var b) =
        map (Var . fromIntegral) (range (fromIntegral a  :: Int, fromIntegral b))
    inRange (Var a, Var b) (Var x) =
        inRange (fromIntegral a, fromIntegral b) (fromIntegral x :: Int)

-- |Literal
data Lit
    -- |Positive literal
    = Pos {-# UNPACK #-} !Var
    -- |Negative literal
    | Neg {-# UNPACK #-} !Var
    deriving (Show, Eq, Ord)

-- |Disjunction of @Lit@.
type Clause = [Lit]

-- |Add a new variable with default parameters
newVar :: Solver -> IO Var
newVar s =
    withSolver s $ \p -> liftM Var (hsminisat_newVar p 1 1)

-- |Add a new variable with parameters specifying variable mode
newVar_ :: Solver
        -> Bool -- ^ polarity
        -> Bool -- ^ dvar
        -> IO Var
newVar_ s polarity dvar =
    withSolver s $ \p -> liftM Var (hsminisat_newVar p (fromBool polarity) (fromBool dvar))
    

-- |Add a clause to the solver.
addClause :: Solver -> Clause -> IO Bool
addClause s ps =
    withSolver s $ \p -> 
    bracket hsminisat_newVecLit hsminisat_deleteVecLit $ \vec -> do
        pushLits vec ps
        liftM toBool $ hsminisat_addClause p vec

{--------------------------------------------------------------------
  Solving:
--------------------------------------------------------------------}

-- |Removes already satisfied clauses.
simplify :: Solver -> IO Bool
simplify s = liftM toBool $ withSolver s hsminisat_simplify

-- |Search without assumptions.
solve :: Solver -> IO Bool
solve s = liftM toBool $ withSolver s (\p -> hsminisat_solve p nullPtr)

-- |Search for a model that respects a given set of assumptions.
solveUnderAssumption :: Solver -> [Lit] -> IO Bool
solveUnderAssumption s ps =
    withSolver s $ \p ->
    bracket hsminisat_newVecLit hsminisat_deleteVecLit $ \vec -> do
        pushLits vec ps
        liftM toBool $ hsminisat_solve p vec

-- |False means solver is in a conflicting state
okay :: Solver -> IO Bool
okay s = liftM toBool $ withSolver s hsminisat_okay

{--------------------------------------------------------------------
  Variable mode:
--------------------------------------------------------------------}

-- |Declare which polarity the decision heuristic should use for a variable.
-- Requires mode 'polarity_user'.
setPolarity :: Solver -> Var -> Bool -> IO ()
setPolarity s (Var v) b =
    withSolver s $ \p -> hsminisat_setPolarity p v (fromBool b)

-- |Declare if a variable should be eligible for selection in the decision heuristic.
setDecisionVar :: Solver -> Var -> Bool -> IO ()
setDecisionVar s (Var v) b =
    withSolver s $ \p -> hsminisat_setDecisionVar p v (fromBool b)

{--------------------------------------------------------------------
  Read state:
--------------------------------------------------------------------}

-- |Lifted @Bool@. @Nothing@ means unspecified.
type LBool = Maybe Bool

class Valued a where
    -- |The current value.
    value :: Solver -> a -> IO LBool
    -- |The value in the last model
    modelValue :: Solver -> a -> IO LBool

instance Valued Var where
    value s (Var cv) =
        withSolver s $ \p -> liftM decodeLBool $ hsminisat_value p cv
    modelValue s (Var v) =
        withSolver s $ \p -> liftM decodeLBool $ hsminisat_modelValue p v

instance Valued Lit where
    value s (Pos v) = value s v
    value s (Neg v) = liftM (fmap not) (value s v)
    modelValue s (Pos v) = modelValue s v
    modelValue s (Neg v) = liftM (fmap not) (modelValue s v)

-- |The current number of assigned literals.
nAssigns :: Solver -> IO Int
nAssigns s = liftM fromIntegral $ withSolver s hsminisat_nAssigns

-- |The current number of original clauses.
nClauses :: Solver -> IO Int
nClauses s = liftM fromIntegral $ withSolver s hsminisat_nClauses

-- |The current number of learnt clauses.
nLearnts :: Solver -> IO Int
nLearnts s = liftM fromIntegral $ withSolver s hsminisat_nLearnts

-- |The current number of variables.
nVars :: Solver -> IO Int
nVars s = liftM fromIntegral $ withSolver s hsminisat_nVars

vars :: Solver -> IO [Var]
vars s = do
  n <- nVars s
  return [Var i | i <- [0..(fromIntegral n - 1)]]

{--------------------------------------------------------------------
  Extra results:
--------------------------------------------------------------------}

-- If problem is satisfiable, this vector contains the model (if any).
model :: Solver -> IO (Array Var LBool)
model s =
  withSolver s $ \p -> do
    n <- hsminisat_nVars p
    a <- newArray_ (Var 0, Var (n - 1)) :: IO (IOArray Var LBool)
    flip mapM_ [0..(n-1)] $ \i -> do
      b <- hsminisat_modelValue p i
      writeArray a (Var i) (decodeLBool b)
    unsafeFreeze a

{--------------------------------------------------------------------
  Mode of operation:
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Statistics: (read-only member variable):
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Implementation details:
--------------------------------------------------------------------}

type CSolver = ()
type CVar = CInt
type CVecLit = ()

foreign import ccall hsminisat_newVecLit
    :: IO (Ptr CVecLit)
foreign import ccall hsminisat_deleteVecLit
    :: Ptr CVecLit -> IO ()
foreign import ccall hsminisat_vecLit_pushVar
    :: Ptr CVecLit -> CVar -> CInt -> IO ()

foreign import ccall unsafe hsminisat_newSolver
    :: IO (Ptr CSolver)
foreign import ccall unsafe "&" hsminisat_deleteSolver
    :: FunPtr (Ptr CSolver -> IO ())
foreign import ccall unsafe hsminisat_newVar
    :: Ptr CSolver -> CInt -> CInt -> IO CVar

foreign import ccall safe hsminisat_addClause 
    :: Ptr CSolver -> Ptr CVecLit -> IO CInt
foreign import ccall safe hsminisat_simplify
    :: Ptr CSolver -> IO CInt
foreign import ccall safe hsminisat_solve 
    :: Ptr CSolver -> Ptr CVecLit -> IO CInt
foreign import ccall unsafe hsminisat_okay
    :: Ptr CSolver -> IO CInt

foreign import ccall unsafe hsminisat_setPolarity
    :: Ptr CSolver -> CVar -> CInt -> IO ()
foreign import ccall unsafe hsminisat_setDecisionVar
    :: Ptr CSolver -> CVar -> CInt -> IO ()

foreign import ccall unsafe hsminisat_value
    :: Ptr CSolver -> CVar -> IO CInt
foreign import ccall unsafe hsminisat_modelValue
    :: Ptr CSolver -> CVar -> IO CInt

foreign import ccall unsafe hsminisat_nAssigns
    :: Ptr CSolver -> IO CInt
foreign import ccall unsafe hsminisat_nClauses
    :: Ptr CSolver -> IO CInt
foreign import ccall unsafe hsminisat_nLearnts
    :: Ptr CSolver -> IO CInt
foreign import ccall unsafe hsminisat_nVars
    :: Ptr CSolver -> IO CInt

withSolver :: Solver -> (Ptr CSolver -> IO a) -> IO a
withSolver (Solver fp) = withForeignPtr fp

decodeLBool :: CInt -> LBool
decodeLBool 0    = Nothing
decodeLBool 1    = lTrue
decodeLBool (-1) = lFalse

lTrue, lFalse :: LBool
lTrue  = Just True
lFalse = Just False

pushLits :: Ptr CVecLit -> [Lit] -> IO ()
pushLits p ls = mapM_ f ls
  where
    f (Pos (Var v)) = hsminisat_vecLit_pushVar p v 0
    f (Neg (Var v)) = hsminisat_vecLit_pushVar p v 1
