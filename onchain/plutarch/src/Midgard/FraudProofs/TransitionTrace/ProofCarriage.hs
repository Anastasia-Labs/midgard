-- | Exact reference-byte transport used by transition-trace final yields.
module Midgard.FraudProofs.TransitionTrace.ProofCarriage (
  PCommitment (..),
  pchunks,
  popen,
  pread,
  pfields,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (POutputDatum (..), PTxInInfo (..), PTxOut (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

data PCommitment s = PCommitment {pcommitment'hash :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommitment)

pchunks :: forall s. Term s (PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PTxInInfo) :--> PByteString)
pchunks = phoistAcyclic $ pfix $ \self -> plam $ \indices references -> P.do
  index <- plet $ pfromData $ phead # indices
  remaining <- plet $ ptail # indices
  pif
    (index #>= 0)
    ( P.do
        PTxInInfo{ptxInInfo'resolved} <- pmatch $ pfromData $ pelemAt # index # references
        PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
        bytes <- plet $ pmatch ptxOut'datum $ \case
          POutputDatum dat -> pasByteStr # pto dat
          _ -> perror
        size <- plet $ plengthBS # bytes
        pif
          (size #> 0)
          ( pif
              (pnull # remaining)
              (pif (size #<= 4096) bytes perror)
              (pif (size #== 4096) (bytes <> (self # remaining # references)) perror)
          )
          perror
    )
    perror

-- The commitment freezes transport bytes; transport is not required to equal
-- serialiseData. Native preimages pass their own canonical doors later.
popen :: forall s. Term s (PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PTxInInfo) :--> PPair PCommitment PData)
popen = phoistAcyclic $ plam $ \indices references -> P.do
  bytes <- plet $ pchunks # indices # references
  pmatch (pdeserialise # bytes) $ \case
    PNothing -> perror
    PJust dat -> pcon $ PPair (pcon $ PCommitment $ pdata $ pblake2b_256 # bytes) dat

pread :: forall s. Term s (PCommitment :--> PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PTxInInfo) :--> PData)
pread = phoistAcyclic $ plam $ \commitment indices references -> P.do
  PPair actual dat <- pmatch $ popen # indices # references
  pif (actual #== commitment) dat perror

pfields :: forall s. Term s (PData :--> PPair PData PData)
pfields = phoistAcyclic $ plam $ \dat -> P.do
  fields <- plet $ psndBuiltin # (pasConstr # dat)
  pif
    (plength # fields #== 3)
    (pcon $ PPair (pelemAt # 1 # fields) (pelemAt # 2 # fields))
    perror
