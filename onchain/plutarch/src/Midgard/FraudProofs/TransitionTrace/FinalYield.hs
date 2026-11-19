module Midgard.FraudProofs.TransitionTrace.FinalYield (
  PState (..),
  PArgs (..),
  POpenedOutputs (..),
  POutputSummaries (..),
  pinitial,
  pdispatch,
  pdispatched,
  padvance,
  poneStepWitness,
  poutputSkeleton,
  poutputBytes,
  poutputMetadataSkeleton,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekData (PDataSummaryV1)
import Midgard.Common.Utils (pheadSingleton)
import Midgard.ComputationThread (PStepDatum (..), PStepRedeemer (..))
import Midgard.FraudProofs.TransitionTrace.ProofCarriage qualified as Carriage
import Midgard.LedgerOutputScan qualified as Scan
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

data POpenedOutputs s = POpenedOutputs
  { popened'spendInputKeys :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , popened'outputHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POpenedOutputs)

-- Each element is Aiken's list-encoded three-tuple of DataSummaryV1.
data POutputSummaries s = POutputSummaries
  {psummaries'summaries :: Term s (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData PDataSummaryV1)))))}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputSummaries)

data PState s = PState
  { pstate'kind :: Term s (PAsData PInteger)
  , pstate'phase :: Term s (PAsData PInteger)
  , pstate'proofCommitment :: Term s (PAsData Carriage.PCommitment)
  , pstate'opened :: Term s (PAsData POpenedOutputs)
  , pstate'inputIndex :: Term s (PAsData PInteger)
  , pstate'outputIndex :: Term s (PAsData PInteger)
  , pstate'currentRoot :: Term s (PAsData PByteString)
  , pstate'summaries :: Term s (PAsData POutputSummaries)
  , pstate'scanCbor :: Term s (PAsData PByteString)
  , pstate'valueCbor :: Term s (PAsData PByteString)
  , pstate'valueStart :: Term s (PAsData PInteger)
  , pstate'valueEnd :: Term s (PAsData PInteger)
  , pstate'valueSummary :: Term s (PMaybeData PDataSummaryV1)
  , pstate'descriptorCbor :: Term s (PAsData PByteString)
  , pstate'depositIndex :: Term s (PAsData PInteger)
  , pstate'depositSourceCbor :: Term s (PAsData PByteString)
  , pstate'depositAssetCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PState)

data PArgs s = PArgs
  { pargs'inputIndex :: Term s (PAsData PInteger)
  , pargs'outputIndex :: Term s (PAsData PInteger)
  , pargs'hubRefInputIndex :: Term s (PAsData PInteger)
  , pargs'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pargs'yieldRefInputIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
  , pargs'proofRefIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
  , pargs'outputRefIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
  , pargs'depositEventRefIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PArgs)

pinitial :: forall s. Term s (PInteger :--> Carriage.PCommitment :--> PState)
pinitial = phoistAcyclic $ plam $ \kind commitment ->
  pcon $
    PState
      (pdata kind)
      (pdata $ pif (kind #== 2) 0 6)
      (pdata commitment)
      (pdata $ pcon $ POpenedOutputs (pdata pnil) (pdata pnil))
      (pdata 0)
      (pdata 0)
      (pdata (pconstant ""))
      (pdata $ pcon $ POutputSummaries $ pdata pnil)
      (pdata (pconstant ""))
      (pdata (pconstant ""))
      (pdata 0)
      (pdata 0)
      (pcon PDNothing)
      (pdata (pconstant ""))
      (pdata 0)
      (pdata (pconstant ""))
      (pdata 0)

pdispatch :: forall s. Term s (PScriptHash :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PPair PStepDatum PArgs)
pdispatch = phoistAcyclic $ plam $ \dispatcher inputs redeemers -> P.do
  PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <-
    pmatch $
      pfromData $
        pheadSingleton
          # ( pfilter
                # plam
                  ( \input -> P.do
                      PTxInInfo{ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
                      PTxOut{ptxOut'address} <- pmatch resolved
                      PAddress credential _ <- pmatch ptxOut'address
                      credential #== pcon (PScriptCredential $ pdata dispatcher)
                  )
                # inputs
            )
  PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
  datum <- plet $ pmatch ptxOut'datum $ \case
    POutputDatum dat -> punsafeCoerce @PStepDatum $ pto dat
    _ -> perror
  PBuiltinPair _ raw <-
    pmatch $
      pheadSingleton
        # ( pfilter
              # plam
                (\pair -> pfromData (pfstBuiltin # pair) #== pcon (PSpending ptxInInfo'outRef))
              # redeemers
          )
  pmatch (punsafeCoerce @PStepRedeemer $ pto $ pfromData raw) $ \case
    PContinue args -> pcon $ PPair datum (pfromData $ punsafeCoerce @(PAsData PArgs) args)
    _ -> perror

pdispatched :: forall s. Term s (PScriptHash :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PBuiltinList (PAsData PTxInInfo) :--> PPair (PPair PData PData) PArgs)
pdispatched = phoistAcyclic $ plam $ \dispatcher inputs redeemers references -> P.do
  PPair datum args <- pmatch $ pdispatch # dispatcher # inputs # redeemers
  PStepDatum _ maybeState <- pmatch datum
  state <- plet $ pmatch maybeState $ \case
    PDJust dat -> pfromData $ punsafeCoerce @(PAsData PState) $ pfromData dat
    PDNothing -> perror
  PState{pstate'proofCommitment} <- pmatch state
  PArgs{pargs'proofRefIndices} <- pmatch args
  fields <- plet $ Carriage.pfields # (Carriage.pread # pfromData pstate'proofCommitment # pfromData pargs'proofRefIndices # references)
  pcon $ PPair fields args

padvance :: forall s. Term s (PStepDatum :--> PArgs :--> PBuiltinList (PAsData PTxOut) :--> PState :--> PBool)
padvance = phoistAcyclic $ plam $ \datum args outputs next -> P.do
  PStepDatum prover _ <- pmatch datum
  PArgs{pargs'outputIndex} <- pmatch args
  index <- plet $ pfromData pargs'outputIndex
  pif
    (index #>= 0)
    ( P.do
        PTxOut{ptxOut'datum} <- pmatch $ pfromData $ pelemAt # index # outputs
        pmatch ptxOut'datum $ \case
          POutputDatum actual -> pto actual #== pforgetData (pdata $ pcon $ PStepDatum prover $ pcon $ PDJust $ pdata $ pforgetData $ pdata next)
          _ -> perror
    )
    perror

poneStepWitness :: forall s. Term s (PData :--> PInteger :--> PBuiltinList PData)
poneStepWitness = phoistAcyclic $ plam $ \fault expectedTag -> P.do
  PBuiltinPair tag fields <- pmatch $ pasConstr # fault
  pif
    (tag #== 4)
    ( P.do
        PBuiltinPair witnessTag witnessFields <- pmatch $ pasConstr # (pheadSingleton # fields)
        pif (witnessTag #== expectedTag) witnessFields perror
    )
    perror

poutputSkeleton :: forall s. Term s (PState :--> PByteString :--> PByteString)
poutputSkeleton = phoistAcyclic $ plam $ \state bytes -> P.do
  PState{pstate'valueStart, pstate'valueEnd} <- pmatch state
  (psliceBS # 0 # pfromData pstate'valueStart # bytes)
    <> phexByteStr "018200a0"
    <> (psliceBS # pfromData pstate'valueEnd # (plengthBS # bytes) # bytes)

poutputBytes :: forall s. Term s (PState :--> PArgs :--> PBuiltinList (PAsData PTxInInfo) :--> PByteString)
poutputBytes = phoistAcyclic $ plam $ \state args references -> P.do
  PState{pstate'opened, pstate'outputIndex} <- pmatch state
  POpenedOutputs _ hashes <- pmatch $ pfromData pstate'opened
  PArgs{pargs'outputRefIndices} <- pmatch args
  bytes <- plet $ Carriage.pchunks # pfromData pargs'outputRefIndices # references
  index <- plet $ pfromData pstate'outputIndex
  pif (index #>= 0 #&& pblake2b_256 # bytes #== pfromData (pelemAt # index # pfromData hashes)) bytes perror

poutputMetadataSkeleton :: forall s. Term s (PState :--> PByteString :--> PByteString)
poutputMetadataSkeleton = phoistAcyclic $ plam $ \state bytes -> P.do
  PState{pstate'scanCbor, pstate'valueStart} <- pmatch state
  Scan.PLedgerOutputScanControlV1{Scan.pscan'referenceScriptItemOffset} <- pmatch $ Scan.pdecodeControlV1 # pfromData pstate'scanCbor
  offset <- plet $ pfromData pscan'referenceScriptItemOffset
  prefix <-
    plet $
      (pif (offset #== (-1)) (phexByteStr "a2") (phexByteStr "a3"))
        <> (psliceBS # 1 # (pfromData pstate'valueStart - 1) # bytes)
        <> phexByteStr "018200a0"
  pif (offset #== (-1)) prefix (prefix <> phexByteStr "03" <> (psliceBS # offset # (plengthBS # bytes) # bytes))
