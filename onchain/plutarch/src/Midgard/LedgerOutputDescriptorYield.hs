-- | Four descriptor attestations read one finalize dispatcher's claims.
module Midgard.LedgerOutputDescriptorYield (PDescriptorClaim (..), pdispatch) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekData qualified as Summary
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.ValidationSemanticYield qualified as Semantic
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

data PDescriptorClaim s = PDescriptorClaim
    { pclaim'control :: Term s Raw.PFrame
    , pclaim'descriptorCbor :: Term s PByteString
    , pclaim'valueSummary :: Term s Summary.PDataSummaryV1
    , pclaim'datumSummary :: Term s (PMaybeData Summary.PDataSummaryV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PDescriptorClaim)

psummary :: forall s. Term s PData -> Term s Summary.PDataSummaryV1
psummary dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif
        (tag #== 0 #&& plength # fields #== 3)
        ( pcon $
            Summary.PDataSummaryV1
                (pdata $ pasByteStr # (pelemAt # 0 # fields))
                (pdata $ pasInt # (pelemAt # 1 # fields))
                (pdata $ pasInt # (pelemAt # 2 # fields))
        )
        perror

poptionalSummary :: forall s. Term s PData -> Term s (PMaybeData Summary.PDataSummaryV1)
poptionalSummary dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1 #&& pnull # fields) (pcon PDNothing) $
        pif (tag #== 0 #&& plength # fields #== 1) (pcon $ PDJust $ pdata $ psummary $ phead # fields) perror

pdispatch :: forall s. Term s (PBuiltinList (PAsData PScriptHash) :--> PTxInfo :--> PDescriptorClaim)
pdispatch = phoistAcyclic $ plam $ \dispatchers tx ->
    pmatch (Semantic.puniqueSemanticDispatchV1 # dispatchers # tx) $ \dispatch ->
        plet (Semantic.pdispatch'extra dispatch) $ \extra ->
            pif
                (plength # extra #== 7)
                ( pcon $
                    PDescriptorClaim
                        (Raw.popen # (pasByteStr # (pelemAt # 2 # extra)))
                        (pasByteStr # (pelemAt # 0 # extra))
                        (psummary $ pelemAt # 3 # extra)
                        (poptionalSummary $ pelemAt # 4 # extra)
                )
                perror
