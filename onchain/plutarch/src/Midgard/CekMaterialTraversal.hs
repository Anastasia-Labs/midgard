{-# LANGUAGE OverloadedStrings #-}

-- | Authenticated task stack and visited-set accounting for CEK material.
module Midgard.CekMaterialTraversal (
    PState (..),
    PAction (..),
    pprogramRole,
    pdataRole,
    pemptyStack,
    pdecodeState,
    pdecodeAction,
    pdecodeTask,
    ppush,
    ppushChildren,
    pinitial,
    padvance,
    puniqueDispatch,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekProof qualified as Proof
import Midgard.Common.Utils (pheadSingleton)
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.Env qualified as Env
import Midgard.LedgerState (PCekProgramMaterialDatumV1 (..))
import Midgard.MpfProof qualified as Mpf
import Midgard.MpfProof.Types (PNeighbor (..), PProof (..), PProofStep (..))
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

pprogramRole, pdataRole :: forall s. Term s PTokenName
pprogramRole = pcon $ PTokenName $ pconstant "V1VtCekMatProgramTask"
pdataRole = pcon $ PTokenName $ pconstant "V1VtCekMatDataTask"
pemptyStack :: forall s. Term s PByteString
pemptyStack = pblake2b_256 # pconstant "midgard/cek-material-stack/empty"

data PState s = PState
    { pstate'pendingRoot :: Term s (PAsData PByteString)
    , pstate'visitedRoot :: Term s (PAsData PByteString)
    , pstate'nodeCount :: Term s (PAsData PInteger)
    , pstate'byteLength :: Term s (PAsData PInteger)
    , pstate'expectedNodeCount :: Term s (PAsData PInteger)
    , pstate'expectedByteLength :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PState)

-- The task field retains its exact wire Data, opened by pdecodeTask into the
-- shared proof walk's internal representation when the task is consumed.
data PAction s = PVisit
    { pvisit'inputIndex :: Term s (PAsData PInteger)
    , pvisit'outputIndex :: Term s (PAsData PInteger)
    , pvisit'task :: Term s PData
    , pvisit'tailRoot :: Term s (PAsData PByteString)
    , pvisit'entry :: Term s (PAsData PCekProgramMaterialDatumV1)
    , pvisit'alreadySeen :: Term s (PAsData PBool)
    , pvisit'visitedProof :: Term s (PAsData PProof)
    , pvisit'yieldIndex :: Term s (PAsData PInteger)
    , pvisit'nextPendingRoot :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAction)

pfields :: forall s. Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) -> pif (tag #== 0 #&& plength # fields #== count) fields perror
pint :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PInteger)
pint f index = pdata $ pasInt # (pelemAt # index # f)
pbytes :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
pbytes f index = pdata $ pasByteStr # (pelemAt # index # f)

pdecodeTask :: forall s. Term s (PData :--> Proof.PProgramMaterialTaskV1)
pdecodeTask = phoistAcyclic $ plam $ \raw -> plet (pfields 3 raw) $ \f ->
    pcon $ Proof.PProgramMaterialTaskV1 (pfromData $ pint f 0) (pfromData $ pbytes f 1) (pfromData $ pint f 2)
ptaskData :: forall s. Term s Proof.PProgramMaterialTaskV1 -> Term s PData
ptaskData task = pmatch task $ \t ->
    pforgetData $
        pconstrBuiltin
            # 0
            # (pcons # pforgetData (pdata $ Proof.ptask'kind t) # (pcons # pforgetData (pdata $ Proof.ptask'root t) # (pcons # pforgetData (pdata $ Proof.ptask'expectedLength t) # pnil)))

pdecodeState :: forall s. Term s (PData :--> PState)
pdecodeState = phoistAcyclic $ plam $ \raw -> plet (pfields 6 raw) $ \f ->
    pcon $ PState (pbytes f 0) (pbytes f 1) (pint f 2) (pint f 3) (pint f 4) (pint f 5)

pdecodeProof :: forall s. Term s PData -> Term s PProof
pdecodeProof raw =
    pcon $
        PProof $
            pmap
                # plam
                    ( \item -> pdata $ pmatch (pasConstr # item) $ \(PBuiltinPair tag f) ->
                        pif (tag #== 0) (pif (plength # f #== 2) (pcon $ PBranch (pint f 0) (pbytes f 1)) perror)
                            $ pif
                                (tag #== 1)
                                ( pif
                                    (plength # f #== 2)
                                    (plet (pfields 3 $ pelemAt # 1 # f) $ \neighbor -> pcon $ PFork (pint f 0) (pdata $ pcon $ PNeighbor (pint neighbor 0) (pbytes neighbor 1) (pbytes neighbor 2)))
                                    perror
                                )
                            $ pif (tag #== 2) (pif (plength # f #== 3) (pcon $ PLeaf (pint f 0) (pbytes f 1) (pbytes f 2)) perror) perror
                    )
                # (pasList # raw)

pdecodeAction :: forall s. Term s (PData :--> PAction)
pdecodeAction = phoistAcyclic $ plam $ \raw -> plet (pfields 9 raw) $ \f ->
    plet (pfields 3 $ pelemAt # 4 # f) $ \entry ->
        plet
            ( pmatch (pasConstr # (pelemAt # 5 # f)) $ \(PBuiltinPair tag values) ->
                pif (pnull # values #&& (tag #== 0 #|| tag #== 1)) (tag #== 1) perror
            )
            $ \seen ->
                pcon $
                    PVisit
                        (pint f 0)
                        (pint f 1)
                        (ptaskData $ pdecodeTask # (pelemAt # 2 # f))
                        (pbytes f 3)
                        (pdata $ pcon $ PCekProgramMaterialDatumV1 (pint entry 0) (pbytes entry 1) (pbytes entry 2))
                        (pdata seen)
                        (pdata $ pdecodeProof $ pelemAt # 6 # f)
                        (pint f 7)
                        (pbytes f 8)

ppush :: forall s. Term s (Proof.PProgramMaterialTaskV1 :--> PByteString :--> PByteString)
ppush = phoistAcyclic $ plam $ \task tailRoot ->
    pblake2b_256
        # ( pconstant "midgard/cek-material-stack/item"
                <> ( pserialiseData
                        # pforgetData
                            ( pconstrBuiltin
                                # 0
                                # (pcons # ptaskData task # (pcons # pforgetData (pdata tailRoot) # pnil))
                            )
                   )
          )
ppushChildren :: forall s. Term s (PList Proof.PProgramMaterialTaskV1 :--> PByteString :--> PByteString)
ppushChildren = phoistAcyclic $ plam $ \children tailRoot -> pfoldr # ppush # tailRoot # children

pinitial :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PState)
pinitial = phoistAcyclic $ plam $ \termRoot count bytes ->
    pcon $
        PState
            (pdata $ ppush # pcon (Proof.PProgramMaterialTaskV1 0 termRoot (-1)) # pemptyStack)
            (pdata Env.pemptyMerkleTreeRoot)
            (pdata 0)
            (pdata 0)
            (pdata count)
            (pdata bytes)

padvance :: forall s. Term s (PState :--> PAction :--> PState)
padvance = phoistAcyclic $ plam $ \state action -> pmatch state $ \s -> pmatch action $ \a ->
    plet (pdecodeTask # pvisit'task a) $ \task -> pmatch task $ \t -> pmatch (pfromData $ pvisit'entry a) $ \entry ->
        pif
            ( pfromData (pstate'pendingRoot s)
                #== ppush
                # task
                # pfromData (pvisit'tailRoot a)
                #&& pfromData (pcekProgramMaterial'root entry)
                #== Proof.ptask'root t
            )
            ( plet
                ( pif
                    (pfromData $ pvisit'alreadySeen a)
                    (pif (Mpf.phasV1 # pfromData (pstate'visitedRoot s) # Proof.ptask'root t # pconstant "\x01" # pfromData (pvisit'visitedProof a)) (pfromData $ pstate'visitedRoot s) perror)
                    (pmatch (Mpf.pinsertRoot # pfromData (pstate'visitedRoot s) # Proof.ptask'root t # pconstant "\x01" # pfromData (pvisit'visitedProof a)) $ \case PNothing -> perror; PJust root -> root)
                )
                $ \visited ->
                    plet (pfromData (pstate'nodeCount s) + pif (pfromData $ pvisit'alreadySeen a) 0 1) $ \count ->
                        plet (pfromData (pstate'byteLength s) + pif (pfromData $ pvisit'alreadySeen a) 0 (plengthBS # pfromData (pcekProgramMaterial'preimage entry))) $ \bytes ->
                            pif
                                (count #<= pfromData (pstate'expectedNodeCount s) #&& bytes #<= pfromData (pstate'expectedByteLength s))
                                (pcon s{pstate'pendingRoot = pvisit'nextPendingRoot a, pstate'visitedRoot = pdata visited, pstate'nodeCount = pdata count, pstate'byteLength = pdata bytes})
                                perror
            )
            perror

pdecodeStep :: forall s. Term s PData -> Term s PStepDatum
pdecodeStep raw = plet (pfields 2 raw) $ \f ->
    pcon $
        PStepDatum
            (pdata $ pcon $ PPubKeyHash $ pasByteStr # (phead # f))
            ( pmatch (pasConstr # (pelemAt # 1 # f)) $ \(PBuiltinPair tag values) ->
                pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $
                    pif
                        (tag #== 0 #&& plength # values #== 1)
                        (pcon $ PDJust $ pdata $ pforgetData $ pdata $ pdecodeState # (phead # values))
                        perror
            )

puniqueDispatch :: forall s. Term s (PScriptHash :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PPair PStepDatum PAction)
puniqueDispatch = phoistAcyclic $ plam $ \hash inputs redeemers -> P.do
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
                                    credential #== pcon (PScriptCredential $ pdata hash)
                                )
                            # inputs
                      )
    PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
    raw <- plet $ pmatch ptxOut'datum $ \case POutputDatum dat -> pto dat; _ -> perror
    PBuiltinPair _ redeemer <-
        pmatch $
            pheadSingleton
                # (pfilter # plam (\pair -> pmatch pair $ \(PBuiltinPair purpose _) -> pfromData purpose #== pcon (PSpending ptxInInfo'outRef)) # redeemers)
    PBuiltinPair tag fields <- pmatch $ pasConstr # (pto $ pfromData redeemer)
    pif
        (tag #== 1 #&& plength # fields #== 1)
        (pcon $ PPair (pdecodeStep raw) (pdecodeAction # (phead # fields)))
        perror
