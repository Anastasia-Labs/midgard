module Midgard.FraudProofs.MintAuthorizationScan (
  PMintControl (..),
  pinitialMint,
  pmintComplete,
  padvanceMint,
  PWitnessScanState (..),
  PWitnessScanAction (..),
  padvanceWitness,
  pauthenticateWitness,
  PEvaluateState (..),
  PEvaluateOperation (..),
  PEvaluateAction (..),
  pchunks,
  prawChunks,
  ppayload,
  pauthenticatePayload,
  padvanceEvaluation,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofs.NativeTx.Codec (pdecodeCanonicalIntAt, pdecodeDefiniteBytesAt, pencodeDefiniteBytes, pexpectByte)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor, pdecodeMidgardVersionedScriptAt, pencodeMidgardVersionedScript)
import Midgard.FraudProofs.NativeTx.Preimages (pcanonicalBytesKeyPrecedes, pdecodeCanonicalBytesAt, pdecodeCanonicalMapHeaderAt)
import Midgard.FraudProofs.NativeTx.Types (PMidgardAddressWitness (..))
import Midgard.NativeScriptScan qualified as Scan
import Midgard.NativeTxFieldAccess (pdecodeFieldArrayHeaderAt, pmaxTransactionAggregateFieldBytes)
import Midgard.ScriptProof (pversionedScriptHash)
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (POutputDatum (..), PTxInInfo (..), PTxOut (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

data PMintControl s = PMintControl
  { pmint'itemCount :: Term s (PAsData PInteger)
  , pmint'itemIndex :: Term s (PAsData PInteger)
  , pmint'cursor :: Term s (PAsData PInteger)
  , pmint'itemEnd :: Term s (PAsData PInteger)
  , pmint'remainingAssets :: Term s (PAsData PInteger)
  , pmint'previousAssetName :: Term s (PMaybeData PByteString)
  , pmint'policyId :: Term s (PAsData PByteString)
  , pmint'selectedComplete :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintControl)

pinitialMint :: forall s. Term s (PByteString :--> PInteger :--> PMintControl)
pinitialMint = phoistAcyclic $ plam $ \bytes index -> P.do
  PPair cursor count <- pmatch $ pdecodeFieldArrayHeaderAt # bytes # 0
  pif
    (plengthBS # bytes #<= pmaxTransactionAggregateFieldBytes #&& index #>= 0 #&& index #< count)
    (pcon $ PMintControl (pdata count) (pdata 0) (pdata cursor) (pdata 0) (pdata 0) (pcon PDNothing) (pdata $ pconstant "") (pdata $ pconstant False))
    perror

pmintComplete :: forall s. Term s (PMintControl :--> PInteger :--> PBool)
pmintComplete = phoistAcyclic $ plam $ \control len -> pmatch control $ \c ->
  pmint'itemIndex c
    #== pmint'itemCount c
    #&& pfromData (pmint'cursor c)
    #== len
    #&& pfromData (pmint'remainingAssets c)
    #== 0
    #&& pfromData (pmint'selectedComplete c)
    #&& plengthBS
    # pfromData (pmint'policyId c)
    #== 28

padvanceMint :: forall s. Term s (PMintControl :--> PByteString :--> PInteger :--> PInteger :--> PMintControl)
padvanceMint = phoistAcyclic $ pfix $ \self -> plam $ \control bytes policyIndex remaining -> pmatch control $ \c ->
  pif (remaining #== 0 #|| pmint'itemIndex c #== pmint'itemCount c) control $
    pif
      (pfromData (pmint'remainingAssets c) #> 0)
      ( P.do
          PPair cursor name <- pmatch $ pdecodeCanonicalBytesAt # bytes # pfromData (pmint'cursor c)
          PPair next quantity <- pmatch $ pdecodeCanonicalIntAt # bytes # cursor
          let left = pfromData (pmint'remainingAssets c) - 1
              ordered = pmatch (pmint'previousAssetName c) $ \case PDNothing -> pconstant True; PDJust previous -> pcanonicalBytesKeyPrecedes # pfromData previous # name
          pif
            ( plengthBS
                # name
                #<= 32
                #&& ordered
                #&& quantity
                #/= 0
                #&& next
                #<= pfromData (pmint'itemEnd c)
                #&& (left #/= 0 #|| next #== pfromData (pmint'itemEnd c))
            )
            ( self
                # pcon
                  c
                    { pmint'cursor = pdata next
                    , pmint'remainingAssets = pdata left
                    , pmint'previousAssetName = pcon $ PDJust $ pdata name
                    , pmint'itemIndex = pdata $ pfromData (pmint'itemIndex c) + pif (left #== 0) 1 0
                    , pmint'selectedComplete = pdata $ left #== 0
                    }
                # bytes
                # policyIndex
                # (remaining - 1)
            )
            perror
      )
      ( P.do
          PPair next item <- pmatch $ pdecodeCanonicalBytesAt # bytes # pfromData (pmint'cursor c)
          pif
            (pfromData (pmint'itemIndex c) #< pfromData (pmint'itemCount c) #&& next #<= plengthBS # bytes)
            ( P.do
                nextControl <-
                  plet $
                    pif
                      (pfromData (pmint'itemIndex c) #== policyIndex)
                      ( P.do
                          PPair cursor policy <- pmatch $ pdecodeCanonicalBytesAt # bytes # (pexpectByte # bytes # (next - plengthBS # item) # 130)
                          PPair end count <- pmatch $ pdecodeCanonicalMapHeaderAt # bytes # cursor
                          pif
                            (pnot # pfromData (pmint'selectedComplete c) #&& plengthBS # policy #== 28 #&& count #> 0 #&& end #< next)
                            (pcon c{pmint'cursor = pdata end, pmint'itemEnd = pdata next, pmint'remainingAssets = pdata count, pmint'previousAssetName = pcon PDNothing, pmint'policyId = pdata policy})
                            perror
                      )
                      (pcon c{pmint'cursor = pdata next, pmint'itemIndex = pdata $ pfromData (pmint'itemIndex c) + 1})
                self # nextControl # bytes # policyIndex # (remaining - 1)
            )
            perror
      )

data PWitnessScanState s = PWitnessScanState
  { pwitness'policyId :: Term s (PAsData PByteString)
  , pwitness'badTxId :: Term s (PAsData PByteString)
  , pwitness'priorLedgerRoot :: Term s (PAsData PByteString)
  , pwitness'fieldLength :: Term s (PAsData PInteger)
  , pwitness'fieldChunkHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pwitness'cursor :: Term s (PAsData PInteger)
  , pwitness'itemIndex :: Term s (PAsData PInteger)
  , pwitness'itemCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWitnessScanState)

data PWitnessScanAction s
  = PWitnessAdvance (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData PInteger))))
  | PWitnessFinalize (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWitnessScanAction)

padvanceWitness :: forall s. Term s (PWitnessScanState :--> PByteString :--> PInteger :--> PWitnessScanState)
padvanceWitness = phoistAcyclic $ pfix $ \self -> plam $ \state bytes remaining -> pmatch state $ \s ->
  pif (remaining #== 0 #|| pwitness'itemIndex s #== pwitness'itemCount s) state $ P.do
    PPair next item <- pmatch $ pdecodeDefiniteBytesAt # bytes # pfromData (pwitness'cursor s)
    PPair _ script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
    pif
      ( pfromData (pwitness'itemIndex s)
          #< pfromData (pwitness'itemCount s)
          #&& next
          #<= pfromData (pwitness'fieldLength s)
          #&& psliceBS
          # pfromData (pwitness'cursor s)
          # (next - pfromData (pwitness'cursor s))
          # bytes
          #== pencodeDefiniteBytes
          # item
          #&& pencodeMidgardVersionedScript
          # script
          #== item
          #&& pversionedScriptHash
          # script
          #/= pfromData (pwitness'policyId s)
      )
      (self # pcon s{pwitness'cursor = pdata next, pwitness'itemIndex = pdata $ pfromData (pwitness'itemIndex s) + 1} # bytes # (remaining - 1))
      perror

pauthenticateWitness :: forall s. Term s (PWitnessScanState :--> PBuiltinList (PAsData PByteString) :--> PByteString)
pauthenticateWitness = phoistAcyclic $ plam $ \state parts -> pmatch state $ \s ->
  pauthenticate # pfromData (pwitness'fieldChunkHashes s) # pfromData (pwitness'fieldLength s) # parts

data PEvaluateState s = PEvaluateState
  { pevaluate'policyId :: Term s (PAsData PByteString)
  , pevaluate'scriptLength :: Term s (PAsData PInteger)
  , pevaluate'rawLength :: Term s (PAsData PInteger)
  , pevaluate'signerStart :: Term s (PAsData PInteger)
  , pevaluate'signerCount :: Term s (PAsData PInteger)
  , pevaluate'signerIndex :: Term s (PAsData PInteger)
  , pevaluate'preimageChunkHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pevaluate'signerHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pevaluate'validityStart :: Term s (PAsData PInteger)
  , pevaluate'validityEnd :: Term s (PAsData PInteger)
  , pevaluate'cursor :: Term s (PAsData PInteger)
  , pevaluate'nodeCount :: Term s (PAsData PInteger)
  , pevaluate'stackRoot :: Term s (PAsData PByteString)
  , pevaluate'stackDepth :: Term s (PAsData PInteger)
  , pevaluate'result :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEvaluateState)

data PEvaluateOperation s = PToken | PFrame (Term s (PAsData Scan.PNativeScriptFrameV1)) | PSigner
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEvaluateOperation)

data PEvaluateAction s
  = PEvaluateAdvance
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PInteger))))
      (Term s (PAsData (PBuiltinList (PAsData PEvaluateOperation))))
  | PEvaluateFinalize (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEvaluateAction)

pchunks, prawChunks :: forall s. Term s (PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PByteString))
pchunks = phoistAcyclic $ plam $ \indices refs -> pchunksBounded # indices # refs # 3
prawChunks = phoistAcyclic $ plam $ \indices refs -> pchunksBounded # indices # refs # 5

pchunksBounded :: forall s. Term s (PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PBuiltinList (PAsData PByteString))
pchunksBounded = phoistAcyclic $ plam $ \indices refs maximum ->
  pif
    (plength # indices #> 0 #&& plength # indices #<= maximum)
    ( pmap
        # plam
          ( \index -> P.do
              PTxInInfo{ptxInInfo'resolved} <- pmatch $ pfromData $ pelemAt # pfromData index # refs
              PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
              POutputDatum datum <- pmatch ptxOut'datum
              bytes <- plet $ pasByteStr # pto datum
              pif (plengthBS # bytes #> 0 #&& plengthBS # bytes #<= 15148) (pdata bytes) perror
          )
        # indices
    )
    perror

ppayload :: forall s. Term s (PBuiltinList (PAsData PByteString) :--> PByteString)
ppayload = phoistAcyclic $ plam $ \parts -> pfoldl # plam (\acc part -> acc <> pfromData part) # pconstant "" # parts

pauthenticate :: forall s. Term s (PBuiltinList (PAsData PByteString) :--> PInteger :--> PBuiltinList (PAsData PByteString) :--> PByteString)
pauthenticate = phoistAcyclic $ plam $ \hashes len parts ->
  plet (ppayload # parts) $ \bytes ->
    pif
      (pmap # plam (\part -> pdata $ pblake2b_256 # pfromData part) # parts #== hashes #&& plengthBS # bytes #== len)
      bytes
      perror

pauthenticatePayload :: forall s. Term s (PEvaluateState :--> PBuiltinList (PAsData PByteString) :--> PByteString)
pauthenticatePayload = phoistAcyclic $ plam $ \state parts -> pmatch state $ \s ->
  pauthenticate # pfromData (pevaluate'preimageChunkHashes s) # pfromData (pevaluate'rawLength s) # parts

ptoken :: forall s. Term s (PEvaluateState :--> PByteString :--> PEvaluateState)
ptoken = phoistAcyclic $ plam $ \state bytes -> pmatch state $ \s ->
  pif
    (pevaluate'signerIndex s #== pevaluate'signerCount s #&& pfromData (pevaluate'result s) #== (-1) #&& pfromData (pevaluate'cursor s) #< pfromData (pevaluate'scriptLength s))
    ( P.do
        PJust token <- pmatch $ Scan.ptokenAtV1 # bytes # pfromData (pevaluate'cursor s) # pfromData (pevaluate'cursor s)
        t <- pmatch token
        let count = pfromData (pevaluate'nodeCount s) + 1
            kind = pfromData $ Scan.ptoken'kind t
            cursor = Scan.ptoken'nextOffset t
        pif
          (pfromData cursor #<= pfromData (pevaluate'scriptLength s) #&& count #<= Scan.pmaxNativeScriptNodes)
          ( pif
              (kind #>= Scan.pallNode #&& kind #<= Scan.patLeastNode #&& pfromData (Scan.ptoken'childCount t) #> 0)
              ( P.do
                  PJust frame <- pmatch $ Scan.pframeForTokenV1 # token # pfromData (pevaluate'stackRoot s)
                  let depth = pfromData (pevaluate'stackDepth s) + 1
                  pif
                    (depth #<= Scan.pmaxNativeScriptDepth)
                    (pcon s{pevaluate'cursor = cursor, pevaluate'nodeCount = pdata count, pevaluate'stackRoot = pdata $ Scan.phashFrameV1 # frame, pevaluate'stackDepth = pdata depth})
                    perror
              )
              ( P.do
                  valid <-
                    plet $
                      pif
                        (kind #== Scan.psignatureNode)
                        (pelem # Scan.ptoken'keyHash t # pfromData (pevaluate'signerHashes s))
                        ( pif
                            (kind #== Scan.pafterNode)
                            (pfromData (pevaluate'validityStart s) #>= 0 #&& pfromData (pevaluate'validityStart s) #>= pfromData (Scan.ptoken'slot t))
                            ( pif
                                (kind #== Scan.pbeforeNode)
                                (pfromData (pevaluate'validityEnd s) #>= 0 #&& pfromData (pevaluate'validityEnd s) #<= pfromData (Scan.ptoken'slot t))
                                (pmatch (Scan.pemptyContainerResultV1 # token) $ \case PJust yes -> yes; PNothing -> perror)
                            )
                        )
                  pcon s{pevaluate'cursor = cursor, pevaluate'nodeCount = pdata count, pevaluate'result = pdata $ pif valid 1 0}
              )
          )
          perror
    )
    perror

pframe :: forall s. Term s (PEvaluateState :--> Scan.PNativeScriptFrameV1 :--> PEvaluateState)
pframe = phoistAcyclic $ plam $ \state frame -> pmatch state $ \s ->
  pif
    ( pevaluate'signerIndex s
        #== pevaluate'signerCount s
        #&& (pfromData (pevaluate'result s) #== 0 #|| pfromData (pevaluate'result s) #== 1)
        #&& pfromData (pevaluate'stackDepth s)
        #> 0
        #&& Scan.phashFrameV1
        # frame
        #== pfromData (pevaluate'stackRoot s)
    )
    ( P.do
        PJust applied <- pmatch $ Scan.papplyChildV1 # frame # (pfromData (pevaluate'result s) #== 1)
        pmatch applied $ \case
          Scan.PNativeFramePending next -> pcon s{pevaluate'stackRoot = pdata $ Scan.phashFrameV1 # pfromData next, pevaluate'result = pdata (-1)}
          Scan.PNativeFrameComplete tailRoot valid -> pcon s{pevaluate'stackRoot = tailRoot, pevaluate'stackDepth = pdata $ pfromData (pevaluate'stackDepth s) - 1, pevaluate'result = pdata $ pif (pfromData valid) 1 0}
    )
    perror

psigner :: forall s. Term s (PEvaluateState :--> PByteString :--> PEvaluateState)
psigner = phoistAcyclic $ plam $ \state bytes -> pmatch state $ \s ->
  plet (pfromData (pevaluate'signerStart s) + pfromData (pevaluate'signerIndex s) * 103) $ \offset ->
    pif
      ( pfromData (pevaluate'signerIndex s)
          #< pfromData (pevaluate'signerCount s)
          #&& pfromData (pevaluate'cursor s)
          #== 0
          #&& pfromData (pevaluate'nodeCount s)
          #== 0
          #&& pfromData (pevaluate'result s)
          #== (-1)
          #&& psliceBS
          # offset
          # 2
          # bytes
          #== phexByteStr "5865"
      )
      ( P.do
          PMidgardAddressWitness{paddressWitness'verificationKey} <- pmatch $ pdecodeMidgardAddressWitnessCbor # (psliceBS # (offset + 2) # 101 # bytes)
          pcon
            s
              { pevaluate'signerIndex = pdata $ pfromData (pevaluate'signerIndex s) + 1
              , pevaluate'signerHashes = pdata $ pcons # pdata (pblake2b_224 # pfromData paddressWitness'verificationKey) # pfromData (pevaluate'signerHashes s)
              }
      )
      perror

padvanceEvaluation :: forall s. Term s (PEvaluateState :--> PByteString :--> PBuiltinList (PAsData PEvaluateOperation) :--> PEvaluateState)
padvanceEvaluation = phoistAcyclic $ plam $ \state bytes operations ->
  pif
    (plength # operations #> 0 #&& plength # operations #<= 16)
    (pfoldl # plam (\current operation -> pmatch (pfromData operation) $ \case PToken -> ptoken # current # bytes; PFrame frame -> pframe # current # pfromData frame; PSigner -> psigner # current # bytes) # state # operations)
    perror
