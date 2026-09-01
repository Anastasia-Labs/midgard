module MerkleTree.Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator) where

import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (
  PScriptContext (
    PScriptContext,
    pscriptContext'redeemer,
    pscriptContext'scriptInfo
  ),
 )
import Plutarch.MerkleTree.PatriciaForestry (PMerklePatriciaForestry (PMerklePatriciaForestry))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import MerkleTree.Types.Membership (PMerkleMembershipRedeemer (..), PMerkleNonMembershipRedeemer (..))
import Midgard.MpfProof (pdoExcluding, pdoIncludingByHash)
import Midgard.MpfProof.Types (PProof)

-- | 'Withdraw zero' validator to ensure the merkle tree contains a specific entry.
membershipStakeValidator :: (forall s. Term s (PScriptContext :--> PUnit))
membershipStakeValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  PMerkleMembershipRedeemer
    { pmmInputRoot
    , pmmInputKey
    , pmmInputValue
    , pmmInputProof
    } <-
    pmatch $
      pfromData $
        -- Q (Chase): Should we perhaps use PTryFrom here?
        punsafeCoerce @(PAsData PMerkleMembershipRedeemer) (pto pscriptContext'redeemer)
  let root = pto (pfromData pmmInputRoot)
      key = pfromData pmmInputKey
      validateMembership =
        plengthBS # root #== 32
          #&& ( pdoIncludingByHash
                  # (pblake2b_256 # key)
                  # (pblake2b_256 # pfromData pmmInputValue)
                  # 0
                  # pto (pfromData pmmInputProof)
              )
          #== root
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , validateMembership
    ]

-- | 'Withdraw zero' validator to ensure the merkle tree _no longer_ contains a specific entry.
nonMembershipStakeValidator :: (forall s. Term s (PScriptContext :--> PUnit))
nonMembershipStakeValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  PMerkleNonMembershipRedeemer
    { pmnmInputRoot
    , pmnmInputKey
    , pmnmInputProof
    } <-
    pmatch $
      pfromData $
        -- Q (Chase): Should we perhaps use PTryFrom here?
        punsafeCoerce @(PAsData PMerkleNonMembershipRedeemer) (pto pscriptContext'redeemer)
  let validateAbsence =
        pexcludes
          # pfromData pmnmInputRoot
          # pfromData pmnmInputKey
          # pfromData pmnmInputProof
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , validateAbsence
    ]

-- Test whether an element is absent in the trie with a specific value.
-- | Checks whether the provided Merkle proof excludes the queried key.
-- | Checks that the provided trie proof excludes the requested key.
pexcludes :: Term s (PMerklePatriciaForestry :--> PByteString :--> PProof :--> PBool)
pexcludes = phoistAcyclic $ plam $ \self key proof ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    plengthBS # root #== 32
      #&& (pdoExcluding # (pblake2b_256 # key) # 0 # pto proof)
      #== root
