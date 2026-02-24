module Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator) where

import Plutarch.Core.Integrity (pisRewardingScript)
import Plutarch.Core.ValidationLogic (pvalidateConditions)
import Plutarch.LedgerApi.V3 (
  PScriptContext (
    PScriptContext,
    pscriptContext'redeemer,
    pscriptContext'scriptInfo
  ),
 )
import Plutarch.MerkleTree.PatriciaForestry (PMerklePatriciaForestry (PMerklePatriciaForestry), PProof, pexcluding, phas)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Types.Membership (PMerkleMembershipRedeemer (..), PMerkleNonMembershipRedeemer (..))

-- | 'Withdraw zero' validator to ensure the merkle tree contains a specific entry.
membershipStakeValidator :: ClosedTerm (PScriptContext :--> PUnit)
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
  let validateMembership =
        phas
          # pfromData pmmInputRoot
          # pfromData pmmInputKey
          # pfromData pmmInputValue
          # pfromData pmmInputProof
  pvalidateConditions
    [ pisRewardingScript (pdata pscriptContext'scriptInfo)
    , validateMembership
    ]

-- | 'Withdraw zero' validator to ensure the merkle tree _no longer_ contains a specific entry.
nonMembershipStakeValidator :: ClosedTerm (PScriptContext :--> PUnit)
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
pexcludes :: Term s (PMerklePatriciaForestry :--> PByteString :--> PProof :--> PBool)
pexcludes = phoistAcyclic $ plam $ \self key proof ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    pexcluding # key # proof #== root
