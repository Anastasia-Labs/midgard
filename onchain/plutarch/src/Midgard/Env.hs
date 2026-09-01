{- |
Module      : Midgard.Env
Description : Plutarch port of the constants in @env/default.ak@ that the
              ported modules read.

Aiken resolves @env@ per build target (@default@, @testnet@, …). This module
mirrors the @default@ environment; a target-specific build would need the
equivalent of Aiken's environment selection, which does not exist here yet.
Only the constants the ported code actually reads are present.
-}
module Midgard.Env (
  pemptyData,
  pslashingPenalty,
  pfraudProverReward,
  prequiredBond,
  pinactivitySlashingPenalty,
  pregistrationDuration,
  pmaxInactivityStrikes,
  pplutusVersion,
  peventWaitDuration,
  pmaxTokensAllowedInDeposits,
  puserEventsWitnessScriptPrefix,
  pemptyMerkleTreeRoot,
  pplutarchPhasValidatorHash,
  pplutarchPexcludesValidatorHash,
  pmpfChunkedVerifyValidatorHash,
  pmaxValidityRangeLength,
  pshiftDuration,
  puserEventsNegligenceTimeout,
  pmaxInactivityBetweenBlockCommitments,
  pnewShiftInactivityGracePeriod,
  pposixTimeNone,
) where

import Plutarch.Prelude

{- | Aiken @env.empty_data@ — @""@ as @Data@.

The root of an operator-directory list carries no payload, and this is the exact
encoding @operator_directory.init@ compares against.
-}
pemptyData :: forall (s :: S). Term s PData
pemptyData = pforgetData (pdata (pconstant @PByteString ""))

{- | Aiken @env.posix_time_none@ — @-1@.

The sentinel a native transaction body carries for an /absent/ validity bound.
It is a negative time rather than a @Maybe@ because the compact body is a flat
CBOR array with no room for an option tag, and no real POSIX time is negative.
-}
pposixTimeNone :: forall (s :: S). Term s PInteger
pposixTimeNone = -1

-- | Aiken @env.slashing_penalty@.
pslashingPenalty :: forall (s :: S). Term s PInteger
pslashingPenalty = 0

-- | Aiken @env.fraud_prover_reward@.
pfraudProverReward :: forall (s :: S). Term s PInteger
pfraudProverReward = 0

{- | Aiken @env.required_bond = slashing_penalty + fraud_prover_reward@.

Zero in the default environment, so the bond checks in
'Midgard.OperatorDirectory.pvalidateTransferredOperatorInsertion' are vacuous as
configured. They are ported anyway, because a deployment environment that sets a
real penalty is the point of the constant.
-}
prequiredBond :: forall (s :: S). Term s PInteger
prequiredBond = pslashingPenalty + pfraudProverReward

-- | Aiken @env.inactivity_slashing_penalty@.
pinactivitySlashingPenalty :: forall (s :: S). Term s PInteger
pinactivitySlashingPenalty = 0

{- | Aiken @env.registration_duration@ — a @PosixTimeDuration@, so milliseconds.

The delay between an operator registering and becoming eligible to activate. 30
in the default environment, which is 30 /milliseconds/ and so effectively no
delay at all; a real deployment sets this to a meaningful interval.
-}
pregistrationDuration :: forall (s :: S). Term s PInteger
pregistrationDuration = 30

{- | Aiken @env.max_inactivity_strikes@.

The strike count at which an active operator may be retired against its will and
partially slashed. It is also a ceiling: the active set refuses to record a
strike beyond it, which stops an attacker pinning an operator's UTxO in place by
striking it forever.
-}
pmaxInactivityStrikes :: forall (s :: S). Term s PInteger
pmaxInactivityStrikes = 5

{- | Aiken @env.shift_duration@ — a @PosixTimeDuration@, so milliseconds.

How long one operator's turn at committing blocks lasts.

/This is the one constant that differs between Aiken's two environments./
@env/default.ak@ says 30 — thirty milliseconds — and @env/testnet.ak@ says
@60 * 60 * 1000@, one hour. The two files are otherwise identical, so raising
this to an hour is the entire reason the second environment exists.

This module mirrors @env/default.ak@, as the rest of the port does, so the value
here is the development one.

That has a consequence worth knowing before changing it. The scheduler's
inactivity check requires the inactivity threshold to fall before the end of the
shift being judged, and that threshold is at least
@shift_start + new_shift_inactivity_grace_period@ — five minutes. At 30ms the
shift ends long before that, so both skipped-operator branches are unreachable;
at an hour they behave normally. See "Midgard.Validators.Scheduler".
-}
pshiftDuration :: forall (s :: S). Term s PInteger
pshiftDuration = 30

{- | Aiken @env.user_events_negligence_timeout@ — five minutes.

How long an operator may go without committing a block once a user event is
waiting for inclusion. Past this the operator can be struck as having skipped
its shift.
-}
puserEventsNegligenceTimeout :: forall (s :: S). Term s PInteger
puserEventsNegligenceTimeout = 5 * 60 * 1000

{- | Aiken @env.max_inactivity_between_block_commitments@.

The same idea as 'puserEventsNegligenceTimeout' but unconditional: how long an
operator may go without committing a block even with nothing queued.

The Aiken source writes this as @10 * 6 * 1000@ — one minute, not the ten
minutes the arithmetic looks like it was reaching for. This port keeps the value
as written rather than the value apparently intended; if that is a typo it is a
typo in the protocol's parameters and belongs fixed on the Aiken side, where the
deployed scripts are generated from.
-}
pmaxInactivityBetweenBlockCommitments :: forall (s :: S). Term s PInteger
pmaxInactivityBetweenBlockCommitments = 10 * 6 * 1000

{- | Aiken @env.new_shift_inactivity_grace_period@ — five minutes.

A newly appointed operator's grace period. Without it an operator could be
struck for inactivity in the instant between being appointed and having any
chance to act.
-}
pnewShiftInactivityGracePeriod :: forall (s :: S). Term s PInteger
pnewShiftInactivityGracePeriod = 5 * 60 * 1000

-- | Aiken @env.plutus_version@.
pplutusVersion :: forall (s :: S). Term s PInteger
pplutusVersion = 3

{- | Aiken @env.event_wait_duration@ — 60 seconds in milliseconds.

How far in the future a user event's inclusion time must sit: the window in
which an operator is expected to pick the event up.
-}
peventWaitDuration :: forall (s :: S). Term s PInteger
peventWaitDuration = 60_000

{- | Aiken @env.max_tokens_allowed_in_deposits@.

A ceiling on how many distinct non-NFT assets — Ada included — one deposit may
carry, so a single deposit cannot make the L2 ledger entry unboundedly large.
-}
pmaxTokensAllowedInDeposits :: forall (s :: S). Term s PInteger
pmaxTokensAllowedInDeposits = 10

{- | Aiken @env.user_events_witness_script_prefix@.

The compiled witness staking script with its nonce parameter still outstanding.
Each user event derives its own witness script hash by appending the event's
nonce to this prefix and hashing — see
'DesignPatterns.ParameterValidation.papplyPrehashedParam'. That is what binds an
event NFT one-to-one with a staking credential the transaction must register.

687 bytes, copied verbatim from @env/default.ak@. It is an opaque constant here:
this package does not compile the witness script, so a change on the Aiken side
must be copied across or the derived hashes diverge.
-}
puserEventsWitnessScriptPrefix :: forall (s :: S). Term s PByteString
puserEventsWitnessScriptPrefix =
  phexByteStr $
    concat
      [ "5902ce0101003229800aba2aba1aab9faab9eaab9dab9a9bae0024888888966002646530"
      , "01300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800"
      , "a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a005004"
      , "8b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4"
      , "c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c0"
      , "04c040dd500545900e201c40382653001300100198071baa009918091809980998099809"
      , "9809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc380"
      , "12400314a0809901319199119801001000912cc004006007132325980099b910150018ac"
      , "c004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae301600130180014"
      , "05c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980"
      , "099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae"
      , "301700130190014060297adef6c60148000c048dd50031bae30143012375401915980098"
      , "0400144c8c96600266ebc00401e2b3001300930133754003132598009805980a1baa0018"
      , "acc004cdd7980b980a9baa00230173015375400314a316404d16404c602c602e00516404"
      , "9164048602a002660066eb0c004c048dd50051bad301430123754019159800980418089b"
      , "aa0058992cc004c020c048dd5000c56600266ebcc054c04cdd5000980a98099baa0068a5"
      , "18b20228b20223014330033758600260246ea8028dd6980a18091baa00c8b20204040808"
      , "0444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0000a400280"
      , "19012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c01"
      , "225820"
      ]

{- | Aiken @env.empty_merkle_tree_root@.

The MPF root of the empty trie. It is a sentinel throughout the counted-root
scheme: a root equal to this means "no entries", and must pair with a count of
zero.
-}
pemptyMerkleTreeRoot :: forall (s :: S). Term s PByteString
pemptyMerkleTreeRoot =
  phexByteStr "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

{- | Aiken @env.plutarch_phas_validator_hash@.

The script hash of the Plutarch membership staking validator — the @phas@ script
generated by this very package (see "MerkleTree.Validators.Membership"). Merkle
membership is proved by that script's withdrawal, and the callers here only
check its redeemer.

This constant must track the generated script: it is the deployed hash of
@membership-stake.plutus.json@, and if that script changes without this being
updated, every membership proof silently checks the wrong script's redeemer.
-}
pplutarchPhasValidatorHash :: forall (s :: S). Term s PByteString
pplutarchPhasValidatorHash =
  phexByteStr "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

{- | Aiken @env.plutarch_pexcludes_validator_hash@.

The @pexcludes@ twin of 'pplutarchPhasValidatorHash': the staking validator that
proves a key is /absent/ from a Merkle root. Same delegation shape, same
re-pinning obligation.

Note that this port's @pexcludes@ script and Aiken's are not currently the same
script — see the MPF arithmetic divergence in @README.md@ — so this constant
being the Aiken tree's value is a statement about the deployed script, not about
what "MerkleTree.Validators.Membership" would compile to today.
-}
pplutarchPexcludesValidatorHash :: forall (s :: S). Term s PByteString
pplutarchPexcludesValidatorHash =
  phexByteStr "a9ec251d6476217b1abccd5f035dec1272a4b04f640f503fca9e734d"

{- | Aiken @env.mpf_chunked_verify_validator_hash@.

The merkelized verifier of published-chunk proof carriage (issue #545), compiled
from @validators/mpf-chunked-verify.ak@. A step that takes the published-chunk
route names this hash so that the walk over the chunk UTxOs runs once, in its
own script, rather than inside every step that wants a proof.
-}
pmpfChunkedVerifyValidatorHash :: forall (s :: S). Term s PByteString
pmpfChunkedVerifyValidatorHash =
  phexByteStr "cb5a7ec4def35ce3ec75c40919992e1b4e8839b4f6b6a2d3b06e7469"

{- | Aiken @env.max_validity_range_length@.

The widest a "short" validity range may be — eight minutes in milliseconds. Block headers bind their event
interval's end to the commit transaction's upper bound, so an unbounded range
would let an operator claim an arbitrarily wide interval for one block.
-}
pmaxValidityRangeLength :: forall (s :: S). Term s PInteger
pmaxValidityRangeLength = 8 * 60 * 1000
