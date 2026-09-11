{-# LANGUAGE CPP #-}

{- | Compiled Aiken environment. Cabal's explicit @-ftestnet@ flag selects
@env/testnet.ak@; the ordinary build selects @env/default.ak@. Environment
selection changes validator hashes and is never read from a transaction.
-}
module Midgard.Env (
  environmentName,
  pemptyData,
  pslashingPenalty,
  pfraudProverReward,
  prequiredBond,
  pinactivitySlashingPenalty,
  pregistrationDuration,
  pmaxInactivityStrikes,
  pcoinsPerUtxoByte,
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

-- | The environment compiled into this library and its exported validators.
environmentName :: String
#ifdef MIDGARD_TESTNET
environmentName = "testnet"
#else
environmentName = "default"
#endif

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
#ifdef MIDGARD_TESTNET
pslashingPenalty = 500_000_000
#else
pslashingPenalty = 25_000_000_000
#endif

-- | Aiken @env.fraud_prover_reward@.
pfraudProverReward :: forall (s :: S). Term s PInteger
#ifdef MIDGARD_TESTNET
pfraudProverReward = 400_000_000
#else
pfraudProverReward = 75_000_000_000
#endif

-- | Aiken @env.required_bond = slashing_penalty + fraud_prover_reward@.
prequiredBond :: forall (s :: S). Term s PInteger
prequiredBond = pslashingPenalty + pfraudProverReward

-- | Aiken @env.inactivity_slashing_penalty@.
pinactivitySlashingPenalty :: forall (s :: S). Term s PInteger
#ifdef MIDGARD_TESTNET
pinactivitySlashingPenalty = 100_000_000
#else
pinactivitySlashingPenalty = 10_000_000_000
#endif

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

-- | Aiken @env.coins_per_utxo_byte@, pinned to the target-chain snapshot.
pcoinsPerUtxoByte :: forall (s :: S). Term s PInteger
pcoinsPerUtxoByte = 4_310

-- | Aiken @env.shift_duration@: 30 milliseconds in default, one hour in testnet.
pshiftDuration :: forall (s :: S). Term s PInteger
#ifdef MIDGARD_TESTNET
pshiftDuration = 60 * 60 * 1000
#else
pshiftDuration = 30
#endif

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

Copied verbatim from @env/default.ak@. It is an opaque constant here:
this package does not compile the witness script, so a change on the Aiken side
must be copied across or the derived hashes diverge.
-}
puserEventsWitnessScriptPrefix :: forall (s :: S). Term s PByteString
puserEventsWitnessScriptPrefix =
  phexByteStr $
    concat
      [ "5902e20101003229800aba2aba1aab9faab9eaab9dab9a9bae00248888889660026465300130"
      , "0800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d200"
      , "29b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c60"
      , "2600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd500"
      , "5456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e20"
      , "1c40382653001300100198071baa0099180918099809980998099809800a444b300130070028"
      , "9919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801"
      , "001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501"
      , "644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c"
      , "064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c80"
      , "1401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd500"
      , "31bae301430123754019159800980400144c8c966002003168992cc004cdd780080445660026"
      , "01460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea80"
      , "0629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540"
      , "146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea8"
      , "0062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033"
      , "758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2"
      , "b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e6020008"
      , "8b200e180400098021baa0088a4d1365640084c1225820"
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
  phexByteStr "03adaadf3154dafde48eea40030cecf5690b07c495f4c74029e4ab6a"

{- | Aiken @env.mpf_chunked_verify_validator_hash@.

The merkelized verifier of published-chunk proof carriage (issue #545), compiled
from @validators/mpf-chunked-verify.ak@. A step that takes the published-chunk
route names this hash so that the walk over the chunk UTxOs runs once, in its
own script, rather than inside every step that wants a proof.
-}
pmpfChunkedVerifyValidatorHash :: forall (s :: S). Term s PByteString
pmpfChunkedVerifyValidatorHash =
  phexByteStr "dfd0e01fe351bd1d6f75a1ba728d06fb8b11d56bc3bf9ee98e025040"

{- | Aiken @env.max_validity_range_length@.

The widest a "short" validity range may be — eight minutes in milliseconds. Block headers bind their event
interval's end to the commit transaction's upper bound, so an unbounded range
would let an operator claim an arbitrarily wide interval for one block.
-}
pmaxValidityRangeLength :: forall (s :: S). Term s PInteger
pmaxValidityRangeLength = 8 * 60 * 1000
