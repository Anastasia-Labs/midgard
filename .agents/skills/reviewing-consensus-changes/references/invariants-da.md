# Data-availability invariants: committee, attestation and challenge

Scope: `onchain/aiken/validators/da-params-governor.ak`, `da-attestation.ak`,
`availability-challenge.ak`, `onchain/aiken/lib/midgard/availability-challenge*.ak`
and the SDK availability builders.

Status, recurrence and line conventions are as in
[invariants-state-queue.md](invariants-state-queue.md). This file is shallower
than the state-queue and fraud-proof files: it covers the governor, the
attestation apply and rescue paths, and one known gap.

## DA1. Governed thresholds never drop below two thirds

Status: VERIFIED.

Rule: for a committee or owner set of size `n >= 1`, the threshold lies in
`[ceil(2n/3), n]`, both at mint and on every continued datum.

Enforced: `governed_threshold_floor` and `valid_datum`
(`validators/da-params-governor.ak:126-163`) `[aiken-test: da-params-governor/]`
(`da_params_governor_rejects_empty_owner_set` `:717`,
`da_params_governor_mint_rejects_initial_datum_below_floor` `:845`,
`da_params_governor_spend_rejects_continued_datum_below_floor` `:874`; all
fail).

Provenance: `989753337` (2026-08-07, Q63) introduced the floor. The review of
that work (`535e2e836`, `9b3cca8db`) found that every test called the private
`valid_datum` directly, so deleting the handler's call left all of them green;
tests now drive the real spend and mint handlers. `ac67670d1` (#602) settled
the `ceil(2n/3)` floor with `n >= 1`.

Recurrence: 3. Review action: a test of a private predicate does not prove
the handler calls it.

## DA2. An attestation applies only under the parameters that signed it

Status: VERIFIED.

Rule: applying an attestation requires its committee hash and threshold to
equal the current governed parameters, read from an authentic reference
input, and its count to meet that threshold.

Enforced: `validators/da-attestation.ak:497-501`
`[aiken-test: da-attestation/]` (`da_attestation_apply_rejects_rotated_committee`
`:1408`, `da_attestation_apply_rejects_governed_threshold_change` `:1421`,
`da_attestation_apply_rejects_stale_params_reference_input` `:1456`,
`da_attestation_apply_rejects_old_committee_quorum` `:1475`; all fail).

Provenance: `b6a414310` (2026-08-07, Q62: non-retroactive committee
rotation).

## DA3. A stranded attestation can always be rescued

Status: VERIFIED.

Rule: an attestation left behind by a committee rotation or threshold change
can be closed with a full refund to its beneficiary, and only when the
parameters really changed.

Enforced: `RescueStrandedAttestation` (`validators/da-attestation.ak:548-600`)
`[aiken-test: da-attestation/]`
(`da_attestation_rescue_rejects_unrotated_committee_attestation` `:1062`,
`da_attestation_rescue_rejects_replayed_mint_binding` `:1096`,
`da_attestation_rescue_control_threshold_change_refunds_quorum_attestation`
`:1112`, `da_attestation_rescue_rejects_refund_short_of_attestation_value`
`:1189`, `da_attestation_rescue_rejects_redirected_beneficiary` `:1203`).

Provenance: `b6a414310` (Q63c, partial-attestation rescue). Rotation without
a rescue path strands funds, so DA2 and DA3 change together.

Recurrence: 2.

## DA4. Availability-challenge fees are capped

Status: PARTIAL (the settle and timeout caps are tested; the open and close
caps were read but no refusing test was checked).

Rule: every availability-challenge transition pays a positive fee no larger
than its governed cap.

Enforced: `lib/midgard/availability-challenge-validation.ak:393-394` (open),
`:674-675` (settle), `:756-757` (close)
`[aiken-test: availability-challenge.test/]`
(`q58_settle_rejects_excessive_fee` `:2198`,
`q58_settle_rejects_batched_second_tranche_fee_charge` `:2218`,
`q58_timeout_rejects_excessive_fee` `:2337`; all fail).

Provenance: `3e3090aa1` (2026-08-31, the availability-challenge wave).

## Known gap: no release for an unchallenged DA bond

Status: PARTIAL (gap recorded, no fix).

`availability.MintRedeemerV1` (`lib/midgard/availability-challenge.ak:77-130`)
has `MintBondFromAttestation`, `OpenChallenge`, `SettleTranche`,
`CloseChallenge`, `TimeoutChallenge` and `Yield`, and no arm that releases a
bond that was never challenged. A change in this area must say whether it
closes, keeps or widens that gap.
