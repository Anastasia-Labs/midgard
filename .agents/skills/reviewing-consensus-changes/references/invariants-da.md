# Data-availability invariants: committee, attestation and challenge

Scope: `onchain/aiken/validators/da-params-governor.ak`, `da-attestation.ak`,
`availability-challenge.ak`, `onchain/aiken/lib/midgard/availability-challenge*.ak`
and the SDK availability builders.

Status, recurrence and line conventions are as in
[invariants-state-queue.md](invariants-state-queue.md). This file is shallower
than the state-queue and fraud-proof files: it covers the governor, the
attestation apply and rescue paths, the pooled committee bond at apply, and
the commitment binding a challenge record carries.

## DA1. Governed thresholds never drop below two thirds

Status: VERIFIED.

Rule: for a committee or owner set of size `n >= 1`, the threshold lies in
`[ceil(2n/3), n]`, both at mint and on every continued datum.

Enforced: `governed_threshold_floor` and `valid_datum`
(`validators/da-params-governor.ak:127-166`) `[aiken-test: da-params-governor/]`
(`da_params_governor_rejects_empty_owner_set` `:683`,
`da_params_governor_mint_rejects_initial_datum_below_floor` `:811`,
`da_params_governor_spend_rejects_continued_datum_below_floor` `:840`; all
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

Enforced: `validators/da-attestation.ak:461-471`
`[aiken-test: da-attestation/]` (`da_attestation_apply_rejects_rotated_committee`
`:1476`, `da_attestation_apply_rejects_governed_threshold_change` `:1489`,
`da_attestation_apply_rejects_stale_params_reference_input` `:1524`,
`da_attestation_apply_rejects_old_committee_quorum` `:1543`; all fail).

Provenance: `b6a414310` (2026-08-07, Q62: non-retroactive committee
rotation).

## DA3. A stranded attestation can always be rescued

Status: VERIFIED.

Rule: an attestation left behind by a committee rotation or threshold change
can be closed with a full refund to its beneficiary, and only when the
parameters really changed.

Enforced: `RescueStrandedAttestation` (`validators/da-attestation.ak:534-590`)
`[aiken-test: da-attestation/]`
(`da_attestation_rescue_rejects_unrotated_committee_attestation` `:1090`,
`da_attestation_rescue_rejects_replayed_mint_binding` `:1124`,
`da_attestation_rescue_control_threshold_change_refunds_quorum_attestation`
`:1140`, `da_attestation_rescue_rejects_refund_short_of_attestation_value`
`:1218`, `da_attestation_rescue_rejects_redirected_beneficiary` `:1232`).

Provenance: `b6a414310` (Q63c, partial-attestation rescue). Rotation without
a rescue path strands funds, so DA2 and DA3 change together.

Recurrence: 2.

## DA4. Availability-challenge fees are capped

Status: PARTIAL (the settle and timeout caps are tested; the open and close
caps were read but no refusing test was checked).

Rule: every availability-challenge transition pays a positive fee no larger
than its governed cap.

Enforced: `lib/midgard/availability-challenge-validation.ak:290-291` (open),
`:559-560` (settle), `:636-637` (close), `:843-844` (timeout)
`[aiken-test: availability-challenge.test/]`
(`q58_settle_rejects_excessive_fee` `:2140`,
`q58_settle_rejects_batched_second_tranche_fee_charge` `:2160`,
`q58_timeout_rejects_excessive_fee` `:2236`; all fail).

Provenance: `3e3090aa1` (2026-08-31, the availability-challenge wave).

## DA5. An attestation applies only while the pooled bond backs it, and its value returns to its beneficiary

Status: PARTIAL (tests and mutation runs read on the #688 branch, not yet
committed; no provenance commit to cite).

Rule: `ApplyToStateQueue` reads the authentic DA bond pool (payment credential
`Script(da_bond_pool_policy_id)`, NFT quantity 1, inline datum) and requires
`Bonded` with `pool_backing >= da_bond_lovelace`, where the backing excludes
the pool floor. The attestation's whole value, less the burnt token, goes to
`rescue_beneficiary` at `refund_output_index`, which differs from the node
output. Whoever submits the apply takes nothing.

Enforced: `validators/da-attestation.ak:478-515`
`[aiken-test: da-attestation/]`
(`da_attestation_apply_control_exact_pool_backing_and_exact_refund` `:1656`
passes at exactly one bond; `..._rejects_pool_backing_below_one_bond`,
`..._rejects_withdrawing_pool`, `..._rejects_pool_nft_at_foreign_credential`,
`..._rejects_pool_datum_hash`, `..._rejects_short_refund` and
`..._rejects_refund_to_foreign_address` at `:1700-1832`; all fail).
The two index-distinctness checks are implied by the token and datum shapes
and cannot fail alone; they are defence in depth.

Provenance: #688 (spec #685, decision C3; the refund index is an amendment
that closed the submitter taking the attestation's lovelace).

## DA6. A node's DA status binds one commitment, and every challenge step presents its preimage

Status: PARTIAL (tests and mutation runs read on the #688 branch, not yet
committed).

Rule: apply writes `Attested{commitment_hash_v1(burned commitment)}`, with the
commitment canonical and naming this deployment and header. An open decodes
the commitment from its record output and requires the node's input status to
be `Attested{commitment_hash_v1(record.commitment)}`; close and the correction
lock's timeout path compare the node against both
`Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
Only the status may change across these transitions: the node's value,
address, link, header and fraud marker are carried exactly.

Enforced: `commitment_hash_v1` (`lib/midgard/availability-challenge.ak:219`,
the one copy); the shared node core `da_availability_status_transition`
(`lib/midgard/state-queue.ak:470`, value equality `:487`); the timeout node
match `timeout_node_status_matches`
(`lib/midgard/availability-challenge-validation.ak:763`)
`[aiken-test: availability-challenge.test/]`
(`da_attestation_apply_rejects_node_attested_to_another_commitment`
`validators/da-attestation.ak:1930`;
`availability_open_rejects_record_preimage_of_another_attested_hash` `:2538`,
`q58_close_rejects_node_challenged_under_another_commitment` `:2866` and
`..._under_another_challenge` `:2880`,
`q58_timeout_rejects_node_challenged_under_another_commitment` `:2278` and
`..._under_another_challenge` `:2966`; all fail).
Blind spot: the node output's `reference_script` is not pinned by this core
`[review]`.

Provenance: #688 (decisions C1, C4, C5, G6).

## DA7. A challenge record is authentic only at the availability address holding its token

Status: PARTIAL (tests and mutation runs read on the #688 branch, not yet
committed).

Rule: open pins the record output exactly (availability address, no reference
script, `challenge_record_lovelace` plus the one minted DACH, inline datum
equal to the expected record, `opened_at` the inclusive upper bound). Settle,
close and timeout accept a record only through `authenticated_challenge_record`
(address and exact value); the correction lock's Idle acquire reads it from
the one availability-address input holding the DACH. An open lands strictly
before `header.end_time + da_challenge_window_ms_v1`.

Enforced: `lib/midgard/availability-challenge-validation.ak:71` (reader),
`:314` (window), `:329` (record datum); `validators/correction-lock.ak`
Idle acquire `[aiken-test: availability-challenge.test/]`
(`availability_open_accepts_last_millisecond_of_challenge_window` `:1936`,
`availability_open_rejects_upper_bound_at_challenge_window_end` `:1945`,
`availability_open_rejects_record_at_foreign_script_address` `:2582`,
`q58_settle_rejects_record_at_foreign_address` `:2783`,
`q58_close_rejects_record_without_challenge_token` `:2850`)
`[aiken-test: correction-lock/]`
(`correction_lock_handler_rejects_availability_acquire_with_record_at_foreign_address`
`:784`, `..._with_record_without_challenge_token` `:803`; all fail).

The index-distinctness checks of open (`challenger_input_index` and
`record_output_index` against the state-queue indices, `:292-293`), close
(`:640-643`) and timeout (`:846`) are implied by the address, value and token
shapes each role must have, and none can fail alone: with any one removed,
every test stays green, including the `q58_*_aliased_*` tests, whose
refusals are overdetermined. They are defence in depth, as in DA5.

Provenance: #688 (decisions C4, C6, G6).

## Known gap: the interim timeout does not bind the pool slash

Status: PARTIAL (gap recorded; closed by #693).

The per-block DA bond, its mint arm and its yield are deleted (#688); the
pooled committee bond (#687) has its own quorum withdrawal. Until #693, a
timeout burns the record and refunds the challenger
(`q58_timeout_accepts_interim_record_refund_to_challenger`
`validators/availability-challenge.test.ak:2217`), and
`validate_timeout_challenge` neither reads the pool nor pays a slash;
`TimeoutChallenge.da_slash_output_index` is unread.

The pool is still exposed. The pool's `Slash` arm
(`validators/da-bond-pool.ak:182-222`) checks only that the state-queue mint
redeemer is constructor 5 and that the correction lock is `Idle`, then that
the pool keeps `pool_in - min(da_bond, backing)`. It does not pin where the
taken lovelace goes. An interim timeout transaction meets both conditions, so
whoever submits it can also spend the pool through `Slash` and send up to one
bond to any address. The loss is capped at one bond per expired challenge.
#693 closes it: the timeout yield requires the pool input, pins the pool
output, and pays `taken` to the fee (`fee_part`) and the one challenger output
(`payout`) (decisions D1-D3). A change in this area must say whether it
closes, keeps or widens that gap.
