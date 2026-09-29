# Data-availability invariants: committee, attestation and challenge

Scope: `onchain/aiken/validators/da-params-governor.ak`, `da-attestation.ak`,
`availability-challenge.ak`, `onchain/aiken/lib/midgard/availability-challenge*.ak`
and the SDK availability builders.

Status, recurrence and line conventions are as in
[invariants-state-queue.md](invariants-state-queue.md). This file is shallower
than the state-queue and fraud-proof files: it covers the governor, the
attestation apply and rescue paths, the pooled committee bond at apply and at
timeout, and the commitment binding a challenge record carries.

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
than its governed cap. At timeout the cap bounds only the challenger's share
`c = tx.fee - fee_part`; the slashed penalty share is burned on top of it
(DA8).

Enforced: `lib/midgard/availability-challenge-validation.ak:292-293` (open),
`:561-562` (settle), `:638-639` (close), `:877-879` (timeout: `tx.fee > 0`,
`0 <= c <= max_timeout_fee_lovelace`)
`[aiken-test: availability-challenge.test/]`
(`q58_settle_rejects_excessive_fee` `:2227`,
`q58_settle_rejects_batched_second_tranche_fee_charge` `:2247`,
`q58_timeout_rejects_challenger_fee_one_above_the_cap` `:3378`,
`..._on_an_empty_pool` `:3391`, `q58_timeout_rejects_zero_fee_on_an_empty_pool`
`:3403`; all fail).

Provenance: `3e3090aa1` (2026-08-31, the availability-challenge wave); #693
moved the timeout cap onto `c` (decision D4).

## DA5. An attestation applies only while the pooled bond backs it, and its value returns to its beneficiary

Status: VERIFIED (code and tests re-read at the cited lines in the
restacked tree, base `77244f439`; provenance is #688, `f8c91dae8`).

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

Provenance: #688 (`f8c91dae8`; spec #685, decision C3; the refund index is
an amendment that closed the submitter taking the attestation's lovelace).

## DA6. A node's DA status binds one commitment, and every challenge step presents its preimage

Status: VERIFIED (code and tests re-read at the cited lines in the
restacked tree, base `77244f439`; provenance is #688, `f8c91dae8`, and #693,
`dd830b918`).

Rule: apply writes `Attested{commitment_hash_v1(burned commitment)}`, with the
commitment canonical and naming this deployment and header. An open decodes
the commitment from its record output and requires the node's input status to
be `Attested{commitment_hash_v1(record.commitment)}`; close and the correction
lock's timeout path compare the node against both
`Challenged{commitment_hash_v1(record.commitment), record.challenge_asset_name}`.
Only the status may change across these transitions: the node's value,
address, link, header and fraud marker are carried exactly, and the node
output carries no reference script (SQ10 in
[invariants-state-queue.md](invariants-state-queue.md)).

Enforced: `commitment_hash_v1` (`lib/midgard/availability-challenge.ak:226`,
the one copy); the shared node core `da_availability_status_transition`
(`lib/midgard/state-queue.ak:504`, value equality `:521`, reference-script pin
`:522`); the timeout node match `timeout_node_status_matches`
(`lib/midgard/availability-challenge-validation.ak:765`)
`[aiken-test: availability-challenge.test/]`
(`da_attestation_apply_rejects_node_attested_to_another_commitment`
`validators/da-attestation.ak:1930`;
`availability_open_rejects_record_preimage_of_another_attested_hash` `:2625`,
`q58_close_rejects_node_challenged_under_another_commitment` `:2975` and
`..._under_another_challenge` `:2989`,
`q58_timeout_rejects_node_challenged_under_another_commitment` `:2362` and
`..._under_another_challenge` `:3600`; all fail). The reference-script pin is
refused through each user: Apply
(`da_attestation_apply_rejects_node_output_reference_script`
`validators/da-attestation.ak:1989`, control `:1656`), Open
(`availability_open_rejects_state_queue_output_with_reference_script`
`:2798`, control `q58_open_accepts_exact_distinct_inputs_outputs_and_signer`
`:1996`), Close (`q58_close_rejects_state_queue_output_with_reference_script`
`:3031`, control `q58_close_accepts_exact_distinct_terminal_outputs` `:1973`),
and at the core (`da_core_apply_rejects_node_output_reference_script`,
`da_core_open_...` and `da_core_close_...` at
`lib/midgard/state-queue.test.ak:667-683`, control `:655`); all fail.

Provenance: #688 (`f8c91dae8`; decisions C1, C4, C5, G6); #693
(`dd830b918`; ruling P2) added the reference-script pin.

## DA7. A challenge record is authentic only at the availability address holding its token

Status: VERIFIED (code and tests re-read at the cited lines in the
restacked tree, base `77244f439`; provenance is #688, `f8c91dae8`).

Rule: open pins the record output exactly (availability address, no reference
script, `challenge_record_lovelace` plus the one minted DACH, inline datum
equal to the expected record, `opened_at` the inclusive upper bound). Settle,
close and timeout accept a record only through `authenticated_challenge_record`
(address and exact value); the correction lock's Idle acquire reads it from
the one availability-address input holding the DACH. An open lands strictly
before `header.end_time + da_challenge_window_ms_v1`.

Enforced: `lib/midgard/availability-challenge-validation.ak:73` (reader),
`:316` (window), `:331` (record datum); `validators/correction-lock.ak`
Idle acquire `[aiken-test: availability-challenge.test/]`
(`availability_open_accepts_last_millisecond_of_challenge_window` `:2019`,
`availability_open_rejects_upper_bound_at_challenge_window_end` `:2028`,
`availability_open_rejects_record_at_foreign_script_address` `:2669`,
`q58_settle_rejects_record_at_foreign_address` `:2892`,
`q58_close_rejects_record_without_challenge_token` `:2959`)
`[aiken-test: correction-lock/]`
(`correction_lock_handler_rejects_availability_acquire_with_record_at_foreign_address`
`:784`, `..._with_record_without_challenge_token` `:803`; all fail).

The index-distinctness checks of open (`challenger_input_index` and
`record_output_index` against the state-queue indices, `:294-295`), close
(`:642-645`) and timeout (`:881-883`, `:885`: record against terminal input,
pool input against record and terminal inputs, pool output against challenger
output) are implied by the address, value and token shapes each role must
have, and none can fail alone: with any one removed, every test stays green,
including the `q58_*_aliased_*` tests, whose refusals are overdetermined. They are defence in depth, as in DA5.

Provenance: #688 (`f8c91dae8`; decisions C4, C6, G6).

## DA8. A lost challenge charges the pool, up to one bond, in the timeout transaction

Status: VERIFIED (code and tests re-read at the cited lines in the
restacked tree, base `77244f439`; provenance is #693, `dd830b918`).

Rule: `TimeoutChallenge` spends the authentic DA bond pool at
`pool_input_index` (payment credential `Script(da_bond_pool_policy_id)`, NFT
quantity 1, inline datum). The pool input is mandatory even at zero backing,
and a `Withdrawing` pool is slashable. With the clamped backing
`max(0, lovelace - floor)`:

- `taken = min(da_bond, backing)`, `fee_part = min(penalty, taken)`,
  `payout = taken - fee_part`;
- the pool output at `pool_output_index` keeps the input's address and datum
  exactly, carries no reference script, and holds `pool_in - taken` lovelace
  plus the NFT and no other token;
- `tx.fee == fee_part + c` with `0 <= c <= max_timeout_fee_lovelace`, and `c`
  comes out of the challenger's remaining reserve;
- exactly one output pays the challenger, carrying
  `remaining - c + challenge_record_lovelace + payout` (refund and payout
  merged, D3, so a payout below min-UTxO never makes the timeout unbuildable);
- the pool input differs from the record and terminal inputs, and the pool
  output from the challenger output. These index checks are defence in depth
  (DA7): none can fail alone, because the pool input and output must carry the
  pool NFT at the pool script and the challenger output must be an exact
  enterprise ADA output.

The pool's `Slash` arm runs only beside this rule (B4, G4). It requires the
state-queue mint redeemer to be constructor 5
(`RemoveUnavailableBlockAfterTimeout`) and the correction lock to be `Idle`.
At an Idle lock that redeemer forces a DACH burn of -1
(`validators/state-queue.ak:1373-1374`; a `Locked` resume step mints 0 at
`:1375-1378`), and only `TimeoutChallenge` can make it there. Open mints and
takes two inputs (`lib/midgard/availability-challenge-validation.ak:290`,
`:324`), Settle burns only a tranche token (`:583`), and Close takes exactly
three inputs and two outputs (`:640-641`) and continues the node to
`Published` (`:661`). The pool NFT is unique, so Slash and the yield read the
same pool output; TopUp and the three withdraw arms each contradict the
yield's value or datum equality.

Enforced: `validate_timeout_challenge`
(`lib/midgard/availability-challenge-validation.ak:807`; split `:859-871`,
fee `:877-879`, indices `:882-885` (defence in depth), pool output
`:886-892`, challenger output `:925-931`) with the pool reader
`get_authentic_pool_input`
(`lib/midgard/da-bond-pool.ak:97`); the pool `Slash` arm
(`validators/da-bond-pool.ak:182-220`, constructor 5 at `:198-199`, Idle lock
at `:201-205`) `[aiken-test: availability-challenge.test/]`. Accepted with
exact amounts: `q58_timeout_accepts_full_pool_slash_with_the_penalty_as_the_fee`
`:2304` and `q58_timeout_accepts_*` at `:3131-3271` (above one bond, partial,
below the penalty, empty pool, dust payout, `Withdrawing` keeping
`unlock_at`, `c` at the cap). Refused, all fail:
`q58_timeout_rejects_without_pool_input` `:3296`,
`..._pool_index_naming_a_non_pool_input` `:3287`,
`..._counterfeit_pool_without_nft` `:3316`,
`..._pool_nft_at_another_credential` `:3333`,
`..._fee_one_lovelace_below_the_penalty_share` `:3355`,
`..._negative_challenger_fee_on_a_short_pool` `:3362`,
`..._challenger_fee_one_above_the_cap` `:3378`,
`..._merged_challenger_output_one_lovelace_short` `:3418` and `_over` `:3424`,
`..._second_challenger_output` `:2376`,
`..._refund_and_payout_split_across_two_outputs` `:3432`, the
`q58_timeout_rejects_pool_output_*` family at `:3452-3555` (lovelace ±1, no
NFT, extra token, datum changed either way or to another `unlock_at`, stake
credential, reference script), and
`..._pool_output_index_aliased_with_challenger_output` `:3570`, which the
address and value shapes refuse, not `:885` alone (with `:882`, `:883` and
`:885` deleted it still fails; no test aliases the pool input with the record
or terminal input). The reader:
`da_bond_pool_input_reader_*` at `lib/midgard/da-bond-pool.test.ak:201-241`
`[aiken-test: midgard/da-bond-pool.test/]`. The Slash arm alone:
`da_bond_pool_slash_rejects_without_state_queue_redeemer` `:773`,
`..._other_state_queue_redeemer` `:797` and
`..._resume_step_locked_correction_lock` `:824`, control
`da_bond_pool_slash_accepts_honest_bonded` `:730`
`[aiken-test: da-bond-pool.test/]`. The whole transaction (pool Slash,
availability mint, timeout yield, state-queue removal, correction lock) runs
every script in `[aiken-test: da-bond-pool-timeout.test/]`: accepted
`pool_timeout_e2e_*_pool_prune` and `_head` at `:1208-1314`; refused, each
with an every-other-script-accepts control,
`pool_timeout_e2e_resume_step_*_pool_refuses_slash` `:1364-1368`,
`..._close_swap_*_close_yield_refuses` `:1421-1425`,
`..._optimised_close_*_close_yield_refuses` `:1434-1438` and
`..._without_pool_*_timeout_yield_refuses` `:1467-1471` (prune and head
each), and `..._top_up_in_place_of_slash_pool_refuses` `:1498`.

Liability is one bond per withholding episode, not one pool. A timeout needs
the withheld block to be the queue head: `remove_unavailable_head_v1`
(`validators/state-queue.ak:1006`) requires `removed_link == None` (`:1048`),
and `prune_unavailable_block_descendant_v1` (`:941`) requires the head link to
be the unavailable header's hash (`:964`). The slash runs only on the first,
Idle-lock step; the blocks queued after the withheld one are pruned in
`Locked` resume steps without a slash. Apply requires a `Bonded` pool with one
full bond of backing (DA5). So no block applied while the pool was full
survives a slash to face a smaller pool, and the pool is never slashed twice
for blocks applied before the first slash. A pool below one bond (`taken =
backing < da_bond`) is reachable only by a late timeout after the owners'
`CompleteWithdraw`. The withdrawal delay puts `unlock_at` at or after the
latest timely timeout (A6 in
[the decision record](../../../../docs/midgard/decisions/da-committee-bond-pool.md)),
so a timely challenger always meets a full bond `[review]`. Review action: a
change that lets a timeout remove a non-head block, or lets Apply run against
less than one bond, breaks this bound.

Provenance: #693 (`dd830b918`; decisions D1-D4, G2, G3, G4, B4); the
one-bond-per-episode bound is ruling P7. The interim rule it replaces, from
#688 (`f8c91dae8`), refunded the challenger and took nothing from the pool.
Blind spot: Slash never reads the availability mint redeemer; against a
Close in its place it relies on Close's input and output counts and its node
transition, so relaxing Close (for example, batching it) reopens a drain
`[review]`.
