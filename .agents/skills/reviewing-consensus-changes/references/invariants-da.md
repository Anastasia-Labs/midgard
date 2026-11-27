# Data-availability invariants: committee, attestation and challenge

Scope: `onchain/aiken/validators/da-params-governor.ak`, `da-attestation.ak`,
`availability-challenge.ak`, `onchain/aiken/lib/midgard/availability-challenge*.ak`
and the SDK availability builders.

Status and citation conventions are as in
[invariants-state-queue.md](invariants-state-queue.md). This file is shallower
than the state-queue and fraud-proof files: it covers the governor, the
attestation apply and rescue paths, the pooled committee bond at apply and at
timeout, the commitment binding a challenge record carries, and the timeout's
separation from the operator directories.

## DA1. Governed thresholds never drop below two thirds

Status: VERIFIED.

Rule: for a committee or owner set of size `n >= 1`, the threshold lies in
`[ceil(2n/3), n]`, both at mint and on every continued datum.

Enforced: `governed_threshold_floor` and `valid_datum`
(`validators/da-params-governor.ak`) `[aiken-test: da-params-governor/]`
(`da_params_governor_rejects_empty_owner_set`,
`da_params_governor_mint_rejects_initial_datum_below_floor`,
`da_params_governor_spend_rejects_continued_datum_below_floor`; all
fail).

Review action: a test of a private predicate does not prove the handler
calls it; the tests above drive the real spend and mint handlers.

## DA2. An attestation applies only under the parameters that signed it

Status: VERIFIED.

Rule: applying an attestation requires its committee hash and threshold to
equal the current governed parameters, read from an authentic reference
input, and its count to meet that threshold.

Enforced: `validators/da-attestation.ak`
`[aiken-test: da-attestation/]` (`da_attestation_apply_rejects_rotated_committee`, `da_attestation_apply_rejects_governed_threshold_change`,
`da_attestation_apply_rejects_stale_params_reference_input`,
`da_attestation_apply_rejects_old_committee_quorum`; all fail).

## DA3. A stranded attestation can always be rescued

Status: VERIFIED.

Rule: an attestation left behind by a committee rotation or threshold change
can be closed with a full refund to its beneficiary, and only when the
parameters really changed.

Enforced: `RescueStrandedAttestation` (`validators/da-attestation.ak`)
`[aiken-test: da-attestation/]`
(`da_attestation_rescue_rejects_unrotated_committee_attestation`,
`da_attestation_rescue_rejects_replayed_mint_binding`,
`da_attestation_rescue_control_threshold_change_refunds_quorum_attestation`, `da_attestation_rescue_rejects_refund_short_of_attestation_value`, `da_attestation_rescue_rejects_redirected_beneficiary`).

Rotation without a rescue path strands funds, so DA2 and DA3 change
together.

## DA4. Availability-challenge fees are capped

Status: PARTIAL (the settle and timeout caps are tested; the open and close
caps were read but no refusing test was checked).

Rule: every availability-challenge transition pays a positive fee no larger
than its governed cap. At timeout the cap bounds only the challenger's share
`c = tx.fee - fee_part`; the slashed penalty share is burned on top of it
(DA8).

Enforced: `lib/midgard/availability-challenge-validation.ak` (open, settle,
close and timeout: `tx.fee > 0`, `0 <= c <= max_timeout_fee_lovelace`)
`[aiken-test: availability-challenge.test/]`
(`settle_rejects_excessive_fee`,
`settle_rejects_batched_second_tranche_fee_charge`,
`timeout_rejects_challenger_fee_one_above_the_cap`,
`..._on_an_empty_pool`, `timeout_rejects_zero_fee_on_an_empty_pool`; all fail).

## DA5. An attestation applies only while the pooled bond backs it, and its value returns to its beneficiary

Status: VERIFIED (code and tests re-read at the cited lines as of
2026-09-28).

Rule: `ApplyToStateQueue` reads the authentic DA bond pool (payment credential
`Script(da_bond_pool_policy_id)`, NFT quantity 1, inline datum) and requires
`Bonded` with `pool_backing >= da_bond_lovelace`, where the backing excludes
the pool floor. The attestation's whole value, less the burnt token, goes to
`rescue_beneficiary` at `refund_output_index`, which differs from the node
output. Whoever submits the apply takes nothing.

Enforced: `validators/da-attestation.ak`
`[aiken-test: da-attestation/]`
(`da_attestation_apply_control_exact_pool_backing_and_exact_refund`
passes at exactly one bond; `..._rejects_pool_backing_below_one_bond`,
`..._rejects_withdrawing_pool`, `..._rejects_pool_nft_at_foreign_credential`,
`..._rejects_pool_datum_hash`, `..._rejects_short_refund` and
`..._rejects_refund_to_foreign_address`; all fail).
The two index-distinctness checks are implied by the token and datum shapes
and cannot fail alone; they are defence in depth.

Decision: the [DA bond pool decision](../../../../docs/midgard/decisions/da-committee-bond-pool.md).

## DA6. A node's DA status binds one commitment, and every challenge step presents its preimage

Status: VERIFIED (code and tests re-read at the cited lines as of
2026-09-28).

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

Enforced: `commitment_hash_v1` (`lib/midgard/availability-challenge.ak`,
the one copy); the shared node core `da_availability_status_transition`
(`lib/midgard/state-queue.ak`: value equality and the reference-script
pin); the timeout node match `timeout_node_status_matches`
(`lib/midgard/availability-challenge-validation.ak`)
`[aiken-test: availability-challenge.test/]`
(`da_attestation_apply_rejects_node_attested_to_another_commitment` in
`validators/da-attestation.ak`;
`availability_open_rejects_record_preimage_of_another_attested_hash`,
`close_rejects_node_challenged_under_another_commitment` and
`..._under_another_challenge`,
`timeout_rejects_node_challenged_under_another_commitment` and
`..._under_another_challenge`; all fail). The reference-script pin is
refused through each user: Apply
(`da_attestation_apply_rejects_node_output_reference_script` in
`validators/da-attestation.ak`, beside its control), Open
(`availability_open_rejects_state_queue_output_with_reference_script`, control `open_accepts_exact_distinct_inputs_outputs_and_signer`), Close (`close_rejects_state_queue_output_with_reference_script`, control `close_accepts_exact_distinct_terminal_outputs`),
and at the core (`da_core_apply_rejects_node_output_reference_script`,
`da_core_open_...` and `da_core_close_...` in
`lib/midgard/state-queue.test.ak`, beside their control); all fail.

Decision: the [DA bond pool decision](../../../../docs/midgard/decisions/da-committee-bond-pool.md).

## DA7. A challenge record is authentic only at the availability address holding its token

Status: VERIFIED (code and tests re-read at the cited lines as of
2026-09-28).

Rule: open pins the record output exactly (availability address, no reference
script, `challenge_record_lovelace` plus the one minted DACH, inline datum
equal to the expected record, `opened_at` the inclusive upper bound). Settle,
close and timeout accept a record only through `authenticated_challenge_record`
(address and exact value); the correction lock's Idle acquire reads it from
the one availability-address input holding the DACH. An open lands strictly
before `header.end_time + da_challenge_window_ms_v1`.

Enforced: `lib/midgard/availability-challenge-validation.ak` (reader, window,
record datum); `validators/correction-lock.ak`
Idle acquire `[aiken-test: availability-challenge.test/]`
(`availability_open_accepts_last_millisecond_of_challenge_window`,
`availability_open_rejects_upper_bound_at_challenge_window_end`,
`availability_open_rejects_record_at_foreign_script_address`,
`settle_rejects_record_at_foreign_address`,
`close_rejects_record_without_challenge_token`)
`[aiken-test: correction-lock/]`
(`correction_lock_handler_rejects_availability_acquire_with_record_at_foreign_address`, `..._with_record_without_challenge_token`; all fail).

The index-distinctness checks of open (`challenger_input_index` and
`record_output_index` against the state-queue indices), close and timeout
(record against terminal input,
pool input against record and terminal inputs, pool output against challenger
output) are implied by the address, value and token shapes each role must
have, and none can fail alone: with any one removed, every test stays green,
including the `q58_*_aliased_*` tests, whose refusals are overdetermined. They are defence in depth, as in DA5.

Decision: the [DA bond pool decision](../../../../docs/midgard/decisions/da-committee-bond-pool.md).

## DA8. A lost challenge charges the pool, up to one bond, in the timeout transaction

Status: VERIFIED (code and tests re-read at the cited lines as of
2026-09-28).

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
(`validators/state-queue.ak`; a `Locked` resume step mints 0), and only `TimeoutChallenge` can make it there. Open mints and
takes two inputs (`lib/midgard/availability-challenge-validation.ak`), Settle burns only a tranche token, and Close takes exactly
three inputs and two outputs and continues the node to
`Published`. The pool NFT is unique, so Slash and the yield read the
same pool output; TopUp and the three withdraw arms each contradict the
yield's value or datum equality.

Enforced: `validate_timeout_challenge`
(`lib/midgard/availability-challenge-validation.ak`: split, fee, indices
(defence in depth), pool output, challenger output) with the pool reader
`get_authentic_pool_input`
(`lib/midgard/da-bond-pool.ak`); the pool `Slash` arm
(`validators/da-bond-pool.ak`, constructor 5, Idle lock) `[aiken-test: availability-challenge.test/]`. Accepted with
exact amounts: `timeout_accepts_full_pool_slash_with_the_penalty_as_the_fee` and `timeout_accepts_*` (above one bond, partial,
below the penalty, empty pool, dust payout, `Withdrawing` keeping
`unlock_at`, `c` at the cap). Refused, all fail:
`timeout_rejects_without_pool_input`,
`..._pool_index_naming_a_non_pool_input`,
`..._counterfeit_pool_without_nft`,
`..._pool_nft_at_another_credential`,
`..._fee_one_lovelace_below_the_penalty_share`,
`..._negative_challenger_fee_on_a_short_pool`,
`..._challenger_fee_one_above_the_cap`,
`..._merged_challenger_output_one_lovelace_short` and `_over`,
`..._second_challenger_output`,
`..._refund_and_payout_split_across_two_outputs`, the
`timeout_rejects_pool_output_*` family (lovelace ±1, no
NFT, extra token, datum changed either way or to another `unlock_at`, stake
credential, reference script), and
`..._pool_output_index_aliased_with_challenger_output`, which the
address and value shapes refuse, not the index check alone (with the three
index-distinctness checks deleted it still fails; no test aliases the pool input with the record
or terminal input). The reader:
`da_bond_pool_input_reader_*` in `lib/midgard/da-bond-pool.test.ak`
`[aiken-test: midgard/da-bond-pool.test/]`. The Slash arm alone:
`da_bond_pool_slash_rejects_without_state_queue_redeemer`,
`..._other_state_queue_redeemer` and
`..._resume_step_locked_correction_lock`, control
`da_bond_pool_slash_accepts_honest_bonded`
`[aiken-test: da-bond-pool.test/]`. The whole transaction (pool Slash,
availability mint, timeout yield, state-queue removal, correction lock) runs
every script in `[aiken-test: da-bond-pool-timeout.test/]`: accepted
`pool_timeout_e2e_*_pool_prune` and `_head`; refused, each
with an every-other-script-accepts control,
`pool_timeout_e2e_resume_step_*_pool_refuses_slash`,
`..._close_swap_*_close_yield_refuses`,
`..._optimised_close_*_close_yield_refuses` and
`..._without_pool_*_timeout_yield_refuses` (prune and head
each), and `..._top_up_in_place_of_slash_pool_refuses`.

Liability is one bond per withholding episode, not one pool. A timeout needs
the withheld block to be the queue head: `remove_unavailable_head_v1`
(`validators/state-queue.ak`) requires `removed_link == None`,
and `prune_unavailable_block_descendant_v1` requires the head link to
be the unavailable header's hash. The slash runs only on the first,
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

The one-bond-per-episode bound is the liability rule of the
[DA bond pool decision](../../../../docs/midgard/decisions/da-committee-bond-pool.md).

Blind spot: Slash never reads the availability mint redeemer; against a
Close in its place it relies on Close's input and output counts and its node
transition, so relaxing Close (for example, batching it) reopens a drain
`[review]`.

## DA9. A timeout touches no operator directory

Status: VERIFIED.

Rule: a `TimeoutChallenge` transaction mints and burns nothing under the
registered-, active- or retired-operators policy the hub datum names. A
directory change and a timeout go in separate transactions, as the operator
slashes do among themselves (`other_directory_is_untouched_v1`,
`lib/midgard/operator-directory.ak`).

Enforced: `validate_timeout_challenge`, one pass over `tx.mint` against the
three hub-datum policies
(`lib/midgard/availability-challenge-validation.ak`)
`[aiken-test: availability-challenge.test/]`. Refused, all fail:
`timeout_rejects_registered_operators_mint`,
`..._active_operators_mint` and `..._retired_operators_mint`;
accepted: `timeout_accepts_mint_outside_the_operator_directories`. Each refusal goes green, that is accepted, with its own policy
dropped from the condition. In the emulator,
`demo/midgard-node/tests/availability-challenge-timeout-operator-directory.test.ts`
composes a forced retirement into a timeout; the timeout withdrawal refuses
it, and the same two as separate transactions land in one block.
