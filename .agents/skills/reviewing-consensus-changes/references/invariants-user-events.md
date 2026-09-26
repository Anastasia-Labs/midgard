# User-event invariants: deposits, withdrawals, reserve and payout

Scope: `onchain/aiken/validators/payout.ak`, `reserve.ak`,
`validators/user-events/`, `onchain/aiken/lib/midgard/event-history/`, the
withdrawal and deposit fraud-proof families, the operator-slash economics in
`lib/midgard/operator-directory.ak`, and their SDK builders.

Status, recurrence and line conventions are as in
[invariants-state-queue.md](invariants-state-queue.md). This subtree is being
rewritten around authenticated event history (`090436fc3`, 2026-09-23, "in
progress"); where the old `validators/user-events/` path and the
event-history path disagree, review against the event-history path and flag
the old one.

## UE1. A payout's datum cannot change while it is being funded

Status: VERIFIED.

Rule: `AddFunds` spends a payout and reproduces it with the identical datum;
only value may grow.

Enforced: `validators/payout.ak:180` (input datum) and `:243` (output datum)
`[aiken-test: payout/]` (`payout_add_funds_rejects_datum_mutation` `:903`,
fail).

Provenance: #420 ("Reserve and Payout — Large Datum attack — Locking
Funds"): an accumulating payout had no datum restriction, so an attacker
could inject a large datum and the payout could never complete.

## UE2. A payout starts only for a finalized, valid withdrawal

Status: PARTIAL (the retirement arms are tested; the full order-to-payout
lifecycle has no single emulator test).

Rule: a withdrawal order is retired into a payout only if its event is a
finalized member of the settlement's withdrawals tree with validity
`WithdrawalIsValid`; an order the operator marked invalid can only be
refunded, whole, to its refund address.

Enforced: `lib/midgard/event-history/retirement.ak:179-195` (refund) and
`:197-238` (payout start); the payout mint delegates to the retirement
observer (`validators/payout.ak:406-471`)
`[aiken-test: midgard/event-history/retirement.test/]`
(`settlement_membership_does_not_replace_finality` `:370`,
`valid_withdrawal_cannot_use_invalid_refund_path` `:500`,
`finalized_valid_withdrawal_starts_exact_payout` `:505`,
`payout_cannot_skip_its_authentication_token` `:512`,
`payout_cannot_exceed_withdrawal_target` `:523`) and `[aiken-test: payout/]`
(`payout_mint_requires_retirement_withdrawal` `:1459`,
`payout_mint_binds_retiring_order_input` `:1491`).

Provenance: #418 ("Reserve and Payout Incomplete Specification — No
Initialization"); first prototype `b2691d77d`; rewritten in `090436fc3`.

Recurrence: 2.

## UE3. The payout reaches exactly the withdrawal's value

Status: VERIFIED.

Rule: reserve top-ups move the payout toward its `l2_value` target and never
past it; the concluding payment pays exactly the target, with no surplus left
in the accumulator and no continuing payout output.

Enforced: `validators/payout.ak:277-287` (non-negative remaining need,
positive contribution, exact change), `:329-354` (conclude: the payout input equals the target and the L1 output
pays exactly the target to the datum's address and datum)
`[aiken-test: payout/]` (`:1040` no positive contribution, `:1064`
overfunding, `:1101` second reserve input, `:1263` underfunded target, `:1278`
surplus, `:1293` continuing output; all fail).

Provenance: #418 found no binding between the payout and the withdrawal's
value, datum or address: one 1 ADA withdrawal could draw any amount from the
reserve, and the destination could be changed at mint.

## UE4. Withdrawal bytes are the Aiken serialisation, map order preserved

Status: VERIFIED.

Rule: the signed withdrawal preimage, the committed leaf and the on-chain
signature check use the body exactly as Aiken's `cbor.serialise` renders it:
definite-length maps in their original order, never re-encoded by Lucid.

Enforced: `withdrawal_signature_is_valid_v1`
(`lib/midgard/fraud-proofs/withdrawal-mistag/step-03.ak:190-193`) and its twin
`withdrawalSigningMessageCbor`
(`demo/midgard-sdk/src/withdrawal-signature.ts:27-44`) `[review]`.

Provenance: `019c4eea2` and `deda00a64` (2026-09-05, admit Aiken asset-map
encoding; preserve map order in commitments and signatures), `81cf04e14`
(2026-09-12, sign over the Aiken serialisation).

Recurrence: 3. Blind spot: no golden channel pins the SDK bytes against the
Aiken bytes for withdrawal bodies.

## UE5. The operator owns the validity verdict

Status: VERIFIED.

Rule: a "fabricated" family compares only the content the L1 order fixes
(body and signature); it never convicts a block for the validity tag, which
depends on L2 state the order cannot know.

Enforced: `WithdrawalContentV1`
(`lib/midgard/fraud-proofs/fabricated-withdrawal/step-01.ak:255-276`)
`[aiken-test: fraud-proofs/fabricated-withdrawal/step-03/]` and
`[aiken-test: fraud-proofs/fabricated-withdrawal/step-04/]`
(`rejects_validity_only_difference`, `step-03.ak:256`, `step-04.ak:258`,
fail). Decision:
[0007-operator-owned-event-validity.md](../../../../docs/fault-proofs/decisions/0007-operator-owned-event-validity.md).

Provenance: `a97604c86` (2026-09-12, exclude the operator-owned verdict),
`c8bb922f7` (report absent or mismatched origins as fabricated).

Recurrence: 2.

## UE6. A withdrawal order is funded for both of its exits

Status: PARTIAL (the predicate and its test were read; the provenance is a
ruling on one input, not a recorded defect).

Rule: an order's locked ADA is non-negative per asset, no more than the
target's ADA, and enough for both the refund output and the payout output at
minimum-UTxO; the full target also meets minimum-UTxO.

Enforced: `withdrawal_is_funded`
(`lib/midgard/event-history/funding.ak:48-72`)
`[aiken-test: midgard/event-history/funding.test/]`
(`withdrawal_funding_checks_initial_payout_refund_and_target`, `:60`).

Provenance: the #627 ruling on the on-chain source of `coins_per_utxo_byte`.

## UE7. Slashing transactions pay exactly the penalty as fee

Status: PARTIAL (the on-chain equality is tested; one SDK builder is not
exact).

Rule: a slash spends a bond tranche of exactly `required_bond` (or the
inactivity-reduced tranche) and pays exactly the matching penalty as the
transaction fee. The fee is an equality, so an SDK builder that lets Lucid
add a change output overshoots it and the honest transaction is refused.

Enforced: `fraudulent_operator_slash_economics_are_exact_v1`
(`lib/midgard/operator-directory.ak:124-137`, called at `:326`)
`[aiken-test: midgard/operator-directory/]`
(`q53_fraud_slash_fee_and_tranche_are_exact`, `:416`); SDK exact-fee
completion for operator exit in
`demo/midgard-sdk/src/operator-lifecycle/exact-fee.ts` `[review]`.

Provenance: the exact-fee builders arrived with `32d4a2c48` (2026-09-16); the
file header explains why a change output makes the fee inexact.

Open recurrence (from reading; not executed): the bad-settlement slash
builder in `demo/midgard-sdk/src/settlement.ts:744` still uses a bare
`.setMinFee(params.slashingPenaltyLovelace)` with Lucid-managed change, has
no caller in `demo/`, and has no emulator test.

## Miss patterns in this subtree

- An equality on the fee or value that the honest SDK builder cannot hit
  (UE7).
- Bytes re-encoded by a library on one side of a signature (UE4).
- A family that convicts on a field the operator legitimately decides (UE5).
- Two generations of the same path live at once during a rewrite; review
  both, and state which one is authoritative.
