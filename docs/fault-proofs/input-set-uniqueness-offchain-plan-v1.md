# Input-set-uniqueness fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `inputSetUniqueness` (`0000001a`). Generic Init, deployment
inspection/identity, both mandatory reference scripts, a production runner
factory, and watcher installation are wired. Typed family modules exist;
family-specific CLI verbs and live/preprod acceptance remain open.

## Fault statement

An operator-accepted committed transaction violates the canonical input-set
rules when:

- the spend-input field contains the same output reference twice;
- the reference-input field contains the same output reference twice; or
- one output reference appears in both fields.

The family is intra-transaction. Empty spend sets belong to `zeroInput`, and
cross-transaction double spends belong to `doubleSpend`.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/input-set-uniqueness/`
- `onchain/aiken/lib/midgard/fraud-proofs/input-set-uniqueness/`

Step 01 binds an operator-accepted native transaction through the shared
counted-`transactions_root` path and passes the transaction identity forward.
Step 02 opens the authenticated spend/reference fields and proves one of three
claims:

1. duplicate spend inputs;
2. duplicate reference inputs; or
3. spend/reference overlap.

The proof compares canonical fixed-width output-reference items by ordinal;
prover-supplied byte offsets are not accepted. Both steps support explicit
prover cancellation. The terminal transition burns the computation-thread
token and mints the permanent fault-proof token.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/input-set-uniqueness.ts`
- family implementation:
  `demo/midgard-fault-proofs/src/input-set-uniqueness/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The proving core detects the three claim shapes, prepares authenticated field
carriage, and drives Init/step/cancel transactions. It supports direct and
published carriage tiers. Registration is part of the canonical deployment
identity; this is not an emulator-only category.

## Verification status

Focused Aiken selectors cover acceptance binding, exact duplicate/overlap
predicates, index bounds, field anchoring, and valid-transaction refusal.
Emulator suites cover lifecycle conviction/removal, honest-block refusal,
adversarial carriage mutation, and the larger carriage tier:

- `submit-init-emulator-input-set-uniqueness-lifecycle.test.ts`
- `submit-init-emulator-input-set-uniqueness-adversarial.test.ts`
- `submit-init-emulator-input-set-uniqueness-tier2.test.ts`

## Remaining work

- expose the family through the operational CLI/workflow surface;
- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence;
- rerun maximum-field lifecycles under the shared Van Rossem emulator limits
  when the compiler, blueprint, field bounds, or Cardano protocol limits
  change.
