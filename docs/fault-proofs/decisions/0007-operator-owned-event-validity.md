# 0007 — The operator owns committed user-event validity

- Status: Accepted
- Scope: Classification of committed deposit and withdrawal leaves against their
  L1 source events in the `fabricatedDeposit`, `fabricatedWithdrawal`, and
  `withdrawalMistag` families
- Recorded: 2026-09-12

## Decision

The operator owns validity. The validity verdict a block stamps on a committed
user event is the operator's claim, and that claim is the thing a fault proof
judges. The L1 does not hold a verdict of its own: the withdrawal order datum
carries a placeholder validity because the order is created before any block
adjudicates it, and no L1 record can say whether an L2 output existed.

Three families are each a statement of the form "the operator claimed X, and
the chain shows not-X". Their evidence is the committed leaf plus the L1 fact
that contradicts it:

- `fabricatedDeposit`: the block commits a deposit and the L1 holds no such
  deposit event. Proved by showing the committed identity has no authentic
  deposit, or that the authentic deposit's content differs from the committed
  one.
- `fabricatedWithdrawal`: the block commits a withdrawal and the L1 holds no
  such withdrawal order, or the authentic order's body or signature differs
  from the committed one. The committed validity verdict is not part of this
  comparison.
- `withdrawalMistag`: the block records a withdrawal under the wrong tag. The
  committed verdict says the withdrawal is valid when the referenced L2 output
  does not exist, or says it does not exist when it does. Proved against the
  authenticated ledger state, not against the L1 order datum.

If a replay says a deposit never happened on L1 but the block says it did, that
is directly the fabricated-deposit fraud, and it is provable by showing the L1
has no such deposit. The same holds for a fabricated withdrawal and for a
withdrawal recorded under the wrong tag. None of these is a replay abort, a
prerequisite failure, or a reason to disqualify the block from classification.

## Consequences

- The fabricated-withdrawal content rule compares the committed leaf's body and
  signature against the authentic L1 order. A committed validity that differs
  from the placeholder in the L1 order datum is not a fabrication and must not
  convict. The on-chain step-03 and step-04 fidelity variant that treats an
  "overridden validity" as fabricated content is withdrawn, and the SDK twin
  follows it. This is a consensus change: it changes the fraud-proof
  validators, the blueprint, and the deployment identity, and requires a
  redeploy.
- Honest tagging in the SDK derives the committed validity from the ledger, and
  the honest control of every withdrawal fixture must survive the fabricated
  families with the verdict it actually earned.
- The validation-dispute replay's admission of a committed deposit or
  withdrawal whose L1 origin is missing or differs must report the finding
  owed to `fabricatedDeposit` or `fabricatedWithdrawal` rather than throwing
  out of classification.
- Where a leaf carries a verdict the chain contradicts, the finding belongs to
  `withdrawalMistag`; where the leaf's identity or content has no L1 backing,
  the finding belongs to the fabricated family. The two do not overlap.

## Authorities

- [Fabricated-withdrawal step 03](../../../onchain/aiken/validators/fraud-proofs/fabricated-withdrawal/step-03.ak)
- [Fabricated-withdrawal step 04](../../../onchain/aiken/validators/fraud-proofs/fabricated-withdrawal/step-04.ak)
- [SDK fabricated-withdrawal rule](../../../demo/midgard-sdk/src/fraud-proof/fabricated-withdrawal.ts)
- [SDK withdrawal order construction](../../../demo/midgard-sdk/src/user-events/withdrawal.ts)
- [Validation-dispute replay admission](../../../demo/midgard-fault-proofs/src/validation-dispute/replay.ts)
- [Classification rule order](../../../demo/midgard-fault-proofs/src/workflow/classification.ts)
