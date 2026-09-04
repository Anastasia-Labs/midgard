# 0003 — Q27/Q32/Q43 semantic dispositions

- **Status:** ACCEPTED by the Goal owner's 2026-08-04 direction to resolve
  this queue now.
- **Scope:** canonical V1 only. This record decides proof-family shape; it
  does not mark Q27, Q32, or Q43 complete, and does not alter the task
  manifest or GOAL_PROGRESS ledger.
- **Normative basis:** `GOAL_SPEC.md` §3.1(4), §8.3 C42/C43/C49, and §9.3
  Q27/Q32/Q43; `docs/consensus-profile-v1.md` §3 and §8.

## Q27 — `min-ada`: required family, not structural N/A

**Decision:** Canonical V1 has an output minimum-Ada rule. Q27 remains an
independent required family after C42 and C49 provide the rule; it may not be
classified N/A, replaced by an off-chain wallet check, or bound to a constant.

The current source proves the gap rather than an exemption:

- `onchain/aiken/lib/midgard/validation-machine-v1.ak` has value conservation
  and `E_MIN_FEE`, but no min-Ada rejection rule; the C49 manifest anchor
  records this exact absence.
- `demo/midgard-validation/src/phase-a.ts` validates a parameterized minimum
  fee only. The existing SDK `CML.min_ada_required` use in
  `demo/midgard-sdk/src/fraud-proof/validation-proof-item.ts` funds a
  _proof publication output_, not a canonical L2 output and is therefore not
  a consensus check.
- The ValidationClaim context carries fee parameters but not the target
  ledger's UTxO-cost parameter, so the current applied claim cannot prove the
  rule.

**Required executable consequence (C42/C49 → Q27):** bind the exact,
target-snapshot `coinsPerUtxoByte`/applicable ledger min-UTxO rule and the
canonical output bytes into the on-chain validation state and its TypeScript
twin; introduce a stable min-Ada rejection code; prove an exact minimum,
one-unit-below rejection, parameter mutation, malformed output, and
valid-block negative. The Q27 proof builder and final validator must consume
that semantic terminal. A target parameter or output-codec change invalidates
the vectors and proof-fit evidence. This unblocks a single bounded Q27
assignment once C42/C49 are ready; it does **not** authorize a fallback that
shrinks supported Cardano output capability.

## Q32 — `req-signer-set`: structural N/A, reduces to Signatures

**Decision:** No standalone Q32 fraud-proof family is permitted. A required
signer hash without its witness is a transaction-invalidating constraint, not
a distinct state transition. It is already convicted by the canonical
Signatures validation-trace path as `E_MISSING_REQUIRED_WITNESS`, with the
exact no-op terminal. An additional non-required vkey witness cannot authorize
an input or turn an absent required signer into present; input authorization
remains separately covered by ResolveInputs/Signatures.

**Executable evidence:**

- Aiken `signatures_proves_a_missing_required_signer_is_an_exact_no_op` builds
  a 28-byte required signer, an empty authenticated signer frontier, and
  accepts only the exact `E_MISSING_REQUIRED_WITNESS` successor:
  `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`.
- TypeScript phase A independently rejects the same shape at
  `validateRequiredSigners` in `demo/midgard-validation/src/phase-a.ts`; its
  focused fixture asserts `MissingRequiredWitness`.

The eventual structural-N/A row must cite those tests and retain the mutation
control: substituting a membership proof or weakening the absent branch must
not admit an Accepted successor. Q32's only remaining downstream work is
matrix/catalogue reconciliation, not a new deployed family.

## Q43 — valid normal L2 transaction made a no-op: structural N/A, reduces to transition-trace accepted route

**Decision:** No standalone Q43 family is permitted. Claim-totality is
already enforced at the transition binding, independently of the optional
accepted-terminal mismatch diagnostic. For every authenticated normal L2
source, `L2TransactionTransition` reconstructs the exact spends and outputs
from its canonical compact preimages and rejects any claimed post-root that
differs. Route index 4 sends this witness to the deployed
`accepted_transaction_v1` final validator.

This disposition deliberately does not rely on the narrower
`AcceptedTransactionTransitionMismatch` branch. That branch only compares a
committed accepted terminal witness to the transition step. The
`L2TransactionTransition` branch recomputes the valid transaction's exact
post-root, so it catches the harder equal-root case: `claimed post = pre`.

**Executable totality evidence:**

- Aiken
  `accepts_valid_l2_transaction_no_op_transition_fault` constructs one valid
  authenticated L2 spend-and-produce transaction, sets its claimed post-root
  exactly equal to its authenticated pre-root, and proves the accepted route
  accepts the fraud proof:
  `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.test.ak`.
- Its adjacent control
  `rejects_l2_transaction_when_trace_matches_expected_post_root` uses the
  recomputed root and rejects the same proof.
- `onchain/aiken/validators/fraud-proofs/transition-trace/route-v1.ak` maps
  `L2TransactionTransition` and `AcceptedTransactionTransitionMismatch` to
  final route 4; `accepted-transaction-v1.ak` invokes
  `validate_accepted_transaction_fault_proof`.

The eventual structural-N/A row must retain the no-op fixture and matching
post-root control, plus source/event-to-step membership mutation controls.
Q43's C60 dependency remains a release-order dependency; it does not justify
creating a duplicate proof family or leaving a valid transaction's transition
unproven.

## Focused evidence commands

```bash
cd onchain/aiken && node scripts/run-focused-check.mjs midgard/validation_machine_v1 signatures_proves_a_missing_required_signer_is_an_exact_no_op
cd onchain/aiken && node scripts/run-focused-check.mjs midgard/fraud_proofs/transition_trace/proof accepts_valid_l2_transaction_no_op_transition_fault rejects_l2_transaction_when_trace_matches_expected_post_root
cd demo && pnpm --filter @al-ft/midgard-validation test -- --run tests/phase-a.test.ts -t 'rejects missing required native key witnesses'
```
