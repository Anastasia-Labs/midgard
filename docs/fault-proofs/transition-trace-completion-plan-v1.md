# Transition-trace completion plan (v1)

Scope: close the three 🟠 Partial gaps recorded for the `transitionTrace`
family (category id `00000004`) in `docs/fault-proofs/catalogue-status.md`
§1 row 4 and §2, without touching
`onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak` semantics.
Transaction binding stays exclusively on
`verify_native_tx_in_state_queue_node` / `pass_native_tx_to_next_step`
(`common.ak`), and no rejecting-successor clearing clause is reintroduced
(VM-DEFECT-2).

## 1. CLI verb surface (GAP A)

The offchain library (`demo/midgard-fault-proofs/src/transition-trace/`)
is complete — reconstruction, detection, witness builders, and both
`submitTransitionTraceProof` / `submitTransitionTraceProofFromFiles` exist —
but `bin.ts` never imports it. Two verbs are wired:

### `prepare-transition-trace`

```
midgard-fault-proofs prepare-transition-trace \
  --da-payload-envelope <retained-da-envelope.cbor(.json)> \
  --header-hash <committed 28-byte header hash, hex> \
  [--output-dir <dir>]
```

Implemented by `src/prepare-transition-trace.ts`
(`prepareTransitionTraceFromDaEnvelopeV1`): reads the envelope (raw
lowercase CBOR hex or `{"cborHex": …}` JSON — same file convention as the
validation-dispute CBOR readers), authenticates it byte-for-byte against
the pinned committed header hash via `reconstructDaPayloadV1`
(fail-closed on any root/count/preimage mismatch), runs
`detectTransitionTraceFaults`, and writes each buildable detection's
`TransitionFaultProof` as Data-encoded CBOR plus a `plan.json`
(kind/invariant/diagnostic/final-index per detection, including
non-buildable ones with their reasons).

**Decision (owner review requested).** The generic `prepare-*` gate in
`bin.ts` (Q03/RF-043) rejects every legacy prepare verb because their
inputs (`--midgard-node-url`, `--transactions-file`, samples) are
caller-asserted, unauthenticated diagnostics. `prepare-transition-trace`
is dispatched **before** that gate, deliberately: its only evidence input
is the retained-DA payload envelope, and every byte of it is
authenticated against `--header-hash` by `reconstructDaPayloadV1` — the
identical binding the on-chain envelope check enforces
(`proof.header` must hash to `challenged_header_hash`, which must equal
the committed state-queue leaf). A wrong or malicious `--header-hash` pin
yields either a reconstruction failure or a proof the chain itself
rejects; no unsound submission can be minted through this verb. This is
the transition-trace analogue of the security-grade lane rather than a
diagnostic lane. The `CanonicalPrepareCommandV1` union was deliberately
NOT extended: that union is the sealed native-tx evidence lane (5
commands) and widening it is an owner-level format decision.

Known limitation, recorded rather than papered over: evidence-dependent
kinds (`omittedDueL1Event`, `outOfWindowSourceEvent`,
`acceptedTransactionTransitionMismatch`, `l2TransactionTransitions`)
need L1-event/ledger evidence that retained DA alone cannot supply; the
CLI v1 prepare covers the header-derivable kinds and reports the others
as out of scope in `plan.json` guidance. Library callers pass
`TransitionTraceDetectionEvidence` directly.

### `submit-transition-trace-proof`

```
midgard-fault-proofs submit-transition-trace-proof \
  --blueprint <path> --deployment-info <path> \
  --thread-out-ref <txHash#outputIndex> \
  --transition-fault-proof <proof.cbor(.json)> \
  [--reference-input <txHash#outputIndex> ...] \
  [network/provider/wallet options]
```

Implemented by `src/submit-transition-trace-proof.ts`
(`submitTransitionTraceProofFromCborFile`): decodes the file with
`Data.from(hex, TransitionFaultProof)` (fail-closed on any schema
mismatch), resolves each repeatable `--reference-input` outref to a live
UTxO, and drives the existing two-transaction route→final submitter.
`--reference-input` exists because the l1-event finals (`OmittedDueL1Event`,
`OutOfWindowSourceEvent`, and event-anchored `InvalidOneStepTransition`
variants) must exhibit the authentic L1 event NFT UTxO as a reference
input at the witness's `event_ref_input_index`.

`submit-init` and `remove-fraudulent-block` already accept
`--fraud-category transitionTrace`; nothing to add there. Both new verbs
join the command allowlist; neither is on the RF-043 retired list (that
list is exactly the pre-canonical native-tx step verbs).

## 2. Aiken sub-variant test matrix (GAP B)

Ground truth from `proof.ak` (types at :221-281, impls at :1551-1810).
"Pair" = positive (fault convicts) + negative (honest data refused at the
exact check). File: `proof.test.ak`.

### OmittedDueL1EventWitness (3 sub-variants)

| Sub-variant                 | Before                                              | After                                                                                                                                                                                                                                                                  |
| --------------------------- | --------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| OmittedDueDeposit           | positive only (`accepts_omitted_due_deposit_fault`) | unchanged                                                                                                                                                                                                                                                              |
| OmittedDueWithdrawal        | none                                                | **pair+**: `accepts_omitted_withdrawal_at_authenticated_end_time_boundary`, `rejects_omitted_withdrawal_at_excluded_start_time_boundary`, `rejects_omitted_withdrawal_published_after_challenged_block`, `rejects_omitted_withdrawal_already_committed_in_source_root` |
| OmittedDueForcedTransaction | boundary trio                                       | unchanged                                                                                                                                                                                                                                                              |

The withdrawal negatives pin both live checks:
`timed_l1_event_is_due` (strict-open start, closed end) and the
non-membership verification against `withdrawals_root`/`withdrawal_count`
(a withdrawal actually committed in the root refuses the empty
non-membership proof).

### OutOfWindowSourceEventWitness (3 sub-variants)

| Sub-variant                  | Before                                                                    | After                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| ---------------------------- | ------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| OutOfWindowDeposit           | positive only                                                             | unchanged                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| OutOfWindowWithdrawal        | none                                                                      | **pair+**: `accepts_late_withdrawal_as_out_of_window_source_event` (exercises `validity_override` reconciling the datum's submitted validity with the committed leaf), `rejects_in_window_withdrawal_as_out_of_window_source_event` (due event refused at `!timed_l1_event_is_due`), `rejects_out_of_window_withdrawal_with_mismatched_validity_override` (committed value must equal `WithdrawalInfo{..datum.info, validity: override}`) |
| OutOfWindowForcedTransaction | positive only (`accepts_late_forced_order_as_out_of_window_source_event`) | unchanged                                                                                                                                                                                                                                                                                                                                                                                                                                 |

### CountFaultWitness (5 sub-variants, from the impl)

| Sub-variant                       | Before                                | After                                                                                                                    |
| --------------------------------- | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| HeaderTotalCountMismatch          | none                                  | **pair** (header-only: `total_event_count` vs sum of the 4 source counts)                                                |
| HeaderTransitionStepCountMismatch | none                                  | **pair** (header-only: `transition_step_count` vs `total_event_count`)                                                   |
| SourceRootCountMismatch           | none                                  | **pair** (WithdrawalsRootDomain representative: count proof consistent with `withdrawals_root` but ≠ `withdrawal_count`) |
| EventToStepRootCountMismatch      | none                                  | **pair**                                                                                                                 |
| TransitionTraceRootCountMismatch  | positive only (`accepts_count_fault`) | + negative (`rejects_count_fault_when_transition_step_count_matches_committed_root`)                                     |

All negatives use the internally consistent `base_header()` so the refusal
lands exactly on the mismatch predicate, not on proof malformation.

## 3. Emulator representation audit (GAP C)

Before: only `InvalidOneStepTransition/InvalidForcedTransactionNoOpTransition`
is emulator-proven end to end
(`tests/submit-init-emulator-transition-trace.test.ts`, final validator 3).
The three families extended by GAP B have no emulator representation, and
neither do finals 0 (control) and 6 (l1-event).

New file `tests/submit-init-emulator-transition-trace-subvariants.test.ts`
(the existing suite is deliberately not grown — wasm heap ceiling note at
its head), one representative scenario per newly tested class, each
through submit-init → route → final → fraud-proof mint → block removal,
plus one adversarial-polarity case:

1. **OmittedDueWithdrawal** (final 6): consistent empty committed block;
   authentic withdrawal event NFT (always-succeeds hub `withdrawal`
   policy) with an in-window `inclusion_time`, exhibited via
   `additionalReferenceInputs`; `event_ref_input_index` computed from the
   ledger-sorted reference-input order against the hub oracle UTxO.
2. **OutOfWindowWithdrawal** (final 6): block commits the withdrawal leaf
   (root/count 1) with a matching trace step and event-to-step entry; the
   L1 event's `inclusion_time` falls after `end_time`.
3. **CountFault / HeaderTransitionStepCountMismatch** (final 0):
   header-only fraud (`transition_step_count` 1, `total_event_count` 0).
   The proof is built directly with `makeTransitionFaultProof` — by
   design `reconstructDaPayloadV1` fail-closes on count-inconsistent
   payloads, so a count-fraud block has no honest reconstruction to
   build from.
4. **Adversarial polarity**: the scenario-2 honest-side mirror — a
   withdrawal correctly omitted because it is not due
   (`inclusion_time > end_time`) accused via `OmittedDueWithdrawal`; the
   final transaction is refused at `timed_l1_event_is_due`
   (surfaces as `complete({ localUPLCEval: true })` throwing).

Fraud-proof token permanence and state-queue NFT burn are asserted as in
the existing suite; fault-proof scripts are exhibited as reference scripts
only (owner ruling).

## 4. Verification

- `~/.aiken-fork/bin/aiken check -m 'fraud_proofs/transition_trace/'`
  (trailing slash — bare `-m` filters can match zero tests and pass
  vacuously; the run must report a non-zero count).
- Blueprint for TS suites built with the repo's patched fork
  (`~/.aiken-fork/bin/aiken build`, pin `2a78108c`).
- `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/bin.test.ts
tests/transition-trace-submit-routing.test.ts
tests/submit-init-emulator-transition-trace.test.ts
tests/submit-init-emulator-transition-trace-subvariants.test.ts`
- `pnpm --dir demo/midgard-fault-proofs typecheck && pnpm --dir
demo/midgard-fault-proofs lint`
