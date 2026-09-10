# Batch 04 report — demo/midgard-fault-proofs (9 files)

Package dir for every command below: `demo/midgard-fault-proofs`.
No commit, no stash, no checkout, no `.hs` file touched. Every production mutation used for
failure evidence was restored byte-exactly and verified with `diff <backup> <file>` (the package
carries a large pre-existing uncommitted wave, so an empty `git diff` is not a valid check).

## ESCALATION — another agent clobbered an uncommitted-wave src file (not mine, not fixed)

`demo/midgard-fault-proofs/src/remove-fraudulent-block.ts`
now equals `HEAD` byte for byte (97,815 bytes, mtime 2026-09-09 21:42:27) but the shared
session scratchpad holds
`<scratchpad>/rfb.bak`
(103,561 bytes, mtime 21:21:15), which is the uncommitted-wave version — it differs from HEAD in
161 lines and exports `readFraudSlashFundingAuthority` / `FraudSlashFundingAuthority`.
A sibling fault-proofs agent evidently backed the wave version up at 21:21 and then wrote HEAD's
content over it at 21:42 instead of the backup. Consequences observed now:

- `npx tsc --noEmit` in this package is red with 6 errors it did not have earlier in this session:
  - `src/workflow/funding-reservation-permit.ts:18 TS2305` no exported member `readFraudSlashFundingAuthority`
  - `src/workflow/funding-reservation-permit.ts:749,749,947 TS7006` implicit `any` (fallout of the above)
  - `tests/workflow-runtime.test.ts:613 TS2694`, `:639 TS2339` same missing symbol
- `src/workflow/funding-reservation-permit.ts` is still the wave version (`M`, 491/466), so the
  package no longer typechecks as a whole.

I did NOT restore it (outside my batch, and COMMON.md forbids checkout/reset-style recovery on my
own judgement). I preserved a copy at
`<scratchpad>/PRESERVED-remove-fraudulent-block.wave.ts`.
Suggested recovery, for the owner/orchestrator to authorise:
`cp .../scratchpad/PRESERVED-remove-fraudulent-block.wave.ts demo/midgard-fault-proofs/src/remove-fraudulent-block.ts`
then re-run `npx tsc --noEmit` in the package. Please decide soon — the scratchpad is temporary.

## Package-level verification

- `npx tsc --noEmit` — red with exactly the 6 errors above; all six are the clobber fallout, none
  is in a file I touched. Per-file: no error is reported for any of my nine files.
- `npx eslint <the nine files> --max-warnings=0` — clean.
- `npx prettier --check <the nine files>` — clean.

---

## 1. tests/submit-init-emulator-observer-order-invalid-publication.test.ts (strengthen; skip-no-cause)

Contract: all four applied `observerOrderInvalid` validators publish as reference-script UTxOs
under the 15,872-byte reliability reserve on a real emulator submission.

Changed: replaced `describe.runIf(hasFamily)` with a hard assertion that the rebuilt blueprint
carries every expected validator title (fails closed, §14); replaced the `console.info` size dump
with per-step assertions on (a) the published reference script's identity
(`validatorToScriptHash(published.utxo.scriptRef) === step.spendingScriptHash`) and (b)
`published.publicationMeasurement.completeSignedBytes <= publicationReserveBytes`; asserted the
applied step count.

Failure evidence: removed one expected title's validator from the rebuilt blueprint copy fed to the
suite (controlled fault) — fails at
`expect(...).toEqual([])` with message `observerOrderInvalid validators missing from the rebuilt blueprint`.
Previously the same fault produced a silent green skip.

Verification: `npx vitest run tests/submit-init-emulator-observer-order-invalid-publication.test.ts
tests/distinct-asset-accumulation-limit-publication-fit.test.ts
tests/missing-redeemer-publication-fit.test.ts` → 3 files / 3 tests passed.

## 2. tests/distinct-asset-accumulation-limit-publication-fit.test.ts (strengthen)

Same treatment as (1) for the six applied `distinctAssetAccumulationLimit` validators: hard
presence assertion instead of `describe.runIf`, per-step script-identity + reserve assertions,
asserted step count. Same failure-evidence fault and same green run above.

## 3. tests/missing-redeemer-publication-fit.test.ts (strengthen)

Same treatment as (1) for the seven applied `missingRedeemer` validators, and the `console.info`
size dump is gone. Same failure-evidence fault and same green run above.

## 4. tests/phas-membership-batch.test.ts (keep; negative-uncontrolled)

Contract: batched PHAS membership openings equal the individual openings in requested order
including duplicates; wrong value / wrong key / wrong root are refused; empty request → empty list.

Changed: replaced the three bare `rejects.toThrow()` negatives with a helper that pins the error
type and the exact refusal identity:

    const expectPhasRefusal = async (operation, code, message) => {
      const caught = await operation.then((v) => v, (cause) => cause);
      expect(caught, "refusal must throw a challenger error").toBeInstanceOf(TransitionTraceChallengerError);
      expect(error.code).toBe(code);        // "missingWitnessData" | "proofConstructionFailed"
      expect(error.message).toBe(message);
    };

Failure evidence: in the batch path, made the wrong-root case fall through to the generic
construction error instead of the specific refusal — test fails at `expect(error.code).toBe(...)`
("expected 'proofConstructionFailed' to be 'missingWitnessData'"). Src restored, `diff` identical.

Verification: green in the 5-file run under §5 below.

## 5. tests/prepare-l2-tx-mistag.test.ts (strengthen; circular oracle)

Contract: `prepareL2TxMistagFromTransactions` selects the committed code-1 (TxIsInvalid) leaf and
refuses a block with only code-0 leaves.

Changed: rewrote (4 tests). The circular `expectedRoot` — which used the very helpers under test
(`decodeTransactionMaterial` / `buildTrieView` / `transactionSourceTrieItem`) — is replaced by an
independent reference model that builds the trie through the third-party MPF package and commits
it through the SDK's counted-root program:

    const trie = await Trie.fromList(leaves);
    const committedRoot = await Effect.runPromise(
      commitCountedRootProgram({ domain: ROOT_DOMAINS.transactionsV1, phasRoot, count: BigInt(leaves.length) }),
    );

Only the leaf encoder (`deriveL2TransactionSourceCbor`) is shared, and that is documented in the
file. Added the code-1 selection/refusal polarity pair and a root-mismatch refusal.

Failure evidence: mutated the counted-root domain used by the production preparer so it commits
under a different domain tag — test fails at the root equality against the independent model
("expected '…' to be '…'"), which the old self-consistent oracle could not see. Src restored,
`diff` identical.

## 6. tests/cross-block-duplicate-event-prepare.test.ts (strengthen; fixture-hides-decision)

Contract: cross-block duplicate-event preparation refuses same-header, burned, misbound and
root-forged settlement evidence.

Changed: rewrote (7 tests). The `as unknown as CanonicalBlockEvidence` cast is gone. A
`buildEventBlock` helper now constructs a real deposits/withdrawals/forced-transactions payload,
recomputes `depositsRoot` / `withdrawalsRoot` / `forcedTransactionsRoot` / `eventToStepRoot` /
`validationTracesRoot` and all counts, supplies `forced_transaction_preimages`, re-hashes the
header via `await Effect.runPromise(SDK.hashBlockHeader(header))`, and the evidence is produced by
the production entry point `canonicalBlockEvidenceFromVerifiedPayload` (so `reconstructDaPayload`
really runs). The identity-echo positives are replaced by an independent digest oracle:

    const expectedValueDigest = (valueBytes) =>
      computeHash32(Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(valueBytes.toString("hex")), "hex")).toString("hex");

Failure evidence: mutated the preparer so the duplicate-event value digest is taken over the raw
value bytes instead of the canonicalised Plutus-data encoding — test fails at the digest equality
against the independent oracle. Src restored, `diff` identical.

## 7. tests/transaction-output-non-canonical-workflow.test.ts (strengthen; self-pin/stubbed-decision)

Contract: the workflow exposes a callback-free runner, binds reference scripts against the
finalized manifest identity, and detects the sole non-canonical retained output.

Changed: rewrote (5 tests).

- `Object.keys(runner).sort()` self-pin removed; replaced by a behavioral category gate — the
  runner accepts its own catalogue category and refuses a foreign one _before_ loading any runtime
  configuration (`runtimeConfigLoads` must still be 0 on refusal).
- The manifest transcription is replaced by
  `expect([step01..step04]).toEqual(FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transactionOutputNonCanonical)`
  plus a loop that omits each of the eight manifest entry names in turn from the binding input and
  requires a refusal.
- The self-derived accept case is replaced by asserting the full detection object and both
  polarities ("0 exact findings" and "2 exact findings").
- The stubbed `l1.observe` echo is replaced by accept/reject pairs for `submitStep04`/`submitStep03`,
  `submitInit` and `removeDescendants`, plus `expect(observedHeaderHashes).toEqual([headerHash])`
  so the real resolver decision runs.

Failure evidence: three mutations. (M1) dropped one manifest entry from
`FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transactionOutputNonCanonical` → fails at the
`toEqual(...)` manifest identity assertion. (M2) made the detector report the first retained
output instead of the exact violation → fails at the detection-object equality. (M3) removed the
category gate → the expected `rejects.toThrow(/category mismatch/)` no longer holds. Honest caveat
on M3: with the gate removed the run also produces a downstream "Cannot read properties of
undefined"; the test still fails at the intended assertion, but the failure is one step indirect.
Src restored; verified with `diff <backup> src/transaction-output-non-canonical/workflow.ts`
(IDENTICAL) rather than with `git diff`, because that file is part of the pre-existing wave.

## 8. tests/spend-input-signer-missing-central-journal.test.ts (strengthen; impl-coupled)

Contract: the durable central-journal adapter records intent before submit and refuses tx/stage
substitution across restart.

Changed: rewrote (8 tests).

- **Dropped `testOnlyJournalCategoryAlias: "fieldItemWidthIllegal"`.** The adapter is now built
  exactly as production builds it, so `identity.category` — and therefore the hashed
  `workflowId` — is the real `spendInputSignerMissing` category.
- New test "records the family's own durable workflow identity": asserts the full
  `FraudProofWorkflowIdentity` (`schemaVersion` from the exported constant,
  `deploymentFingerprint`, `category`, `target.headerHash`, `decisionDigest`), that every entry
  carries `bridge.workflowId`, and that three adapters differing only by `headerHash` /
  `decisionDigest` produce three _distinct_ workflow ids (`new Set(...).size === 3`).
- The four exact event-kind array equalities are gone. Ordering is now expressed by
  vocabulary-light index helpers (`intentIndex`, `submittedIndex`, `confirmedIndex` searching for
  the event that names the transaction hash), so a journal event-vocabulary change no longer fails
  the file without a contract violation, while intent-before-submit ordering is still pinned.
- Added the missing no-side-effect assertions: after the refused transaction substitution the
  journal must be unchanged (`toHaveLength(afterFirst)` and no intent for the substitute hash);
  after the refused completion the journal must still be empty; after the refused stage
  substitution there must be no `confirmed` entry.
- Added the controlled positive counterpart to each refusal (a clean adapter reconciling the same
  hash against the stage it actually targeted; a drifted stage that is a substitution rather than
  an abandonment), so the refusals are causal, not incidental.
- The directory-backed restart test now also proves a restarted process refuses to adopt a
  different hash for the same unresolved action.

Failure evidence (5 mutations to `src/spend-input-signer-missing/central-journal.ts`, each restored
and verified with `diff`, `RESTORED` printed each time):

| #   | mutation                                                                                                            | failing assertion                                                                                                                                    |
| --- | ------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| M1  | `journalCategory` defaults to `"fieldItemWidthIllegal"` (i.e. re-introduces the very defect the alias used to hide) | "records the family's own durable workflow identity" → `expected { …(5) } to deeply equal { …(5) }` (category) — 1 failed / 7 passed                 |
| M2  | substitution check relaxed to compare only `actionId`, not `txHash`                                                 | "reuses an identical crash intent …" and the restart test → `promise resolved "undefined" instead of rejecting` — 2 failed / 6 passed                |
| M3  | `submission_intent` append removed from `boundary`                                                                  | `expected -1 to be greater than or equal to 0` (intent index), plus 4 more — 5 failed / 3 passed                                                     |
| M4  | `reconcile` drops the `observedStage === recovery.targetStage` condition                                            | "refuses a confirmed hash paired with a substituted authenticated stage" → `promise resolved "undefined" instead of rejecting` — 1 failed / 7 passed |
| M5  | `identity.target.headerHash` pinned to a constant                                                                   | "records the family's own durable workflow identity" → identity `toEqual` (headerHash / distinctness) — 1 failed / 7 passed                          |

Src observation, reported not acted on (rule 6): with this file no longer passing the alias,
`testOnlyJournalCategoryAlias` in `src/spend-input-signer-missing/central-journal.ts` (and the same
parameter in its three sibling families) is dead test-only production code. Removing it is a src
change my row did not call for, so I left it.

Deliberate skip: the row's `by_construction` asks to "promote this file into the shared
parameterized suite for all 8 families". Only four such sibling test files exist in the package and
they are plausibly owned by other batches, so promoting would edit files outside my batch
(COMMON.md scope rule). I applied the row's substantive part — dropping the alias and de-coupling
from the event vocabulary — in place. Recommend a follow-up ticket for the shared suite.

Verification: `npx vitest run tests/spend-input-signer-missing-central-journal.test.ts
tests/phas-membership-batch.test.ts tests/prepare-l2-tx-mistag.test.ts
tests/cross-block-duplicate-event-prepare.test.ts
tests/transaction-output-non-canonical-workflow.test.ts` → 5 files / 25 tests passed.

## 9. tests/cek-selection-yield-lifecycle.test.ts (strengthen; weak-assert + measurement-not-gate)

Contract: forged CEK selection yields (native; Plutus; 160-lambda; Data graph; restart; cancel) run
to award and block removal on the real emulator, and a dishonest challenger is refused at
semantic-resolution.

Changed:

- The five positive lifecycle tests no longer assert `txHash.toHaveLength(64)` +
  `removal.transactions.length > 0`. They call a new `expectAuthenticatedRemoval(result)` that pins
  the authenticated end state: `initResult.fraudCategoryName === "validationTraceDispute"`;
  `initResult.fraudulentHeaderHash === setup.headerHash`; the minted fraud-proof unit's asset name
  equals `initResult.computationThreadAssetName` while its policy id is _not_ the computation-thread
  policy; `removal.fraudCategory` / `fraudCategoryId` / `fraudulentHeaderHash` bound to the same
  init and setup; `removal.fraudProofOutRef === awardResult.fraudProofOutRef`;
  `removal.fraudProver === initResult.fraudProver`; `removal.transactions` contains a
  `remove-target`; and the challenged block's state-queue UTxO is really gone
  (`utxosAtWithUnit(contracts.stateQueue.spendingScriptAddress, setup.stateQueueBlockUnit)` → length 0).
- §13 measurement-not-gate: every submitted transaction is now gated _inline, on every run_
  (previously the only acceptance criterion lived inside `buildVanRossemFitLedger`, reachable only
  under `MIDGARD_WRITE_FIT_LEDGER=1`):

      expect(m.completeSignedBytes, `${name} signed bytes`).toBeLessThan(VAN_ROSSEM_MAX_SIGNED_TX_BYTES);
      expect(m.executionMemory, `${name} execution memory`).toBeLessThan(VAN_ROSSEM_MAX_MEMORY_UNITS);
      expect(m.executionSteps, `${name} execution steps`).toBeLessThan(VAN_ROSSEM_MAX_CPU_UNITS);

  The `afterAll` block is now purely the ledger _generator_: it stays behind
  `MIDGARD_WRITE_FIT_LEDGER=1` (keeping `vitest -t <one test>` usable) and no longer carries the
  file's only acceptance criterion.

- The cancel test (no fraud proof minted) and the two dishonest-challenger refusals are unchanged.

Deliberate partial skip: the row also asks to "move the ledger writer out of the test into a
bench/generator with `--check`". The package already has that mechanism —
`tests/support/measured-fit-ledger.ts` (`createMeasuredFitRecorder` / `verifyMeasuredFitLedger`,
fragment protocol + `MIDGARD_WRITE_FIT_LEDGER`) — but switching this family onto it changes the
regeneration protocol for `docs/fault-proofs/size-plans/validation-trace-cek-selection-fit-ledger.json`
and touches a support file shared by 43 sibling tests owned by other batches. I therefore fixed the
§13 defect in place (the measurement is now a gate) and left the file-writing path alone.

Failure evidence:

| #   | mutation                                                                                                    | failing assertion                                                                                                                       |
| --- | ----------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| M-A | `src/submit-init.ts:489` `fraudulentHeaderHash: resolvedHeaderHash` → a constant                            | `expected '00000…' to be 'c335073143f2937012f3d54123ca148a2488d…'` (init/setup header binding) — 1 failed                               |
| M-B | `src/validation-dispute/submit.ts` finalization result reports `fraudProofUnit` with a corrupted asset name | `expected '0000ffff' to be '00000006e38b314c9c630a7c8c2cb1e988b0d…'` (minted fraud-proof unit ≠ computation-thread identity) — 1 failed |

Both restored (`RESTORED_A`, `RESTORED_B` via `diff` against scratchpad backups).

Two further attempts are reported honestly as _not_ clean evidence for the new assertions:

- M-C (`VAN_ROSSEM_MAX_SIGNED_TX_BYTES` 16_384 → 1_000): the suite goes red, but at the harness's
  own publication-target check ("… is 509 bytes and exceeds the 15,872-byte publication target"),
  which consumes the same constant and fires before my inline gate. So the constant is load-bearing,
  but this does not isolate my gate.
- M-D (`fraudProofOutRef` output index + 1): red, but the corrupted out-ref is consumed downstream,
  so the run dies with a `TxSubmitError` before reaching my `fraudProofOutRef` equality. Same for a
  controlled fault that inflated `completeSignedBytes` in `tests/support/emulator/measurement.ts`
  (+20,000): it trips `requireL1ProofEnvelope` first. All three were restored exactly.

Verification: `npx vitest run tests/cek-selection-yield-lifecycle.test.ts` → 1 file / 8 tests passed
(106.7 s). Flake note: one intermediate full-file run immediately after a src restore reported all
8 failed; the subsequent isolated run and two full runs were green, so I record it as transient
(several sibling batch agents were running vitest concurrently at that moment) and not a property
of the file.

## Src edits

None retained. Every src file touched was touched only as a failure-evidence mutation and restored
byte-exactly: `src/spend-input-signer-missing/central-journal.ts`, `src/submit-init.ts`,
`src/validation-dispute/submit.ts`, `src/proof-fit/van-rossem-fit-ledger.ts`,
`src/transaction-output-non-canonical/workflow.ts`, the PHAS module, the l2-tx-mistag preparer, the
cross-block preparer, and the test-support file `tests/support/emulator/measurement.ts`.
The only outstanding src problem in this package is the clobbered
`src/remove-fraudulent-block.ts` described in the escalation at the top — not mine, not fixed.
