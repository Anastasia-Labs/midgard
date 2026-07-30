import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const readSource = async (relativePath: string): Promise<string> =>
  await readFile(new URL(relativePath, import.meta.url), "utf8");

describe("speculative commit safety guards", () => {
  it("keeps the T3 exact-set check atomic with pending-journal preparation", async () => {
    const workerSource = await readSource(
      "../src/workers/commit-block-header.ts",
    );
    const revalidation = workerSource.slice(
      workerSource.indexOf(
        "export const revalidateAndPersistSpeculativeCandidateSources",
      ),
      workerSource.indexOf("type ResolvedCommitBaseLedgerEntries"),
    );

    expect(revalidation.match(/LOCK TABLE/g)).toHaveLength(3);
    expect(revalidation.match(/sameSpeculativeSourceIdSet/g)).toHaveLength(3);
    expect(revalidation).toContain(
      "pending user-event ID set changed before journal preparation",
    );

    const journalSource = await readSource(
      "../src/database/pendingBlockFinalizations.ts",
    );
    const activeGuard = journalSource.indexOf(
      "Refusing to prepare a new pending block while another active pending-finalization record exists",
    );
    const sourceRevalidation = journalSource.indexOf(
      "yield* options.beforeJournalInsert",
    );
    const journalInsert = journalSource.indexOf(
      "INSERT INTO ${sql(tableName)}",
      sourceRevalidation,
    );
    expect(activeGuard).toBeGreaterThan(0);
    expect(sourceRevalidation).toBeGreaterThan(activeGuard);
    expect(journalInsert).toBeGreaterThan(sourceRevalidation);
  });

  it("keeps flag-off barriers inline and speculative builds write-deferred", async () => {
    const source = await readSource("../src/workers/commit-block-header.ts");

    expect(source).toMatch(
      /speculativeBuild === undefined\s*\? yield\* acquireCommitLucidOnce[\s\S]*?fetchAndInsertDepositUTxOsForCommitBarrier/,
    );
    expect(source).toMatch(
      /speculativeBuild === undefined\s*\? yield\* acquireCommitLucidOnce[\s\S]*?fetchAndInsertWithdrawalUTxOsForCommitBarrier/,
    );
    expect(source).toMatch(
      /speculativeBuild === undefined\s*\? yield\* acquireCommitLucidOnce[\s\S]*?fetchAndInsertTxOrderUTxOsForCommitBarrier/,
    );
    expect(source).toContain(
      "deferDatabaseWrites: speculativeBuild !== undefined",
    );
  });

  it("defers Lucid construction until a ready candidate receives Submit", async () => {
    const source = await readSource("../src/workers/commit-block-header.ts");
    expect(source).not.toContain("proofValidationLucid");
    expect(source).toContain("workerInput.data.forcedValidationSlotConfig");
    const outerProvider = source.slice(
      source.indexOf("export const provideCommitBlockWorkerServices"),
      source.indexOf("const pendingUserEventCountUpTo"),
    );
    expect(outerProvider).not.toContain("Effect.provide(Lucid.Default)");

    const candidateTail = source.slice(
      source.indexOf(
        "const instruction = yield* awaitSpeculativeInstruction(candidate)",
      ),
      source.indexOf(
        "const submissionContracts",
        source.indexOf(
          "const instruction = yield* awaitSpeculativeInstruction(candidate)",
        ),
      ),
    );
    const invalidateReturn = candidateTail.indexOf(
      'instruction.type === "InvalidateSpeculativeCandidate"',
    );
    const acquireLucid = candidateTail.indexOf("yield* acquireCommitLucidOnce");
    expect(invalidateReturn).toBeGreaterThanOrEqual(0);
    expect(acquireLucid).toBeGreaterThan(invalidateReturn);
  });

  it("releases after parking and flushes only after the resumed submission tail", async () => {
    const source = await readSource("../src/workers/commit-block-header.ts");
    const lifecycle = source.slice(
      source.indexOf("let activeSpeculativeLedgerMpf"),
      source.indexOf(
        ': nodeConfig.MPF_ENGINE !== "legacy"',
        source.indexOf("let activeSpeculativeLedgerMpf"),
      ),
    );
    const parkAndClose = lifecycle.indexOf(
      "parkSpeculativeMpfsForConfirmationWait",
    );
    const releaseLease = lifecycle.indexOf(
      "MpfEngineStateDB.releaseLedgerStoreLease",
    );
    const awaitInstruction = lifecycle.indexOf(
      "yield* awaitSpeculativeInstruction(candidate)",
    );
    const reacquireLease = lifecycle.indexOf(
      "MpfEngineStateDB.acquireLedgerStoreLease",
    );
    const stateQueueLeaseRevalidation = lifecycle.indexOf(
      "StateQueueMutationLeasesDB.revalidate",
      awaitInstruction,
    );
    const resumeParked = lifecycle.indexOf(
      "resumeSpeculativeMpfsForSubmission",
    );
    const runSubmissionTail = lifecycle.indexOf(
      "const result = yield* Effect.either",
      resumeParked,
    );
    const postSubmitFlush = lifecycle.indexOf(
      "activeSpeculativeLedgerMpf.flushBlockOverlay",
      runSubmissionTail,
    );

    expect(parkAndClose).toBeGreaterThan(0);
    expect(releaseLease).toBeGreaterThan(parkAndClose);
    expect(awaitInstruction).toBeGreaterThan(releaseLease);
    expect(stateQueueLeaseRevalidation).toBeGreaterThan(awaitInstruction);
    expect(reacquireLease).toBeGreaterThan(stateQueueLeaseRevalidation);
    expect(resumeParked).toBeGreaterThan(reacquireLease);
    expect(runSubmissionTail).toBeGreaterThan(resumeParked);
    expect(postSubmitFlush).toBeGreaterThan(runSubmissionTail);
    expect(lifecycle.slice(0, runSubmissionTail)).not.toContain(
      "promoteParkedOverlay",
    );
    const invalidationReturn = lifecycle.search(
      /instruction\.type !==\s*"SubmitSpeculativeCandidate"/,
    );
    expect(invalidationReturn).toBeGreaterThan(awaitInstruction);
    expect(invalidationReturn).toBeLessThan(stateQueueLeaseRevalidation);
  });

  it("retains only an opaque Architecture G handle while confirmation is pending", async () => {
    const source = await readSource("../src/workers/commit-block-header.ts");
    const start = source.indexOf(
      "const awaitInstructionWithRetainedNativeHandle",
    );
    const end = source.indexOf(': nodeConfig.MPF_ENGINE !== "legacy"', start);
    const lifecycle = source.slice(start, end);
    const releaseLease = lifecycle.indexOf(
      "MpfEngineStateDB.releaseLedgerStoreLease",
    );
    const awaitInstruction = lifecycle.indexOf(
      "yield* awaitSpeculativeInstruction(candidate)",
    );
    const invalidationReturn = lifecycle.search(
      /instruction\.type !==\s*"SubmitSpeculativeCandidate"/,
    );
    const stateQueueLeaseRevalidation = lifecycle.indexOf(
      "StateQueueMutationLeasesDB.revalidate",
      awaitInstruction,
    );
    const reacquireLease = lifecycle.indexOf(
      "MpfEngineStateDB.acquireLedgerStoreLease",
    );

    expect(start).toBeGreaterThan(0);
    expect(releaseLease).toBeGreaterThan(0);
    expect(awaitInstruction).toBeGreaterThan(releaseLease);
    expect(invalidationReturn).toBeGreaterThan(awaitInstruction);
    expect(stateQueueLeaseRevalidation).toBeGreaterThan(invalidationReturn);
    expect(reacquireLease).toBeGreaterThan(stateQueueLeaseRevalidation);
    expect(lifecycle).not.toContain("parkSpeculativeMpfsForConfirmationWait");
    expect(lifecycle).not.toContain("LEDGER_MPF_DB_PATH");
    const retainForJournal = source.indexOf(
      "nativeMpfClient.retainForJournal(context.handle)",
    );
    const closeClient = source.indexOf(
      "nativeMpfClient?.close()",
      retainForJournal,
    );
    expect(retainForJournal).toBeGreaterThan(0);
    expect(closeClient).toBeGreaterThan(retainForJournal);
  });

  it("guards both speculative stages against reset and records restart rebuilds", async () => {
    const source = await readSource(
      "../src/fibers/speculative-commit-builder.ts",
    );
    const builderResetGuard = source.indexOf(
      'invalidateSpeculativeCommitCandidate(globals, config, "T5")',
      source.indexOf("export const runSpeculativeCommitBuilderOnce"),
    );
    const submitterStart = source.indexOf(
      "export const submitSpeculativeCandidateOnConfirmation",
    );
    const submitterResetGuard = source.indexOf(
      'invalidateSpeculativeCommitCandidate(globals, config, "T5")',
      submitterStart,
    );
    const submitLease = source.indexOf(
      "StateQueueMutationLeasesDB.tryWithLease",
      submitterStart,
    );

    expect(builderResetGuard).toBeGreaterThan(0);
    expect(submitterResetGuard).toBeGreaterThan(submitterStart);
    expect(submitLease).toBeGreaterThan(submitterResetGuard);
    expect(source).toContain(
      "pipeline_trace phase=candidate_invalidated reason=T7 state=restart_rebuild",
    );
  });
});
