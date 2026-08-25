import { describe, expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  assertSpeculativeDepositSnapshotIsMemoryOnly,
  buildBlockConfirmationAction,
  DepositsDB,
  Effect,
  fetchLatestCommittedBlock,
  Globals,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  NodeConfig,
  Option,
  PendingBlockFinalizationsDB,
  resetActiveRuntimePaths,
  runCommitWorkerUntilSubmitted,
  runConfirmationJournalInsertionRace,
  runNodeDatabaseEffect,
  runSpeculativeWorkerWithInstruction,
  serializeStateQueueUTxO,
  type SpeculativeCommitWorkerInstruction,
  submitDepositAndRefreshBarriers,
} from "./deposit-flow-emulator-shared.js";

describe.sequential("deposit flow emulator", () => {
  it("preserves a newer submitted journal when a delayed confirmation worker captured no pending journal", async () => {
    await runConfirmationJournalInsertionRace("during_worker");
  }, 240_000);

  it("preserves a newer submitted journal inserted after the initial confirmation snapshot guard", async () => {
    await runConfirmationJournalInsertionRace("after_snapshot_guard");
  }, 240_000);

  it("discards a ready candidate and preserves payload when confirmation reports T1 stale recovery", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      const testNodeConfig = await makeNodeConfigForFixture(fixture);
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const recoveredBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: recoveredBase,
      });
      await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const { watermarks } = await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 13_000_000n,
        projectToLedger: false,
      });

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          Effect.gen(function* () {
            yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
              baseBlockEndTimeMs: blockN.blockEndTimeMs,
              candidateEndTimeMs: candidate.endTimeMs,
            });
            const serializedRecoveredBase =
              yield* serializeStateQueueUTxO(recoveredBase);
            yield* buildBlockConfirmationAction(() =>
              Effect.succeed({
                type: "StaleUnconfirmedRecoveryOutput",
                stalePendingHeaderHash: blockN.submittedHeaderHash,
                staleSubmittedTxHash: blockN.submittedTxHash,
                latestBlocksUTxO: serializedRecoveredBase,
                canonicalHeaders: [],
              }),
            ).pipe(
              Effect.provideService(Globals, globals),
              Effect.provideService(NodeConfig, testNodeConfig),
            );
            return {
              type: "InvalidateSpeculativeCandidate",
              reason: "T1",
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T1",
      });
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            PendingBlockFinalizationsDB.retrieveActive(),
          ),
        ),
      ).toBe(true);
      const pendingDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(new Date(Date.now())),
      );
      expect(pendingDeposits).toHaveLength(2);
      expect(
        pendingDeposits.every(
          (entry) => entry[DepositsDB.Columns.PROJECTED_HEADER_HASH] === null,
        ),
      ).toBe(true);
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);
});
