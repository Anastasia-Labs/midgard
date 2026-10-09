import "./helpers/follower-emulator-installed.js";

import { afterEach, describe, expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  buildBlockConfirmationAction,
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
  Ref,
  resetActiveRuntimePaths,
  runCommitWorkerUntilSubmitted,
  runConfirmationJournalInsertionRace,
  runNodeDatabaseEffect,
  serializeStateQueueUTxO,
  submitDepositAndRefreshBarriers,
} from "./deposit-flow-emulator-shared.js";

const submissionGlobals = (globals: Globals) =>
  Effect.all({
    unconfirmedTxHash: Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
    localFinalizationPending: Ref.get(globals.LOCAL_FINALIZATION_PENDING),
    latestLocalBlockEndTimeMs: Ref.get(globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
    blocksInQueue: Ref.get(globals.BLOCKS_IN_QUEUE),
  });

describe("deposit flow emulator", { concurrent: false }, () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  it("preserves a newer submitted journal when a delayed confirmation worker captured no pending journal", async () => {
    await runConfirmationJournalInsertionRace("during_worker");
  }, 240_000);

  it("preserves a newer submitted journal inserted after the initial confirmation snapshot guard", async () => {
    await runConfirmationJournalInsertionRace("after_snapshot_guard");
  }, 240_000);

  it("leaves a signed commit intent to the history owner when confirmation reports stale unconfirmed recovery", async () => {
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

    await runNodeDatabaseEffect(
      Effect.gen(function* () {
        const journalBefore =
          yield* PendingBlockFinalizationsDB.retrieveActive();
        // The early return needs a journal that carries a signed intent; pin
        // that precondition so this case cannot pass vacuously.
        expect(
          Option.getOrThrow(journalBefore)[
            PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH
          ],
        ).not.toBeNull();
        const globalsBefore = yield* submissionGlobals(globals);
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
        expect(yield* PendingBlockFinalizationsDB.retrieveActive()).toEqual(
          journalBefore,
        );
        expect(yield* submissionGlobals(globals)).toEqual(globalsBefore);
      }),
    );
  }, 240_000);
});
