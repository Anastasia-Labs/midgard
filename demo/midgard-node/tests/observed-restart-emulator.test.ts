import "./helpers/follower-emulator-installed.js";

import { Effect, Option, Ref } from "effect";
import { expect, it, vi } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  ContractDeploymentIdentity,
  Database,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  Globals,
  LucidService,
  mergeMaturityWindow,
  MidgardContracts,
  NodeConfig,
  runCommitWorkerUntilSubmitted,
  runMergeUntilMerged,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

/**
 * A node restarted after its own block landed on L1 but before it followed
 * the landing: the restarted node's first synchronization takes the block in
 * (its rebase applies it), so the restart holds no unresolved submission,
 * and merge follows.
 */
it("a restart before the node followed its own block's landing applies the block and leaves no unresolved submission, so merge follows", async () => {
  const initial = await openProductionLifecycle();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  const { fixture, lucidService } = initial;
  const wallet = fixture.depositorLucid;
  const run = <A, E>(effect: Effect.Effect<A, E, any>): Promise<A> =>
    Effect.runPromise(
      effect.pipe(
        Effect.provideService(LucidService, lucidService as any),
        Effect.provideService(MidgardContracts, fixture.contracts as any),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make(
            fixture.runtimeOverrides!.deploymentIdentity,
          ),
        ),
        Effect.provideService(Globals, h.globals),
        Effect.provideService(NodeConfig, h.production.nodeConfig),
        Effect.provideService(MempoolLedgerCache, h.production.cache),
        Effect.provide(Database.layer),
        Effect.provide(IntentJournalWithoutFollower),
      ) as Effect.Effect<A, E, never>,
    );
  const submission = () =>
    run(
      Effect.all({
        txHash: Ref.get(h.globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
        sinceMs: Ref.get(h.globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS),
      }),
    );

  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    const deposit = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: await wallet.wallet().address(),
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const depositHash = await (
      await deposit.tx.sign.withWallet().complete()
    ).submit();
    expect(await wallet.awaitTx(depositHash)).toBe(true);
    await h.synchronize();
    const admitted = (
      await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          fixture.operatorLucid,
          SDK.eventHistoryDeploymentFromContracts(
            SDK.requireEventHistoryContracts(fixture.contracts).deposit,
          ),
        ),
      )
    ).filter((utxo) => utxo.utxo.txHash === depositHash);
    expect(admitted).toHaveLength(1);
    await h.deployment.chain.awaitLedgerTime(
      Number(admitted[0]!.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();

    const commit = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService,
      latestBlock: await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      ),
      nodeConfig: h.production.nodeConfig,
      production: { ...h.production, globals: h.globals },
    });
    expect(await fixture.operatorLucid.awaitTx(commit.submittedTxHash)).toBe(
      true,
    );
    const headerHash = commit.submittedHeaderHash;

    h = await initial.restartRuntime();
    await h.applyLanded(headerHash);
    expect(await submission()).toEqual({ txHash: "", sinceMs: 0 });
    const applied = Option.getOrThrow(
      await run(
        PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(headerHash, "hex"),
        ),
      ),
    );

    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals: h.globals,
      headerHash,
    });
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(
        fixture.operatorLucid,
        applied[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME].getTime(),
      ).readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());
    // Throws on any other outcome, skipped_unresolved_commitment included.
    const merged = await runMergeUntilMerged({
      fixture,
      lucidService,
      globals: h.globals,
      production: h.production,
    });
    expect(merged.status).toBe("merged");
    expect(await submission()).toEqual({ txHash: "", sinceMs: 0 });
  } finally {
    try {
      await h.close();
    } finally {
      vi.useRealTimers();
    }
  }
});
