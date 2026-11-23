import "./helpers/follower-emulator-installed.js";

import { SqlClient } from "@effect/sql";
import {
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Either, Option, Runtime } from "effect";
import { expect, it, vi } from "vitest";

import { runMpfAudit } from "../src/commands/mpf-audit.js";
import {
  DepositsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { mergeAction } from "../src/fibers/merge.merge-action.js";
import type { MergeActionResult } from "../src/fibers/merge.registered-merge-due-work-skip.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { IntentJournal } from "../src/services/intent-journal.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  ContractDeploymentIdentity,
  Database,
  emulatorStateQueueSnapshot,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  Globals,
  LucidService,
  mergeMaturityWindow,
  MidgardContracts,
  NodeConfig,
  runCommitWorkerUntilSubmitted,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { dropPendingEmulatorTransaction } from "./helpers/emulator-rollback.js";
import { assertRefusedMergeFoldsNothing } from "./helpers/merge-confirmation-refused.js";
import { assertLandedMergeFoldedByProcessing } from "./helpers/merge-landed-finalization-folded-by-processing.js";
import { assertMergeWaitsForLedgerTip } from "./helpers/merge-trailing-ledger-tip.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

const mergeHooks = vi.hoisted(() => ({
  /** Slots the L1 clock runs ahead of the emulator's ledger tip. */
  l1ClockLeadSlots: 0,
}));
// Between blocks the L1 clock (`slotNow`: the last tip plus elapsed time)
// runs ahead of the ledger tip; a test sets how far.
vi.mock("../src/l1-heads.js", async (importOriginal) => {
  const { Effect: E } = await import("effect");
  const actual = await importOriginal<typeof import("../src/l1-heads.js")>();
  const l1NowUnixTimeMs: typeof actual.l1NowUnixTimeMs = (api) =>
    E.map(
      actual.l1NowUnixTimeMs(api),
      (ms) => ms + mergeHooks.l1ClockLeadSlots * 1_000,
    );
  return { ...actual, l1NowUnixTimeMs };
});

const Status = PendingBlockFinalizationsDB.OwnBlockStatus;

const openMergeLifecycle = async () => {
  const initial = await openProductionLifecycle();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  const { fixture, lucidService } = initial;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);

  const run = <A, E>(
    effect: Effect.Effect<A, E, any>,
    lucid: unknown = lucidService,
    signal?: AbortSignal,
  ): Promise<A> =>
    Effect.runPromise(
      effect.pipe(
        Effect.provideService(LucidService, lucid as any),
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
        // The node's journal records the merge, as `runNode`'s does: a merge
        // never goes out unjournaled.
        Effect.provideService(IntentJournal, h.production.journal),
      ) as Effect.Effect<A, E, never>,
      { signal },
    );
  const sqlRun = <A>(
    statement: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown, any>,
  ): Promise<A> => run(Effect.flatMap(SqlClient.SqlClient, statement));
  const queuedBlocks = async () =>
    (
      await Effect.runPromise(
        emulatorStateQueueSnapshot(
          fixture.operatorLucid,
          fixture.contracts.stateQueue,
          "startup",
        ).pipe(Effect.provide(Database.layer)),
      )
    ).blockCount;
  /** The block's derived status: folded once its landed merge is folded. */
  const ownStatus = (headerHash: string) =>
    run(
      PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
        Buffer.from(headerHash, "hex"),
      ),
    );
  // The block's deposits, all in `status`: projected while its merge is not
  // folded, consumed once it is.
  const expectDeposits = async (
    headerHash: string,
    status: DepositsDB.Status,
  ) => {
    const statuses = (
      await run(
        DepositsDB.retrieveByProjectedHeaderHash(
          Buffer.from(headerHash, "hex"),
        ),
      )
    ).map((entry) => entry[DepositsDB.Columns.STATUS]);
    expect(statuses.length).toBeGreaterThan(0);
    expect(new Set(statuses)).toEqual(new Set([status]));
  };
  const expectedRoot = async (headerHash: string) => {
    const journal = await run(
      PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(headerHash, "hex"),
      ),
    );
    return Option.getOrThrow(journal)[
      PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
    ];
  };
  const submit = async (built: { tx: TxSignBuilder }) => {
    const signed = await built.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await wallet.awaitTx(hash)).toBe(true);
    await h.synchronize();
    return hash;
  };
  // One deposit, committed, landed, applied and attested: a
  // block the merge fiber may merge once it matures.
  const commitDepositBlock = async (lovelace: bigint, mature = true) => {
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await h.synchronize();
    const depositTxHash = await submit(
      await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          wallet,
          fixture.contracts,
          {
            l2Address: address,
            l2Datum: null,
            lovelace,
            additionalAssets: {},
            referenceScripts: fixture.referenceScripts.deposit,
          },
        ),
      ),
    );
    const admitted = (
      await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(
          fixture.operatorLucid,
          SDK.eventHistoryDeploymentFromContracts(histories.deposit),
        ),
      )
    ).filter((deposit) => deposit.utxo.txHash === depositTxHash);
    expect(admitted.length).toBeGreaterThan(0);
    await h.deployment.chain.awaitLedgerTime(
      Math.max(...admitted.map(({ facts }) => Number(facts.inclusion_time))) +
        1000,
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
    await h.applyLanded(commit.submittedHeaderHash);
    const tail = (
      await Effect.runPromise(
        emulatorStateQueueSnapshot(
          fixture.operatorLucid,
          fixture.contracts.stateQueue,
          "startup",
        ).pipe(Effect.provide(Database.layer)),
      )
    ).tailCommitBase;
    expect(tail.headerHash).not.toBeNull();
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals: h.globals,
      headerHash: tail.headerHash!,
    });
    if (!mature) return tail.headerHash!;
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(fixture.operatorLucid, tail.blockEndTimeMs)
        .readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    await expectDeposits(tail.headerHash!, DepositsDB.Status.Projected);
    return tail.headerHash!;
  };
  // A merge attempt through `lucid` over a queue of any length, retried past
  // the merge-submit due work a first attempt may register while the local
  // ledger catches up. `stop` interrupts it, as the node stopping would.
  const mergeUntilSubmitted = async (
    lucid: unknown = lucidService,
    stop?: AbortSignal,
  ): Promise<Either.Either<MergeActionResult, unknown>> => {
    const mergeConfig = {
      ...h.production.nodeConfig,
      MIN_QUEUE_LENGTH_FOR_MERGING: 1,
    };
    for (let round = 1; round <= 3; round += 1) {
      const result = await run(
        Effect.either(
          mergeAction().pipe(Effect.provideService(NodeConfig, mergeConfig)),
        ),
        lucid,
        stop,
      );
      if (
        Either.isLeft(result) ||
        result.right.status !== "skipped_oldest_block_local_ledger_not_ready"
      )
        return result;
      const dueWork = listSlotAwareDueWork().filter(
        (entry) => entry.kind === "merge_submit_validity",
      );
      expect(dueWork).toHaveLength(1);
      await advanceEmulatorToDueWork(fixture, dueWork[0]!);
      await h.synchronize();
    }
    throw new Error("Merge did not submit after three rounds");
  };
  // The Lucid service with its L1 client's confirmation wait replaced, and
  // its status read too when `transactionStatus` is given.
  const withConfirmationWait = (
    awaitTx: (
      txHash: string,
      land: LucidEvolution["awaitTx"],
    ) => Promise<boolean>,
    transactionStatus?: (
      txHash: string,
      read: LucidEvolution["transactionStatus"],
    ) => ReturnType<LucidEvolution["transactionStatus"]>,
  ) => {
    const api = new Proxy(lucidService.api, {
      get(target, property) {
        if (property === "awaitTx")
          return (txHash: string) =>
            awaitTx(txHash, target.awaitTx.bind(target));
        if (property === "transactionStatus" && transactionStatus !== undefined)
          return (txHash: string) =>
            transactionStatus(txHash, target.transactionStatus.bind(target));
        const value = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    return { ...lucidService, api };
  };

  // A merge of the oldest block that the node stops waiting on: the node
  // stops while the submitted merge awaits its L1 confirmation, so the
  // attempt ends unconfirmed with nothing folded. The stop is an
  // interruption: the merge carries no upper validity bound, so its wait
  // ends on its own only at the confirmation budget or on a status answer,
  // and in this harness the status read sees the merge land first. The merge
  // then leaves the mempool (the emulator, unlike a real provider, hides a
  // pending transaction's inputs), so L1 has not taken it; the signed merge
  // is returned to hand to L1 again.
  const abandonMergeConfirmation = async (headerHash: string) => {
    let heldTxHash: string | undefined;
    let heldTxCbor: string | undefined;
    const stop = new AbortController();
    const stoppingLucid = withConfirmationWait(async (txHash) => {
      if (heldTxCbor !== undefined) {
        heldTxHash = txHash;
        stop.abort();
      }
      return false;
    });
    const submitThrough = fixture.emulator.submitTx;
    fixture.emulator.submitTx = async (signedCbor) => {
      heldTxCbor = signedCbor;
      return submitThrough.call(fixture.emulator, signedCbor);
    };
    let abandoned: unknown;
    try {
      abandoned = await mergeUntilSubmitted(stoppingLucid, stop.signal).then(
        () => "returned",
        (error: unknown) => error,
      );
    } finally {
      fixture.emulator.submitTx = submitThrough;
    }
    expect(heldTxHash).toBeDefined();
    expect(heldTxCbor).toBeDefined();
    expect(
      Runtime.isFiberFailure(abandoned) &&
        Cause.isInterruptedOnly(abandoned[Runtime.FiberFailureCauseId]),
    ).toBe(true);
    expect(await ownStatus(headerHash)).toBe(Status.Applied);
    await expectDeposits(headerHash, DepositsDB.Status.Projected);
    dropPendingEmulatorTransaction(fixture.emulator, heldTxHash!);
    h.observer.forgetDropped(heldTxHash!);
    h.lucidService.api.clearUTxOOverride();
    fixture.operatorLucid.clearUTxOOverride();
    return { txHash: heldTxHash!, txCbor: heldTxCbor! };
  };

  await advanceEmulatorPastLatestBlockEndTime(fixture);

  return {
    fixture,
    get h() {
      return h;
    },
    run,
    sqlRun,
    queuedBlocks,
    ownStatus,
    expectDeposits,
    expectedRoot,
    commitDepositBlock,
    mergeUntilSubmitted,
    withConfirmationWait,
    abandonMergeConfirmation,
    restart: async (afterStop?: () => Promise<void>) => {
      h = await initial.restartRuntime({ afterStop });
    },
    close: async () => {
      try {
        await h.close();
      } finally {
        mergeHooks.l1ClockLeadSlots = 0;
        vi.useRealTimers();
      }
    },
  };
};

export type MergeLifecycle = Awaited<ReturnType<typeof openMergeLifecycle>>;

/** The block is folded, its deposits consumed once, and the confirmed
 * ledger at its root. */
const expectFinalizedOnce = async (m: MergeLifecycle, headerHash: string) => {
  expect(await m.ownStatus(headerHash)).toBe(Status.Folded);
  await m.expectDeposits(headerHash, DepositsDB.Status.Consumed);
  // The ledger audit at the live native owner's durable root.
  const owner = await m.run(Effect.flatMap(Globals, (g) => g.NATIVE_MPF_OWNER));
  if (owner === undefined) throw new Error("Expected live native owner");
  const audit = await m.run(
    runMpfAudit({
      readNativeDurableRoot: Effect.promise(
        async () => (await owner.diagnostics()).durableRoot,
      ),
    }),
  );
  expect(audit.confirmedRoot).toBe(await m.expectedRoot(headerHash));
  expect(audit.diverged).toBe(false);
};

/**
 * The merge attempt writes no local ledger: when it returns, the follower's
 * landed queue shows the merge, yet the block is only applied and its
 * deposits projected. Landed-block processing folds it at the next driver
 * run, once, and a further run changes nothing.
 */
it("leaves a confirmed merge's fold to landed-block processing, which folds it exactly once", async () => {
  const m = await openMergeLifecycle();
  try {
    const block = await m.commitDepositBlock(12_000_000n);
    const merged = await m.mergeUntilSubmitted();
    expect(Either.isRight(merged) && merged.right).toMatchObject({
      status: "merged",
      headerHash: block,
    });
    expect(await m.queuedBlocks()).toBe(0);
    expect(await m.ownStatus(block)).toBe(Status.Applied);
    await m.expectDeposits(block, DepositsDB.Status.Projected);
    await m.h.synchronize();
    await expectFinalizedOnce(m, block);
    await m.h.synchronize();
    await expectFinalizedOnce(m, block);
  } finally {
    await m.close();
  }
});

it("folds a merge that lands after the node stopped awaiting its confirmation, across a restart, and never before it lands", async () => {
  const m = await openMergeLifecycle();
  const { fixture } = m;
  try {
    const block = await m.commitDepositBlock(12_000_000n);
    expect(await m.queuedBlocks()).toBe(1);

    // The node stops while the merge awaits confirmation; the signed merge is
    // kept to hand to L1 again below.
    const held = await m.abandonMergeConfirmation(block);
    // Not landed: L1 still queues the block, and no driver run folds it.
    expect(await m.queuedBlocks()).toBe(1);
    await m.h.synchronize();
    expect(await m.ownStatus(block)).toBe(Status.Applied);
    await m.expectDeposits(block, DepositsDB.Status.Projected);

    // The same signed merge lands while the node is down.
    await m.restart(async () => {
      expect(await fixture.emulator.submitTx(held.txCbor)).toBe(held.txHash);
      fixture.emulator.awaitBlock(1);
      expect(Object.keys(fixture.emulator.mempool)).toEqual([]);
    });
    expect(await m.queuedBlocks()).toBe(0);
    // Landed-block processing folded it on restart, once.
    await expectFinalizedOnce(m, block);
    await m.h.synchronize();
    await expectFinalizedOnce(m, block);
  } finally {
    await m.close();
  }
});

it("fails a merge whose transaction L1 reports failed as a merge that did not confirm, and folds nothing", async () => {
  const m = await openMergeLifecycle();
  try {
    await assertRefusedMergeFoldsNothing(m);
  } finally {
    await m.close();
  }
});

it("folds a landed merge by landed-block processing alone, whatever base its retained journal names", async () => {
  const m = await openMergeLifecycle();
  try {
    await assertLandedMergeFoldedByProcessing(m, expectFinalizedOnce);
  } finally {
    await m.close();
  }
});

it("defers a merge while the ledger tip trails its lower bound, sends nothing, and merges once the tip reaches it", async () => {
  const m = await openMergeLifecycle();
  try {
    await assertMergeWaitsForLedgerTip(m, mergeHooks, expectFinalizedOnce);
  } finally {
    await m.close();
  }
});
