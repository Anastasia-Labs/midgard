import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import {
  type LucidEvolution,
  toUnit,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect, Either, Option, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import {
  DepositsDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { mergeAction, type MergeActionResult } from "../src/fibers/merge.js";
import { runLedgerPayloadAudit } from "../src/fibers/mpf-payload-audit.js";
import { listSlotAwareDueWork } from "../src/fibers/slot-aware-due-work.js";
import { HistoryProducer } from "../src/services/event-history-producer.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { MempoolLedgerCache } from "../src/services/mempool-ledger-cache.js";
import { fetchStateQueueSnapshotProgram } from "../src/services/state-queue-topology.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
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
  refreshWalletUtxosFromProvider,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { dropPendingEmulatorTransaction } from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import { assertLandedMergeParentRefusal } from "./helpers/merge-landed-finalization-parent-refusal.js";

// Every confirmed merge's in-flight local-finalization outcome as the merge
// builder reports it, and an optional confirmation window replacing the
// caller's deadline, so a test can make the confirmation wait give up.
const mergeHooks = vi.hoisted(() => ({
  confirmedFinalizations: [] as {
    readonly headerHash: string;
    readonly succeeded: boolean;
  }[],
  confirmationWindowMs: undefined as number | undefined,
}));
vi.mock(
  "../src/transactions/state-queue/merge-to-confirmed-state.js",
  async (importOriginal) => {
    const { Effect: E, Exit: X } = await import("effect");
    const actual =
      await importOriginal<
        typeof import("../src/transactions/state-queue/merge-to-confirmed-state.js")
      >();
    const buildAndSubmitMergeTx: typeof actual.buildAndSubmitMergeTx = (
      lucid,
      fetchConfig,
      contracts,
      options,
    ) =>
      actual.buildAndSubmitMergeTx(lucid, fetchConfig, contracts, {
        ...options,
        ...(mergeHooks.confirmationWindowMs === undefined
          ? {}
          : {
              confirmationDeadlineMs:
                Date.now() + mergeHooks.confirmationWindowMs,
            }),
        onConfirmedFinalization: (outcome) => {
          mergeHooks.confirmedFinalizations.push({
            headerHash: outcome.headerHash,
            succeeded: X.isSuccess(outcome.exit),
          });
          return options?.onConfirmedFinalization?.(outcome) ?? E.void;
        },
      });
    return { ...actual, buildAndSubmitMergeTx };
  },
);

// A target no queued block has: the merge attempt runs its landed-merge
// catch-up and then skips without building a transaction.
const CATCH_UP_ONLY = { expectedHeaderHash: "ff".repeat(28) };

const openMergeLifecycle = async () => {
  mergeHooks.confirmedFinalizations.length = 0;
  mergeHooks.confirmationWindowMs = undefined;
  const initial = await openHistoryProductionOwnerLifecycle();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  const { fixture, lucidService } = initial;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);

  const run = <A, E>(
    effect: Effect.Effect<A, E, any>,
    lucid: unknown = lucidService,
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
      ) as Effect.Effect<A, E, never>,
    );
  const sqlRun = <A>(
    statement: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown, any>,
  ): Promise<A> => run(Effect.flatMap(SqlClient.SqlClient, statement));
  const queuedBlocks = async () =>
    (
      await Effect.runPromise(
        fetchStateQueueSnapshotProgram(
          fixture.operatorLucid,
          fixture.contracts.stateQueue,
          "startup",
        ),
      )
    ).topology.parsedNodeCount - 1;
  const mergeJob = (headerHash: string) =>
    run(
      MutationJobsDB.retrieveByJobId(
        MutationJobsDB.confirmedMergeFinalizationJobId(headerHash),
      ),
    );
  // The block's deposits, all in `status`: projected while its merge is not
  // finalized, consumed once it is.
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
  const authorityState = async () =>
    (
      await sqlRun(
        (sql) =>
          sql<{
            readonly state: string;
          }>`SELECT state FROM event_history_authority`,
      )
    )[0]!.state;
  const catchUp = () => run(mergeAction(true, CATCH_UP_ONLY));

  const submit = async (built: { tx: TxSignBuilder }) => {
    const signed = await built.tx.sign.withWallet().complete();
    const hash = await signed.submit();
    expect(await wallet.awaitTx(hash)).toBe(true);
    await refreshWalletUtxosFromProvider(wallet);
    await h.synchronize();
    return hash;
  };
  // One deposit, committed, confirmed, locally finalized and attested: a
  // block the merge fiber may fold once it matures.
  const commitDepositBlock = async (lovelace: bigint) => {
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
    await h.synchronize();
    await runBlockConfirmation(
      h.globals,
      fixture.contracts,
      lucidService,
      h.production.nodeConfig,
      h.production,
    );
    const recovery = await runLocalFinalizationRecoveryWorker(
      h.globals,
      fixture.contracts,
      lucidService,
      fixture.runtimeOverrides!.deploymentIdentity,
      h.production.nodeConfig,
      { ...h.production, globals: h.globals },
    );
    expect(recovery.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
    const tail = (
      await Effect.runPromise(
        fetchStateQueueSnapshotProgram(
          fixture.operatorLucid,
          fixture.contracts.stateQueue,
          "startup",
        ),
      )
    ).tailCommitBase;
    expect(tail.headerHash).not.toBeNull();
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService,
      globals: h.globals,
      headerHash: tail.headerHash!,
    });
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
  // A forced merge through `lucid`, retried past the merge-submit due work a
  // first attempt may register while the local ledger catches up.
  const mergeUntilSubmitted = async (
    lucid: unknown = lucidService,
  ): Promise<Either.Either<MergeActionResult, unknown>> => {
    for (let round = 1; round <= 3; round += 1) {
      const result = await run(Effect.either(mergeAction(true)), lucid);
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
  // The Lucid service with its L1 client's confirmation wait replaced; the
  // merging-wallet switch runs under the merge's producer permit, which it
  // hands to the replacement.
  const withConfirmationWait = (
    awaitTx: (
      txHash: string,
      land: LucidEvolution["awaitTx"],
      permit: Authority.Token,
    ) => Promise<boolean>,
  ) => {
    let permit: Authority.Token | undefined;
    const api = new Proxy(lucidService.api, {
      get(target, property) {
        if (property === "awaitTx")
          return (txHash: string) =>
            awaitTx(txHash, target.awaitTx.bind(target), permit!);
        const value = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    return {
      ...lucidService,
      api,
      switchToOperatorsMergingWallet: Effect.gen(function* () {
        permit = (yield* HistoryProducer).token;
        yield* lucidService.switchToOperatorsMergingWallet;
      }),
    };
  };

  // A forced merge of the oldest block whose confirmation wait never sees the
  // submitted merge and ends at its deadline, unfinalized. The merge then
  // leaves the emulator's mempool (the emulator, unlike a real provider,
  // hides a pending transaction's inputs), so L1 has not taken it; the signed
  // merge is returned to hand to L1 again.
  const expireMergeConfirmation = async (headerHash: string) => {
    let heldTxHash: string | undefined;
    let heldTxCbor: string | undefined;
    const holdingLucid = withConfirmationWait(async (txHash) => {
      heldTxHash = txHash;
      return new Promise<boolean>(() => {});
    });
    const submitThrough = fixture.emulator.submitTx;
    fixture.emulator.submitTx = async (signedCbor) => {
      heldTxCbor = signedCbor;
      return submitThrough.call(fixture.emulator, signedCbor);
    };
    const reportedFinalizations = mergeHooks.confirmedFinalizations.length;
    mergeHooks.confirmationWindowMs = 1_500;
    let expired: Either.Either<MergeActionResult, unknown>;
    try {
      expired = await Promise.race([
        mergeUntilSubmitted(holdingLucid),
        new Promise<never>((_resolve, reject) =>
          setTimeout(
            () =>
              reject(new Error("the confirmation wait outlived its deadline")),
            60_000,
          ),
        ),
      ]);
    } finally {
      mergeHooks.confirmationWindowMs = undefined;
      fixture.emulator.submitTx = submitThrough;
    }
    expect(heldTxHash).toBeDefined();
    expect(heldTxCbor).toBeDefined();
    expect(
      Either.isLeft(expired) &&
        formatUnknownError(expired.left, { includeCause: true }),
    ).toMatch(
      /local merge finalization blocked[\s\S]*Transaction confirmation deadline passed/,
    );
    expect(mergeHooks.confirmedFinalizations).toHaveLength(
      reportedFinalizations,
    );
    expect(await mergeJob(headerHash)).toBeUndefined();
    dropPendingEmulatorTransaction(fixture.emulator, heldTxHash!);
    h.observer.forgetDropped(heldTxHash!);
    h.lucidService.api.clearUTxOOverride();
    fixture.operatorLucid.clearUTxOOverride();
    return { txHash: heldTxHash!, txCbor: heldTxCbor! };
  };

  // The shared worker shard keeps earlier suites' recovery plan rows, which
  // the production lifecycle reset does not own.
  await sqlRun((sql) => sql`DELETE FROM event_history_recovery_plans`);
  await advanceEmulatorPastLatestBlockEndTime(fixture);

  return {
    fixture,
    get h() {
      return h;
    },
    run,
    sqlRun,
    queuedBlocks,
    mergeJob,
    expectDeposits,
    expectedRoot,
    authorityState,
    catchUp,
    commitDepositBlock,
    mergeUntilSubmitted,
    withConfirmationWait,
    expireMergeConfirmation,
    restart: async (afterStop?: () => Promise<void>) => {
      h = await initial.restartRuntime({ afterStop });
    },
    close: async () => {
      try {
        await h.close();
      } finally {
        mergeHooks.confirmationWindowMs = undefined;
        vi.useRealTimers();
      }
    },
  };
};

export type MergeLifecycle = Awaited<ReturnType<typeof openMergeLifecycle>>;

const expectFinalizedOnce = async (
  m: MergeLifecycle,
  headerHash: string,
  attempts: number,
) => {
  expect(await m.mergeJob(headerHash)).toMatchObject({
    [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
    [MutationJobsDB.Columns.ATTEMPTS]: attempts,
  });
  await m.expectDeposits(headerHash, DepositsDB.Status.Consumed);
  const audit = await m.run(runLedgerPayloadAudit);
  expect(audit.confirmedRoot).toBe(await m.expectedRoot(headerHash));
  expect(audit.diverged).toBe(false);
};

it("finalizes a landed merge whose local finalization failed or was refused by history recovery, exactly once", async () => {
  const m = await openMergeLifecycle();
  const { fixture } = m;
  try {
    // --- A: the finalization fails after its ledger fold committed. -------
    const first = await m.commitDepositBlock(12_000_000n);
    expect(await m.queuedBlocks()).toBe(1);
    const liveOwner = await Effect.runPromise(
      Ref.get(m.h.globals.NATIVE_MPF_OWNER),
    );
    if (liveOwner === undefined) throw new Error("Expected live native owner");
    let failures = 0;
    const failingOwner = new Proxy(liveOwner, {
      get(target, property) {
        if (property === "diagnostics")
          return async () => {
            const job = await m.mergeJob(first);
            if (
              failures === 0 &&
              job?.[MutationJobsDB.Columns.STATUS] ===
                MutationJobsDB.Status.Running
            ) {
              failures += 1;
              throw new Error("test: owner unreachable after the ledger fold");
            }
            return target.diagnostics();
          };
        const value = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    await Effect.runPromise(
      Ref.set(m.h.globals.NATIVE_MPF_OWNER, failingOwner),
    );
    let firstMerge: Either.Either<MergeActionResult, unknown>;
    try {
      firstMerge = await m.mergeUntilSubmitted();
    } finally {
      await Effect.runPromise(Ref.set(m.h.globals.NATIVE_MPF_OWNER, liveOwner));
    }
    expect(failures).toBe(1);
    expect(Either.isLeft(firstMerge)).toBe(true);
    expect(mergeHooks.confirmedFinalizations).toEqual([
      { headerHash: first, succeeded: false },
    ]);
    expect(await m.queuedBlocks()).toBe(0);
    expect(await m.mergeJob(first)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Failed,
      [MutationJobsDB.Columns.ATTEMPTS]: 1,
    });
    // The fold committed: the confirmed ledger is already at the block.
    expect((await m.run(runLedgerPayloadAudit)).confirmedRoot).toBe(
      await m.expectedRoot(first),
    );
    // The next merge attempt retries it before anything else, on the ledger
    // the failed attempt already folded.
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, first, 2);

    // --- B: history recovery revokes Ready while the merge lands. ---------
    const second = await m.commitDepositBlock(9_000_000n);
    expect(await m.queuedBlocks()).toBe(1);
    let revocations = 0;
    const revokingLucid = m.withConfirmationWait(
      async (txHash, land, permit) => {
        await m.run(
          Authority.beginRecovery(
            permit,
            "test: history recovery began while the merge confirmed",
          ),
        );
        revocations += 1;
        return land(txHash);
      },
    );
    const refused = await m.mergeUntilSubmitted(revokingLucid);
    expect(revocations).toBe(1);
    expect(Either.isLeft(refused)).toBe(true);
    expect(await m.queuedBlocks()).toBe(0);
    expect(mergeHooks.confirmedFinalizations.at(-1)).toEqual({
      headerHash: second,
      succeeded: false,
    });
    const refusedJob = await m.mergeJob(second);
    expect(refusedJob).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Failed,
      [MutationJobsDB.Columns.ATTEMPTS]: 1,
    });
    expect(refusedJob?.[MutationJobsDB.Columns.LAST_ERROR]).toContain(
      "History authority generation or owner changed",
    );
    await m.expectDeposits(second, DepositsDB.Status.Projected);
    expect(await m.authorityState()).toBe("recovering");
    // No merge attempt, and so no catch-up, runs while the owner is not
    // Ready.
    const whileRecovering = await m.run(
      Effect.either(mergeAction(true, CATCH_UP_ONLY)),
    );
    expect(
      Either.isLeft(whileRecovering) &&
        (whileRecovering.left as { readonly _tag?: string })._tag,
    ).toBe("MergeProducerPermitUnavailable");
    expect(await m.mergeJob(second)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Failed,
      [MutationJobsDB.Columns.ATTEMPTS]: 1,
    });

    // The revoked owner's lease lapses (expired here, not waited out) and the
    // node restarts: startup leaves the failed job to the runtime.
    await m.restart(async () => {
      await m.run(
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`UPDATE event_history_authority
            SET lease_until = clock_timestamp()`,
        ),
      );
    });
    expect(await m.authorityState()).toBe("ready");
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, second, 2);
    // Finalized once: a further attempt finds nothing left to do.
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, second, 2);
    expect(await m.mergeJob(first)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
      [MutationJobsDB.Columns.ATTEMPTS]: 2,
    });
    expect(Object.keys(fixture.emulator.mempool)).toEqual([]);
  } finally {
    await m.close();
  }
});

it("finalizes a merge that lands after its confirmation wait gave up, across a restart, and never before it lands", async () => {
  const m = await openMergeLifecycle();
  const { fixture } = m;
  try {
    const block = await m.commitDepositBlock(12_000_000n);
    expect(await m.queuedBlocks()).toBe(1);

    // The confirmation wait ends at its deadline; the signed merge is kept to
    // hand to L1 again below.
    const held = await m.expireMergeConfirmation(block);
    expect(mergeHooks.confirmedFinalizations).toEqual([]);
    // Not landed: L1 still queues the block, and no attempt finalizes it.
    expect(await m.queuedBlocks()).toBe(1);
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    expect(await m.mergeJob(block)).toBeUndefined();
    await m.expectDeposits(block, DepositsDB.Status.Projected);

    // The same signed merge lands while the node is down.
    await m.restart(async () => {
      expect(await fixture.emulator.submitTx(held.txCbor)).toBe(held.txHash);
      fixture.emulator.awaitBlock(1);
      expect(Object.keys(fixture.emulator.mempool)).toEqual([]);
    });
    expect(await m.queuedBlocks()).toBe(0);
    expect(await m.mergeJob(block)).toBeUndefined();
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, block, 1);
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, block, 1);
    expect(mergeHooks.confirmedFinalizations).toEqual([]);
  } finally {
    await m.close();
  }
});

it("refuses to fold a landed merge whose retained journal names a different canonical parent", async () => {
  const m = await openMergeLifecycle();
  try {
    await assertLandedMergeParentRefusal(
      m,
      () => mergeHooks.confirmedFinalizations,
    );
  } finally {
    await m.close();
  }
});

/**
 * A previous merge that lands after an attempt's catch-up read L1, but before
 * the attempt builds, is finalized before the next block is merged on top of
 * it, so each is finalized exactly once. While it has not landed, the walk
 * from the last finalized merge leaves it and the block after it alone.
 */
it("finalizes a merge that lands between an attempt's catch-up and its build before merging the next block, and never before it lands", async () => {
  const m = await openMergeLifecycle();
  const { fixture } = m;
  try {
    const first = await m.commitDepositBlock(12_000_000n);
    const firstMerge = await m.mergeUntilSubmitted();
    expect(Either.isRight(firstMerge) && firstMerge.right.status).toBe(
      "merged",
    );
    await expectFinalizedOnce(m, first, 1);

    const second = await m.commitDepositBlock(9_000_000n);
    const third = await m.commitDepositBlock(7_000_000n);
    expect(await m.queuedBlocks()).toBe(2);
    const held = await m.expireMergeConfirmation(second);

    // Not landed: the walk starts at the first block's completed merge and
    // finalizes neither queued block.
    expect(await m.queuedBlocks()).toBe(2);
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    for (const queued of [second, third]) {
      expect(await m.mergeJob(queued)).toBeUndefined();
      await m.expectDeposits(queued, DepositsDB.Status.Projected);
    }

    // The held merge lands just after the next attempt's catch-up read the
    // state-queue root, so that catch-up still sees the second block queued.
    const rootUnit = toUnit(
      fixture.contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    );
    let landings = 0;
    const api = new Proxy(m.h.lucidService.api, {
      get(target, property) {
        if (property === "utxosAtWithUnit")
          return async (address: string, unit: string) => {
            const utxos = await target.utxosAtWithUnit(address, unit);
            if (unit === rootUnit && landings === 0) {
              landings += 1;
              expect(await fixture.emulator.submitTx(held.txCbor)).toBe(
                held.txHash,
              );
              fixture.emulator.awaitBlock(1);
              expect(Object.keys(fixture.emulator.mempool)).toEqual([]);
            }
            return utxos;
          };
        const value = Reflect.get(target, property, target);
        return typeof value === "function" ? value.bind(target) : value;
      },
    });
    const merged = await m.run(Effect.either(mergeAction(true)), {
      ...m.h.lucidService,
      api,
    });
    expect(landings).toBe(1);
    expect(Either.isRight(merged) && merged.right).toMatchObject({
      status: "merged",
      headerHash: third,
    });
    expect(await m.queuedBlocks()).toBe(0);
    // The second block was finalized before the third was built on it, and
    // the third by its own confirmed merge.
    expect(mergeHooks.confirmedFinalizations).toEqual([
      { headerHash: first, succeeded: true },
      { headerHash: third, succeeded: true },
    ]);
    expect(await m.mergeJob(second)).toMatchObject({
      [MutationJobsDB.Columns.STATUS]: MutationJobsDB.Status.Completed,
      [MutationJobsDB.Columns.ATTEMPTS]: 1,
    });
    await m.expectDeposits(second, DepositsDB.Status.Consumed);
    await expectFinalizedOnce(m, third, 1);
    expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
    await expectFinalizedOnce(m, third, 1);
    expect(await m.mergeJob(second)).toMatchObject({
      [MutationJobsDB.Columns.ATTEMPTS]: 1,
    });
  } finally {
    await m.close();
  }
});
