import { inspect } from "node:util";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as MutationJobs from "../../src/database/mutationJobs.js";
import { Database } from "../../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import {
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
} from "../deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import { prepareTimedOutTailRemoval } from "./history-timeout-correction-fixture.js";

/** Bounded wait: `work` must settle within `ms` of real time, so a wedge
 * fails here instead of hanging until the test timeout. */
export const settleWithin = async <A>(work: Promise<A>, ms: number) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  const stuck = new Promise<never>((_, reject) => {
    timer = setTimeout(
      () => reject(new Error(`Did not settle within ${ms} ms`)),
      ms,
    );
  });
  try {
    return await Promise.race([work, stuck]);
  } finally {
    clearTimeout(timer);
    work.catch(() => undefined);
  }
};

/** The bound on one history-owner synchronization at or after a signed
 * intent's TTL, where a wedged owner would otherwise hang until the test
 * timeout. */
export const SYNCHRONIZE_BOUND_MS = 240_000;

/** One bounded history-owner synchronization. */
export const synchronizeBounded = (h: {
  synchronize: () => Promise<unknown>;
}) => settleWithin(h.synchronize(), SYNCHRONIZE_BOUND_MS);

export const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

export type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;

export type Handle = Pick<
  Lifecycle,
  | "fixture"
  | "lucidService"
  | "globals"
  | "production"
  | "synchronize"
  | "runWithoutSynchronizing"
>;

export type Removal = Awaited<
  ReturnType<Awaited<ReturnType<typeof prepareTimedOutTailRemoval>>["submit"]>
>;

/** Submit one deposit as the running node's users do; returns its L2
 * inclusion time. */
export const submitDeposit = async (h: Handle, lovelace: bigint) => {
  const { fixture, lucidService } = h;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  // A scheduler refresh pins its predicted change; this flow spends the same
  // wallet before the next one.
  lucidService.api.clearUTxOOverride();
  await ensureSeparateCollateralUtxo(wallet);
  await advanceHistoryAdmissionClock(fixture, "deposit");
  await alignCommitSchedulerBeforeTestWorker({
    fixture,
    lucidService,
    targetEndTimeMs:
      fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  });
  await h.synchronize();
  const built = await Effect.runPromise(
    SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
      l2Address: address,
      l2Datum: null,
      lovelace,
      additionalAssets: {},
      referenceScripts: fixture.referenceScripts.deposit,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  expect(await wallet.awaitTx(await signed.submit())).toBe(true);
  await h.synchronize();
  return built.metadata.inclusionTime;
};

/** Confirm the committed block and run its local finalization with the
 * production commit worker, as the running node does. Resolves to the
 * worker's output; a worker refusal rejects. */
const runLocalFinalization = async (h: Handle) => {
  const { fixture, lucidService, globals, production } = h;
  await runBlockConfirmation(
    globals,
    fixture.contracts,
    lucidService,
    production.nodeConfig,
    production,
  );
  return runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixture.runtimeOverrides!.deploymentIdentity,
    production.nodeConfig,
    { ...production, globals },
  );
};

/** Confirm and locally finalize the block `headerHash`, which must succeed. */
export const finalizeLocally = async (h: Handle, headerHash: string) => {
  const finalized = await runLocalFinalization(h);
  expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The block must be locally finalized");
  expect(finalized.finalizedHeaderHash).toBe(headerHash);
  await synchronizeBounded(h);
  return finalized.finalizedHeaderHash;
};

/** An outref no ledger holds: a spend of it can never apply to any base. */
export const ABSENT_OUTREF_HEX = "ab".repeat(34);

/** The live f5215638 defect, reproduced deterministically: the journal's
 * ledger delta spends an outref absent from its authenticated base, so every
 * local finalization attempt fails before its SQL mutation. */
const corruptLedgerDelta = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(headerHash, "hex");
      const [row] = yield* sql<{ ledger_delta_spent: unknown }>`SELECT
        ledger_delta_spent FROM pending_block_finalizations
        WHERE header_hash = ${header}`;
      const stored = row!.ledger_delta_spent;
      const spent = (
        typeof stored === "string" ? JSON.parse(stored) : stored
      ) as string[];
      // Written back the way the journal writes it.
      const rows = yield* sql`UPDATE pending_block_finalizations
        SET ledger_delta_spent = ${JSON.stringify([...spent, ABSENT_OUTREF_HEX])}
        WHERE header_hash = ${header}
        RETURNING header_hash`;
      expect(rows).toHaveLength(1);
    }),
  );

export const readLocalFinalizationJob = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<MutationJobs.Entry>`SELECT * FROM
        local_mutation_jobs WHERE job_id = ${MutationJobs.localBlockFinalizationJobId(
          headerHash,
        )}`;
      return rows[0];
    }),
  );

/** Commit the next block on the current state-queue tail with the production
 * owner, as the running node does, then confirm it and locally finalize it.
 * With `failed`, local finalization fails deterministically (the live
 * f5215638 state): a failed job row, the journal still pending, and the
 * ledger never advanced. */
export const commitLocallyFinalizedBlock = async (
  h: Pick<Lifecycle, "deployment"> & Handle,
  inclusionTime: number,
  localFinalization: "completed" | "failed" = "completed",
) => {
  const { fixture, lucidService, globals, production } = h;
  await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  const committed = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(committed.submittedTxHash)).toBe(
    true,
  );
  await h.synchronize();
  if (localFinalization === "completed")
    return finalizeLocally(h, committed.submittedHeaderHash);
  const headerHash = committed.submittedHeaderHash;
  await corruptLedgerDelta(headerHash);
  // Each local finalization attempt starts the block's job, and the job's
  // authenticated materializer refuses the delta before any SQL mutation:
  // the failed job and the still-waiting journal of the live f5215638 state.
  for (let attempt = 1; attempt <= 2; attempt += 1) {
    const outcome = await runLocalFinalization(h).then(
      (output) => output,
      (error: unknown) => ({
        type: "Rejected" as const,
        error: inspect(error, { depth: 20 }),
      }),
    );
    expect(outcome.type).not.toBe("SuccessfulLocalFinalizationRecoveryOutput");
    const job = await readLocalFinalizationJob(headerHash);
    expect(
      job?.[MutationJobs.Columns.STATUS],
      inspect(outcome, { depth: 20 }),
    ).toBe(MutationJobs.Status.Failed);
    expect(job?.[MutationJobs.Columns.ATTEMPTS]).toBe(attempt);
    // The job's last error names the underlying reason, not only the
    // DatabaseError's summary.
    expect(job?.[MutationJobs.Columns.LAST_ERROR]).toContain(
      "Pending-finalization ledger delta is invalid for its authenticated base",
    );
    expect(job?.[MutationJobs.Columns.LAST_ERROR]).toContain(
      `ledger delta spends an outref absent from its authenticated base: ${ABSENT_OUTREF_HEX}`,
    );
  }
  await h.synchronize();
  return headerHash;
};

/**
 * Commit the next block with the production commit worker and hand its signed
 * commit to L1, which then loses it: the transaction leaves the emulator
 * mempool without ever landing, as a commit does when a conflicting removal of
 * its parent wins. The journal keeps the submission and its signed intent.
 */
export const submitUnlandedBlock = async (
  h: Pick<Lifecycle, "deployment" | "observer"> & Handle,
  inclusionTime: number,
  { alignScheduler = true }: { alignScheduler?: boolean } = {},
) => {
  const { fixture, lucidService, globals, production } = h;
  await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await synchronizeBounded(h);
  const committed = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
    alignScheduler,
  });
  dropPendingEmulatorTransaction(fixture.emulator, committed.submittedTxHash);
  h.observer.forgetDropped(committed.submittedTxHash);
  // A wallet view pinned to the lost commit's predicted change is stale.
  lucidService.api.clearUTxOOverride();
  fixture.operatorLucid.clearUTxOOverride();
  expect(
    (await fixture.operatorLucid.transactionStatus(committed.submittedTxHash))
      .status,
  ).not.toBe("confirmed");
  await synchronizeBounded(h);
  return committed;
};

type EmulatorLedger = Record<
  string,
  { utxo: { txHash: string }; spent: boolean }
>;

/** Drop the only pending transaction from the emulator: its outputs leave the
 * mempool and the ledger inputs it marked spent are unspent again. Between
 * blocks, every spent ledger entry belongs to a pending transaction. */
export const dropPendingEmulatorTransaction = (
  emulator: Lifecycle["fixture"]["emulator"],
  txHash: string,
) => {
  const state = emulator as unknown as {
    ledger: EmulatorLedger;
    mempool: EmulatorLedger;
    transactionHistory: Record<string, { status: string }>;
  };
  const pending = Object.entries(state.transactionHistory).filter(
    ([, status]) => status.status === "pending",
  );
  expect(pending.map(([hash]) => hash)).toEqual([txHash]);
  for (const [outRef, entry] of Object.entries(state.mempool)) {
    expect(entry.utxo.txHash).toBe(txHash);
    delete state.mempool[outRef];
  }
  for (const entry of Object.values(state.ledger)) entry.spent = false;
  delete state.transactionHistory[txHash];
};

/** Advance to just after the next operator shift starts. */
export const advanceToNextShift = async (h: Handle) => {
  const { fixture } = h;
  const scheduler = await Effect.runPromise(
    SDK.fetchSchedulerUTxOProgram(fixture.operatorLucid, {
      schedulerAddress: fixture.contracts.scheduler.spendingScriptAddress,
      schedulerPolicyId: fixture.contracts.scheduler.policyId,
    }),
  );
  if (scheduler.datum === "NoActiveOperators")
    throw new Error("The first scheduler appointment is missing");
  const shift = SELECTED_DEPLOYMENT_PROFILE.timing.operator_shift_ms;
  let next = Number(scheduler.datum.ActiveOperator.start_time) + shift;
  while (next <= fixture.emulator.now()) next += shift;
  await advanceEmulatorPastUnixTime(fixture, next + 1_000);
  vi.setSystemTime(new Date(fixture.emulator.now()));
};

export type ContentHandle = Pick<Lifecycle, "deployment" | "command"> & Handle;
