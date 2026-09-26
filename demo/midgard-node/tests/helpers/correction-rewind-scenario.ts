import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../../src/fibers/attestation-timeout-correction.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import type { StateQueueCorrectionObserverSource } from "../../src/services/state-queue-correction-observer.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
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

export const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

export type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;
type Handle = Pick<
  Lifecycle,
  "fixture" | "lucidService" | "globals" | "production" | "synchronize"
>;
type Removal = Awaited<
  ReturnType<Awaited<ReturnType<typeof prepareTimedOutTailRemoval>>["submit"]>
>;

/** Submit one deposit as the running node's users do; returns its L2
 * inclusion time. */
const submitDeposit = async (h: Handle, lovelace: bigint) => {
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
  wallet.overrideUTxOs(await wallet.utxosAt(address));
  await h.synchronize();
  return built.metadata.inclusionTime;
};

/** Commit, confirm and locally finalize the next block on the current
 * state-queue tail with the production owner, as the running node does. */
const commitLocallyFinalizedBlock = async (
  h: Pick<Lifecycle, "deployment"> & Handle,
  inclusionTime: number,
) => {
  const { fixture, lucidService, globals, production } = h;
  const identity = fixture.runtimeOverrides!.deploymentIdentity;
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
  await runBlockConfirmation(
    globals,
    fixture.contracts,
    lucidService,
    production.nodeConfig,
    production,
  );
  const finalized = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    identity,
    production.nodeConfig,
    { ...production, globals },
  );
  expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The deposit block must be locally finalized");
  expect(finalized.finalizedHeaderHash).toBe(committed.submittedHeaderHash);
  await h.synchronize();
  return finalized.finalizedHeaderHash;
};

/** Advance to just after the next operator shift starts. */
const advanceToNextShift = async (h: Handle) => {
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

/**
 * The live preprod shape: deposit blocks committed, confirmed and locally
 * finalized by the production owner, never attested, then removed on L1 by
 * accepted attestation-timeout corrections of the queue tail. The observer
 * cursor is bootstrapped on the pre-removal queue, as the running node's
 * fiber had it. Actual deployed validators and emulator-confirmed
 * transactions; only chain-point names and observer transport are synthetic.
 */
export const openCorrectionRewindScenario = async ({
  blocks,
}: {
  readonly blocks: number;
}) => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { fixture } = h;
    const identity = fixture.runtimeOverrides!.deploymentIdentity;
    const manifestId = identity.manifestId;
    if (manifestId === undefined)
      throw new Error("The fixture deployment must be manifest-bound");
    const requiredFinalityDepth = BigInt(
      h.deployment.manifest.l1Finality.confirmationDepth,
    );
    expect(requiredFinalityDepth).toBeGreaterThan(1n);
    // The shared worker shard keeps earlier suites' observer and recovery
    // plan rows; the production lifecycle reset does not own them.
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM state_queue_terminal_observer_states`;
        yield* sql`DELETE FROM event_history_recovery_plans`;
      }),
    );
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    const headers: string[] = [];
    if (blocks === 1) {
      const inclusion = await submitDeposit(h, 12_000_000n);
      headers.push(await commitLocallyFinalizedBlock(h, inclusion));
    } else if (blocks === 2) {
      // A two-block unattested suffix exists only if the second block is
      // committed before the first one's DA attestation timeout: the node
      // refuses to commit on an expired unattested tail. Each commit also
      // needs its whole validity range inside one operator shift, so both
      // commits happen early in a fresh shift, with the second deposit
      // submitted before the first commit and included only after the first
      // block's end.
      const first = await submitDeposit(h, 12_000_000n);
      await advanceToNextShift(h);
      const second = await submitDeposit(h, 13_000_000n);
      headers.push(await commitLocallyFinalizedBlock(h, first));
      expect((await readJournal(headers[0]!)).depositEventIds).toHaveLength(1);
      headers.push(await commitLocallyFinalizedBlock(h, second));
    } else throw new Error("Scenario supports one or two removed blocks");
    const removals: Removal[] = [];
    const fetchConfig = {
      stateQueueAddress: fixture.contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
    };
    const readQueue = async () =>
      Promise.all(
        (
          await Effect.runPromise(
            SDK.fetchSortedStateQueueUTxOsProgram(
              fixture.operatorLucid,
              fetchConfig,
            ),
          )
        ).map(async (node, index) => ({
          headerHash:
            index === 0
              ? null
              : await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(node)),
          outRef: `${node.utxo.txHash}#${node.utxo.outputIndex}`,
        })),
      );
    // Every accepted removal, replayed from whatever cursor the observer holds.
    const source: StateQueueCorrectionObserverSource = {
      readQueue,
      observeTransitions: async (previous) => {
        const start = removals.findIndex(
          ({ checkpoint }) =>
            JSON.stringify(checkpoint.previousQueue) ===
            JSON.stringify(previous),
        );
        if (start < 0)
          throw new Error("No accepted removal extends the cursor");
        return removals.slice(start).map(({ checkpoint }) => checkpoint);
      },
      canonicalDepth: async (transition) => {
        const removal = removals.find(
          ({ checkpoint }) =>
            checkpoint.transactionHash === transition.transactionHash,
        );
        if (removal === undefined)
          throw new Error("Missing accepted correction receipt");
        const status = await fixture.operatorLucid.transactionStatus(
          transition.transactionHash,
        );
        if (status.status !== "confirmed") return null;
        return BigInt(
          fixture.emulator.blockHeight - removal.acceptedHeight + 1,
        );
      },
    };
    /** One correction-fiber tick. A refusal is rethrown with its full cause
     * chain, the same text the fiber logs. */
    const tick = async (
      globals: Globals,
      options: { readonly rewindThroughHistoryOwner?: boolean } = {},
    ) => {
      const exit = await Effect.runPromiseExit(
        reconcileStateQueueCorrections({
          source,
          deploymentIdentityDigest: manifestId,
          stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
          requiredFinalityDepth,
          deploymentManifest: identity.manifest,
          ledgerDeltaLogMax:
            h.production.nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
          rewindThroughHistoryOwner: options.rewindThroughHistoryOwner ?? true,
        }).pipe(
          Effect.provideService(Globals, globals),
          Effect.provide(Database.layer),
        ),
      );
      if (Exit.isSuccess(exit)) return exit.value;
      throw new Error(Cause.pretty(exit.cause));
    };
    expect((await tick(h.globals)).status).toBe("bootstrapped");
    expect((await readObserver()).cursorQueue).toEqual(await readQueue());
    /** Remove the current queue tail, which must be `headerHash`. The
     * removal is an ordinary L1 transaction from the test wallet, so it can
     * also be submitted while the node is down (`observe: false`). */
    const removeTail = async (
      headerHash: string,
      { observe = true }: { readonly observe?: boolean } = {},
    ) => {
      const removal = await prepareTimedOutTailRemoval({
        fixture,
        targetHeaderHash: headerHash,
        deploymentIdentityDigest: manifestId,
      });
      const removed = await removal.submit();
      removals.push(removed);
      if (observe) await h.synchronize();
      return removed;
    };
    /** Advance until every accepted removal reaches the release depth;
     * `observe: false` advances L1 only, as while the node is down. */
    const awaitRemovalFinality = async (
      handle: Handle = h,
      { observe = true }: { readonly observe?: boolean } = {},
    ) => {
      const latest = Math.max(...removals.map((r) => r.acceptedHeight));
      const needed =
        Number(requiredFinalityDepth) -
        (fixture.emulator.blockHeight - latest + 1);
      if (needed > 0) fixture.emulator.awaitBlock(needed);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      if (observe) await handle.synchronize();
    };
    /** The next source block: a forward append at an open gate, which is what
     * notices an owed rewind in production (one L1 block later). */
    const nextSourceBlock = async (handle: Handle = h) => {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await handle.synchronize();
    };
    return {
      h,
      headers,
      deposit: (lovelace: bigint, handle: Handle = h) =>
        submitDeposit(handle, lovelace),
      removals,
      manifestId,
      requiredFinalityDepth,
      tick,
      removeTail,
      awaitRemovalFinality,
      nextSourceBlock,
      readQueue,
    };
  } catch (error) {
    await h.close();
    vi.useRealTimers();
    throw error;
  }
};

export const readDeposits = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM deposits_utxos
        ORDER BY inclusion_time`;
      return rows.map((row) => ({
        status: row.status,
        projectedHeader: row.projected_header_hash?.toString("hex") ?? null,
      }));
    }),
  );

export const readJournal = (headerHash: string) =>
  read(Pending.retrieveByHeaderHash(Buffer.from(headerHash, "hex"))).then(
    (row) => Option.getOrThrow(row),
  );

export const readObserverRow = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        deployment_identity_digest: Buffer;
        state_queue_policy_id: Buffer;
        state_digest: Buffer;
        state_record: unknown;
      }>`SELECT deployment_identity_digest, state_queue_policy_id, state_digest,
          state_record FROM state_queue_terminal_observer_states`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

export const readObserver = async () => {
  const record = (await readObserverRow()).state_record;
  return (typeof record === "string" ? JSON.parse(record) : record) as {
    cursorQueue: unknown;
    admitted: readonly { transactionHash: string; transitionDigest: string }[];
    stateDigest: string;
  };
};

/** A crash after local reconciliation but before the observer saved: the
 * durable cursor is still the pre-removal one. */
export const restoreObserverRow = (
  row: Awaited<ReturnType<typeof readObserverRow>>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const record =
        typeof row.state_record === "string"
          ? row.state_record
          : JSON.stringify(row.state_record);
      yield* sql`UPDATE state_queue_terminal_observer_states
        SET state_digest = ${row.state_digest}, state_record = ${record}
        WHERE deployment_identity_digest = ${row.deployment_identity_digest}`;
    }),
  );

export const readRecoveryPlans = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        state: string;
        intent: string;
      }>`SELECT state, intent FROM event_history_recovery_plans
        ORDER BY created_at`;
      return rows.map((row) => ({
        state: row.state,
        intent: JSON.parse(row.intent) as {
          domain: string;
          headerHash: string;
          members?: readonly { headerHash: string; transitionDigest: string }[];
          expectedRoot: string;
          targetRoot: string;
        },
      }));
    }),
  );

export const readSqlLedgerRoot = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        root_hex: string;
        utxo_payload_entry_count: number | string | null;
      }>`SELECT root_hex, utxo_payload_entry_count FROM mpf_engine_state
        WHERE store_name = 'ledger'`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

/** Commit the next block with the production commit worker and wait for its
 * L1 acceptance. */
export const commitNextBlock = async (h: Handle) => {
  const { fixture, lucidService, globals, production } = h;
  const next = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(next.submittedTxHash)).toBe(true);
  return next;
};

export const closeLifecycle = async (h: Pick<Lifecycle, "close">) => {
  try {
    await h.close();
  } finally {
    vi.useRealTimers();
  }
};
