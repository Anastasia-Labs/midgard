import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../src/fibers/attestation-timeout-correction.js";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  makeGlobalsService,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import { prepareTimedOutTailRemoval } from "./helpers/history-timeout-correction-fixture.js";

const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;

/** The live preprod shape: a deposit block committed, confirmed and locally
 * finalized by the production owner, never attested, then removed on L1 by an
 * accepted attestation-timeout correction. The observer cursor is bootstrapped
 * on the pre-removal queue, as the running node's fiber had it. */
const openRemovedLocallyFinalizedDepositBlock = async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { fixture, lucidService, globals, production } = h;
    const identity = fixture.runtimeOverrides!.deploymentIdentity;
    const manifestId = identity.manifestId;
    if (manifestId === undefined)
      throw new Error("The fixture deployment must be manifest-bound");
    const requiredFinalityDepth = BigInt(
      h.deployment.manifest.l1Finality.confirmationDepth,
    );
    expect(requiredFinalityDepth).toBeGreaterThan(0n);
    // The shared worker shard keeps earlier suites' observer rows.
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM state_queue_terminal_observer_states`;
      }),
    );
    const wallet = fixture.depositorLucid;
    const address = await wallet.wallet().address();
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
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    expect(await wallet.awaitTx(await signed.submit())).toBe(true);
    wallet.overrideUTxOs(await wallet.utxosAt(address));
    await h.synchronize();
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      ),
    );
    expect(deposits).toHaveLength(1);
    await h.deployment.chain.awaitLedgerTime(
      Number(deposits[0]!.facts.inclusion_time) + 1000,
    );
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
    const headerHash = finalized.finalizedHeaderHash;
    await h.synchronize();
    const removal = await prepareTimedOutTailRemoval({
      fixture,
      targetHeaderHash: headerHash,
      deploymentIdentityDigest: manifestId,
    });
    // One correction-fiber tick. A refusal is rethrown with its full cause
    // chain, the same text the fiber logs.
    const reconcile = async (globals: Globals) => {
      const exit = await Effect.runPromiseExit(
        reconcileStateQueueCorrections({
          source: removal.source,
          deploymentIdentityDigest: manifestId,
          stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
          requiredFinalityDepth,
          deploymentManifest: identity.manifest,
          ledgerDeltaLogMax:
            production.nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
        }).pipe(
          Effect.provideService(Globals, globals),
          Effect.provide(Database.layer),
        ),
      );
      if (Exit.isSuccess(exit)) return exit.value;
      throw new Error(Cause.pretty(exit.cause));
    };
    expect((await reconcile(globals)).status).toBe("bootstrapped");
    const cursorBefore = await readObserver();
    expect(cursorBefore.cursorQueue).toEqual(removal.previousQueue);

    const removed = await removal.submit();
    await h.synchronize();
    // Depth 1 is below the release depth: nothing is admitted or reincluded.
    if (requiredFinalityDepth > 1n) {
      const early = await reconcile(globals);
      expect(early.admittedTransactionHashes).toEqual([]);
      expect(await readDeposit()).toEqual({
        status: "projected",
        projectedHeader: headerHash,
      });
      expect((await readJournal(headerHash)).status).toBe(
        Pending.Status.Finalized,
      );
      fixture.emulator.awaitBlock(Number(requiredFinalityDepth - 1n));
      vi.setSystemTime(fixture.emulator.now());
      await h.synchronize();
    }
    // The exact live state: removal final on L1, local reconciliation owed.
    expect(await readDeposit()).toEqual({
      status: "projected",
      projectedHeader: headerHash,
    });
    const journal = await readJournal(headerHash);
    expect(journal.status).toBe(Pending.Status.Finalized);
    expect(journal.correctionDigest).toBeNull();
    return {
      h,
      headerHash,
      removed,
      reconcile,
    };
  } catch (error) {
    await h.close();
    vi.useRealTimers();
    throw error;
  }
};

const readDeposit = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM deposits_utxos`;
      expect(rows).toHaveLength(1);
      return {
        status: rows[0]!.status,
        projectedHeader:
          rows[0]!.projected_header_hash?.toString("hex") ?? null,
      };
    }),
  );

const readJournal = (headerHash: string) =>
  read(Pending.retrieveByHeaderHash(Buffer.from(headerHash, "hex"))).then(
    (row) => {
      const journal = Option.getOrThrow(row);
      return {
        status: journal.status,
        correctionDigest: journal.correction_transition_digest ?? null,
      };
    },
  );

const readObserverRow = () =>
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

const readObserver = async () => {
  const record = (await readObserverRow()).state_record;
  return (typeof record === "string" ? JSON.parse(record) : record) as {
    cursorQueue: unknown;
    admitted: readonly { transactionHash: string }[];
    stateDigest: string;
  };
};

/** A crash after reinclusion committed but before the observer saved: the
 * durable cursor is still the pre-removal one. */
const restoreObserverRow = (row: Awaited<ReturnType<typeof readObserverRow>>) =>
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

const expectReincluded = async (
  headerHash: string,
  removed: Awaited<
    ReturnType<Awaited<ReturnType<typeof prepareTimedOutTailRemoval>>["submit"]>
  >,
) => {
  // The deposit is back in the pending set; the removed header is gone.
  expect(await readDeposit()).toEqual({
    status: "projected",
    projectedHeader: null,
  });
  const journal = await readJournal(headerHash);
  expect(journal.status).toBe(Pending.Status.Abandoned);
  expect(journal.correctionDigest).not.toBeNull();
  const observer = await readObserver();
  expect(observer.cursorQueue).toEqual(removed.nextQueue);
  expect(
    observer.admitted.map(({ transactionHash }) => transactionHash),
  ).toEqual([removed.accepted.transaction.txHash]);
  return journal.correctionDigest;
};

const assertUnchanged = async (
  headerHash: string,
  observerBefore: Awaited<ReturnType<typeof readObserverRow>>,
) => {
  expect(await readDeposit()).toEqual({
    status: "projected",
    projectedHeader: headerHash,
  });
  expect((await readJournal(headerHash)).status).toBe(Pending.Status.Finalized);
  expect(await readObserverRow()).toEqual(observerBefore);
};

const closeLifecycle = async (h: Pick<Lifecycle, "close">) => {
  try {
    await h.close();
  } finally {
    vi.useRealTimers();
  }
};

it("reincludes a removed block's deposit after a restart that preceded local reconciliation, and replays an unsaved observer idempotently", async () => {
  const opened = await openRemovedLocallyFinalizedDepositBlock();
  let h: Pick<Lifecycle, "close"> = opened.h;
  try {
    const { headerHash, removed, reconcile } = opened;
    const observerBefore = await readObserverRow();
    // The process restarts after the removal is final on L1 and before the
    // correction fiber reconciles it: new owner, Globals, cache and runtime.
    const restarted = await opened.h.restartRuntime();
    h = restarted;
    // The superseded runtime's owner cannot produce: refused, nothing written.
    await expect(reconcile(opened.h.globals)).rejects.toThrow(
      "Current authenticated history producer is required",
    );
    await assertUnchanged(headerHash, observerBefore);
    // A runtime without an initialized owner is refused the same way.
    await expect(reconcile(await makeGlobalsService())).rejects.toThrow(
      "Current authenticated history producer is required",
    );
    await assertUnchanged(headerHash, observerBefore);

    const result = await reconcile(restarted.globals);
    expect(result.admittedTransactionHashes).toEqual([
      removed.accepted.transaction.txHash,
    ]);
    const digest = await expectReincluded(headerHash, removed);
    const reincluded = await readObserverRow();

    // Crash after the reinclusion transaction but before the observer save:
    // the next tick replays the same transition and changes nothing.
    await restoreObserverRow(observerBefore);
    const replayed = await reconcile(restarted.globals);
    expect(replayed.admittedTransactionHashes).toEqual([
      removed.accepted.transaction.txHash,
    ]);
    expect(await expectReincluded(headerHash, removed)).toBe(digest);
    expect(await readObserverRow()).toEqual(reincluded);
    // And a steady tick after that is a no-op.
    expect(
      (await reconcile(restarted.globals)).admittedTransactionHashes,
    ).toEqual([]);
    expect(await expectReincluded(headerHash, removed)).toBe(digest);
    await restarted.synchronize();
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("reincludes a removed block's deposit in the same process that committed and locally finalized it", async () => {
  const { h, headerHash, removed, reconcile } =
    await openRemovedLocallyFinalizedDepositBlock();
  try {
    const result = await reconcile(h.globals);
    expect(result.admittedTransactionHashes).toEqual([
      removed.accepted.transaction.txHash,
    ]);
    await expectReincluded(headerHash, removed);
    await h.synchronize();
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);
