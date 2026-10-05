import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit } from "effect";
import { expect, vi } from "vitest";

import { reconcileStateQueueCorrections } from "../../src/fibers/attestation-timeout-correction.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import type { StateQueueCorrectionObserverSource } from "../../src/services/state-queue-correction-observer.js";
import { blockedReasons } from "../../src/services/state-queue-correction-rewind.load-retained-chain.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  SDK,
} from "../deposit-flow-emulator-shared.js";
import {
  advanceToNextShift,
  commitLocallyFinalizedBlock,
  type Handle,
  type Lifecycle,
  read,
  type Removal,
  submitDeposit,
  submitUnlandedBlock,
  synchronizeBounded,
} from "./correction-rewind-scenario.commit-locally-finalized-block.js";
import {
  buildFullContentRemovedBlock,
  readJournal,
  readObserver,
  readObserverRow,
} from "./correction-rewind-scenario.insert-forced-transfer.js";
import { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import { prepareTimedOutTailRemoval } from "./history-timeout-correction-fixture.js";

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
  localFinalization = "completed",
  unlandedTail = false,
  content = false,
  transportFactory,
}: {
  readonly blocks: number;
  /** `failed`: the one removed block's local finalization failed (the live
   * f5215638 state) instead of completing. */
  readonly localFinalization?: "completed" | "failed";
  /** With one block: the removed block carries an L2 transfer, a
   * withdrawal, a forced transaction and a deposit, over a merged block that
   * funded them (see `buildFullContentRemovedBlock`). */
  readonly content?: boolean;
  /** With two blocks: the second block's commit is submitted but never lands
   * (`headers[1]` is then an unlanded descendant of `headers[0]`). */
  readonly unlandedTail?: boolean;
  /** The history transport, for a scenario that rewrites the served queue. */
  readonly transportFactory?: NonNullable<
    Parameters<typeof openHistoryProductionOwnerLifecycle>[0]
  >["transportFactory"];
}) => {
  const h = await openHistoryProductionOwnerLifecycle({ transportFactory });
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
    // plan rows (and their logged rewind gates); the production lifecycle
    // reset does not own them.
    blockedReasons.clear();
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM state_queue_terminal_observer_states`;
        yield* sql`DELETE FROM event_history_recovery_plans`;
      }),
    );
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    const headers: string[] = [];
    let removedContent:
      | Awaited<ReturnType<typeof buildFullContentRemovedBlock>>
      | undefined;
    if (content) {
      if (blocks !== 1 || unlandedTail || localFinalization !== "completed")
        throw new Error(
          "A full-content scenario removes one landed, locally finalized block",
        );
      removedContent = await buildFullContentRemovedBlock(h);
      headers.push(removedContent.headerHash);
    } else if (blocks === 1) {
      const inclusion = await submitDeposit(h, 12_000_000n);
      headers.push(
        await commitLocallyFinalizedBlock(h, inclusion, localFinalization),
      );
    } else if (blocks === 2 && localFinalization === "completed") {
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
      headers.push(
        unlandedTail
          ? (await submitUnlandedBlock(h, second)).submittedHeaderHash
          : await commitLocallyFinalizedBlock(h, second),
      );
    } else
      throw new Error(
        "Scenario supports one or two removed blocks, and a failed local finalization only for one",
      );
    if (unlandedTail && blocks !== 2)
      throw new Error("An unlanded tail needs a removable parent block");
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
    /** While set, the observer's authenticated view reports a rollback of
     * the first removal below its release depth: the queue is its pre-state
     * again and the removal transaction is absent. */
    let rolledBack = false;
    // Every accepted removal, replayed from whatever cursor the observer holds.
    const source: StateQueueCorrectionObserverSource = {
      readQueue: async () =>
        rolledBack
          ? (removals[0]!.checkpoint.previousQueue as Awaited<
              ReturnType<typeof readQueue>
            >)
          : readQueue(),
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
        if (rolledBack) return null;
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
    const tick = async (globals: Globals) => {
      const exit = await Effect.runPromiseExit(
        reconcileStateQueueCorrections({
          source,
          deploymentIdentityDigest: manifestId,
          stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
          requiredFinalityDepth,
          deploymentManifest: identity.manifest,
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
      if (observe) await synchronizeBounded(h);
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
      if (observe) await synchronizeBounded(handle);
    };
    /** The next source block: a forward append at an open gate, which is what
     * notices an owed rewind in production (one L1 block later). Bounded, so
     * a wedged owner fails here instead of at the test timeout. */
    const nextSourceBlock = async (handle: Handle = h) => {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await synchronizeBounded(handle);
    };
    /** The next source block while the owed rewind is refused: the owner
     * journals it and keeps its gate closed, so nothing waits for readiness. */
    const nextSourceBlockWhileRefused = async (
      handle: Pick<Lifecycle, "appendTipWhileGateClosed"> = h,
    ) => {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      return handle.appendTipWhileGateClosed();
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
      nextSourceBlockWhileRefused,
      readQueue,
      removedContent,
      /** Report a post-admission rollback of the first removal to the
       * correction observer (see `rolledBack`). */
      simulateRemovalRollback: () => {
        if (removals.length !== 1)
          throw new Error("Rollback simulation supports one removal");
        rolledBack = true;
      },
      authority: {
        manifestId,
        stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
        requiredFinalityDepth,
      },
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

/** A crash after local reconciliation but before the observer saved: the
 * durable cursor is still the pre-removal one. */
export const observerRowRestore = (
  row: Awaited<ReturnType<typeof readObserverRow>>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const record =
      typeof row.state_record === "string"
        ? row.state_record
        : JSON.stringify(row.state_record);
    yield* sql`UPDATE state_queue_terminal_observer_states
      SET state_digest = ${row.state_digest}, state_record = ${record}
      WHERE deployment_identity_digest = ${row.deployment_identity_digest}`;
  });

export const restoreObserverRow = (
  row: Awaited<ReturnType<typeof readObserverRow>>,
) => read(observerRowRestore(row));
