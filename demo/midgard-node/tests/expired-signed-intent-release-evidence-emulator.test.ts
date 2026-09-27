import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Ref } from "effect";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../src/fibers/attestation-timeout-correction.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import type { StateQueueCorrectionObserverSource } from "../src/services/state-queue-correction-observer.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  runLocalFinalizationRecoveryWorker,
} from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  finalizeLocally,
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readObserver,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  advanceL1ToSlot,
  expectReplaced,
  type Handle,
  landSignedCommitAsFork,
  makeRewritableQueueTransport,
  moveToExactSlot,
  nativeRoot,
  readImmutableCounts,
  readPlans,
  resetSharedRows,
  signedTtl,
  snapshotUnreplaced,
  UNLANDED,
} from "./helpers/signed-intent-replacement.js";

/**
 * Which authenticated evidence decides a signed commit E past its TTL when
 * the exact-point queue no longer shows its base D or E itself: E landed and
 * was merged (confirmed state, journaled canonical history, observed merge),
 * D was merged (its successor at that merge decides), or a correction removed
 * D (the correction path owns E). Actual deployed validators, the production
 * history owner and Architecture G; the served queue and the correction
 * observer's authenticated transitions are synthetic where the emulator cannot
 * produce the chain (a merge of a block this node did not merge).
 */

const C = Pending.Columns;

/** The served queue reduced to its root, whose confirmed state is `header`
 * (the merged block): every node output is dropped. */
const mergedIntoRootView =
  (h: Pick<Handle, "fixture">, header: string) =>
  (outputs: readonly LedgerSnapshotOutput[]) => {
    const { policyId } = h.fixture.contracts.stateQueue;
    const rootUnit = policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME;
    const roots = outputs.filter((output) => output.assets[rootUnit] === 1n);
    expect(roots).toHaveLength(1);
    const root = roots[0]!;
    const datum = Data.from(root.datum!, SDK.LinkedListDatum);
    if (!("Root" in datum.data)) throw new Error("The root has a node datum");
    const confirmed = Data.castFrom(datum.data.Root.data, SDK.ConfirmedState);
    const merged: LedgerSnapshotOutput = {
      ...root,
      datum: Data.to(
        {
          data: {
            Root: {
              data: SDK.castConfirmedStateToData({
                ...confirmed,
                prevHeaderHash: confirmed.headerHash,
                headerHash: header,
              }) as never,
            },
          },
          link: null,
        },
        SDK.LinkedListDatum,
      ),
    };
    return [
      ...outputs.filter(
        (output) =>
          !Object.keys(output.assets).some((unit) => unit.startsWith(policyId)),
      ),
      merged,
    ];
  };

/** Locally finalize the block the history owner made available. No
 * confirmation pass runs first: the emulator's own ledger is not the chain
 * the served view shows (only the source view carries the merge), and local
 * finalization reads only the authenticated node the owner recorded. */
const finalizeRecordedBlock = async (h: Handle, headerHash: string) => {
  const { fixture, lucidService, globals, production } = h;
  const finalized = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixture.runtimeOverrides!.deploymentIdentity,
    production.nodeConfig,
    { ...production, globals },
  );
  expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The block must be locally finalized");
  expect(finalized.finalizedHeaderHash).toBe(headerHash);
  await h.synchronize();
};

const expectLandedAndFinalizedOnce = async (
  h: Handle,
  journal: Pending.Record,
  finalize: (
    h: Handle,
    headerHash: string,
  ) => Promise<unknown> = finalizeRecordedBlock,
) => {
  const header = journal[C.HEADER_HASH].toString("hex");
  const observed = await readJournal(header);
  expect(observed[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
  expect(observed[C.CORRECTION_TRANSITION_DIGEST]).toBeNull();
  expect(await readPlans()).toEqual([]);
  expect(Effect.runSync(Ref.get(h.globals.LOCAL_FINALIZATION_PENDING))).toBe(
    true,
  );
  await finalize(h, header);
  expect((await readJournal(header))[C.STATUS]).not.toBe(
    Pending.Status.Abandoned,
  );
  expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
  expect(await readPlans()).toEqual([]);
};

describe.sequential("signed-intent release evidence", () => {
  it("confirms a signed commit whose header the confirmed state holds after a merge, and locally finalizes it once", async () => {
    const view = makeRewritableQueueTransport();
    const h = await openHistoryProductionOwnerLifecycle({
      transportFactory: view.transportFactory,
    });
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(h.fixture);
      const inclusion = await submitDeposit(h, 12_000_000n);
      const lost = await submitUnlandedBlock(h, inclusion);
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      expect(journal.depositEventIds).toHaveLength(1);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // E landed and was merged: the root's confirmed state is E, and neither
      // D nor E is a node. (Kills "drop the confirmed-state evidence": D is
      // then absent with nothing recorded, and E stays unreconciled.)
      view.setRewrite(mergedIntoRootView(h, header));
      moveToExactSlot(h, ttl);
      await h.synchronize();
      await expectLandedAndFinalizedOnce(h, journal);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("confirms a signed commit its journaled canonical history holds though the queue shows neither it nor its base, and commits each transfer once", async () => {
    const view = makeRewritableQueueTransport();
    const h = await openHistoryProductionOwnerLifecycle({
      transportFactory: view.transportFactory,
    });
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(h.fixture);
      const { txIds } = await admitTwoFundedTransfers(h);
      const lost = await submitUnlandedBlock(
        h,
        h.fixture.emulator.now() - 1000,
      );
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // E is included (as on a fork where it landed in its window), and the
      // served queue is that chain after E and a later foreign block were
      // merged: only the history this node journaled shows E. (Kills "drop
      // the canonical-history evidence".)
      advanceL1ToSlot(h, ttl);
      await landSignedCommitAsFork(h, journal[C.SIGNED_TX_CBOR]!);
      view.setRewrite(mergedIntoRootView(h, "f1".repeat(28)));
      await h.synchronize();
      await expectLandedAndFinalizedOnce(h, journal);
      expect(await readImmutableCounts(txIds)).toEqual(
        Object.fromEntries(txIds.map((id) => [id, 1])),
      );
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);
});

/** A synthetic authenticated merge of `previous[1]` into the root. */
const mergeCheckpoint = (
  scenario: Awaited<ReturnType<typeof openCorrectionRewindScenario>>,
  previous: readonly SDK.StateQueueTransitionNode[],
  transactionHash: string,
  blockNo: number,
) => {
  const policyId = scenario.h.fixture.contracts.stateQueue.policyId;
  const [root, merged, ...rest] = previous;
  const lockRef = `${"ee".repeat(32)}#0`;
  const [rootTx, rootIndex] = root!.outRef.split("#") as [string, string];
  const zero = "00".repeat(32);
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: scenario.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: transactionHash,
    chainPointId: transactionHash,
    slot: blockNo.toString(),
    blockNo: blockNo.toString(),
    transactionIndex: "0",
    finalityDepth: "1",
    mintPolicyIds: [policyId],
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            MergeToConfirmedStateV1: {
              yield_to_ref_input_index: 0n,
              header_node_key: merged!.headerHash!,
              confirmed_state_input_outref: {
                transactionId: rootTx,
                outputIndex: BigInt(rootIndex),
              },
              confirmed_state_output_index: 0n,
              m_settlement_redeemer_index: null,
              merged_block_withdrawals_root: zero,
              merged_block_forced_transactions_root: zero,
              merged_block_transactions_root: zero,
              merged_block_deposits_root: zero,
              merged_block_transition_trace_root: zero,
              merged_block_event_to_step_root: zero,
              merged_block_validation_traces_root: zero,
              merged_block_withdrawal_count: 0n,
              merged_block_forced_transaction_count: 0n,
              merged_block_l2_transaction_count: 0n,
              merged_block_deposit_count: 0n,
              merged_block_total_event_count: 0n,
              merged_block_transition_step_count: 0n,
              merged_block_validation_trace_count: 0n,
            },
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [root!.outRef, merged!.outRef],
    referenceInputOutRefs: [lockRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: lockRef,
      datum: "Idle",
    },
    previousQueue: previous,
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }, ...rest],
  });
  if (checkpoint === null) throw new Error("The synthetic merge is not exact");
  expect(checkpoint.checkpointKind).toBe("merge");
  return checkpoint;
};

/** Record merges of the queue's first nodes with the production correction
 * observer: the cursor is re-seeded at `start` (D followed by `successors`),
 * then each merge is observed, pending below the release depth. */
const observeMerges = async (
  scenario: Awaited<ReturnType<typeof openCorrectionRewindScenario>>,
  start: readonly SDK.StateQueueTransitionNode[],
  merges: number,
) => {
  const { h } = scenario;
  const identity = h.fixture.runtimeOverrides!.deploymentIdentity;
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  let queue = start;
  for (let index = 0; index < merges; index += 1) {
    const checkpoint = mergeCheckpoint(
      scenario,
      queue,
      (0xa1 + index).toString(16).repeat(32),
      10 + index,
    );
    checkpoints.push(checkpoint);
    queue = checkpoint.nextQueue;
  }
  const tick = async (source: StateQueueCorrectionObserverSource) => {
    const exit = await Effect.runPromiseExit(
      reconcileStateQueueCorrections({
        source,
        deploymentIdentityDigest: scenario.manifestId,
        stateQueuePolicyId: h.fixture.contracts.stateQueue.policyId,
        requiredFinalityDepth: scenario.requiredFinalityDepth,
        deploymentManifest: identity.manifest,
        ledgerDeltaLogMax:
          h.production.nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
        rewindThroughHistoryOwner: true,
      }).pipe(
        Effect.provideService(Globals, h.globals),
        Effect.provide(Database.layer),
      ),
    );
    if (Exit.isSuccess(exit)) return exit.value;
    throw new Error(Cause.pretty(exit.cause));
  };
  await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
    }),
  );
  const unexpected = async () => {
    throw new Error("No transition is observed while seeding");
  };
  expect(
    (
      await tick({
        readQueue: async () => start,
        observeTransitions: unexpected,
        canonicalDepth: unexpected,
      })
    ).status,
  ).toBe("bootstrapped");
  expect(
    (
      await tick({
        readQueue: async () => queue,
        observeTransitions: async (previous) => {
          expect(previous).toEqual(start);
          return checkpoints;
        },
        canonicalDepth: async () => 1n,
      })
    ).status,
  ).toBe("reconciled");
  const observer = (await readObserver()) as unknown as {
    pending: readonly { transitionKind: string }[];
  };
  expect(observer.pending.map(({ transitionKind }) => transitionKind)).toEqual(
    checkpoints.map(() => "merge"),
  );
};

describe.sequential(
  "signed-intent release after its base left the queue",
  () => {
    it("replaces a signed commit whose base was merged with a foreign successor, never before its TTL", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        expect(UNLANDED).toContain(journal[C.STATUS]);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        expect(h.fixture.emulator.slot).toBeLessThan(ttl);
        const queue = await scenario.readQueue();
        expect(queue.map(({ headerHash }) => headerHash)).toEqual([null, base]);
        // A foreign block F took D's slot; D and then F were merged, so the
        // served queue is a root whose confirmed state is F. Only the observed
        // merge of D names D's successor. (Kills "defer whenever D is absent".)
        const foreign = "f2".repeat(28);
        await observeMerges(
          scenario,
          [...queue, { headerHash: foreign, outRef: `${"f3".repeat(32)}#1` }],
          2,
        );
        const untouched = await snapshotUnreplaced(header);
        view.setRewrite(mergedIntoRootView(h, foreign));
        moveToExactSlot(h, ttl - 1);
        await h.synchronize();
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        moveToExactSlot(h, ttl);
        await h.synchronize();
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("confirms a signed commit an observed merge folded into the confirmed state, and locally finalizes it once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const [root] = await scenario.readQueue();
        // E landed on D; D was merged before the observer's cursor, then E and
        // a later block G were merged. Only the observed merge of E shows E
        // landed: no merge of D is recorded. (Kills "drop the observed-merge
        // evidence".)
        await observeMerges(
          scenario,
          [
            root!,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: "f4".repeat(28), outRef: `${"f5".repeat(32)}#1` },
          ],
          2,
        );
        view.setRewrite(mergedIntoRootView(h, "f4".repeat(28)));
        moveToExactSlot(h, ttl);
        await h.synchronize();
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("confirms a signed commit named as its base's successor by the observed merge of its base, and locally finalizes it once", async () => {
      const view = makeRewritableQueueTransport();
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
        transportFactory: view.transportFactory,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        // E landed on D; D, E and a later block G were merged, but the
        // observer has recorded only the merge of D so far. That merge names E
        // as D's successor. (Kills "wait when the merged successor is E".)
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: "f6".repeat(28), outRef: `${"f7".repeat(32)}#1` },
          ],
          1,
        );
        view.setRewrite(mergedIntoRootView(h, "f6".repeat(28)));
        moveToExactSlot(h, ttl);
        await h.synchronize();
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("defers a signed commit whose base a correction removed to the correction path, which then abandons it under the correction", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h } = scenario;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const untouched = await snapshotUnreplaced(header);
        const removal = await scenario.removeTail(base);
        // The removal waited out D's attestation timeout, past E's TTL; the
        // observer has not recorded it yet, so nothing is decided.
        expect(h.fixture.emulator.slot).toBeGreaterThanOrEqual(ttl);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Observed below its release depth: the correction names D, which this
        // node journals, so E defers to the correction path. (Kills "replace
        // when a correction removed D".)
        await scenario.tick(h.globals);
        await scenario.nextSourceBlock();
        await scenario.nextSourceBlock();
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Admitted: the correction rewind abandons E under the correction.
        await scenario.awaitRemovalFinality();
        expect(
          (await scenario.tick(h.globals)).admittedTransactionHashes,
        ).toEqual([removal.accepted.transaction.txHash]);
        await scenario.nextSourceBlock();
        const digest = (await readObserver()).admitted[0]!.transitionDigest;
        const abandoned = await readJournal(header);
        expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
        expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);
  },
);

const INJECTED_PLAN_FAILURE =
  "injected crash while marking the release applied";

/** A database fault at the plan's final state change, inside the release's
 * own transaction. */
const refusePlanApplication = (refuse: boolean) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      if (!refuse) {
        yield* sql`DROP TRIGGER IF EXISTS midgard_test_refuse_plan_applied
          ON event_history_recovery_plans`;
        yield* sql`DROP FUNCTION IF EXISTS midgard_test_refuse_plan_applied()`;
        return;
      }
      yield* sql.unsafe(`CREATE OR REPLACE FUNCTION midgard_test_refuse_plan_applied()
        RETURNS trigger LANGUAGE plpgsql AS $$
        BEGIN RAISE EXCEPTION '${INJECTED_PLAN_FAILURE}'; END $$`);
      yield* sql.unsafe(`CREATE TRIGGER midgard_test_refuse_plan_applied
        BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW
        WHEN (NEW.state = 'applied' AND OLD.state = 'prepared')
        EXECUTE FUNCTION midgard_test_refuse_plan_applied()`);
    }),
  );

describe.sequential("signed-intent release with a retained plan", () => {
  it("discards a retained release plan once the signed commit is seen landed, and locally finalizes it once without a crash loop", async () => {
    const initial = await openHistoryProductionOwnerLifecycle();
    let h: Handle & Pick<typeof initial, "close"> = initial;
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
      const inclusion = await submitDeposit(initial, 12_000_000n);
      const lost = await submitUnlandedBlock(initial, inclusion);
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // The release is prepared and then crashes before it applies: its plan
      // is retained, nothing else is persisted.
      await refusePlanApplication(true);
      moveToExactSlot(initial, ttl);
      const failure = await initial.synchronize().then(
        () => undefined,
        (error: unknown) => inspect(error, { depth: 40 }),
      );
      expect(failure).toContain("Failed to apply history recovery plan");
      expect((await readPlans()).map(({ state }) => state)).toEqual([
        "prepared",
      ]);
      expect(UNLANDED).toContain((await readJournal(header))[C.STATUS]);
      // Meanwhile the chain followed included E inside its window. The
      // restarted owner sees E landed: it discards the retained plan and
      // records the observation instead of stopping on it at every start.
      // (Kills "fail on a retained plan when E landed".)
      await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
      const restarted = await initial.restartRuntime({
        afterStop: () => refusePlanApplication(false),
      });
      h = restarted;
      // The restarted runtime hydrates the journal without its node, as any
      // startup does; E is on the emulator's own chain, so the confirmation
      // pass re-derives it before local finalization.
      await expectLandedAndFinalizedOnce(restarted, journal, finalizeLocally);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      await refusePlanApplication(false);
      await closeLifecycle(h);
    }
  }, 900_000);
});
