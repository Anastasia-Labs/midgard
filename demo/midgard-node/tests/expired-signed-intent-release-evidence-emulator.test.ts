import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { SIGNED_HEADER_RECOVERY_DOMAIN } from "../src/database/eventHistoryRecoveryPlans.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../src/fibers/attestation-timeout-correction.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.js";
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
  readSqlLedgerRoot,
  settleWithin,
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
  nextPoint,
  readEmulatorQueue,
  readImmutableCounts,
  readPlans,
  resetSharedRows,
  seedCorrectionObserver,
  signedTtl,
  snapshotUnreplaced,
  synchronizeWithin,
  UNLANDED,
  updateJournal,
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
type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;

/** The served queue reduced to its root, whose confirmed state is `header`
 * (the merged block) over `previous` (by default the confirmed header before
 * it, as one merge leaves it): every node output is dropped. */
const mergedIntoRootView =
  (h: Pick<Handle, "fixture">, header: string, previous?: string) =>
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
                prevHeaderHash: previous ?? confirmed.headerHash,
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
  await synchronizeWithin(h);
};

/** The block local finalization replays next, named by its node's asset. */
const availableBlockAssetName = (h: Handle) => {
  const available = Effect.runSync(
    Ref.get(h.globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK),
  );
  return available === "" ? "" : available.assetName;
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
      await synchronizeWithin(h);
      await expectLandedAndFinalizedOnce(h, journal);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("re-derives a landed and merged block's node after a restart, and locally finalizes it once", async () => {
    const view = makeRewritableQueueTransport();
    const initial = await openHistoryProductionOwnerLifecycle({
      transportFactory: view.transportFactory,
    });
    let h: Handle & Pick<typeof initial, "close"> = initial;
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
      const inclusion = await submitDeposit(initial, 12_000_000n);
      const lost = await submitUnlandedBlock(initial, inclusion);
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      view.setRewrite(mergedIntoRootView(initial, header));
      moveToExactSlot(initial, ttl);
      await synchronizeWithin(initial);
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      // The node restarts before local finalization. E's node is merged, so
      // no queue read yields it again; the startup hydration re-derives it
      // from E's retained signed commit. (Kills "hydrate an observed journal
      // without its node": local finalization then has no block and the node
      // is wedged.)
      const restarted = await initial.restartRuntime({ synchronize: false });
      h = restarted;
      await synchronizeWithin(restarted);
      await expectLandedAndFinalizedOnce(restarted, journal);
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
      await synchronizeWithin(h);
      await expectLandedAndFinalizedOnce(h, journal);
      expect(await readImmutableCounts(txIds)).toEqual(
        Object.fromEntries(txIds.map((id) => [id, 1])),
      );
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("replaces a signed commit built on the root once the confirmed state shows a foreign block took the root's slot, never before its TTL", async () => {
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
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // E was built on the root: its base is the confirmed header, which no
      // merge or correction ever names. The observer's view is current and
      // records nothing.
      expect(await seedCorrectionObserver(h)).toHaveLength(1);
      // A foreign block F took the root's slot and was merged: the confirmed
      // state is F and links to E's base. (Kills "drop the confirmed-state
      // slot holder": E's base is absent with nothing recorded, and E defers
      // forever.)
      view.setRewrite(mergedIntoRootView(h, "f2".repeat(28)));
      const untouched = await snapshotUnreplaced(header);
      moveToExactSlot(h, ttl - 1);
      await synchronizeWithin(h);
      expect(await snapshotUnreplaced(header)).toEqual(untouched);
      moveToExactSlot(h, ttl);
      await synchronizeWithin(h);
      await expectReplaced(journal, { handle: h });
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);

  it("revives this node's replaced root-built block that the confirmed state holds from its own signed commit, abandons the unlanded replacement, and locally finalizes the winner once", async () => {
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
      expect(await readEmulatorQueue(h)).toHaveLength(1);
      moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
      await synchronizeWithin(h);
      await expectReplaced(journal, { handle: h });
      // N: E's members on the same base (the root), handed to L1 and lost.
      // The scheduler alignment is skipped as in the revival tests; the view
      // below is synthetic anyway.
      const next = await submitUnlandedBlock(
        h,
        h.fixture.emulator.now() - 1000,
        { alignScheduler: false },
      );
      const replacement = await readJournal(next.submittedHeaderHash);
      expect(replacement[C.BASE_TAIL_HEADER_HASH]).toEqual(
        journal[C.BASE_TAIL_HEADER_HASH],
      );
      // E landed on the root after all and was merged: the confirmed state is
      // E and links to the base. E's node exists only as its signed commit
      // created it; the root is not it. (Kills "drop the confirmed-state slot
      // holder": N defers forever. Kills "take the queue entry holding the
      // winner's header as its node": the root is revived as E's node.)
      await seedCorrectionObserver(h);
      view.setRewrite(mergedIntoRootView(h, header));
      moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
      await synchronizeWithin(h);
      await expectReplaced(replacement, { globalsReset: false, handle: h });
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      expect(availableBlockAssetName(h)).toBe(
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
      );
      await finalizeRecordedBlock(h, header);
      expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
    } finally {
      await closeLifecycle(h);
    }
  }, 900_000);
});

/** What the synthetic observer history is bound to: a correction-rewind
 * scenario, or a lifecycle through `observerContext`. */
type ObserverContext = Pick<
  Scenario,
  "manifestId" | "requiredFinalityDepth"
> & {
  readonly h: Pick<Handle, "fixture" | "globals">;
};

const observerContext = (h: Handle): ObserverContext => {
  const manifestId = h.fixture.runtimeOverrides!.deploymentIdentity.manifestId;
  if (manifestId === undefined)
    throw new Error("The fixture deployment must be manifest-bound");
  return {
    h,
    manifestId,
    requiredFinalityDepth: BigInt(
      h.deployment.manifest.l1Finality.confirmationDepth,
    ),
  };
};

/** A synthetic authenticated merge of `previous[1]` into the root, whose
 * continued root is output `rootOutputIndex` of `transactionHash`. */
const mergeCheckpoint = (
  scenario: ObserverContext,
  previous: readonly SDK.StateQueueTransitionNode[],
  transactionHash: string,
  blockNo: number,
  rootOutputIndex = 0,
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
              confirmed_state_output_index: BigInt(rootOutputIndex),
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
    nextQueue: [
      {
        headerHash: null,
        outRef: `${transactionHash}#${rootOutputIndex.toString()}`,
      },
      ...rest,
    ],
  });
  if (checkpoint === null) throw new Error("The synthetic merge is not exact");
  expect(checkpoint.checkpointKind).toBe("merge");
  return checkpoint;
};

/** A synthetic authenticated commit of `headerHash` onto the tail of
 * `previous` (the root when the queue is empty), which it spends. */
const appendCheckpoint = (
  context: ObserverContext,
  previous: readonly SDK.StateQueueTransitionNode[],
  transactionHash: string,
  headerHash: string,
  blockNo: number,
) => {
  const policyId = context.h.fixture.contracts.stateQueue.policyId;
  const tail = previous.at(-1)!;
  const lockRef = `${"ee".repeat(32)}#0`;
  const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpoint({
    deploymentIdentityDigest: context.manifestId,
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
            CommitBlockHeader: {
              yield_to_ref_input_index: 0n,
              new_block_output_index: 1n,
              continued_latest_block_output_index: 0n,
              operator: "99".repeat(28),
              scheduler_ref_input_index: 0n,
              active_operators_input_index: 0n,
              active_operators_redeemer_index: 0n,
              m_confirmed_state_ref_input_index: null,
              m_head_state_queue_node_ref_input_index: null,
            },
          },
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [tail.outRef],
    referenceInputOutRefs: [lockRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: lockRef,
      datum: "Idle",
    },
    previousQueue: previous,
    nextQueue: [
      ...previous.slice(0, -1),
      { headerHash: tail.headerHash, outRef: `${transactionHash}#0` },
      { headerHash, outRef: `${transactionHash}#1` },
    ],
  });
  if (checkpoint === null) throw new Error("The synthetic commit is not exact");
  expect(checkpoint.checkpointKind).toBe("append");
  return checkpoint;
};

/** One tick of the production correction observer over a synthetic source. */
const observerTick = async (
  scenario: ObserverContext,
  source: StateQueueCorrectionObserverSource,
) => {
  const { h } = scenario;
  const exit = await Effect.runPromiseExit(
    reconcileStateQueueCorrections({
      source,
      deploymentIdentityDigest: scenario.manifestId,
      stateQueuePolicyId: h.fixture.contracts.stateQueue.policyId,
      requiredFinalityDepth: scenario.requiredFinalityDepth,
      deploymentManifest:
        h.fixture.runtimeOverrides!.deploymentIdentity.manifest,
    }).pipe(
      Effect.provideService(Globals, h.globals),
      Effect.provide(Database.layer),
    ),
  );
  if (Exit.isSuccess(exit)) return exit.value;
  throw new Error(Cause.pretty(exit.cause));
};

/** The observer's pending terminal transitions, in record order. */
const readPending = async () =>
  (
    (await readObserver()) as unknown as {
      pending: readonly { transactionHash: string; transitionKind: string }[];
    }
  ).pending;

/** Record `checkpoints` with the production correction observer, from a
 * cursor re-seeded at `start`: every terminal among them is pending below the
 * release depth. Returns the observer's cursor queue after them and the
 * terminals' transaction hashes. */
const recordTransitions = async (
  context: ObserverContext,
  start: readonly SDK.StateQueueTransitionNode[],
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[],
) => {
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
      await observerTick(context, {
        readQueue: async () => start,
        observeTransitions: unexpected,
        canonicalDepth: unexpected,
      })
    ).status,
  ).toBe("bootstrapped");
  return extendTransitions(context, start, checkpoints);
};

/** Record `checkpoints` from the observer's current cursor `from`, pending
 * below the release depth, as `recordTransitions` does. */
const extendTransitions = async (
  context: ObserverContext,
  from: readonly SDK.StateQueueTransitionNode[],
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpoint[],
) => {
  const queue = checkpoints.at(-1)?.nextQueue ?? from;
  const terminals = checkpoints.filter(
    ({ checkpointKind }) => checkpointKind !== "append",
  );
  const before = (await readPending()).length;
  expect(
    (
      await observerTick(context, {
        readQueue: async () => queue,
        observeTransitions: async (previous) => {
          expect(previous).toEqual(from);
          return checkpoints;
        },
        canonicalDepth: async () => 1n,
      })
    ).status,
  ).toBe("reconciled");
  expect(
    (await readPending())
      .slice(before)
      .map(({ transactionHash }) => transactionHash),
  ).toEqual(terminals.map(({ transactionHash }) => transactionHash));
  return {
    queue,
    transactionHashes: terminals.map(({ transactionHash }) => transactionHash),
  };
};

/** Record merges of the queue's first nodes with the production correction
 * observer: the cursor is re-seeded at `start` (D followed by `successors`),
 * then each merge is observed, pending below the release depth. Returns the
 * observer's cursor queue after the merges and their transaction hashes. */
const observeMerges = async (
  scenario: ObserverContext,
  start: readonly SDK.StateQueueTransitionNode[],
  merges: number,
) => {
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
  const recorded = await recordTransitions(scenario, start, checkpoints);
  expect(
    (await readPending()).map(({ transitionKind }) => transitionKind),
  ).toEqual(checkpoints.map(() => "merge"));
  return recorded;
};

/** The merges `observeMerges` recorded reach the release depth: the observer
 * admits them (final), its cursor unchanged. */
const admitObservedMerges = async (
  scenario: ObserverContext,
  observed: Awaited<ReturnType<typeof observeMerges>>,
) => {
  const result = await observerTick(scenario, {
    readQueue: async () => observed.queue,
    observeTransitions: async () => {
      throw new Error("The cursor is current; nothing new is observed");
    },
    canonicalDepth: async () => scenario.requiredFinalityDepth,
  });
  expect(result.admittedTransactionHashes).toEqual(observed.transactionHashes);
  expect(
    (await readObserver()).admitted.map(
      ({ transactionHash }) => transactionHash,
    ),
  ).toEqual(observed.transactionHashes);
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
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("decides again when the correction observer records its base's merge after a first decision found nothing recorded, and replaces the commit", async () => {
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
        // D and a foreign successor F were merged, but the observer (cursor
        // at [root, D]) has recorded neither yet: past E's TTL the first
        // decision defers, and so does the next point while its view is
        // unchanged.
        const foreign = "f8".repeat(28);
        view.setRewrite(mergedIntoRootView(h, foreign));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        await nextPoint(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer then records the merge of D, naming F as D's
        // successor: the next point replaces E. (Kills "a nothing-recorded
        // deferral is sticky": E is never decided again.)
        await observeMerges(
          scenario,
          [...queue, { headerHash: foreign, outRef: `${"f9".repeat(32)}#1` }],
          2,
        );
        await nextPoint(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("decides again when the correction observer, blocked at the first decision, then records its base's merge naming the commit, and locally finalizes it once", async () => {
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
        // E landed on D, and D, E and G were merged. The observer has no view
        // at all (its row is gone), so the first decision past E's TTL
        // defers.
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM state_queue_terminal_observer_states`;
          }),
        );
        const later = "fa".repeat(28);
        view.setRewrite(mergedIntoRootView(h, later));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer bootstraps and records the merge of D, which names E
        // as D's successor: the next point records E landed. (Kills "a
        // blocked-observer deferral is sticky".)
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: later, outRef: `${"fb".repeat(32)}#1` },
          ],
          1,
        );
        await nextPoint(h);
        await expectLandedAndFinalizedOnce(h, journal);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("never records a signed commit landed when a correction removed it after it landed, though its journaled canonical history holds it", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h } = scenario;
      try {
        const [, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const untouched = await snapshotUnreplaced(header);
        // E lands inside its window; the correction fiber's cursor then sees
        // it as the queue tail.
        advanceL1ToSlot(h, ttl);
        await landSignedCommitAsFork(h, journal[C.SIGNED_TX_CBOR]!);
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`DELETE FROM state_queue_terminal_observer_states`;
          }),
        );
        expect((await scenario.tick(h.globals)).status).toBe("bootstrapped");
        // A correction removes E, and the observer records it below its
        // release depth before the history owner sees any of this.
        await scenario.removeTail(header, { observe: false });
        await scenario.tick(h.globals);
        // E is in the journaled canonical history, but the correction path
        // owns it: nothing is recorded or replaced. (Kills "drop the
        // correction-of-the-block deferral": E is recorded landed and
        // locally finalized although it was removed.)
        await scenario.nextSourceBlock();
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("replaces a signed commit whose base a correction removed when this node no longer journals that base", async () => {
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
        await scenario.removeTail(base);
        expect(h.fixture.emulator.slot).toBeGreaterThanOrEqual(ttl);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        await scenario.tick(h.globals);
        // D's journal is pruned (as for a base this node never journaled):
        // no correction path reconciles it with E, so the correction of D
        // decides for replacement. (Kills "always defer on a correction of
        // the base": E waits for a correction path that never comes.)
        await read(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const pruned = yield* sql`DELETE FROM pending_block_finalizations
              WHERE header_hash = ${Buffer.from(base, "hex")}
              RETURNING header_hash`;
            expect(pruned).toHaveLength(1);
          }),
        );
        await scenario.nextSourceBlock();
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("replaces a signed commit whose base was merged while it was still the queue tail", async () => {
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
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        expect(queue.map(({ headerHash }) => headerHash)).toEqual([null, base]);
        // D was merged while it was still the tail, and a later foreign block
        // F was merged after it: the served queue shows neither E nor D, so
        // only the observed merge of D decides, and it names no successor.
        // (Kills "defer when the merged base had no successor".)
        await observeMerges(scenario, queue, 1);
        view.setRewrite(mergedIntoRootView(h, "fe".repeat(28)));
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced block named as its merged base's successor from its own signed commit, abandons the unlanded replacement, and locally finalizes the winner once", async () => {
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
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const queue = await scenario.readQueue();
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        await expectReplaced(journal, { handle: h });
        // N: E's members on the same base, handed to L1 and lost. The
        // scheduler alignment is skipped as in the revival tests; the view
        // below is synthetic anyway.
        const lost = await submitUnlandedBlock(
          h,
          h.fixture.emulator.now() - 1000,
          { alignScheduler: false },
        );
        const replacement = await readJournal(lost.submittedHeaderHash);
        expect(replacement[C.BASE_TAIL_HEADER_HASH].toString("hex")).toBe(base);
        // E landed on D after all; D, E and G were merged, and only the merge
        // of D is recorded, naming E as D's successor. E's node exists only
        // as its signed commit created it. (Kills "revive only a successor
        // whose node is on the queue": N waits forever.)
        const later = "fc".repeat(28);
        await observeMerges(
          scenario,
          [
            ...queue,
            {
              headerHash: header,
              outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
            },
            { headerHash: later, outRef: `${"fd".repeat(32)}#1` },
          ],
          1,
        );
        view.setRewrite(mergedIntoRootView(h, later));
        moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
        await synchronizeWithin(h);
        await expectReplaced(replacement, { globalsReset: false, handle: h });
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.ObservedWaitingStability,
        );
        expect(availableBlockAssetName(h)).toBe(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
        );
        await finalizeRecordedBlock(h, header);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
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
        await synchronizeWithin(h);
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
        await synchronizeWithin(h);
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

/** Every durable row and runtime flag a revival would change. */
const snapshotRevival = async (h: Handle) => ({
  rows: await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return {
        journals: yield* sql`SELECT header_hash, status,
          correction_transition_digest FROM pending_block_finalizations
          ORDER BY header_hash`,
        deposits: yield* sql`SELECT event_id, projected_header_hash
          FROM deposits_utxos ORDER BY event_id`,
        ledger: yield* sql`SELECT root_hex FROM mpf_engine_state
          WHERE store_name = 'ledger'`,
      };
    }),
  ),
  native: await nativeRoot(h),
  localFinalizationPending: Effect.runSync(
    Ref.get(h.globals.LOCAL_FINALIZATION_PENDING),
  ),
  available: availableBlockAssetName(h),
});

const RETAINED_SIGNED_HEADER_RECOVERY_ID = "fc".repeat(32);

/** A prepared signed-header recovery plan at the current cursor, as a crash
 * between its native restore and its SQL repair leaves it. Its header is no
 * journal of this node, so that recovery finds no candidate to resume it
 * with and the plan stays retained. */
const retainSignedHeaderPlan = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const cursors = yield* sql<{
        binding_digest: Buffer;
        manifest_id: Buffer;
        revision: string;
        head_hash: Buffer;
        snapshot_digest: Buffer;
      }>`SELECT binding_digest, manifest_id, revision, head_hash,
          snapshot_digest FROM event_history_cursor`;
      expect(cursors).toHaveLength(1);
      const cursor = cursors[0]!;
      const headerHash = "fd".repeat(28);
      yield* sql`INSERT INTO event_history_recovery_plans
        (recovery_id, binding_digest, manifest_id, header_hash, intent,
         evidence_digest, checkpoint_revision, head_hash, snapshot_digest,
         owner_generation, state)
        VALUES (${Buffer.from(RETAINED_SIGNED_HEADER_RECOVERY_ID, "hex")},
          ${cursor.binding_digest}, ${cursor.manifest_id},
          ${Buffer.from(headerHash, "hex")},
          ${JSON.stringify({
            domain: SIGNED_HEADER_RECOVERY_DOMAIN,
            headerHash,
            expectedRoot: "fe".repeat(32),
          })},
          ${Buffer.from("fb".repeat(32), "hex")}, ${cursor.revision},
          ${cursor.head_hash}, ${cursor.snapshot_digest}, 0, 'prepared')`;
    }),
  );

const discardSignedHeaderPlan = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM event_history_recovery_plans
        WHERE recovery_id = ${Buffer.from(RETAINED_SIGNED_HEADER_RECOVERY_ID, "hex")}`;
    }),
  );

/** Two blocks of this node built on one root output: E, replaced at its TTL
 * while the queue still showed that root as the tail, and its replacement N,
 * handed to L1 and lost (the scheduler alignment is skipped as in the revival
 * tests; the served view is synthetic anyway). */
const replacedRootBuiltPair = async (h: Handle) => {
  await resetSharedRows();
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  const inclusion = await submitDeposit(h, 12_000_000n);
  const lost = await submitUnlandedBlock(h, inclusion);
  const header = lost.submittedHeaderHash;
  const journal = await readJournal(header);
  expect(await readEmulatorQueue(h)).toHaveLength(1);
  moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
  await synchronizeWithin(h);
  await expectReplaced(journal, { handle: h });
  const next = await submitUnlandedBlock(h, h.fixture.emulator.now() - 1000, {
    alignScheduler: false,
  });
  const replacement = await readJournal(next.submittedHeaderHash);
  expect(replacement[C.BASE_TAIL_OUT_REF]).toBe(journal[C.BASE_TAIL_OUT_REF]);
  expect(replacement[C.BASE_TAIL_HEADER_HASH]).toEqual(
    journal[C.BASE_TAIL_HEADER_HASH],
  );
  return { header, journal, replacement };
};

/** A synthetic merge of `base` (the confirmed state's block) that leaves the
 * queue empty under the root output `rootOutRef`, and the queue before it. */
const mergeLeavingRoot = (
  context: ObserverContext,
  base: string,
  rootOutRef: string,
) => {
  const [transactionHash, index] = rootOutRef.split("#") as [string, string];
  const previous: readonly SDK.StateQueueTransitionNode[] = [
    { headerHash: null, outRef: `${"d0".repeat(32)}#0` },
    { headerHash: base, outRef: `${"d1".repeat(32)}#1` },
  ];
  const merge = mergeCheckpoint(
    context,
    previous,
    transactionHash,
    10,
    Number(index),
  );
  expect(merge.nextQueue).toEqual([{ headerHash: null, outRef: rootOutRef }]);
  return { previous, merge };
};

/** Appends of `headers` in order onto `from`, then merges of each, from block
 * `blockNo` on. Each append's transaction is `transactions[i]` when given. */
const appendThenMerge = (
  context: ObserverContext,
  from: readonly SDK.StateQueueTransitionNode[],
  headers: readonly string[],
  blockNo: number,
  transactions: readonly string[] = [],
) => {
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpoint[] = [];
  let queue = from;
  const push = (checkpoint: SDK.StateQueueAuthenticatedReplayCheckpoint) => {
    checkpoints.push(checkpoint);
    queue = checkpoint.nextQueue;
  };
  headers.forEach((header, index) =>
    push(
      appendCheckpoint(
        context,
        queue,
        transactions[index] ?? (0xb1 + index).toString(16).repeat(32),
        header,
        blockNo + index,
      ),
    ),
  );
  headers.forEach((_, index) =>
    push(
      mergeCheckpoint(
        context,
        queue,
        (0xc1 + index).toString(16).repeat(32),
        blockNo + headers.length + index,
      ),
    ),
  );
  return checkpoints;
};

describe.sequential(
  "signed-intent release of a root-built commit after merges",
  () => {
    it("replaces a root-built commit once the observer records the transition after the root was left empty, naming a foreign block, though merges hid the slot from the confirmed state; never before", async () => {
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
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        expect(await readEmulatorQueue(h)).toHaveLength(1);
        const context = observerContext(h);
        const base = journal[C.BASE_TAIL_HEADER_HASH].toString("hex");
        // E was built on the root output B, which the merge of its base D
        // left with an empty queue; the observer recorded that merge. A
        // foreign F then took B, a foreign G followed, and both were merged:
        // the confirmed state is G over F, so nothing links it to D.
        const { previous, merge } = mergeLeavingRoot(
          context,
          base,
          journal[C.BASE_TAIL_OUT_REF],
        );
        const recorded = await recordTransitions(context, previous, [merge]);
        const foreign = "f4".repeat(28);
        const later = "f5".repeat(28);
        view.setRewrite(mergedIntoRootView(h, later, foreign));
        const untouched = await snapshotUnreplaced(header);
        moveToExactSlot(h, ttl - 1);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // Past E's TTL no later transition is recorded yet: which block took
        // B is unknown, so E defers. (Kills "drop the root-emptying arm":
        // the recorded merge of D reads as D merged while still the tail, and
        // E is replaced. Kills "replace while no later transition is
        // recorded".)
        moveToExactSlot(h, ttl);
        await synchronizeWithin(h);
        expect(await snapshotUnreplaced(header)).toEqual(untouched);
        // The observer then records F's and G's appends and merges: the
        // merge of F names F as the block that took B, so the next point
        // replaces E. (Kills "the root-emptying arm always defers".)
        await extendTransitions(
          context,
          recorded.queue,
          appendThenMerge(context, recorded.queue, [foreign, later], 11),
        );
        await nextPoint(h);
        await expectReplaced(journal, { handle: h });
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced root-built block that the observer records taking the emptied root's slot, abandons its unlanded replacement, and locally finalizes the winner once", async () => {
      const view = makeRewritableQueueTransport();
      const h = await openHistoryProductionOwnerLifecycle({
        transportFactory: view.transportFactory,
      });
      try {
        const { header, journal, replacement } = await replacedRootBuiltPair(h);
        const context = observerContext(h);
        // E landed on the root output B after all, which the merge of its
        // base D had left empty; G followed and E and G were merged, all
        // still pending. The confirmed state is G over E: neither it nor an
        // admitted transition shows E landed.
        const { previous, merge } = mergeLeavingRoot(
          context,
          journal[C.BASE_TAIL_HEADER_HASH].toString("hex"),
          journal[C.BASE_TAIL_OUT_REF],
        );
        const later = "f6".repeat(28);
        await recordTransitions(context, previous, [
          merge,
          ...appendThenMerge(context, merge.nextQueue, [header, later], 11, [
            journal[C.INTENDED_TX_HASH]!.toString("hex"),
          ]),
        ]);
        view.setRewrite(mergedIntoRootView(h, later, header));
        // Past N's TTL, the merge after B was left empty names E as the
        // block that took it: N is replaced and E revived. (Kills "drop the
        // root-emptying arm": the recorded merge of D reads as D merged
        // while still the tail, and N is replaced without reviving E.)
        moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
        await synchronizeWithin(h);
        await expectReplaced(replacement, { globalsReset: false, handle: h });
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.ObservedWaitingStability,
        );
        expect(availableBlockAssetName(h)).toBe(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
        );
        await finalizeRecordedBlock(h, header);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);

    it("revives this node's replaced block on the same base output once an admitted merge folds it, with no transition recorded around its base; never on a pending one", async () => {
      const view = makeRewritableQueueTransport();
      const h = await openHistoryProductionOwnerLifecycle({
        transportFactory: view.transportFactory,
      });
      try {
        const { header, journal, replacement } = await replacedRootBuiltPair(h);
        const context = observerContext(h);
        // E landed on the root and G followed; the observer saw only the
        // queue [root, E, G] and then the merges of E and G, still pending.
        // Nothing it recorded names E's base or its root output.
        const eTx = journal[C.INTENDED_TX_HASH]!.toString("hex");
        const later = "f7".repeat(28);
        const observed = await observeMerges(
          context,
          [
            { headerHash: null, outRef: `${eTx}#0` },
            { headerHash: header, outRef: `${eTx}#1` },
            { headerHash: later, outRef: `${"c7".repeat(32)}#1` },
          ],
          2,
        );
        view.setRewrite(mergedIntoRootView(h, later, header));
        // Past N's TTL a pending merge of E may still be retracted: N defers.
        // (Kills "a pending merge shows a replaced sibling landed".)
        const untouched = await snapshotUnreplaced(
          replacement[C.HEADER_HASH].toString("hex"),
        );
        moveToExactSlot(h, signedTtl(replacement[C.SIGNED_TX_CBOR]!));
        await synchronizeWithin(h);
        expect(
          await snapshotUnreplaced(replacement[C.HEADER_HASH].toString("hex")),
        ).toEqual(untouched);
        // Once the merges are admitted, E's own landing decides: N is
        // replaced and E revived. (Kills "drop the replaced-sibling arm": N
        // defers forever.)
        await admitObservedMerges(context, observed);
        await nextPoint(h);
        await expectReplaced(replacement, { globalsReset: false, handle: h });
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.ObservedWaitingStability,
        );
        expect(availableBlockAssetName(h)).toBe(
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
        );
        await finalizeRecordedBlock(h, header);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
      } finally {
        await closeLifecycle(h);
      }
    }, 900_000);
  },
);

describe.sequential("replaced-block revival evidence", () => {
  it("revives a replaced block while no journal is active only once an admitted merge saw it on the queue, never on a pending merge's hint or while a plan is retained, and hands it to local finalization", async () => {
    const scenario = await openCorrectionRewindScenario({
      blocks: 2,
      unlandedTail: true,
    });
    const { h } = scenario;
    try {
      const [, header] = scenario.headers as [string, string];
      const journal = await readJournal(header);
      const queue = await scenario.readQueue();
      moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
      await synchronizeWithin(h);
      await expectReplaced(journal, { handle: h });
      const replaced = await snapshotRevival(h);
      // The observer records a merge of D that saw E as D's successor, below
      // its release depth: a hint that makes E a revival candidate, bound to
      // no checkpoint and not final. No journal is active and nothing bound
      // to the checkpoint shows E landed, so nothing is revived. (Kills
      // "revive a candidate on the observer's hint alone" and "take a pending
      // merge as evidence".)
      const observed = await observeMerges(
        scenario,
        [
          ...queue,
          {
            headerHash: header,
            outRef: `${journal[C.INTENDED_TX_HASH]!.toString("hex")}#1`,
          },
        ],
        1,
      );
      await nextPoint(h);
      expect(await snapshotRevival(h)).toEqual(replaced);
      // The merge becomes final while a signed-header recovery plan is
      // retained: the revival waits for it, and the gate stays closed.
      // (Kills "revive while a plan is retained".)
      await admitObservedMerges(scenario, observed);
      await retainSignedHeaderPlan();
      try {
        h.fixture.emulator.awaitBlock(1);
        vi.setSystemTime(new Date(h.fixture.emulator.now()));
        expect(await h.appendTipWhileGateClosed()).toBeDefined();
        expect(await snapshotRevival(h)).toEqual(replaced);
      } finally {
        await discardSignedHeaderPlan();
      }
      // With no plan retained, the admitted merge that saw E on the queue
      // revives it. (Kills "drop the admitted-merge evidence": E stays
      // abandoned.)
      await nextPoint(h);
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      expect((await readSqlLedgerRoot()).root_hex).toBe(
        journal[C.EXPECTED_UTXOS_ROOT],
      );
      expect(
        Effect.runSync(Ref.get(h.globals.LOCAL_FINALIZATION_PENDING)),
      ).toBe(true);
      expect(availableBlockAssetName(h)).toBe(
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header,
      );
      // The native root stays at the base: local finalization replays E.
      expect(await nativeRoot(h)).toBe(journal[C.BASE_UTXOS_ROOT]);
    } finally {
      await discardSignedHeaderPlan();
      await closeLifecycle(h);
    }
  }, 900_000);
});

const INJECTED_PLAN_FAILURE =
  "injected crash while marking the release applied";
const INJECTED_REPLAY_INTERRUPT = "injected crash after the native replay";

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
      const failure = await settleWithin(initial.synchronize(), 240_000).then(
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

  it("replays a landed, locally finalized block natively when it discards a retained plan whose native rewind already ran", async () => {
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
      // E's local finalization completed (markLocalFinalizationComplete):
      // recording E observed finalizes it without replaying it again.
      await updateJournal(header, {
        [C.STATUS]: Pending.Status.SubmittedUnconfirmed,
      });
      // Past E's TTL the release prepares its plan and runs the native rewind
      // to E's base; its SQL application then fails, so the plan is retained
      // with native state at the base.
      await refusePlanApplication(true);
      moveToExactSlot(initial, ttl);
      const failure = await settleWithin(
        initial.synchronize().then(
          () => undefined,
          (error: unknown) => inspect(error, { depth: 40 }),
        ),
        240_000,
      );
      expect(failure).toContain("Failed to apply history recovery plan");
      expect((await readPlans()).map(({ state }) => state)).toEqual([
        "prepared",
      ]);
      expect(await nativeRoot(initial)).toBe(journal[C.BASE_UTXOS_ROOT]);
      // E landed after all. The restarted owner discards the retained plan
      // and records E finalized before the native owner's startup would
      // replay E's journal, so it must replay E natively itself first. (Kills
      // "discard the retained plan without replaying E natively": native
      // state stays at E's base while E is recorded finalized.)
      await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
      const restarted = await initial.restartRuntime({
        synchronize: false,
        afterStop: () => refusePlanApplication(false),
      });
      h = restarted;
      await synchronizeWithin(restarted);
      expect((await readJournal(header))[C.STATUS]).toBe(
        Pending.Status.Finalized,
      );
      expect(await readPlans()).toEqual([]);
      expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
    } finally {
      await refusePlanApplication(false);
      await closeLifecycle(h);
    }
  }, 900_000);

  it("resumes a retained release plan before an owed correction rewind of its base, and converges once", async () => {
    const scenario = await openCorrectionRewindScenario({
      blocks: 2,
      unlandedTail: true,
    });
    const { h: initial } = scenario;
    let h: Handle & Pick<typeof initial, "close"> = initial;
    try {
      const [base, header] = scenario.headers as [string, string];
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      // Past E's TTL with D still the tail, the release prepares E's
      // replacement and then fails to apply it: the plan is retained.
      await refusePlanApplication(true);
      moveToExactSlot(initial, ttl);
      const failure = await settleWithin(
        initial.synchronize().then(
          () => undefined,
          (error: unknown) => inspect(error, { depth: 40 }),
        ),
        240_000,
      );
      expect(failure).toContain("Failed to apply history recovery plan");
      expect((await readPlans()).map(({ state }) => state)).toEqual([
        "prepared",
      ]);
      // While the node is down, an attestation-timeout correction removes D
      // and becomes final; the correction fiber admits it.
      const removal = await scenario.removeTail(base, { observe: false });
      await scenario.awaitRemovalFinality(initial, { observe: false });
      const restarted = await initial.restartRuntime({
        synchronize: false,
        afterStop: () => refusePlanApplication(false),
      });
      h = restarted;
      expect(
        (await scenario.tick(restarted.globals)).admittedTransactionHashes,
      ).toEqual([removal.accepted.transaction.txHash]);
      // Bounded: the release resumes its own retained plan first (an owed
      // rewind waits for every retained plan), then the rewind abandons D
      // under the correction. (Kills "honor the owed rewind before the
      // retained release plan": each waits for the other and the owner never
      // becomes ready.)
      await synchronizeWithin(restarted);
      await scenario.nextSourceBlock(restarted);
      await expectReplaced(journal, { globalsReset: false, handle: restarted });
      const digest = (await readObserver()).admitted[0]!.transitionDigest;
      const removed = await readJournal(base);
      expect(removed[C.STATUS]).toBe(Pending.Status.Abandoned);
      expect(removed[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
      expect(
        (await readPlans()).filter(({ state }) => state !== "applied"),
      ).toEqual([]);
    } finally {
      await refusePlanApplication(false);
      await closeLifecycle(h);
    }
  }, 900_000);

  it("records an unpromoted signed commit landed over its retained base-to-base plan without replaying it natively, so an interrupted attempt stays resumable, and locally finalizes it once", async () => {
    const initial = await openHistoryProductionOwnerLifecycle();
    let h: Handle & Pick<typeof initial, "close"> = initial;
    const prototype = ProductionNativeMpfOwnerService.prototype;
    const recover = prototype.recover;
    let replayedWhileRetained = 0;
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
      const inclusion = await submitDeposit(initial, 12_000_000n);
      const lost = await submitUnlandedBlock(initial, inclusion);
      const header = lost.submittedHeaderHash;
      const journal = await readJournal(header);
      const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
      const base = journal[C.BASE_UTXOS_ROOT];
      const candidate = journal[C.EXPECTED_UTXOS_ROOT];
      // E was never promoted: its commit is signed but unacknowledged and
      // the native root is still at its base. The fixture worker always
      // promotes, so the state is set up directly.
      expect(await nativeRoot(initial)).toBe(candidate);
      await updateJournal(header, {
        [C.STATUS]: Pending.Status.PendingSubmission,
        [C.SUBMITTED_TX_HASH]: null,
      });
      const owner = await Effect.runPromise(
        Ref.get(initial.globals.NATIVE_MPF_OWNER),
      );
      if (owner === undefined) throw new Error("Native owner is not open");
      await owner.restoreCanonicalRoot({
        recoveryId: "0e".repeat(32),
        expectedRoot: candidate,
        targetRoot: base,
      });
      expect(await nativeRoot(initial)).toBe(base);
      // Past E's TTL the release prepares its plan from the base root, base
      // to base, and then fails to apply it: the plan is retained.
      await refusePlanApplication(true);
      moveToExactSlot(initial, ttl);
      const failure = await settleWithin(
        initial.synchronize().then(
          () => undefined,
          (error: unknown) => inspect(error, { depth: 40 }),
        ),
        240_000,
      );
      expect(failure).toContain("Failed to apply history recovery plan");
      const plans = await readPlans();
      expect(plans.map(({ state }) => state)).toEqual(["prepared"]);
      expect(
        (plans[0]!.intent as { expectedRoot?: unknown }).expectedRoot,
      ).toBe(base);
      expect(await nativeRoot(initial)).toBe(base);
      // E landed after all. The restarted owner discards the base-to-base
      // plan and records E landed without replaying it first; the observed
      // journal is replayed natively only after that (as any startup replays
      // an observed journal), with no plan retained. A native replay while
      // the plan is still retained is interrupted right after it ran, as a
      // crash before the discard would. (Kills "replay the landed block over
      // any retained plan": that replay moves the native root to E's
      // candidate, outside the plan's roots, and the interrupted attempt can
      // never resume.)
      await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
      const restarted = await initial.restartRuntime({
        synchronize: false,
        afterStop: async () => {
          await refusePlanApplication(false);
          prototype.recover = async function (
            this: ProductionNativeMpfOwnerService,
            replay,
          ) {
            await recover.call(this, replay);
            if ((await readPlans()).some(({ state }) => state === "prepared")) {
              replayedWhileRetained += 1;
              throw new Error(INJECTED_REPLAY_INTERRUPT);
            }
          };
        },
      });
      h = restarted;
      try {
        await synchronizeWithin(restarted);
      } finally {
        prototype.recover = recover;
      }
      expect(replayedWhileRetained).toBe(0);
      await expectLandedAndFinalizedOnce(restarted, journal, finalizeLocally);
      expect(
        (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
      ).toEqual([header]);
    } finally {
      prototype.recover = recover;
      await refusePlanApplication(false);
      await closeLifecycle(h);
    }
  }, 900_000);
});
