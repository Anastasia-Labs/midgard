/**
 * L2 transaction sharing at a scheduler turn, against the real compiled
 * scheduler validator: two operators' nodes, each on its own database, over
 * real HTTP. The tail operator held the first shift and it has ended; the
 * head operator is next.
 *
 * Without sharing, a transfer submitted only to the tail leaves the head
 * idle, and the tail pays to push the turn to it (see
 * `scheduler-commit-tick-emulator.test.ts`). With sharing, the transfer
 * reaches the head's mempool through its peer route, the head's own commit
 * tick finds the work and takes its turn, and the tail pushes nothing. With
 * the head down, the tail still pushes the turn and takes it back, and
 * nothing on the tail blocks or raises a readiness reason.
 *
 * When the next operator holds every transaction the other has pending, as
 * the other's last catch-up check showed, the other lets it act first: it
 * pushes only after the new-shift grace period. If the next operator lacks
 * one, the push goes at once, and the next operator itself never waits.
 *
 * The block the head would then commit is built by the commit worker, which
 * this test does not run; the operator twin covers that.
 */

import * as SDK from "@al-ft/midgard-sdk";
import {
  generateSeedPhrase,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { blockCommitmentAction } from "../src/fibers/block-commitment.block-commitment-action.js";
import {
  clearRegisteredCommitDueWork,
  shouldSkipIdleCommitPipelineBeforeSchedulerAlignment,
} from "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import {
  alignCommitSchedulerBeforeMutationWorkerIfIdle,
  registerPreLeaseCommitSchedulerDueWorkIfProven,
  shouldSkipForRegisteredCommitDueWork,
} from "../src/fibers/block-commitment.should-skip-for-registered-commit-due-work.js";
import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import { publishL1HeadChange } from "../src/services/l1-head-trigger.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { syncEmulatorChain } from "./helpers/emulator-l1-follower.js";
import { createPeerDatabase } from "./helpers/l2-peer-database.js";
import {
  buildTransfer,
  l2Output,
  operatorFromSeed,
  type PeerNode,
  startPeerNode,
  startShardNode,
  submitTx,
} from "./helpers/l2-peer-node.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
} from "./helpers/operator-inactivity.js";
import {
  activeOperator,
  alignForCommit,
  currentShift,
  expectSelfAppointment,
  initScenario,
  letShiftEnd,
  lucidOf,
  registeredWaitEnd,
  type Scenario,
  walletLovelace,
} from "./helpers/scheduler-successor-scenario.js";

const user = operatorFromSeed(generateSeedPhrase());
const cleanups: (() => Promise<unknown>)[] = [];

afterEach(async () => {
  vi.useRealTimers();
  clearRegisteredCommitDueWork();
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

/** Two operators; the tail holds the first turn, and it has ended. */
const expiredTail = async () => {
  const scenario = await initScenario(2);
  const { fixture } = scenario;
  const [head, tail] = fixture.operators;
  if (head === undefined || tail === undefined)
    throw new Error("Expected two fixture operators");
  await appointFirstSchedulerOperator(fixture);
  await letShiftEnd(fixture);
  return { scenario, head, tail };
};

/** The node of fixture operator `keyHash`, on L1 through its own Lucid. */
const nodeOptions = (
  scenario: Scenario,
  keyHash: string,
  seedPhrase: string,
) => {
  const { fixture } = scenario;
  const lucid: LucidEvolution = lucidOf(scenario, keyHash);
  return {
    operator: operatorFromSeed(seedPhrase),
    activeOperators: fixture.operators.map((operator) => operator.keyHash),
    lucid,
    l1: {
      referenceScriptsAddress: fixture.referenceScriptsAddress,
      switchToOperatorsMainWallet: Effect.void,
      submitSlotSnapshot: () =>
        Effect.succeed({
          source: "test",
          currentSlot: fixture.emulator.slot,
          observedAtMs: fixture.emulator.now(),
          slotLengthMs: 1_000,
        }),
    },
    contracts: fixture.contracts,
    config: { COMMIT_EVENT_DEPTH: 0 },
  };
};

/** A node on its own database. */
const startOwnDatabaseNode = async (
  options: ReturnType<typeof nodeOptions>,
  peers: readonly string[] = [],
) => {
  const { database, drop } = await createPeerDatabase("operator");
  cleanups.push(drop);
  const node = await startPeerNode({ ...options, database, peers });
  cleanups.push(node.close);
  return node;
};

/** A node on the file's shard, where the emulator's follower store lives. */
const startFollowingNode = async (
  options: ReturnType<typeof nodeOptions>,
  peers: readonly string[] = [],
) => {
  const node = await startShardNode({ ...options, peers });
  cleanups.push(node.close);
  return node;
};

/** The node reads the wall clock for its commit target: the emulator's. */
const clockAtEmulator = (scenario: Scenario) => {
  vi.useFakeTimers({ toFake: ["Date"], shouldAdvanceTime: true });
  vi.setSystemTime(scenario.fixture.emulator.now());
};

/**
 * One commitment tick of `node` up to the commit worker, as
 * `blockCommitmentAction` runs it: whether it skipped as idle, what it
 * counted, and whether each pre-lease scheduler step let it on.
 */
const tickUpToWorker = (scenario: Scenario, node: PeerNode, keyHash: string) =>
  node.run(
    Effect.gen(function* () {
      yield* syncEmulatorChain(lucidOf(scenario, keyHash));
      const idle = yield* shouldSkipIdleCommitPipelineBeforeSchedulerAlignment;
      const backlog = yield* Ref.get(node.globals.COMMIT_PIPELINE_BACKLOG);
      if (idle) return { idle, backlog };
      // Each step runs only when the one before it let the tick go on, as
      // in the real tick.
      const waitsOnRegisteredDueWork =
        yield* shouldSkipForRegisteredCommitDueWork;
      const seen = { idle, backlog, waitsOnRegisteredDueWork };
      if (waitsOnRegisteredDueWork) return seen;
      const waitsOnPlannedDueWork =
        yield* registerPreLeaseCommitSchedulerDueWorkIfProven;
      if (waitsOnPlannedDueWork) return { ...seen, waitsOnPlannedDueWork };
      return {
        ...seen,
        waitsOnPlannedDueWork,
        waitsAfterAlignment:
          yield* alignCommitSchedulerBeforeMutationWorkerIfIdle,
      };
    }).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );

const eventually = async (check: () => Promise<boolean>, what: string) => {
  const deadline = performance.now() + 10_000;
  while (!(await check())) {
    if (performance.now() >= deadline)
      throw new Error(`Timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

const funding = l2Output(user, 0x44, 50_000_000n);

/**
 * `sender`'s node with a transfer submitted to it only, shared with
 * `peer`, and then one catch-up check with `peer`, as an L1 advance runs it;
 * resolves once the check is recorded. Whether `peer` holds the transfer is
 * up to `peer`'s ledger.
 */
const shareThenCatchUp = async (
  sender: PeerNode,
  peer: PeerNode,
  peerKeyHash: string,
) => {
  const tx = await buildTransfer(user, [funding], 10_000_000n, sender.config);
  expect((await submitTx(sender.url, tx.txCbor)).status).toBe(202);
  await sender.drain();
  await eventually(
    async () => (await peer.admissions()).length > 0,
    "the peer to receive the transfer",
  );
  await peer.drain();
  const before = peer.received("/peer/inventory");
  await sender.run(publishL1HeadChange(sender.globals));
  await eventually(
    async () =>
      peer.received("/peer/inventory") > before &&
      (await sender.run(Ref.get(sender.globals.L2_PEER_HELD_TXS))).has(
        peerKeyHash,
      ),
    "the catch-up check with the peer",
  );
  return tx;
};

describe("L2 transaction sharing at a scheduler turn", () => {
  it("the next operator takes its turn with a transfer submitted only to the other, which pushes nothing", async () => {
    const { scenario, head, tail } = await expiredTail();
    const { fixture } = scenario;
    // The head's commit path reads the follower store, which lives on the
    // shard; the tail's commit decision is read from L1 below.
    const headNode = await startFollowingNode(
      nodeOptions(scenario, head.keyHash, head.seedPhrase),
    );
    const tailNode = await startOwnDatabaseNode(
      nodeOptions(scenario, tail.keyHash, tail.seedPhrase),
      [headNode.url],
    );
    await headNode.seedLedger([funding]);
    await tailNode.seedLedger([funding]);
    const tx = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      tailNode.config,
    );

    // Submitted to the tail only; it reaches the head through its peer
    // route, under the same id and with the same bytes.
    expect((await submitTx(tailNode.url, tx.txCbor)).status).toBe(202);
    await tailNode.drain();
    await eventually(
      async () => (await headNode.admissions()).length > 0,
      "the head to receive the transfer",
    );
    await headNode.drain();
    expect(await headNode.admissions()).toMatchObject([
      { tx_id: tx.txId, submit_source: "peer", status: "accepted" },
    ]);
    expect(await headNode.mempool()).toEqual([
      { tx_id: tx.txId, tx: Buffer.from(tx.txCbor) },
    ]);

    const headLovelace = await walletLovelace(scenario, head.keyHash);
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    clockAtEmulator(scenario);

    // The head's tick finds the work and takes its turn.
    const headTick = await tickUpToWorker(scenario, headNode, head.keyHash);
    expect(headTick).toEqual({
      idle: false,
      backlog: { mempoolTxCount: 1, pendingUserEventCount: 0 },
      waitsOnRegisteredDueWork: false,
      waitsOnPlannedDueWork: false,
      waitsAfterAlignment: false,
    });
    const turn = await fetchInactivityDirectorySnapshot(fixture);
    expect(activeOperator(turn.scheduler.datum).operator).toBe(head.keyHash);
    expect(await walletLovelace(scenario, head.keyHash)).toBeLessThan(
      headLovelace,
    );
    expect(
      await Effect.runPromise(activeLivenessReasons(headNode.globals)),
    ).toEqual([]);

    // The tail, holding the same transfer, waits for the head's shift and
    // submits nothing.
    const { result } = await alignForCommit(scenario, tail.keyHash);
    expect("dueWork" in result).toBe(true);
    const after = await fetchInactivityDirectorySnapshot(fixture);
    expect(after.scheduler.utxo.txHash).toBe(turn.scheduler.utxo.txHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);
  });

  it("with the next operator holding every pending transfer, the other pushes only after the grace period", async () => {
    const { scenario, head, tail } = await expiredTail();
    const headNode = await startOwnDatabaseNode(
      nodeOptions(scenario, head.keyHash, head.seedPhrase),
    );
    const tailNode = await startFollowingNode(
      nodeOptions(scenario, tail.keyHash, tail.seedPhrase),
      [headNode.url],
    );
    await headNode.seedLedger([funding]);
    await tailNode.seedLedger([funding]);
    const tx = await shareThenCatchUp(tailNode, headNode, head.keyHash);
    expect(await headNode.admissions()).toMatchObject([
      { tx_id: tx.txId, status: "accepted" },
    ]);

    const shift = await currentShift(scenario);
    expect(shift.operator).toBe(tail.keyHash);
    const holdEnd = shift.end + SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS;
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    clockAtEmulator(scenario);
    const waits = {
      idle: false,
      backlog: { mempoolTxCount: 1, pendingUserEventCount: 0 },
      waitsOnRegisteredDueWork: false,
      waitsOnPlannedDueWork: false,
      waitsAfterAlignment: true,
    };

    // At the shift end the tail waits for the grace period, not the push.
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual(
      waits,
    );
    const waitEnd = registeredWaitEnd(scenario);
    expect(waitEnd).toBeGreaterThanOrEqual(holdEnd);
    expect(waitEnd).toBeLessThanOrEqual(holdEnd + 3_000n);
    expect((await currentShift(scenario)).operator).toBe(tail.keyHash);

    // A minute before the grace period ends it still has pushed nothing.
    advanceEmulatorPastUnixTime(scenario.fixture.emulator, holdEnd - 60_000n);
    vi.setSystemTime(scenario.fixture.emulator.now());
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual({
      ...waits,
      waitsOnRegisteredDueWork: true,
      waitsOnPlannedDueWork: undefined,
      waitsAfterAlignment: undefined,
    });
    expect((await currentShift(scenario)).operator).toBe(tail.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);
    expect(
      await Effect.runPromise(activeLivenessReasons(tailNode.globals)),
    ).toEqual([]);

    // The head never took the turn: once the grace period is over the tail
    // pushes it, as it would have at once without sharing.
    advanceEmulatorPastUnixTime(scenario.fixture.emulator, holdEnd + 5_000n);
    vi.setSystemTime(scenario.fixture.emulator.now());
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual({
      ...waits,
      waitsAfterAlignment: true,
    });
    expect((await currentShift(scenario)).operator).toBe(head.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBeLessThan(
      tailLovelace,
    );
  });

  it("with the next operator missing a pending transfer, the other pushes at once", async () => {
    const { scenario, head, tail } = await expiredTail();
    // The head cannot see the transfer's input yet, so it forgets the
    // transfer each time it is sent and the catch-up finds it missing.
    const headNode = await startOwnDatabaseNode(
      nodeOptions(scenario, head.keyHash, head.seedPhrase),
    );
    const tailNode = await startFollowingNode(
      nodeOptions(scenario, tail.keyHash, tail.seedPhrase),
      [headNode.url],
    );
    await tailNode.seedLedger([funding]);
    const tx = await shareThenCatchUp(tailNode, headNode, head.keyHash);
    expect(
      (await tailNode.run(Ref.get(tailNode.globals.L2_PEER_HELD_TXS)))
        .get(head.keyHash)
        ?.has(tx.txIdHex),
    ).toBe(false);
    expect(await headNode.mempool()).toEqual([]);

    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    clockAtEmulator(scenario);
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual({
      idle: false,
      backlog: { mempoolTxCount: 1, pendingUserEventCount: 0 },
      waitsOnRegisteredDueWork: false,
      waitsOnPlannedDueWork: false,
      waitsAfterAlignment: true,
    });
    expect((await currentShift(scenario)).operator).toBe(head.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBeLessThan(
      tailLovelace,
    );
  });

  it("the next operator takes its turn at once even when its peer holds the same transfers", async () => {
    const { scenario, head, tail } = await expiredTail();
    // The head takes a turn, and it ends: the tail is now the successor.
    await expectSelfAppointment(scenario, head.keyHash);
    await letShiftEnd(scenario.fixture);
    const headNode = await startOwnDatabaseNode(
      nodeOptions(scenario, head.keyHash, head.seedPhrase),
    );
    const tailNode = await startFollowingNode(
      nodeOptions(scenario, tail.keyHash, tail.seedPhrase),
      [headNode.url],
    );
    await headNode.seedLedger([funding]);
    await tailNode.seedLedger([funding]);
    const tx = await shareThenCatchUp(tailNode, headNode, head.keyHash);
    expect(
      (await tailNode.run(Ref.get(tailNode.globals.L2_PEER_HELD_TXS)))
        .get(head.keyHash)
        ?.has(tx.txIdHex),
    ).toBe(true);

    clockAtEmulator(scenario);
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual({
      idle: false,
      backlog: { mempoolTxCount: 1, pendingUserEventCount: 0 },
      waitsOnRegisteredDueWork: false,
      waitsOnPlannedDueWork: false,
      waitsAfterAlignment: false,
    });
    expect((await currentShift(scenario)).operator).toBe(tail.keyHash);
  });

  it("with the next operator down, the other still pushes the turn, takes it back and stays ready", async () => {
    const { scenario, head, tail } = await expiredTail();
    const headNode = await startOwnDatabaseNode(
      nodeOptions(scenario, head.keyHash, head.seedPhrase),
    );
    await headNode.stopServing();
    const tailNode = await startFollowingNode(
      nodeOptions(scenario, tail.keyHash, tail.seedPhrase),
      [headNode.url],
    );
    await tailNode.seedLedger([funding]);
    const tx = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      tailNode.config,
    );

    expect((await submitTx(tailNode.url, tx.txCbor)).status).toBe(202);
    await tailNode.drain();
    expect(await tailNode.admissions()).toMatchObject([
      { tx_id: tx.txId, status: "accepted" },
    ]);

    // The tail's real commitment tick pushes the turn to the head, which
    // never takes it.
    clockAtEmulator(scenario);
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    await tailNode.run(
      Effect.zipRight(
        syncEmulatorChain(lucidOf(scenario, tail.keyHash)),
        blockCommitmentAction,
      ).pipe(Effect.provide(IntentJournalWithoutFollower)),
    );
    const pushed = activeOperator(
      (await fetchInactivityDirectorySnapshot(scenario.fixture)).scheduler
        .datum,
    );
    expect(pushed.operator).toBe(head.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBeLessThan(
      tailLovelace,
    );
    expect(
      await Effect.runPromise(activeLivenessReasons(tailNode.globals)),
    ).toEqual([]);

    // Once the head's shift has ended, the tail takes the turn back.
    await letShiftEnd(scenario.fixture);
    vi.setSystemTime(scenario.fixture.emulator.now());
    clearRegisteredCommitDueWork();
    expect(await tickUpToWorker(scenario, tailNode, tail.keyHash)).toEqual({
      idle: false,
      backlog: { mempoolTxCount: 1, pendingUserEventCount: 0 },
      waitsOnRegisteredDueWork: false,
      waitsOnPlannedDueWork: false,
      waitsAfterAlignment: false,
    });
    expect(
      activeOperator(
        (await fetchInactivityDirectorySnapshot(scenario.fixture)).scheduler
          .datum,
      ).operator,
    ).toBe(tail.keyHash);
    expect(
      await Effect.runPromise(activeLivenessReasons(tailNode.globals)),
    ).toEqual([]);
    // Its own admissions kept flowing throughout.
    expect(await tailNode.mempool()).toHaveLength(1);
  });
});
