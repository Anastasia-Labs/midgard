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
 * The block the head would then commit is built by the commit worker, which
 * this test does not run; the operator twin covers that.
 */
import "./utils.js";

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
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
} from "./helpers/operator-inactivity.js";
import {
  activeOperator,
  alignForCommit,
  initScenario,
  letShiftEnd,
  lucidOf,
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
      return {
        idle,
        backlog,
        waitsOnRegisteredDueWork: yield* shouldSkipForRegisteredCommitDueWork,
        waitsOnPlannedDueWork:
          yield* registerPreLeaseCommitSchedulerDueWorkIfProven,
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
