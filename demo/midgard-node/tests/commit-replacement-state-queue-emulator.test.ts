/**
 * Whichever lands wins (plan §8.3, I3) for two real state-queue commits on
 * one tail. The production commit worker builds both, journaling them in
 * the production intent journal over the follower's facts. The production
 * history owner runs the landed-block rebase. The node's landed-block
 * processing (`processLandedQueue` over `nodeLandedBlockPorts`, the follower
 * run's hook) reads the emulator's queue through the follower stand-in.
 *
 * - (a) The replacement lands. The old journal is disposed of, the native
 *   root returns to the base, and the next commit builds on the winner.
 * - (b) The old signed commit lands instead. The landed-block rebase revives
 *   its journal and disposes of the replacement's, the native root is the
 *   old commit's, and the next commit builds on it, carrying the L2
 *   transfer the disposed-of replacement had taken from the mempool.
 * - (c) A deposit leaves the chain (a rollback past its admission). The
 *   journal holding it is disposed of, S6 abandons its intent, and the
 *   replacement omits the deposit.
 *
 * Keeping the old commit from landing: every old commit (and the
 * replacement in (b)) is dropped from the emulator's mempool before any
 * block (`dropPendingEmulatorTransaction`). That is a commit lost in
 * transit. Its inputs stay live and its signed bytes stay valid in the
 * journal, so (b) can still land them with `emulator.submitTx`. A
 * conflicting landed spend would instead make the old commit dead by the
 * chain, and (b) could not happen. The emulator has no mempool expiry, so
 * each round names what kills the dropped commit:
 *
 * - (a) Its validity window expires: S6 derives `expired` from the
 *   follower's tip. This is the production cause for a lost commit.
 * - (b) S6's `abandoned` event, as written when the family no longer wants
 *   the intent. Expiry is not usable here: an expired commit cannot land.
 * - (c) The rollback orphans a deposit it holds (`l1_event_keys`); the
 *   commit itself stays valid. The production disposition disposes of its
 *   journal, and an S6 pass with the §8.4 commit predicate abandons it.
 *
 * Stand-in limits: the follower stand-in writes no `l1_txs` landings and
 * drops spent outputs instead of marking their spender. The journey writes
 * each landing (`recordLanding`), and writes (b)'s replacement dead as S6's
 * abandoned event where the follower would read it conflicted. (c) rolls
 * back with no unpublished L2 transaction in the node's mempool; the
 * requeue of one after a journal disposal is not covered here.
 */
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import { LANDED_COMMIT_BASE_PENDING } from "../src/workers/commit-block-header.resolve-commit-base-ledger-entries.js";
import { nativeRoot } from "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceEmulatorToDueWork,
  alignCommitSchedulerBeforeTestWorker,
  fetchLatestCommittedBlock,
  runCommitWorker,
} from "./deposit-flow-emulator-shared.js";
import {
  followFork,
  openNodeServices,
} from "./helpers/commit-replacement-state-queue.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  closeLifecycle,
  depositorL2Utxos,
  dropPendingEmulatorTransaction,
  finalizeLocally,
  type Lifecycle,
  readJournal,
  submitDeposit,
  synchronizeBounded,
} from "./helpers/correction-rewind-scenario.js";
import { emulatorState } from "./helpers/emulator-snapshot.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";

const C = Pending.Columns;
const S = Pending.Status;

let source: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
let h: Lifecycle;
/** The handle the rounds run under: `h`, or after (c)'s rollback, its fork. */
let live: Lifecycle;
let observer: Pick<Lifecycle["observer"], "forgetDropped">;
let node: Awaited<ReturnType<typeof openNodeServices>>;
/** The landed block the next commit must build on. */
let winner: string;

/** One commit through the production worker with the follower's journal. */
const commit = async () => {
  const { fixture, globals, production } = live;
  const lucidService = node.nodeLucid;
  for (let attempt = 1; attempt <= 4; attempt += 1) {
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs: Date.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await synchronizeBounded(live);
    await node.mirrorTracked();
    const output = await runCommitWorker(
      fixture.contracts,
      lucidService,
      await fetchLatestCommittedBlock(fixture.operatorLucid, fixture.contracts),
      production.nodeConfig,
      fixture.runtimeOverrides!.deploymentIdentity,
      { ...production, globals },
      node.journal,
    );
    if (output?.type === "SubmittedAwaitingConfirmationOutput") return output;
    if (
      output?.type === "AwaitingCommitBaseOutput" &&
      output.detail === LANDED_COMMIT_BASE_PENDING
    )
      continue;
    if (output?.type !== "RegisteredDueWorkOutput")
      throw new Error(`Unexpected commit output: ${JSON.stringify(output)}`);
    await advanceEmulatorToDueWork(fixture, output.dueWork);
  }
  throw new Error("The commit was not submitted");
};

const journalOf = async (headerHash: string) => {
  const journal = await readJournal(headerHash);
  return {
    header: headerHash,
    status: journal[C.STATUS],
    base: journal[C.BASE_TAIL_HEADER_HASH].toString("hex"),
    baseRoot: journal[C.BASE_UTXOS_ROOT],
    root: journal[C.EXPECTED_UTXOS_ROOT],
    txHash: journal[C.INTENDED_TX_HASH]!.toString("hex"),
    signed: journal[C.SIGNED_TX_CBOR]!,
    deposits: journal.depositEventIds.map((id) => id.toString("hex")).sort(),
    txs: journal.mempoolTxIds.map((id) => id.toString("hex")).sort(),
  };
};
type Journal = Awaited<ReturnType<typeof journalOf>>;

const statusOf = async (journal: Journal) =>
  (await journalOf(journal.header)).status;

/** A commit whose transaction the emulator loses before any block. */
const commitUnlanded = async () => {
  const committed = await commit();
  dropPendingEmulatorTransaction(h.fixture.emulator, committed.submittedTxHash);
  observer.forgetDropped(committed.submittedTxHash);
  h.fixture.operatorLucid.clearUTxOOverride();
  await synchronizeBounded(live);
  return journalOf(committed.submittedHeaderHash);
};

/** A commit that lands, then the follower run's landed-block processing. */
const commitLanded = async () => {
  const committed = await commit();
  expect(await h.fixture.operatorLucid.awaitTx(committed.submittedTxHash)).toBe(
    true,
  );
  await synchronizeBounded(live);
  const landed = await journalOf(committed.submittedHeaderHash);
  await node.recordLanding(landed.signed);
  expect(await node.processLanded()).toBeUndefined();
  return landed;
};

/** A deposit, with the ledger past its inclusion time. */
const deposit = async (lovelace: bigint) => {
  const inclusion = await submitDeposit(live, lovelace);
  await live.deployment.chain.awaitLedgerTime(inclusion + 1000);
  vi.setSystemTime(h.fixture.emulator.now());
  await synchronizeBounded(live);
};

/** `journal` won: it landed, the node processed it, and it finalizes. */
const expectWinner = async (journal: Journal) => {
  expect(await node.landedRow(journal.header)).toEqual({
    kind: "own",
    state: "processed",
    applied: true,
  });
  await finalizeLocally(live, journal.header);
  expect(await nativeRoot(h)).toBe(journal.root);
  winner = journal.header;
};

beforeAll(async () => {
  h = await openHistoryProductionOwnerLifecycle({
    transportFactory: (recorded) =>
      (source = makeRollbackHistoryTransport(recorded)),
  });
  live = h;
  observer = h.observer;
  node = await openNodeServices(h);
  await advanceEmulatorPastLatestBlockEndTime(h.fixture);
  await synchronizeBounded(h);
  // The confirmed-ledger frontier starts at genesis: nothing to process.
  expect(await node.processLanded()).toBeUndefined();
}, 300_000);

afterAll(async () => {
  await node?.close();
  if (h !== undefined) await closeLifecycle(h);
});

/** (a) The replacement lands: the old journal is disposed of, and the next
 * commit builds on the winner. */
const replacementLands = async () => {
  await deposit(12_000_000n);
  const old = await commitUnlanded();
  expect(old.status).toBe(S.SubmittedLocalFinalizationPending);
  expect(old.deposits).toHaveLength(1);
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "live",
  });
  const ttl = Number(decodeTransaction(old.signed).invalidAfter);
  await advanceEmulatorPastUnixTime(
    h.fixture,
    h.fixture.operatorLucid.slotToUnixTime(ttl) + 1_000,
  );
  await synchronizeBounded(live);
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "expired",
  });
  await node.disposeDead();
  await synchronizeBounded(live);
  expect(await statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(h)).toBe(old.baseRoot);

  const replacement = await commitLanded();
  expect(replacement.header).not.toBe(old.header);
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual(old.deposits);
  expect(replacement.root).toBe(old.root);
  await expectWinner(replacement);
  expect(await statusOf(old)).toBe(S.Abandoned);
};

/** (b) The old signed commit lands instead: its journal is revived, and
 * the next commit builds on it. */
const oldCommitLands = async () => {
  await deposit(13_000_000n);
  const old = await commitUnlanded();
  // (a)'s next commit builds on its winner.
  expect(old.base).toBe(winner);
  expect(old.deposits).toHaveLength(1);
  await node.abandon(old.txHash);
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "abandoned",
  });
  await node.disposeDead();
  await synchronizeBounded(live);
  expect(await statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(h)).toBe(old.baseRoot);

  // The replacement also carries an L2 transfer. With the old content in
  // the same scheduler window it would have the old header hash, whose
  // signed abandoned journal row the journal insert does not replace.
  const [funding] = await depositorL2Utxos(live);
  const transfer = await buildDepositorTransfer(live, [funding!], 2_000_000n);
  expect(await admitTransfer(live, transfer)).toBe("accepted");
  const replacement = await commitUnlanded();
  expect(replacement.header).not.toBe(old.header);
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual(old.deposits);
  expect(old.txs).toEqual([]);
  expect(replacement.txs).toHaveLength(1);
  expect(replacement.status).toBe(S.SubmittedLocalFinalizationPending);

  // The old signed bytes land on the tail both commits extend.
  expect(Number(decodeTransaction(old.signed).invalidAfter)).toBeGreaterThan(
    h.fixture.emulator.slot + 20,
  );
  expect(await h.fixture.emulator.submitTx(old.signed.toString("hex"))).toBe(
    old.txHash,
  );
  expect(await h.fixture.operatorLucid.awaitTx(old.txHash)).toBe(true);
  await synchronizeBounded(live);
  await node.recordLanding(old.signed);
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "landed",
  });
  await node.processLanded();
  await synchronizeBounded(live);
  expect(await statusOf(old)).toBe(S.ObservedWaitingStability);
  expect(await statusOf(replacement)).toBe(S.Abandoned);
  expect(await nativeRoot(h)).toBe(old.root);
  await expectWinner(old);
  // The follower reads the replacement conflicted by the landed commit's
  // spend of the tail both commits spend (dead, superseded). The stand-in
  // drops spent outputs rather than marking their spender, so it would read
  // the replacement live; S6's abandoned event writes the same dead status.
  await node.abandon(replacement.txHash);

  // The next commit builds on the revived block. It carries the transfer the
  // disposed-of replacement had taken from the mempool, which went back to it.
  const next = await commitLanded();
  expect(next.base).toBe(old.header);
  expect(next.deposits).toEqual([]);
  expect(next.txs).toEqual(replacement.txs);
  await expectWinner(next);
};

/** (c) A deposit leaves the chain: the journal holding it is disposed of,
 * and the replacement omits it. */
const depositLeavesChain = async () => {
  // The ancestor: nothing pending on L1, and no unpublished L2 transaction.
  expect(Object.keys(h.fixture.emulator.mempool)).toHaveLength(0);
  const point = source!.points.at(-1)!.point;
  expect(point.slot).toBe(h.fixture.emulator.slot);
  const ancestor = {
    id: point.id,
    height: point.height,
    state: emulatorState(h.fixture.emulator),
  };

  await deposit(15_000_000n);
  const old = await commitUnlanded();
  expect(old.base).toBe(winner);
  expect(old.deposits).toHaveLength(1);
  expect(old.txs).toEqual([]);

  ({ fork: live, observer } = await followFork({
    h,
    source: source!,
    ancestor,
  }));
  await synchronizeBounded(live);
  // Nothing else kills it: its signed commit is live.
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "live",
  });
  await node.disposeDead();
  await synchronizeBounded(live);
  expect(await statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(h)).toBe(old.baseRoot);
  // S6's next pass: the commit predicate refuses a disposed-of journal, so
  // the intent is abandoned and never sent, which releases its inputs.
  const { report, sent } = await node.reconcileIntents();
  expect(
    report?.intents.find(
      ({ intent }) => intent.txHash.toString("hex") === old.txHash,
    )?.action,
  ).toBe("abandon");
  expect(sent).not.toContain(old.txHash);
  expect(await node.intentStatus(old.txHash)).toMatchObject({
    kind: "abandoned",
  });

  // The replacement carries an L2 transfer admitted on the fork, and omits
  // the deposit that left the chain.
  const [funding] = await depositorL2Utxos(live);
  const transfer = await buildDepositorTransfer(live, [funding!], 2_000_000n);
  expect(await admitTransfer(live, transfer)).toBe("accepted");
  const replacement = await commitLanded();
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual([]);
  expect(replacement.txs).toHaveLength(1);
  await expectWinner(replacement);
};

// One journey: the shared emulator harness resets the node runtime after
// each test, so the rounds run in one test, each on the one before.
describe("two state-queue commits on one tail", () => {
  it("whichever lands wins: (a) the replacement, (b) the old commit, (c) after a deposit left the chain", async () => {
    await replacementLands();
    await oldCommitLands();
    await depositLeavesChain();
  }, 900_000);
});
