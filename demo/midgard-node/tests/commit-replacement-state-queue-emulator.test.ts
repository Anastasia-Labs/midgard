/**
 * Whichever lands wins (plan §8.3, I3) for two real state-queue commits on
 * one tail. The production commit worker builds both, journaling them in
 * the production intent journal over the follower's facts. The
 * follower-change driver's recompute runs the landed-block rebase. The
 * node's landed-block processing (`processLandedQueue` over
 * `nodeLandedBlockPorts`, the follower run's hook) reads the emulator's
 * queue through the follower stand-in.
 *
 * - (a) The replacement lands. The old journal is disposed of, the native
 *   root returns to the base, and the next commit builds on the winner.
 * - (b) The old signed commit lands instead. A same-window rebuild of its
 *   content waits for the next window rather than replace its journal. The landed-block rebase revives
 *   its journal and disposes of the replacement's, the native root is the
 *   old commit's, and the next commit builds on it, carrying the L2
 *   transfer the disposed-of replacement had taken from the mempool.
 * - (c) A deposit leaves the chain (a rollback past its admission). The
 *   journal holding it is disposed of, S6 abandons its intent, the L2
 *   transfer it also held stays accepted and pending (its funding output
 *   survives), and the replacement carries the transfer and omits the
 *   deposit.
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
 * abandoned event where the follower would read it conflicted. (c)'s old
 * commit also holds an L2 transfer; the driver's recompute keeps it pending
 * when the disposal rebuilds the working ledger.
 */
import "./helpers/follower-emulator-installed.js";

import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { HELD_HEADER_DETAIL } from "../src/workers/commit-block-header/submission.submit-with-durable-intent.js";
import { advanceEmulatorPastUnixTime } from "./deposit-flow-emulator-shared.js";
import {
  type CommitJourney,
  openCommitJourney,
} from "./helpers/commit-replacement-journey.js";
import {
  nativeRoot,
  synchronizeBounded,
} from "./helpers/correction-admission-scenario.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  depositorL2Utxos,
} from "./helpers/emulator-l2-transfer.js";

const S = Pending.Status;

let j: CommitJourney;

beforeAll(async () => {
  j = await openCommitJourney();
}, 300_000);

afterAll(async () => {
  await j?.close();
});

/** (a) The replacement lands: the old journal is disposed of, and the next
 * commit builds on the winner. */
const replacementLands = async () => {
  await j.deposit(12_000_000n);
  const old = await j.commitUnlanded();
  expect(old.status).toBe(S.SubmittedLocalFinalizationPending);
  expect(old.deposits).toHaveLength(1);
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "live",
  });
  const ttl = Number(decodeTransaction(old.signed).invalidAfter);
  await advanceEmulatorPastUnixTime(
    j.h.fixture,
    j.h.fixture.operatorLucid.slotToUnixTime(ttl) + 1_000,
  );
  await synchronizeBounded(j.live);
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "expired",
  });
  await j.node.disposeDead();
  await synchronizeBounded(j.live);
  expect(await j.statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(j.h)).toBe(old.baseRoot);

  const replacement = await j.commitLanded();
  expect(replacement.header).not.toBe(old.header);
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual(old.deposits);
  expect(replacement.root).toBe(old.root);
  await j.expectWinner(replacement);
  expect(await j.statusOf(old)).toBe(S.Abandoned);
};

/** (b) The old signed commit lands instead: its journal is revived, and
 * the next commit builds on it. */
const oldCommitLands = async () => {
  await j.deposit(13_000_000n);
  const old = await j.commitUnlanded();
  // (a)'s next commit builds on its winner.
  expect(old.base).toBe(j.winner);
  expect(old.deposits).toHaveLength(1);
  await j.node.abandon(old.txHash);
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "abandoned",
  });
  await j.node.disposeDead();
  await synchronizeBounded(j.live);
  expect(await j.statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(j.h)).toBe(old.baseRoot);

  // The same content on the same base in the same scheduler window has the
  // old header hash. Its signed abandoned journal is kept (its bytes land
  // below), so the commit writes nothing and waits for the next window.
  expect(await j.commitOutput()).toEqual({
    type: "AwaitingNextCommitWindowOutput",
    heldHeaderHash: old.header,
    detail: HELD_HEADER_DETAIL,
  });
  expect(await j.journalOf(old.header)).toEqual({
    ...old,
    status: S.Abandoned,
  });

  // The replacement, in a later window, also carries an L2 transfer.
  const [funding] = await depositorL2Utxos(j.live);
  const transfer = await buildDepositorTransfer(j.live, [funding!], 2_000_000n);
  expect(await admitTransfer(j.live, transfer)).toBe("accepted");
  const replacement = await j.commitUnlanded();
  expect(replacement.header).not.toBe(old.header);
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual(old.deposits);
  expect(old.txs).toEqual([]);
  expect(replacement.txs).toHaveLength(1);
  expect(replacement.status).toBe(S.SubmittedLocalFinalizationPending);

  // The old signed bytes land on the tail both commits extend.
  expect(Number(decodeTransaction(old.signed).invalidAfter)).toBeGreaterThan(
    j.h.fixture.emulator.slot + 20,
  );
  expect(await j.h.fixture.emulator.submitTx(old.signed.toString("hex"))).toBe(
    old.txHash,
  );
  expect(await j.h.fixture.operatorLucid.awaitTx(old.txHash)).toBe(true);
  await synchronizeBounded(j.live);
  await j.node.recordLanding(old.signed);
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "landed",
  });
  await j.node.processLanded();
  await synchronizeBounded(j.live);
  expect(await j.statusOf(old)).toBe(S.ObservedWaitingStability);
  expect(await j.statusOf(replacement)).toBe(S.Abandoned);
  expect(await nativeRoot(j.h)).toBe(old.root);
  await j.expectWinner(old);
  // The follower reads the replacement conflicted by the landed commit's
  // spend of the tail both commits spend (dead, superseded). The stand-in
  // drops spent outputs rather than marking their spender, so it would read
  // the replacement live; S6's abandoned event writes the same dead status.
  await j.node.abandon(replacement.txHash);

  // The next commit builds on the revived block. It carries the transfer the
  // disposed-of replacement had taken from the mempool, which went back to it.
  const next = await j.commitLanded();
  expect(next.base).toBe(old.header);
  expect(next.deposits).toEqual([]);
  expect(next.txs).toEqual(replacement.txs);
  await j.expectWinner(next);
};

/** (c) A deposit leaves the chain: the journal holding it is disposed of,
 * and the replacement omits it. */
const depositLeavesChain = async () => {
  // The ancestor: nothing pending on L1, and no unpublished L2 transaction.
  const ancestor = j.ancestor();

  // The old commit also holds an L2 transfer funded by an output that
  // survives the rollback.
  const [funding] = await depositorL2Utxos(j.live);
  await j.deposit(15_000_000n);
  const transfer = await buildDepositorTransfer(j.live, [funding!], 2_000_000n);
  expect(await admitTransfer(j.live, transfer)).toBe("accepted");
  // The admission's time: the mempool row's time stamp is at most this.
  const admittedAtMs = Date.now();
  const old = await j.commitUnlanded();
  expect(old.base).toBe(j.winner);
  expect(old.deposits).toHaveLength(1);
  expect(old.txs).toEqual([transfer.txIdHex]);

  await j.rollBackTo(ancestor);
  // Nothing else kills it: its signed commit is live.
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "live",
  });
  await j.node.disposeDead();
  await synchronizeBounded(j.live);
  expect(await j.statusOf(old)).toBe(S.Abandoned);
  expect(await nativeRoot(j.h)).toBe(old.baseRoot);
  // S6's next pass: the commit predicate refuses a disposed-of journal, so
  // the intent is abandoned and never sent, which releases its inputs.
  const { report, sent } = await j.node.reconcileIntents();
  expect(
    report?.intents.find(
      ({ intent }) => intent.txHash.toString("hex") === old.txHash,
    )?.action,
  ).toBe("abandon");
  expect(sent).not.toContain(old.txHash);
  expect(await j.node.intentStatus(old.txHash)).toMatchObject({
    kind: "abandoned",
  });

  // The disposal's rebuild re-simulated the transfer on the base: its
  // funding output survives the rollback, so it stays accepted and pending,
  // with no re-admission.
  expect(await j.admissionStatus(transfer.txId)).toBe("accepted");
  // Restoring the ancestor also set the emulator's clock back, behind the
  // transfer's admission; L1 time never runs back, so the chain moves past
  // it before the next commit selects the mempool up to its start time.
  await advanceEmulatorPastUnixTime(j.h.fixture, admittedAtMs);
  vi.setSystemTime(j.h.fixture.emulator.now());
  await synchronizeBounded(j.live);
  // The replacement carries the transfer and omits the deposit that left
  // the chain.
  const replacement = await j.commitLanded();
  expect(replacement.base).toBe(old.base);
  expect(replacement.deposits).toEqual([]);
  expect(replacement.txs).toEqual([transfer.txIdHex]);
  await j.expectWinner(replacement);
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
