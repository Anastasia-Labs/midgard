/**
 * An L2 transfer funded by a deposit that leaves the chain. The deposit
 * lands in an own block; a transfer spends the deposit's L2 output, and a
 * second transfer spends that transfer's output. An own commit holding both
 * transfers is lost before any block (`dropPendingEmulatorTransaction`),
 * then the chain rolls back past the deposit (`followFork`): the deposit's
 * block and the deposit leave the chain. The deposit's block is confirmed
 * first: a deposit's L2 output is spendable once its block is.
 *
 * The repair, through the node's own state: the lost commit's journal is
 * disposed of, and S6 leaves its intent waiting on its departed inputs,
 * never sent (a rollback alone never abandons); the working ledger is rebuilt
 * without the deposit's output, so the funded transfer is rejected for its
 * missing input and the second for spending a rejected output (with that
 * cause); neither leaves an output in the working ledger; and the next
 * commit, carrying a deposit made on the fork, holds neither transfer nor
 * the departed deposit.
 *
 * Production pieces and stand-in limits are those of
 * `commit-replacement-state-queue-emulator.test.ts`.
 */
import "./helpers/follower-emulator-installed.js";

import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { REBASE_REJECTIONS } from "../src/landed-blocks/rebase.js";
import { advanceEmulatorPastUnixTime } from "./deposit-flow-emulator-shared.js";
import {
  type CommitJourney,
  openCommitJourney,
} from "./helpers/commit-replacement-journey.js";
import { synchronizeBounded } from "./helpers/correction-admission-scenario.js";
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

const hex = (bytes: Buffer) => bytes.toString("hex");

describe("an L2 transfer funded by a deposit that leaves the chain", () => {
  it("is rejected with its dependent, and the next commit omits both transfers and the deposit", async () => {
    const ancestor = j.ancestor();
    const before = await depositorL2Utxos(j.live);

    // The deposit lands in an own block, which the node confirms: its L2
    // output is spendable from then on. It funds a transfer, and that
    // transfer's output a second one.
    await j.deposit(12_000_000n);
    const holder = await j.commitLanded();
    expect(holder.deposits).toHaveLength(1);
    await j.expectWinner(holder);
    const depositL1Tx = (eventIdHex: string) =>
      j.query(
        (sql) => sql<{ deposit_l1_tx_hash: Buffer }>`
          SELECT deposit_l1_tx_hash FROM deposits_utxos
          WHERE event_id = ${Buffer.from(eventIdHex, "hex")}`,
      );
    const [departed] = await depositL1Tx(holder.deposits[0]!);
    const [depositOutput, ...others] = (await depositorL2Utxos(j.live)).filter(
      (utxo) => !before.some((old) => old.outrefCbor.equals(utxo.outrefCbor)),
    );
    expect(others).toEqual([]);
    const funded = await buildDepositorTransfer(
      j.live,
      [depositOutput!],
      3_000_000n,
    );
    expect(await admitTransfer(j.live, funded)).toBe("accepted");
    const dependent = await buildDepositorTransfer(
      j.live,
      (await depositorL2Utxos(j.live)).filter(
        ({ txHash }) => txHash === funded.txIdHex,
      ),
      1_000_000n,
    );
    expect(await admitTransfer(j.live, dependent)).toBe("accepted");

    // An own commit holds both transfers and is lost before any block.
    const old = await j.commitUnlanded();
    expect(old.base).toBe(holder.header);
    expect(old.deposits).toEqual([]);
    expect(old.txs).toEqual([funded.txIdHex, dependent.txIdHex].sort());

    // The follower run: landed-block processing rebases off the departed
    // block, then the own-commit disposition; then the history owner.
    await j.rollBackTo(ancestor, { synchronize: false });
    expect(await j.node.processLanded()).toBeUndefined();
    await j.node.disposeDead();
    await synchronizeBounded(j.live);
    expect(await j.statusOf(old)).toBe(S.Abandoned);
    // S6's next pass: the lost commit spends the departed block's output, so
    // its inputs are not live facts and the family predicate is not read (a
    // rollback alone never abandons: the block can land again). It waits on
    // its inputs, unsent, until its validity window expires.
    const { report, sent } = await j.node.reconcileIntents();
    expect(
      report?.intents.find(({ intent }) => hex(intent.txHash) === old.txHash)
        ?.action,
    ).toBe("wait_inputs");
    expect(sent).not.toContain(old.txHash);
    expect(await j.node.intentStatus(old.txHash)).toMatchObject({
      kind: "live",
      inputsAvailable: false,
    });

    const ids = [funded.txId, dependent.txId];
    const rejections = await j.query(
      (sql) => sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections
        WHERE tx_id IN ${sql.in(ids)}`,
    );
    expect(
      Object.fromEntries(
        rejections.map((row) => [hex(row.tx_id), row.reject_code]),
      ),
    ).toEqual({
      [funded.txIdHex]: REBASE_REJECTIONS.direct.code,
      [dependent.txIdHex]: REBASE_REJECTIONS.dependent.code,
    });
    const causes = await j.query(
      (sql) => sql<{ tx_id: Buffer; cause_tx_id: Buffer }>`
        SELECT tx_id, cause_tx_id FROM tx_rejection_causes
        WHERE tx_id IN ${sql.in(ids)} OR cause_tx_id IN ${sql.in(ids)}`,
    );
    expect(causes.map((row) => [hex(row.tx_id), hex(row.cause_tx_id)])).toEqual(
      [[dependent.txIdHex, funded.txIdHex]],
    );
    const ledger = await j.query(
      (sql) => sql<{ tx_id: Buffer; outref: Buffer }>`
        SELECT tx_id, outref FROM mempool_ledger`,
    );
    expect(ledger.map((row) => hex(row.outref))).not.toContain(
      hex(depositOutput!.outrefCbor),
    );
    expect(ledger.map((row) => hex(row.tx_id))).not.toEqual(
      expect.arrayContaining([funded.txIdHex]),
    );
    expect(ledger.map((row) => hex(row.tx_id))).not.toEqual(
      expect.arrayContaining([dependent.txIdHex]),
    );

    // The rolled-back holder commit is a live own intent again. It spends an
    // operator-wallet output of a transaction the fork lacks (the harness
    // aligns the scheduler unjournaled, so nothing sends it again), so S6
    // waits on its inputs, and it holds the active-operators node the next
    // commit spends until S6 derives it dead. Its validity window lapses:
    // S6 derives it expired, the production cause for a commit that does
    // not land again.
    expect(await j.node.intentStatus(holder.txHash)).toMatchObject({
      kind: "live",
      inputsAvailable: false,
    });
    const ttl = Number(decodeTransaction(holder.signed).invalidAfter);
    await advanceEmulatorPastUnixTime(
      j.h.fixture,
      j.h.fixture.operatorLucid.slotToUnixTime(ttl) + 1_000,
    );
    await synchronizeBounded(j.live);
    expect(await j.node.intentStatus(holder.txHash)).toMatchObject({
      kind: "expired",
    });

    // The next commit carries a deposit made on the fork, and neither
    // transfer nor the deposit that left the chain.
    await j.deposit(13_000_000n);
    const next = await j.commitLanded();
    expect(next.base).toBe(holder.base);
    expect(next.deposits).toHaveLength(1);
    // A deposit's event id is its nonce input's outref, and the fork's
    // deposit spends the same nonce input as the departed one: the ids are
    // equal. The carried deposit is the fork's L1 transaction.
    const carried = await depositL1Tx(next.deposits[0]!);
    expect(carried).toHaveLength(1);
    expect(hex(carried[0]!.deposit_l1_tx_hash)).not.toBe(
      hex(departed!.deposit_l1_tx_hash),
    );
    expect(next.txs).toEqual([]);
    await j.expectWinner(next);
  }, 900_000);
});
