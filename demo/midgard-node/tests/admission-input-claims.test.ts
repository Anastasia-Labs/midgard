/**
 * First-seen input claims (design D6), over real HTTP on a real database
 * through the production admission writer and tx-queue processor: a second
 * `/submit` tx spending an input a pending tx claimed is refused with 409 and
 * the first is untouched; the claims go when the first tx's verdict lands or
 * the processor forgets it, stay across a lease requeue, and come back after
 * a restart; and the check answers while Phase B holds its lock.
 */

import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { TxAdmissionsDB } from "../src/database/index.js";
import { PEER_TXS_PATH } from "../src/l2-peer/protocol.js";
import { MempoolLedgerCache } from "../src/services/index.js";
import {
  buildTransfer,
  l2Output,
  operatorFromSeed,
  type PeerNode,
  signedPeerPost,
  startPeerNode,
  startShardNode,
  submitTx,
} from "./helpers/l2-peer-node.js";

const sender = operatorFromSeed(generateSeedPhrase());
const receiver = operatorFromSeed(generateSeedPhrase());
const user = operatorFromSeed(generateSeedPhrase());
const funding = l2Output(user, 0x31, 50_000_000n);

let lucid: Awaited<ReturnType<typeof Lucid>>;
const nodes: PeerNode[] = [];

const nodeOptions = () => ({
  operator: receiver,
  activeOperators: [sender.keyHash, receiver.keyHash],
  lucid,
});

/** A node on this file's emptied shard whose ledger holds `funding`. */
const startNode = async () => {
  const node = await startShardNode(nodeOptions());
  nodes.push(node);
  await node.seedLedger([funding]);
  return node;
};

beforeAll(async () => {
  lucid = await Lucid(new Emulator([]), "Custom");
});

afterEach(async () => {
  for (const node of nodes.splice(0)) await node.close();
});

type SpentReply = {
  readonly error: string;
  readonly reason: "claimed" | "not_in_ledger";
  readonly message: string;
  readonly txId: string;
  readonly outRef: string;
  readonly spentBy?: string;
};

const submit = async (node: PeerNode, txCbor: Uint8Array) => {
  const reply = await submitTx(node.url, txCbor);
  return {
    status: reply.status,
    body: JSON.parse(reply.body.toString()) as Record<string, unknown>,
  };
};

/** Submits `tx` and expects the spent-input refusal. */
const expectSpent = async (
  node: PeerNode,
  tx: { readonly txCbor: Uint8Array; readonly txIdHex: string },
  expected: Pick<SpentReply, "reason" | "spentBy">,
) => {
  const reply = await submit(node, tx.txCbor);
  expect(reply.status).toBe(409);
  const body = reply.body as SpentReply;
  expect(body).toMatchObject({
    error: "E_INPUT_SPENT",
    txId: tx.txIdHex,
    ...expected,
  });
  if (expected.spentBy === undefined) expect(body.spentBy).toBeUndefined();
  expect(body.message).toContain(`/tx-status for transaction ${tx.txIdHex}`);
  return body;
};

const statusOf = async (node: PeerNode, txId: Buffer) =>
  (await node.admissions()).find((row) => row.tx_id.equals(txId))?.status;

/** Loads the node's validation cache, as its first validation batch does. */
const warmCache = (node: PeerNode) =>
  node.run(
    Effect.flatMap(MempoolLedgerCache, (cache) =>
      cache.withPhaseBLock(cache.currentState),
    ),
  );

/** Every claim the node holds is gone (the index is empty). */
const expectNoClaims = (node: PeerNode) =>
  expect(node.globals.ADMISSION_INPUT_CLAIMS.stats()).toEqual({
    txs: 0,
    claims: 0,
    produced: 0,
  });

/** Two transfers of `funding` by `user`: the second double-spends it. */
const doubleSpend = async (node: PeerNode) => ({
  first: await buildTransfer(user, [funding], 10_000_000n, node.config),
  second: await buildTransfer(user, [funding], 20_000_000n, node.config),
});

/**
 * A parent transfer of `funding` and two children that each spend the
 * parent's first output, built against a scratch node that accepted the
 * parent and then closed.
 */
const buildForkedChain = async () => {
  const scratch = await startShardNode(nodeOptions());
  try {
    await scratch.seedLedger([funding]);
    const parent = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      scratch.config,
    );
    expect((await submitTx(scratch.url, parent.txCbor)).status).toBe(202);
    await scratch.drain();
    const outputs = (await scratch.spendable(user.address)).filter(
      (utxo) => utxo.txHash === parent.txIdHex && utxo.outputIndex === 0,
    );
    expect(outputs).toHaveLength(1);
    return {
      parent,
      child: await buildTransfer(user, outputs, 5_000_000n, scratch.config),
      sibling: await buildTransfer(user, outputs, 6_000_000n, scratch.config),
    };
  } finally {
    await scratch.close();
  }
};

describe("first-seen input claims", () => {
  it("refuses a double spend of a pending input with 409 and leaves the first tx alone", async () => {
    const node = await startNode();
    const { first, second } = await doubleSpend(node);

    expect((await submit(node, first.txCbor)).status).toBe(202);
    const refusal = await expectSpent(node, second, {
      reason: "claimed",
      spentBy: first.txIdHex,
    });
    expect(refusal.outRef).toBe(funding.outrefCbor.toString("hex"));
    // The duplicate lookup comes first: the first tx resubmitted is still
    // its own duplicate, not a spender of its own claims.
    expect(await submit(node, first.txCbor)).toMatchObject({
      status: 200,
      body: { duplicate: true },
    });

    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("accepted");
    expect(await statusOf(node, second.txId)).toBeUndefined();
    // The verdict released the claim: the input is now simply spent.
    await expectSpent(node, second, { reason: "not_in_ledger" });
    expectNoClaims(node);
  });

  it("releases a rejected tx's claims, so the next spender is admitted", async () => {
    const node = await startNode();
    const { second } = await doubleSpend(node);
    // A transfer of `funding` and an output the ledger never held. Ingress
    // admits it (the ledger view is still cold, so it cannot tell), and it
    // claims `funding`; validation then rejects it for the missing input.
    const first = await buildTransfer(
      user,
      [funding, l2Output(user, 0x33, 50_000_000n)],
      60_000_000n,
      node.config,
    );

    expect((await submit(node, first.txCbor)).status).toBe(202);
    await expectSpent(node, second, {
      reason: "claimed",
      spentBy: first.txIdHex,
    });

    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("rejected");
    expect((await submit(node, second.txCbor)).status).toBe(202);
    await node.drain();
    expect(await statusOf(node, second.txId)).toBe("accepted");
    expectNoClaims(node);
  });

  it("admits a child of a pending parent and refuses its sibling", async () => {
    const { parent, child, sibling } = await buildForkedChain();
    const node = await startNode();

    expect((await submit(node, parent.txCbor)).status).toBe(202);
    // The parent's output is not in the ledger yet, but a pending tx
    // produces it.
    expect((await submit(node, child.txCbor)).status).toBe(202);
    await expectSpent(node, sibling, {
      reason: "claimed",
      spentBy: child.txIdHex,
    });

    await node.drain();
    expect(await statusOf(node, parent.txId)).toBe("accepted");
    expect(await statusOf(node, child.txId)).toBe("accepted");
    expectNoClaims(node);
  });

  it("refuses an input the ledger does not hold and no pending tx produces", async () => {
    const node = await startNode();
    await warmCache(node);
    const unknown = l2Output(user, 0x32, 30_000_000n);
    const spender = await buildTransfer(
      user,
      [unknown],
      10_000_000n,
      node.config,
    );

    await expectSpent(node, spender, { reason: "not_in_ledger" });
    // A deposit landing later makes it spendable without a validation batch.
    await node.seedLedger([unknown]);
    expect((await submit(node, spender.txCbor)).status).toBe(202);
    await node.drain();
    expect(await statusOf(node, spender.txId)).toBe("accepted");
  });

  it("releases the claims of a peer tx the processor forgets", async () => {
    const { parent, child, sibling } = await buildForkedChain();
    const node = await startNode();

    // A peer's tx may spend an output this node has not seen: it is
    // admitted, and claims that input.
    const offered = await signedPeerPost(
      node.url,
      PEER_TXS_PATH,
      [child.txCbor],
      sender.signer,
    );
    expect(JSON.parse(offered.body.toString())).toMatchObject({ admitted: 1 });
    await expectSpent(node, sibling, {
      reason: "claimed",
      spentBy: child.txIdHex,
    });

    // Its parent is unknown here, so the processor forgets it.
    await node.drain();
    expect(await statusOf(node, child.txId)).toBeUndefined();
    await expectSpent(node, sibling, { reason: "not_in_ledger" });

    expect((await submit(node, parent.txCbor)).status).toBe(202);
    expect((await submit(node, sibling.txCbor)).status).toBe(202);
    await node.drain();
    expect(await statusOf(node, sibling.txId)).toBe("accepted");
    expectNoClaims(node);
  });

  it("never refuses a peer tx for its inputs", async () => {
    const node = await startNode();
    const { first, second } = await doubleSpend(node);

    expect((await submit(node, first.txCbor)).status).toBe(202);
    const offered = await signedPeerPost(
      node.url,
      PEER_TXS_PATH,
      [second.txCbor],
      sender.signer,
    );
    expect(JSON.parse(offered.body.toString())).toEqual({
      admitted: 1,
      duplicates: 0,
      refused: 0,
    });

    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("accepted");
    expectNoClaims(node);
  });

  it("keeps a requeued tx's claims until its verdict", async () => {
    const node = await startNode();
    const { first, second } = await doubleSpend(node);

    expect((await submit(node, first.txCbor)).status).toBe(202);
    await node.admissions();
    // A validator leased the row and died: its lease expires and the row
    // goes back to the queue.
    const leased = await node.run(
      TxAdmissionsDB.claimBatchLease({
        limit: 16,
        leaseOwner: "validator-that-died",
        leaseDurationMs: 1,
      }),
    );
    expect(leased).toHaveLength(1);
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(await node.run(TxAdmissionsDB.requeueExpiredLeases)).toBe(1);

    // Still pending, so still claimed.
    await expectSpent(node, second, {
      reason: "claimed",
      spentBy: first.txIdHex,
    });
    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("accepted");
    await expectSpent(node, second, { reason: "not_in_ledger" });
    expectNoClaims(node);
  });

  it("rebuilds the claims from the backlog after a restart", async () => {
    const before = await startNode();
    const { first, second } = await doubleSpend(before);
    expect((await submit(before, first.txCbor)).status).toBe(202);
    expect(await statusOf(before, first.txId)).toBe("queued");
    nodes.splice(nodes.indexOf(before), 1);
    await before.close();

    // The same database, not emptied.
    const node = await startPeerNode(nodeOptions());
    nodes.push(node);
    await expectSpent(node, second, {
      reason: "claimed",
      spentBy: first.txIdHex,
    });
    // The restarted writer holds no recent ids, so the rebuilt tx resubmitted
    // is acknowledged and its write resolves against the stored row; it
    // neither hits its own claims nor gives them back.
    expect((await submit(node, first.txCbor)).status).toBe(202);
    await node.admissions();
    await expectSpent(node, second, {
      reason: "claimed",
      spentBy: first.txIdHex,
    });

    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("accepted");
    expectNoClaims(node);
  });

  it("answers while Phase B holds its lock", async () => {
    const node = await startNode();
    await warmCache(node);
    const { first, second } = await doubleSpend(node);
    const unknown = await buildTransfer(
      user,
      [l2Output(user, 0x33, 30_000_000n)],
      10_000_000n,
      node.config,
    );

    // Every submit runs while the lock is held; one that waited for it
    // would never answer, and the race below would end on the timer.
    const statuses = await node.run(
      Effect.flatMap(MempoolLedgerCache, (cache) =>
        cache.withPhaseBLock(
          Effect.promise(() =>
            Promise.race([
              (async () => [
                (await submit(node, first.txCbor)).status,
                (await submit(node, second.txCbor)).status,
                (await submit(node, unknown.txCbor)).status,
              ])(),
              new Promise<string>((resolve) =>
                setTimeout(() => resolve("blocked"), 10_000),
              ),
            ]),
          ),
        ),
      ),
    );
    expect(statuses).toEqual([202, 409, 409]);

    await node.drain();
    expect(await statusOf(node, first.txId)).toBe("accepted");
  });
});
