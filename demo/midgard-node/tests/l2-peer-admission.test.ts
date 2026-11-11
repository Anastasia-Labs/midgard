/**
 * The forget rule, on a real database through the production queue
 * processor: a peer-forwarded transaction whose verdict only says "not yet"
 * (an input this node has not seen) is forgotten, so the sender's next offer
 * is admitted afresh; every other verdict sticks, and a user's own submission
 * is never forgotten.
 */

import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { PEER_TXS_PATH } from "../src/l2-peer/protocol.js";
import {
  buildChain as buildChainAgainst,
  buildTransfer,
  l2Output,
  operatorFromSeed,
  type PeerNode,
  signedPeerPost,
  startShardNode,
  submitTx,
} from "./helpers/l2-peer-node.js";

const sender = operatorFromSeed(generateSeedPhrase());
const receiver = operatorFromSeed(generateSeedPhrase());
const user = operatorFromSeed(generateSeedPhrase());
const funding = l2Output(user, 0x22, 50_000_000n);

let lucid: Awaited<ReturnType<typeof Lucid>>;
const nodes: PeerNode[] = [];

const startReceiver = async () => {
  const node = await startShardNode({
    operator: receiver,
    activeOperators: [sender.keyHash, receiver.keyHash],
    lucid,
  });
  nodes.push(node);
  await node.seedLedger([funding]);
  return node;
};

const buildChain = () =>
  buildChainAgainst(
    async () => {
      const node = await startReceiver();
      nodes.splice(nodes.indexOf(node), 1);
      return node;
    },
    user,
    funding,
  );

beforeAll(async () => {
  lucid = await Lucid(new Emulator([]), "Custom");
});

afterEach(async () => {
  for (const node of nodes.splice(0)) await node.close();
});

const rowOf = async (node: PeerNode, txId: Buffer) =>
  (await node.admissions()).find((row) => row.tx_id.equals(txId));

const offer = async (node: PeerNode, txs: readonly Uint8Array[]) => {
  const reply = await signedPeerPost(
    node.url,
    PEER_TXS_PATH,
    txs,
    sender.signer,
  );
  expect(reply.status).toBe(200);
  return JSON.parse(reply.body.toString()) as {
    admitted: number;
    duplicates: number;
    refused: number;
  };
};

describe("peer admission forget rule", () => {
  it("forgets a peer child whose parent it has not seen, then admits it afresh", async () => {
    const { parent, child } = await buildChain();
    const node = await startReceiver();

    expect(await offer(node, [child.txCbor])).toMatchObject({ admitted: 1 });
    await node.drain();
    // Forgotten: no verdict is kept and nothing reached the mempool.
    expect(await rowOf(node, child.txId)).toBeUndefined();
    expect(await node.mempool()).toEqual([]);

    expect(await offer(node, [parent.txCbor])).toMatchObject({ admitted: 1 });
    await node.drain();
    expect((await rowOf(node, parent.txId))?.status).toBe("accepted");

    // The same child, offered again, is admitted as new and now accepted,
    // under the same id and with the same bytes.
    expect(await offer(node, [child.txCbor])).toEqual({
      admitted: 1,
      duplicates: 0,
      refused: 0,
    });
    await node.drain();
    expect((await rowOf(node, child.txId))?.status).toBe("accepted");
    const pooled = (await node.mempool()).find((row) =>
      row.tx_id.equals(child.txId),
    );
    expect(pooled?.tx.equals(Buffer.from(child.txCbor))).toBe(true);
  });

  it("keeps a user's child whose parent it has not seen rejected", async () => {
    const { parent, child } = await buildChain();
    const node = await startReceiver();

    expect((await submitTx(node.url, child.txCbor)).status).toBe(202);
    await node.drain();
    expect(await rowOf(node, child.txId)).toMatchObject({
      status: "rejected",
      reject_code: "E_INPUT_NOT_FOUND",
    });

    expect((await submitTx(node.url, parent.txCbor)).status).toBe(202);
    await node.drain();
    await submitTx(node.url, child.txCbor);
    await node.drain();
    expect(await rowOf(node, child.txId)).toMatchObject({
      status: "rejected",
      reject_code: "E_INPUT_NOT_FOUND",
    });
  });

  it("refuses a peer transaction with a forged witness before admission", async () => {
    const node = await startReceiver();
    // The user's own transfer with one bit of its signature flipped.
    const honest = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      node.config,
    );
    const forged = Buffer.from(honest.txCbor);
    const key = Buffer.concat([
      Buffer.from([0x58, 0x20]),
      Buffer.from(user.signer.verificationKeyHex, "hex"),
    ]);
    const signatureAt = forged.indexOf(key) + key.length;
    expect(forged.subarray(signatureAt, signatureAt + 2)).toEqual(
      Buffer.from([0x58, 0x40]),
    );
    forged[signatureAt + 2] ^= 0x01;

    // Peers pass the same stateless checks as `/submit` (design D6).
    expect(await offer(node, [forged])).toEqual({
      admitted: 0,
      duplicates: 0,
      refused: 1,
    });
    expect(await node.admissions()).toEqual([]);
  });

  it("keeps a peer transaction signed by the wrong key rejected", async () => {
    const node = await startReceiver();
    // A well-formed signature, by a key that does not own the input: only
    // the ledger check finds it.
    const stolen = await buildTransfer(
      sender,
      [funding],
      10_000_000n,
      node.config,
    );

    expect(await offer(node, [stolen.txCbor])).toMatchObject({ admitted: 1 });
    await node.drain();
    const [verdict] = await node.admissions();
    expect(verdict).toMatchObject({
      status: "rejected",
      reject_code: "E_MISSING_REQUIRED_WITNESS",
    });

    // The writer keeps no recent peer ids (a forgotten peer row must be
    // admitted again), so the re-offer is acknowledged and its write then
    // resolves against the stored rejection, which stays as it was.
    expect(await offer(node, [stolen.txCbor])).toEqual({
      admitted: 1,
      duplicates: 0,
      refused: 0,
    });
    expect(await node.admissions()).toEqual([verdict]);
  });
});
