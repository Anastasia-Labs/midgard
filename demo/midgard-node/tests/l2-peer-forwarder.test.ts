/**
 * Two operators' nodes, each on its own database, over real HTTP: the
 * production forwarder on the first shares what users submit to it with the
 * second through the second's production peer routes. A user's transfer
 * reaches the peer by delta push with the same id and bytes; a peer that is
 * down never holds the sender back and gets the transfer once it is back; a
 * peer that never answers holds back no other peer; a
 * peer that forgot a transfer it could not yet validate gets it again on the
 * next L1 advance; and with nothing pending no request is ever sent and an
 * L1 advance reads no admission history.
 */
import "./utils.js";

import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { SqlClient } from "@effect/sql";
import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { PEER_INVENTORY_PATH, PEER_TXS_PATH } from "../src/l2-peer/protocol.js";
import { publishL1HeadChange } from "../src/services/l1-head-trigger.js";
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

const op1 = operatorFromSeed(generateSeedPhrase());
const op2 = operatorFromSeed(generateSeedPhrase());
const user = operatorFromSeed(generateSeedPhrase());
const funding = l2Output(user, 0x33, 50_000_000n);
const active = [op1.keyHash, op2.keyHash];

let lucid: Awaited<ReturnType<typeof Lucid>>;
const cleanups: (() => Promise<unknown>)[] = [];

beforeAll(async () => {
  lucid = await Lucid(new Emulator([]), "Custom");
});

afterEach(async () => {
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

/** Operator 2 on its own database, holding `funding`. */
const startOp2 = async () => {
  const { database, drop } = await createPeerDatabase("op2");
  cleanups.push(drop);
  const node = await startPeerNode({
    database,
    operator: op2,
    activeOperators: active,
    lucid,
  });
  cleanups.push(node.close);
  await node.seedLedger([funding]);
  return node;
};

/** Operator 1 on the file's shard, sharing with `peer`. */
const startOp1 = async (peer: PeerNode) => {
  const node = await startShardNode({
    operator: op1,
    activeOperators: active,
    peers: [peer.url],
    lucid,
  });
  cleanups.push(node.close);
  await node.seedLedger([funding]);
  return node;
};

/** Polls `check` until it holds or 10 s pass. */
const eventually = async (check: () => Promise<boolean>, what: string) => {
  const deadline = performance.now() + 10_000;
  while (!(await check())) {
    if (performance.now() >= deadline)
      throw new Error(`Timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

const holds = async (node: PeerNode, txId: Buffer) =>
  (await node.admissions()).some((row) => row.tx_id.equals(txId));

describe("L2 transaction sharing between operators", () => {
  it("sends nothing while nothing is pending", async () => {
    const peer = await startOp2();
    const node = await startOp1(peer);

    // Woken by L1 advances and by a drain that accepted nothing.
    for (let i = 0; i < 3; i++) {
      await node.run(publishL1HeadChange(node.globals));
      await node.drain();
      await new Promise((resolve) => setTimeout(resolve, 200));
    }
    expect(peer.received(PEER_TXS_PATH)).toBe(0);
    expect(peer.received(PEER_INVENTORY_PATH)).toBe(0);
  });

  it("reads no admission history on an L1 advance while nothing is pending", async () => {
    const peer = await startOp2();
    const node = await startOp1(peer);
    // A transfer of an output nobody holds: history, and nothing pending.
    const unknown = l2Output(user, 0x44, 20_000_000n);
    const tx = await buildTransfer(user, [unknown], 10_000_000n, node.config);
    expect((await submitTx(node.url, tx.txCbor)).status).toBe(202);
    await node.drain();
    expect(await node.admissions()).toMatchObject([{ status: "rejected" }]);
    await new Promise((resolve) => setTimeout(resolve, 200));

    // While another session holds the admission table, the L1 head advances:
    // any read of that table by the forwarder would wait on the lock.
    const waiting = await node.run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql.withTransaction(
          Effect.gen(function* () {
            yield* sql`LOCK TABLE tx_admissions IN ACCESS EXCLUSIVE MODE`;
            yield* publishL1HeadChange(node.globals);
            yield* Effect.sleep("500 millis");
            const [row] = yield* sql<{ waiting: string }>`
              SELECT COUNT(*) AS waiting FROM pg_locks
              WHERE relation = 'tx_admissions'::regclass AND NOT granted`;
            return Number(row!.waiting);
          }),
        );
      }),
    );
    expect(waiting).toBe(0);
    expect(peer.received(PEER_TXS_PATH)).toBe(0);
    expect(peer.received(PEER_INVENTORY_PATH)).toBe(0);
  });

  it("pushes a user's transfer to the peer with the same id and bytes", async () => {
    const peer = await startOp2();
    const node = await startOp1(peer);
    const tx = await buildTransfer(user, [funding], 10_000_000n, node.config);

    expect((await submitTx(node.url, tx.txCbor)).status).toBe(202);
    await node.drain();
    await eventually(() => holds(peer, tx.txId), "the peer to admit it");
    expect(await peer.admissions()).toMatchObject([
      { tx_id: tx.txId, submit_source: "peer" },
    ]);
    // Delivered by the push alone, with no inventory exchange.
    expect(peer.received(PEER_INVENTORY_PATH)).toBe(0);

    await peer.drain();
    const [sent] = await node.mempool();
    expect(await peer.mempool()).toEqual([{ tx_id: tx.txId, tx: sent!.tx }]);
    expect(sent!.tx.equals(Buffer.from(tx.txCbor))).toBe(true);
  });

  it("keeps admitting while the peer is down and delivers once it is back", async () => {
    const peer = await startOp2();
    const node = await startOp1(peer);
    await peer.stopServing();
    const first = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      node.config,
    );

    expect((await submitTx(node.url, first.txCbor)).status).toBe(202);
    await node.drain();
    const [change] = (await node.spendable(user.address)).filter(
      (utxo) => utxo.txHash === first.txIdHex,
    );
    const second = await buildTransfer(
      user,
      [change!],
      5_000_000n,
      node.config,
    );
    expect((await submitTx(node.url, second.txCbor)).status).toBe(202);
    await node.drain();
    expect(
      (await node.admissions()).map(({ tx_id, status }) => [tx_id, status]),
    ).toEqual([
      [first.txId, "accepted"],
      [second.txId, "accepted"],
    ]);

    await peer.resumeServing();
    await node.run(publishL1HeadChange(node.globals));
    await eventually(
      async () =>
        (await holds(peer, first.txId)) && (await holds(peer, second.txId)),
      "the peer to receive both",
    );
    await peer.drain();
    expect(
      (await peer.admissions()).map(({ tx_id, status }) => [tx_id, status]),
    ).toEqual([
      [first.txId, "accepted"],
      [second.txId, "accepted"],
    ]);
  });

  it("keeps pushing to one peer while another never answers", async () => {
    const peer = await startOp2();
    // Takes every request and never answers it.
    const hung = createServer(() => undefined);
    hung.listen(0, "127.0.0.1");
    await once(hung, "listening");
    cleanups.push(
      () =>
        new Promise((resolve) => {
          hung.closeAllConnections();
          hung.close(resolve);
        }),
    );
    const hungUrl = `http://127.0.0.1:${(hung.address() as AddressInfo).port.toString()}`;
    const node = await startShardNode({
      operator: op1,
      activeOperators: active,
      peers: [hungUrl, peer.url],
      lucid,
    });
    cleanups.push(node.close);
    await node.seedLedger([funding]);

    const first = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      node.config,
    );
    expect((await submitTx(node.url, first.txCbor)).status).toBe(202);
    await node.drain();
    await eventually(() => holds(peer, first.txId), "the first push");
    // The push to the hung peer now waits out its request timeout; the next
    // transfer still reaches the live peer.
    const [change] = (await node.spendable(user.address)).filter(
      (utxo) => utxo.txHash === first.txIdHex,
    );
    const second = await buildTransfer(
      user,
      [change!],
      5_000_000n,
      node.config,
    );
    expect((await submitTx(node.url, second.txCbor)).status).toBe(202);
    await node.drain();
    await eventually(() => holds(peer, second.txId), "the second push");
  });

  it("re-offers on the next L1 advance a transfer the peer forgot", async () => {
    // The peer has not yet seen the output the transfer spends.
    const { database, drop } = await createPeerDatabase("op2");
    cleanups.push(drop);
    const options = { database, operator: op2, activeOperators: active, lucid };
    const peer = await startPeerNode(options);
    cleanups.push(peer.close);
    const node = await startOp1(peer);
    const tx = await buildTransfer(user, [funding], 10_000_000n, node.config);

    expect((await submitTx(node.url, tx.txCbor)).status).toBe(202);
    await node.drain();
    await eventually(() => holds(peer, tx.txId), "the first push");
    await peer.drain();
    expect(await holds(peer, tx.txId)).toBe(false);

    // The peer restarts having learned the output; the sender's L1 head
    // advances.
    await peer.seedLedger([funding]);
    await peer.close();
    const restarted = await startPeerNode({ ...options, port: peer.port });
    cleanups.push(restarted.close);
    await node.run(publishL1HeadChange(node.globals));
    await eventually(() => holds(restarted, tx.txId), "the catch-up");
    expect(restarted.received(PEER_INVENTORY_PATH)).toBe(1);
    await restarted.drain();
    expect(await restarted.admissions()).toMatchObject([
      { tx_id: tx.txId, status: "accepted" },
    ]);
  });
});
