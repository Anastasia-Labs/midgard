/**
 * Load shedding (design D5) over real HTTP on a real database: past 80 % of
 * either durable admission backlog bound an anonymous `/submit` gets 503
 * with `Retry-After`, answered while the database refuses every connection,
 * and a peer's `/peer/txs` is still admitted up to the full bound.
 */

import { createHash } from "node:crypto";

import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { MutableRef } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  buildLoadTx,
  LOAD_GENESIS_OUTPUT_LOVELACE,
  loadOutRef,
  makeLoadKey,
} from "../scripts/submit-ingress-load.tx.mjs";
import { PEER_TXS_PATH } from "../src/l2-peer/protocol.js";
import { makeDatabaseUnreachable } from "./helpers/l2-peer-database.js";
import {
  operatorFromSeed,
  type PeerNode,
  signedPeerPost,
  startShardNode,
  submitTx,
} from "./helpers/l2-peer-node.js";

const sender = operatorFromSeed(generateSeedPhrase());
const receiver = operatorFromSeed(generateSeedPhrase());

let lucid: Awaited<ReturnType<typeof Lucid>>;
const nodes: PeerNode[] = [];

beforeAll(async () => {
  lucid = await Lucid(new Emulator([]), "Custom");
});

afterEach(async () => {
  for (const node of nodes.splice(0)) await node.close();
});

const startReceiver = async () => {
  const node = await startShardNode({
    operator: receiver,
    activeOperators: [sender.keyHash, receiver.keyHash],
    lucid,
    config: {
      MAX_DURABLE_ADMISSION_BACKLOG: 20,
      MAX_DURABLE_ADMISSION_BACKLOG_BYTES: 100_000,
    },
  });
  nodes.push(node);
  return node;
};

/** A signed tx spending an outref of its own; admission reads no ledger. */
const loadTx = (node: PeerNode, label: string) => {
  const key = makeLoadKey(label, 0);
  return buildLoadTx({
    inputs: [
      {
        key,
        lovelace: LOAD_GENESIS_OUTPUT_LOVELACE,
        outRef: loadOutRef(createHash("sha256").update(label).digest(), 0),
      },
    ],
    outputAddresses: [key.addressBytes, key.addressBytes],
    minFeeA: node.config.MIN_FEE_A,
    minFeeB: node.config.MIN_FEE_B,
  });
};

/**
 * Sets the in-memory backlog gauge to `rows` rows and `bytes` bytes, once
 * every acknowledged tx is written (none holds an in-flight reservation).
 */
const setBacklog = async (node: PeerNode, rows: bigint, bytes: bigint) => {
  await node.admissions();
  MutableRef.update(node.globals.ADMISSION_BACKLOG_GAUGE, (gauge) => ({
    ...gauge,
    ADMISSION_BACKLOG_BASE: rows,
    ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
    ADMISSION_BACKLOG_BYTES_BASE: bytes,
    ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
  }));
};

const offer = (node: PeerNode, tx: { readonly txCbor: Uint8Array }) =>
  signedPeerPost(node.url, PEER_TXS_PATH, [tx.txCbor], sender.signer);

describe("admission load shedding", () => {
  it("sheds anonymous submits past 80 % of the row bound with no database, and admits peers", async () => {
    const node = await startReceiver();
    const below = loadTx(node, "shed-below");
    const anonymous = loadTx(node, "shed-anonymous");
    const peer = loadTx(node, "shed-peer");

    // 15 of 20 rows (75 %): anonymous traffic is still admitted.
    await setBacklog(node, 15n, 0n);
    expect((await submitTx(node.url, below.txCbor)).status).toBe(202);

    // 17 of 20 rows (85 %), with the database refusing every connection:
    // the shed answer needs none.
    await setBacklog(node, 17n, 0n);
    const reconnect = await makeDatabaseUnreachable(node.config.POSTGRES_DB);
    try {
      const shed = await submitTx(node.url, anonymous.txCbor);
      expect(shed.status).toBe(503);
      expect(shed.headers.get("retry-after")).toBe("5");
      expect(JSON.parse(shed.body.toString())).toEqual({
        error: "Durable submission admission backlog is full",
        backlog: "17",
        maxBacklog: "16",
      });
    } finally {
      await reconnect();
    }
    // Still 85 %: a peer tx is admitted.
    const admitted = await offer(node, peer);
    expect(admitted.status).toBe(200);
    expect(JSON.parse(admitted.body.toString())).toEqual({
      admitted: 1,
      duplicates: 0,
      refused: 0,
    });

    // The peer tx is written; the shed one never.
    const stored = new Map(
      (await node.admissions()).map((row) => [
        row.tx_id.toString("hex"),
        row.submit_source,
      ]),
    );
    expect(stored).toEqual(
      new Map([
        [below.txId.toString("hex"), "native"],
        [peer.txId.toString("hex"), "peer"],
      ]),
    );
  });

  it("sheds anonymous submits past 80 % of the byte bound and admits peers to the full bound", async () => {
    const node = await startReceiver();
    const anonymous = loadTx(node, "shed-bytes-anonymous");
    const peer = loadTx(node, "shed-bytes-peer");
    const last = loadTx(node, "shed-bytes-last");

    // 85 % of the bytes, few rows.
    await setBacklog(node, 0n, 85_000n);
    const shed = await submitTx(node.url, anonymous.txCbor);
    expect(shed.status).toBe(503);
    expect(shed.headers.get("retry-after")).toBe("5");
    expect(JSON.parse(shed.body.toString())).toEqual({
      error: "Durable submission admission byte backlog is full",
      backlogBytes: "85000",
      maxBacklogBytes: "80000",
    });
    expect(JSON.parse((await offer(node, peer)).body.toString())).toEqual({
      admitted: 1,
      duplicates: 0,
      refused: 0,
    });

    // A full byte bound refuses peers too.
    await setBacklog(node, 0n, 100_000n);
    const full = await offer(node, last);
    expect(full.status).toBe(503);
    expect(full.headers.get("retry-after")).toBe("5");
  });
});
