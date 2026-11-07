/**
 * The stateless fail-fast checks (design D6) over real HTTP on a real
 * database: a tx Phase A refuses on stateless grounds (a bad signature, a fee
 * under the minimum, a foreign network) or whose validity has passed is
 * refused by `/submit` with validation's code and by `/peer/txs` as refused,
 * answered while the database refuses every connection, and never stored.
 */
import "./utils.js";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { buildLoadTxShape } from "../scripts/submit-ingress-load.tx.mjs";
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
const PREPROD = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

let lucid: Awaited<ReturnType<typeof Lucid>>;
const nodes: PeerNode[] = [];

beforeAll(async () => {
  const emulated = await Lucid(new Emulator([]), "Custom");
  // An emulator's slot 0 is its start, so no slot is long past; the node
  // reads Preprod's mapping instead, where one is.
  lucid = {
    ...emulated,
    config: () => ({ ...emulated.config(), slotConfig: { ...PREPROD } }),
  };
});

afterEach(async () => {
  for (const node of nodes.splice(0)) await node.close();
});

const startReceiver = async () => {
  const node = await startShardNode({
    operator: receiver,
    activeOperators: [sender.keyHash, receiver.keyHash],
    lucid,
    // The public testnet's fee rule (`config.preprod-public.example.yaml`).
    config: { MIN_FEE_A: 1n, MIN_FEE_B: 2_000n },
  });
  nodes.push(node);
  return node;
};

/** A Preprod slot `secondsAgo` seconds before now. */
const preprodSlotSecondsAgo = (secondsAgo: number) =>
  BigInt(
    PREPROD.zeroSlot +
      Math.floor((Date.now() - PREPROD.zeroTime) / PREPROD.slotLength) -
      secondsAgo,
  );

/** One tx of each junk class, and the code validation gives it. */
const junk = (label: number) => [
  {
    name: "a signature over another tx id",
    tx: buildLoadTxShape({
      inputCount: 1,
      outputCount: 2,
      nonce: label,
      signedTxId: Buffer.alloc(32, 0x11),
    }),
    error: "E_INVALID_SIGNATURE",
  },
  {
    name: "a fee under the minimum",
    tx: buildLoadTxShape({
      inputCount: 1,
      outputCount: 2,
      nonce: label + 1,
      feeOverride: 2_000n,
    }),
    error: "E_MIN_FEE",
  },
  {
    name: "a mainnet body",
    tx: buildLoadTxShape({
      inputCount: 1,
      outputCount: 2,
      nonce: label + 2,
      networkId: 1n,
    }),
    error: "E_NETWORK_ID_MISMATCH",
  },
  {
    name: "a validity interval that ended two minutes ago",
    tx: buildLoadTxShape({
      inputCount: 1,
      outputCount: 2,
      nonce: label + 3,
      validityIntervalEnd: preprodSlotSecondsAgo(120),
    }),
    error: "E_VALIDITY_INTERVAL_MISMATCH",
  },
];

const offer = async (node: PeerNode, txs: readonly Uint8Array[]) => {
  const reply = await signedPeerPost(
    node.url,
    PEER_TXS_PATH,
    txs,
    sender.signer,
  );
  expect(reply.status).toBe(200);
  return JSON.parse(reply.body.toString()) as unknown;
};

describe("stateless fail-fast checks", () => {
  it("refuses each junk class at /submit with validation's code, with no database", async () => {
    const node = await startReceiver();
    const classes = junk(100);
    const valid = buildLoadTxShape({ inputCount: 1, outputCount: 2, nonce: 1 });

    const reconnect = await makeDatabaseUnreachable(node.config.POSTGRES_DB);
    try {
      for (const { name, tx, error } of classes) {
        const reply = await submitTx(node.url, tx.txCbor);
        expect({ name, status: reply.status }).toEqual({ name, status: 400 });
        expect(JSON.parse(reply.body.toString())).toMatchObject({ error });
      }
    } finally {
      await reconnect();
    }
    // A valid tx built the same way is admitted.
    expect((await submitTx(node.url, valid.txCbor)).status).toBe(202);

    expect((await node.admissions()).map((row) => row.tx_id)).toEqual([
      valid.txId,
    ]);
  });

  it("refuses each junk class offered by a peer and admits the valid tx beside them", async () => {
    const node = await startReceiver();
    const classes = junk(200);
    const valid = buildLoadTxShape({ inputCount: 1, outputCount: 2, nonce: 2 });

    expect(
      await offer(node, [...classes.map(({ tx }) => tx.txCbor), valid.txCbor]),
    ).toEqual({ admitted: 1, duplicates: 0, refused: classes.length });

    expect(await node.admissions()).toEqual([
      expect.objectContaining({ tx_id: valid.txId, submit_source: "peer" }),
    ]);
  });
});
