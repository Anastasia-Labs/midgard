import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodePendingTransactions,
  overlayPending,
} from "../src/mempool/overlay.js";
import { encodeSimTx, type SimTx, simTxHash } from "../src/testing/index.js";
import type { OutRef } from "../src/types.js";

const addressOf = (byte: number): Buffer =>
  Buffer.concat([Buffer.of(0x60), Buffer.alloc(28, byte)]);
const HERE = addressOf(0x11);
const THERE = addressOf(0x22);

const ledgerRef = (byte: number, index = 0): OutRef => ({
  txHash: Buffer.alloc(32, byte),
  index,
});
const labelOf = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

const A = ledgerRef(0xa1);
const B = ledgerRef(0xb2);
const C = ledgerRef(0xc3);

const tx1: SimTx = {
  inputs: [A],
  outputs: [
    { address: HERE, lovelace: 2_000_000n },
    { address: THERE, lovelace: 1_000_000n },
  ],
  invalidAfter: 9_000n,
  nonce: 1,
};
const tx1Hash = simTxHash(tx1);
const tx2: SimTx = {
  inputs: [{ txHash: tx1Hash, index: 0 }],
  referenceInputs: [C],
  collaterals: [B],
  outputs: [{ address: HERE, lovelace: 1_500_000n }],
  nonce: 2,
};
const tx2Hash = simTxHash(tx2);

describe("decodePendingTransactions", () => {
  it("reads each transaction's id, inputs, outputs and TTL in order", () => {
    const bytes = [encodeSimTx(tx1), encodeSimTx(tx2)];
    const [one, two] = decodePendingTransactions(bytes);
    expect(one?.txHash).toBe(tx1Hash.toString("hex"));
    expect(one?.spends).toEqual([labelOf(A)]);
    expect(one?.reads).toEqual([]);
    expect(one?.validTo).toBe(9_000n);
    expect(one?.creates.map((output) => output.outRef)).toEqual([
      `${tx1Hash.toString("hex")}#0`,
      `${tx1Hash.toString("hex")}#1`,
    ]);
    expect(two?.txHash).toBe(tx2Hash.toString("hex"));
    expect(two?.spends).toEqual([`${tx1Hash.toString("hex")}#0`]);
    expect(two?.reads).toEqual([labelOf(C)]);
    expect(two?.validTo).toBeUndefined();
  });

  it("gives each created output the Lucid shape and canonical CBOR CML gives the same output", () => {
    const policy = "ab".repeat(28);
    const datum = Buffer.from("d8799f4101ff", "hex");
    const script = Buffer.from("4e4d01000033222220051200120011", "hex");
    const rich: SimTx = {
      inputs: [A],
      outputs: [
        {
          address: HERE,
          lovelace: 3_000_000n,
          assets: new Map([[policy, new Map([["cafe", 7n]])]]),
          datum,
          scriptRef: script,
        },
      ],
      nonce: 3,
    };
    const raw = encodeSimTx(rich);
    const [pending] = decodePendingTransactions([raw]);
    const tx = CML.Transaction.from_cbor_bytes(raw);
    const body = tx.body();
    const outputs = body.outputs();
    const output = outputs.get(0);
    try {
      expect(pending?.creates[0]?.utxo).toEqual({
        txHash: simTxHash(rich).toString("hex"),
        outputIndex: 0,
        ...coreToTxOutput(output),
      });
      expect(pending?.creates[0]?.raw.outputCbor).toBe(
        output.to_canonical_cbor_hex(),
      );
    } finally {
      output.free();
      outputs.free();
      body.free();
      tx.free();
    }
  });
});

describe("overlayPending", () => {
  const pending = decodePendingTransactions([
    encodeSimTx(tx1),
    encodeSimTx(tx2),
  ]);
  const overlay = overlayPending(pending);

  it("counts regular inputs spent, never collateral or reference inputs", () => {
    expect([...overlay.spent].sort()).toEqual(
      [labelOf(A), `${tx1Hash.toString("hex")}#0`].sort(),
    );
    expect(overlay.spent.has(labelOf(B))).toBe(false);
    expect(overlay.spent.has(labelOf(C))).toBe(false);
  });

  it("lists the outputs pending transactions create that none of them spends", () => {
    expect(overlay.created.map((output) => output.outRef)).toEqual([
      `${tx1Hash.toString("hex")}#1`,
      `${tx2Hash.toString("hex")}#0`,
    ]);
  });

  it("skips a listed transaction that does not decode", () => {
    const listed = decodePendingTransactions([
      Buffer.from("ff", "hex"),
      encodeSimTx(tx1),
    ]);
    expect(overlayPending(listed)).toEqual(
      overlayPending(decodePendingTransactions([encodeSimTx(tx1)])),
    );
  });
});
