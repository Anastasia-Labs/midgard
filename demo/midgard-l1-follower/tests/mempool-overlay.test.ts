import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodePendingTransactions,
  overlayPending,
} from "../src/mempool/overlay.js";
import { addressText } from "../src/provider/utxo.js";
import { encodeSimTx, type SimTx, simTxHash } from "../src/testing/index.js";
import type { OutRef } from "../src/types.js";

const addressOf = (byte: number): Buffer =>
  Buffer.concat([Buffer.of(0x60), Buffer.alloc(28, byte)]);
const HERE = addressOf(0x11);
const THERE = addressOf(0x22);
const HERE_TEXT = addressText(HERE);

const ledgerRef = (byte: number, index = 0): OutRef => ({
  txHash: Buffer.alloc(32, byte),
  index,
});
const labelOf = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;
const labels = (utxos: readonly UTxO[]): string[] =>
  utxos.map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`);

/** A ledger UTxO at `HERE` for `outRef`. */
const ledgerUtxo = (outRef: OutRef): UTxO => ({
  txHash: outRef.txHash.toString("hex"),
  outputIndex: outRef.index,
  address: HERE_TEXT,
  assets: { lovelace: 5_000_000n },
});

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
  it("reads each transaction's id, bytes, inputs, outputs and TTL in order", () => {
    const bytes = [encodeSimTx(tx1), encodeSimTx(tx2)];
    const [one, two] = decodePendingTransactions(bytes);
    expect(one?.txHash).toBe(tx1Hash.toString("hex"));
    expect(one?.cborHex).toBe(bytes[0]?.toString("hex"));
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

  it("gives each created output the Lucid shape CML gives the same output", () => {
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

  it("lists an address's ledger outputs minus pending spends plus chained pending outputs", () => {
    const listed = overlay.overAddress(HERE_TEXT, [
      ledgerUtxo(A),
      ledgerUtxo(B),
    ]);
    expect(labels(listed)).toEqual([
      labelOf(B),
      `${tx2Hash.toString("hex")}#0`,
    ]);
  });

  it("lists only pending outputs created at the address asked for", () => {
    const there = overlay.overAddress(addressText(THERE), []);
    expect(labels(there)).toEqual([`${tx1Hash.toString("hex")}#1`]);
  });

  it("de-duplicates by out-ref, and spent wins over listed", () => {
    const chained = pending[0]?.creates[0]?.utxo;
    const created = pending[1]?.creates[0]?.utxo;
    expect(chained?.address).toBe(HERE_TEXT);
    expect(created).toBeDefined();
    const listed = overlay.overAddress(HERE_TEXT, [
      ledgerUtxo(B),
      ledgerUtxo(B),
      ledgerUtxo(A),
      ...(created === undefined ? [] : [created]),
      ...(chained === undefined ? [] : [chained]),
    ]);
    expect(labels(listed)).toEqual([
      labelOf(B),
      `${tx2Hash.toString("hex")}#0`,
    ]);
  });

  it("answers by out-ref from the ledger or pending outputs, minus pending spends", () => {
    const tx1Out0 = `${tx1Hash.toString("hex")}#0`;
    const tx1Out1 = `${tx1Hash.toString("hex")}#1`;
    const tx2Out0 = `${tx2Hash.toString("hex")}#0`;
    const found = overlay.overOutRefs(
      [tx2Out0, labelOf(B), labelOf(A), tx1Out0, tx1Out1, labelOf(B)],
      [ledgerUtxo(A), ledgerUtxo(B)],
    );
    expect(labels(found)).toEqual([tx2Out0, labelOf(B), tx1Out1]);
    expect(overlay.overOutRefs([labelOf(C)], [])).toEqual([]);
  });

  it("is the ledger unchanged when the mempool is empty", () => {
    const empty = overlayPending(decodePendingTransactions([]));
    const ledger = [ledgerUtxo(A), ledgerUtxo(B)];
    expect(empty.spent.size).toBe(0);
    expect(empty.overAddress(HERE_TEXT, ledger)).toEqual(ledger);
    expect(empty.overOutRefs([labelOf(B)], ledger)).toEqual([ledgerUtxo(B)]);
  });
});
