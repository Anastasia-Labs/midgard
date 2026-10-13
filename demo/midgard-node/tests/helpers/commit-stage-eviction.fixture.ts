import {
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { walletFromSeed } from "@lucid-evolution/lucid";

import * as LedgerUtils from "../../src/database/utils/ledger.js";

/** Fixture ledger and transactions of the commit-stage eviction tests. */

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
export const at = (seconds: number) => new Date(T0 + seconds * 1_000);
const outRef = (byte: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, byte), outputIndex: 0 });
const ledgerEntry = (byte: number, txByte = byte): LedgerUtils.Entry => ({
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, txByte),
  [LedgerUtils.Columns.OUTREF]: outRef(byte),
  [LedgerUtils.Columns.OUTPUT]: encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace: 1_000_000n + BigInt(byte), assets: new Map() },
  }),
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
});

const U1 = ledgerEntry(0x11);
const U2 = ledgerEntry(0x12);
const U3 = ledgerEntry(0x13);
export const BASE = [U1, U2, U3];
const tx = (byte: number, seconds: number, spent: LedgerUtils.Entry) => ({
  seconds,
  txId: Buffer.alloc(32, byte),
  txCbor: Buffer.from([0xff, byte]),
  spent: [spent[LedgerUtils.Columns.OUTREF]],
  produced: [ledgerEntry(byte + 0x40, byte)],
});
// T2 spends T1's output and T5, pending beyond the selection, spends T3's.
export const T1 = tx(0x0a, 1, U1);
export const T2 = tx(0x0b, 2, T1.produced[0]!);
export const T3 = tx(0x0c, 3, U2);
export const T4 = tx(0x0d, 4, U3);
export const T5 = tx(0x0e, 5, T3.produced[0]!);
export const TXS = [T1, T2, T3, T4, T5];
/** A chain of `nodeCount` lambda nodes; a longer chain shares its material. */
export const lambdaChain = (nodeCount: number) => {
  const material: MidgardCekProgramMaterialEntry[] = [];
  const envelopes: MidgardCekProgramEnvelope[] = [];
  let node: Parameters<typeof encodeMidgardCekTermNode>[0] = { kind: "error" };
  let bytes = 0n;
  for (let count = 1; count <= nodeCount; count += 1) {
    const preimage = encodeMidgardCekTermNode(node);
    const root = hashMidgardCekTermNode(node);
    material.push({ kind: "term", root, preimage });
    bytes += BigInt(preimage.length);
    envelopes.push({
      uplcVersion: [1n, 1n, 0n],
      termRoot: root,
      nodeCount: BigInt(count),
      materialByteLength: bytes,
    });
    node = { kind: "lambda", body: root };
  }
  return { material, envelopes };
};
