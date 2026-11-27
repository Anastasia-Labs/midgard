/**
 * MPF value size estimates and the UTxO entries a batch materializes.
 */

import * as Ledger from "../database/utils/ledger.js";
import {
  type MpfBatchOp,
  type MpfStoredValue,
  type UtxoPayloadEntry,
} from "./types.js";

export const estimateMpfStoredValueBytes = (value: MpfStoredValue): number => {
  if (typeof value === "string") {
    return 128 + 2 * Buffer.byteLength(value);
  }
  const kind = value.__kind;
  const prefix = value.prefix;
  if (kind === "Leaf") {
    if (
      typeof prefix !== "string" ||
      typeof value.key !== "string" ||
      typeof value.value !== "string"
    ) {
      throw new Error("Invalid serialized MPF leaf in block path cache");
    }
    return (
      512 +
      2 *
        (Buffer.byteLength(prefix) +
          Buffer.byteLength(value.key) +
          Buffer.byteLength(value.value))
    );
  }
  if (kind === "Branch") {
    if (
      typeof prefix !== "string" ||
      !Array.isArray(value.children) ||
      value.children.length !== 16 ||
      !value.children.every(
        (child) =>
          child === undefined || child === null || typeof child === "string",
      ) ||
      !Number.isSafeInteger(value.size)
    ) {
      throw new Error("Invalid serialized MPF branch in block path cache");
    }
    const childBytes = value.children.reduce<number>(
      (total, child) =>
        total + (typeof child === "string" ? Buffer.byteLength(child) : 8),
      0,
    );
    return 768 + 2 * (Buffer.byteLength(prefix) + childBytes);
  }
  throw new Error("Invalid serialized MPF node kind in block path cache");
};

const compareBufferHex = (left: Buffer, right: Buffer): number => {
  const leftHex = left.toString("hex");
  const rightHex = right.toString("hex");
  return leftHex < rightHex ? -1 : leftHex > rightHex ? 1 : 0;
};

export const materializeUtxoPayloadEntries = (
  initialLedgerEntries: readonly Ledger.MinimalEntry[],
  ledgerOps: readonly MpfBatchOp[],
  insertedValues: ReadonlyMap<string, Buffer>,
): readonly UtxoPayloadEntry[] => {
  const entries = new Map<string, UtxoPayloadEntry>();
  for (const entry of initialLedgerEntries) {
    entries.set(entry[Ledger.Columns.OUTREF].toString("hex"), {
      outref: Buffer.from(entry[Ledger.Columns.OUTREF]),
      output: Buffer.from(entry[Ledger.Columns.OUTPUT]),
    });
  }
  for (const op of ledgerOps) {
    const key = op.key.toString("hex");
    if (op.type === "delete") {
      entries.delete(key);
      continue;
    }
    const output = insertedValues.get(key);
    if (output === undefined) {
      throw new Error(`Missing full output bytes for insertion ${key}`);
    }
    entries.set(key, {
      outref: Buffer.from(op.key),
      output: Buffer.from(output),
    });
  }
  return [...entries.values()].sort((left, right) =>
    compareBufferHex(left.outref, right.outref),
  );
};

export const encodeUnsignedBigEndian = (value: bigint): Buffer => {
  if (value <= 0n) return Buffer.from([0]);
  const bytes: number[] = [];
  for (let remaining = value; remaining > 0n; remaining >>= 8n) {
    bytes.push(Number(remaining & 0xffn));
  }
  bytes.reverse();
  return Buffer.from(bytes);
};
