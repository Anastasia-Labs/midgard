/**
 * UTxO payload size accounting and the payload root over materialized entries.
 */

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import { keyValuePhasRoot } from "../workers/utils/mpf/phas.js";
import { MpfError } from "./errors.js";
import { ledgerOutputToInsertBatchOp } from "./ledger-delta.js";
import {
  type MpfBatchOp,
  type MpfStoredValue,
  type UtxoPayloadEntry,
} from "./types.js";

export type UtxoPayloadSizeAggregate = SDK.DaPayloadEntrySizeAggregate;

export const utxoPayloadEntryEncodedSize = ({
  outref,
  output,
}: UtxoPayloadEntry): number =>
  SDK.daPayloadEntryEncodedSize([
    outref.toString("hex"),
    output.toString("hex"),
  ]);

export const utxoPayloadAggregateFromEntries = (
  entries: readonly UtxoPayloadEntry[],
): UtxoPayloadSizeAggregate => ({
  entryCount: entries.length,
  encodedTupleBytes: entries.reduce(
    (total, entry) => total + utxoPayloadEntryEncodedSize(entry),
    0,
  ),
});

export const ledgerPayloadAggregateFromEntries = (
  entries: readonly Ledger.MinimalEntry[],
): UtxoPayloadSizeAggregate =>
  utxoPayloadAggregateFromEntries(
    entries.map((entry) => ({
      outref: entry[Ledger.Columns.OUTREF],
      output: entry[Ledger.Columns.OUTPUT],
    })),
  );

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

export const applyLedgerOpsToUtxoPayloadAggregateFromFullValues = (
  base: UtxoPayloadSizeAggregate,
  ops: readonly MpfBatchOp[],
  initialValues: ReadonlyMap<string, Buffer>,
  insertedValues: ReadonlyMap<string, Buffer>,
): Effect.Effect<UtxoPayloadSizeAggregate, MpfError> =>
  Effect.gen(function* () {
    let entryCount = base.entryCount;
    let encodedTupleBytes = base.encodedTupleBytes;
    const currentValues = new Map<string, Buffer | null>();
    for (const op of ops) {
      const keyHex = op.key.toString("hex");
      const current = currentValues.has(keyHex)
        ? currentValues.get(keyHex)
        : (initialValues.get(keyHex) ?? null);
      if (current !== null && current !== undefined) {
        entryCount -= 1;
        encodedTupleBytes -= utxoPayloadEntryEncodedSize({
          outref: op.key,
          output: current,
        });
      } else if (op.type === "delete") {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "DA UTxO size aggregate",
            new Error(`Cannot size deletion of missing UTxO ${keyHex}`),
          ),
        );
      }
      if (op.type === "insert") {
        const inserted = insertedValues.get(keyHex);
        if (inserted === undefined) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "DA UTxO size aggregate",
              new Error(`Missing full output bytes for insertion ${keyHex}`),
            ),
          );
        }
        entryCount += 1;
        encodedTupleBytes += utxoPayloadEntryEncodedSize({
          outref: op.key,
          output: inserted,
        });
        currentValues.set(keyHex, Buffer.from(inserted));
      } else {
        currentValues.set(keyHex, null);
      }
    }
    const aggregate = { entryCount, encodedTupleBytes };
    try {
      SDK.daPayloadEntriesEncodedSizeFromAggregate(aggregate);
    } catch (cause) {
      return yield* Effect.fail(
        MpfError.rootBuild("DA UTxO size aggregate", cause),
      );
    }
    return aggregate;
  });

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

export const computeUtxoPayloadRoot = (
  entries: readonly UtxoPayloadEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.try({
    try: () =>
      entries.map((entry) =>
        ledgerOutputToInsertBatchOp({
          outRef: entry.outref,
          outputCbor: entry.output,
        }),
      ),
    catch: (cause) => MpfError.rootBuild("DA UTxO descriptor root", cause),
  }).pipe(
    Effect.flatMap((ops) =>
      keyValuePhasRoot(
        ops.map((op) => op.key),
        ops.map((op) => op.value),
      ),
    ),
  );

export const encodeUnsignedBigEndian = (value: bigint): Buffer => {
  if (value <= 0n) return Buffer.from([0]);
  const bytes: number[] = [];
  for (let remaining = value; remaining > 0n; remaining >>= 8n) {
    bytes.push(Number(remaining & 0xffn));
  }
  bytes.reverse();
  return Buffer.from(bytes);
};
