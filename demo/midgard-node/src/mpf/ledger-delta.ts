/**
 * Ledger deltas: the insert/delete operation sets a transition effect implies,
 * and the conversion of ledger entries into trie batch operations.
 */

import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  type CanonicalTransitionEffect,
} from "@al-ft/midgard-validation";

import * as Ledger from "../database/utils/ledger.js";
import {
  type MpfBatchOp,
  type MpfInsertBatchOp,
  type UtxoPayloadEntry,
} from "./types.js";

export const transitionEffectToRawLedgerOps = (
  effect: CanonicalTransitionEffect,
): readonly MpfBatchOp[] =>
  effect.operations.map((operation) =>
    operation.type === "delete"
      ? { type: "delete", key: Buffer.from(operation.outRefCbor) }
      : {
          type: "insert",
          key: Buffer.from(operation.outRefCbor),
          value: Buffer.from(operation.outputCbor),
        },
  );

export const transitionEffectToLedgerOps = (
  effect: CanonicalTransitionEffect,
): readonly MpfBatchOp[] =>
  effect.operations.map((operation) =>
    operation.type === "delete"
      ? { type: "delete", key: Buffer.from(operation.outRefCbor) }
      : ledgerOutputToInsertBatchOp({
          outRef: operation.outRefCbor,
          outputCbor: operation.outputCbor,
        }),
  );

export type LedgerDelta = {
  readonly spent: readonly Buffer[];
  readonly produced: readonly UtxoPayloadEntry[];
};

export const collapseLedgerDelta = (
  ops: readonly MpfBatchOp[],
  insertedValues: ReadonlyMap<string, Buffer>,
): LedgerDelta => {
  const finalByOutref = new Map<string, MpfBatchOp>();
  for (const op of ops) finalByOutref.set(op.key.toString("hex"), op);
  const spent: Buffer[] = [];
  const produced: UtxoPayloadEntry[] = [];
  for (const op of finalByOutref.values()) {
    if (op.type === "delete") spent.push(Buffer.from(op.key));
    else {
      const output = insertedValues.get(op.key.toString("hex"));
      if (output === undefined) {
        throw new Error(
          `Missing full output bytes for ledger delta insertion ${op.key.toString("hex")}`,
        );
      }
      produced.push({
        outref: Buffer.from(op.key),
        output: Buffer.from(output),
      });
    }
  }
  return { spent, produced };
};

export const ledgerEntryToInsertBatchOp = (
  entry: Ledger.MinimalEntry,
): MpfInsertBatchOp =>
  ledgerOutputToInsertBatchOp({
    outRef: entry[Ledger.Columns.OUTREF],
    outputCbor: entry[Ledger.Columns.OUTPUT],
  });

export const ledgerOutputToInsertBatchOp = ({
  outRef,
  outputCbor,
}: {
  readonly outRef: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MpfInsertBatchOp => ({
  type: "insert",
  key: Buffer.from(outRef),
  value: buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef,
    outputCbor,
  }).descriptorCbor,
});
