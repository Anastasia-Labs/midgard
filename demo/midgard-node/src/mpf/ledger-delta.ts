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

/**
 * Collapse a block's ordered ledger operations into its net delta against the
 * block's base ledger. An outref's first operation says whether the base holds
 * it (a delete) or not (an insert), and its last one whether the block's final
 * ledger does. An output a later transaction of the same block spends was
 * never in the base, so it is neither spent nor produced.
 */
export const collapseLedgerDelta = (
  ops: readonly MpfBatchOp[],
  insertedValues: ReadonlyMap<string, Buffer>,
): LedgerDelta => {
  const byOutref = new Map<
    string,
    { readonly first: MpfBatchOp; last: MpfBatchOp }
  >();
  for (const op of ops) {
    const outrefHex = op.key.toString("hex");
    const seen = byOutref.get(outrefHex);
    if (seen === undefined) byOutref.set(outrefHex, { first: op, last: op });
    else seen.last = op;
  }
  const spent: Buffer[] = [];
  const produced: UtxoPayloadEntry[] = [];
  for (const [outrefHex, { first, last: op }] of byOutref) {
    const inBase = first.type === "delete";
    const inFinal = op.type === "insert";
    if (inBase && inFinal) {
      // Outrefs are never re-created once spent, so a spent base entry that
      // is present again at the end would substitute an existing UTxO.
      throw new Error(
        `Ledger delta spends and re-creates base outref ${outrefHex}`,
      );
    }
    if (inBase) spent.push(Buffer.from(op.key));
    else if (inFinal) {
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
