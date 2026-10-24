import {
  requireInputIndex,
  requireReferenceInputIndex,
} from "@al-ft/midgard-sdk";
import { type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

export const same = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

/** Position of a spent UTxO in the builder's final transaction. */
export const inputAt = (ctx: RedeemerContext, utxo: UTxO) =>
  requireInputIndex(ctx, utxo, "history fixture");

/** Position of a read UTxO in the builder's final transaction. */
export const referenceAt = (ctx: RedeemerContext, utxo: UTxO) =>
  requireReferenceInputIndex(ctx, utxo, "history fixture");
