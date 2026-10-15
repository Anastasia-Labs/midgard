import { type TransitionTraceForgedStep } from "./transition-trace-final-cases.forged-step.js";

export type TransitionTraceFinalCase = Readonly<{
  assetCount: number;
  outputCount: number;
  outputBytes?: number;
  fieldBytes?: number;
  honest?: boolean;
  depth?: number;
  kind?: string;
  cancelAt?: number;
  corruptAssetIndex?: boolean;
  /** Forge the restored stack entries of the output datum walk. */
  corruptWalk?: boolean;
  /** Forge one streamed step so that exactly one withdraw-zero script refuses. */
  forgeStep?: TransitionTraceForgedStep;
  corruptSourceReference?: boolean;
  datumBytes?: number;
  /** The produced output's exact canonical CBOR, in place of a built one. */
  outputCbor?: string;
  /** An output datum's CBOR, in place of a `datumBytes` byte string. */
  datumCbor?: string;
  drained?: boolean;
  /** Ledger entries the transaction spends besides any drained entry. */
  spentCount?: number;
  /** Unrelated ledger entries that deepen every MPF proof. */
  ledgerFill?: number;
  /** Insert the single output at this MPF depth into a synthesised ledger. */
  ledgerDepth?: number;
}>;
