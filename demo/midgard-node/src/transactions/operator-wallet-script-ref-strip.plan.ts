/**
 * The plan of `strip-operator-wallet-script-refs`: the operator wallet's
 * UTxOs that carry a reference script, in batches that each fit the ledger's
 * transaction-size, value-size and reference-script byte limits (with the
 * sweep's margin). A batch spends its UTxOs back to the operator address in
 * outputs without a reference script: its tokens packed into as few outputs
 * as the value-size budget allows, each with its minimum ADA, and the rest of
 * the lovelace in the change output.
 */
import { type Assets, type UTxO } from "@lucid-evolution/lucid";

import { compareOutRefs, outRefLabel } from "../tx-context.js";
import {
  ledgerMinimumFee,
  REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
  referenceScriptLedgerBytes,
  type ReferenceScriptSweepLimits,
  TX_BASE_BYTES,
  TX_INPUT_BYTES,
  TX_OUTPUT_OVERHEAD_BYTES,
  withMargin,
} from "./reference-script-sweep.reference-script-fee.js";
import {
  packQuarantineOutputs,
  sumTokens,
  valueCborBytes,
} from "./reference-script-sweep.select-retired-reference-script-utxos.js";
import { lovelaceOf } from "./wallet-hygiene.js";

export type OperatorWalletScriptRefStripBatch = {
  readonly batchIndex: number;
  readonly inputs: readonly UTxO[];
  readonly inputLovelace: bigint;
  readonly referenceScriptBytes: number;
  readonly estimatedTxBytes: number;
  readonly estimatedFee: bigint;
  /** The batch's tokens, packed, each output with its minimum ADA. */
  readonly tokenOutputs: readonly Readonly<Assets>[];
};

export type OperatorWalletScriptRefStripPlan = {
  readonly operatorAddress: string;
  readonly budgets: {
    readonly referenceScriptBytesPerBatch: number;
    readonly txBytesPerBatch: number;
    readonly valueBytesPerOutput: number;
    readonly inputsPerBatch: number;
  };
  /** The view's UTxOs without a reference script, which no batch selects. */
  readonly retainedUtxoCount: number;
  readonly batches: readonly OperatorWalletScriptRefStripBatch[];
};

/** A UTxO alone exceeds a batch budget, so no batch can carry it. */
export class OperatorWalletScriptRefStripRefusal extends Error {
  override readonly name = "OperatorWalletScriptRefStripRefusal";
}

export const carriesScriptRef = (utxo: UTxO): boolean =>
  utxo.scriptRef !== undefined && utxo.scriptRef !== null;

export const scriptRefLedgerBytes = (utxos: readonly UTxO[]): number =>
  utxos.reduce(
    (total, utxo) =>
      total +
      (utxo.scriptRef === undefined || utxo.scriptRef === null
        ? 0
        : referenceScriptLedgerBytes(utxo.scriptRef)),
    0,
  );

type BatchShape = Omit<OperatorWalletScriptRefStripBatch, "batchIndex">;

const shapeBatch = (
  inputs: readonly UTxO[],
  operatorAddress: string,
  limits: ReferenceScriptSweepLimits,
  valueBytesPerOutput: number,
): BatchShape => {
  const tokenOutputs = packQuarantineOutputs({
    tokens: sumTokens(inputs),
    quarantineAddress: operatorAddress,
    valueBytesPerOutput,
    coinsPerUtxoByte: limits.coinsPerUtxoByte,
  });
  const referenceScriptBytes = scriptRefLedgerBytes(inputs);
  // TX_BASE_BYTES covers the change output and one witness.
  const estimatedTxBytes =
    TX_BASE_BYTES +
    inputs.length * TX_INPUT_BYTES +
    tokenOutputs.reduce(
      (total, output) =>
        total + TX_OUTPUT_OVERHEAD_BYTES + valueCborBytes(output),
      0,
    );
  return {
    inputs,
    inputLovelace: inputs.reduce((total, utxo) => total + lovelaceOf(utxo), 0n),
    referenceScriptBytes,
    estimatedTxBytes,
    estimatedFee: ledgerMinimumFee(
      limits,
      estimatedTxBytes,
      referenceScriptBytes,
    ),
    tokenOutputs,
  };
};

export const buildOperatorWalletScriptRefStripPlan = ({
  utxos,
  operatorAddress,
  limits,
  maxReferenceScriptBytesPerBatch,
  maxInputsPerBatch = REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
}: {
  readonly utxos: readonly UTxO[];
  readonly operatorAddress: string;
  readonly limits: ReferenceScriptSweepLimits;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
}): OperatorWalletScriptRefStripPlan => {
  const protocolReferenceBudget = withMargin(
    limits.maxReferenceScriptBytesPerTx,
  );
  if (
    maxReferenceScriptBytesPerBatch !== undefined &&
    (!Number.isSafeInteger(maxReferenceScriptBytesPerBatch) ||
      maxReferenceScriptBytesPerBatch <= 0 ||
      maxReferenceScriptBytesPerBatch > protocolReferenceBudget)
  )
    throw new Error(
      `maxReferenceScriptBytesPerBatch must be a positive integer no larger than ${protocolReferenceBudget.toString()}`,
    );
  if (!Number.isSafeInteger(maxInputsPerBatch) || maxInputsPerBatch <= 0)
    throw new Error("maxInputsPerBatch must be a safe positive integer");
  const budgets = {
    referenceScriptBytesPerBatch:
      maxReferenceScriptBytesPerBatch ?? protocolReferenceBudget,
    txBytesPerBatch: withMargin(limits.maxTxSize),
    valueBytesPerOutput: withMargin(limits.maxValueSize),
    inputsPerBatch: maxInputsPerBatch,
  };
  const shape = (inputs: readonly UTxO[]) =>
    shapeBatch(inputs, operatorAddress, limits, budgets.valueBytesPerOutput);
  const fits = (batch: BatchShape): boolean =>
    batch.inputs.length <= budgets.inputsPerBatch &&
    batch.referenceScriptBytes <= budgets.referenceScriptBytesPerBatch &&
    batch.estimatedTxBytes <= budgets.txBytesPerBatch;

  const selected = utxos
    .filter(
      (utxo) => utxo.address === operatorAddress && carriesScriptRef(utxo),
    )
    .sort(compareOutRefs);
  const shapes: BatchShape[] = [];
  let current: BatchShape | undefined;
  for (const utxo of selected) {
    const extended = shape([...(current?.inputs ?? []), utxo]);
    if (fits(extended)) {
      current = extended;
      continue;
    }
    const alone = shape([utxo]);
    if (!fits(alone))
      throw new OperatorWalletScriptRefStripRefusal(
        `${outRefLabel(utxo)} alone exceeds the batch budget (reference_script_bytes=${alone.referenceScriptBytes.toString()},estimated_tx_bytes=${alone.estimatedTxBytes.toString()})`,
      );
    if (current !== undefined) shapes.push(current);
    current = alone;
  }
  if (current !== undefined) shapes.push(current);
  return {
    operatorAddress,
    budgets,
    retainedUtxoCount: utxos.length - selected.length,
    batches: shapes.map((batch, batchIndex) => ({ batchIndex, ...batch })),
  };
};

export type OperatorWalletScriptRefStripPlanSummary = {
  readonly operatorAddress: string;
  readonly budgets: OperatorWalletScriptRefStripPlan["budgets"];
  readonly retainedUtxoCount: number;
  readonly batches: readonly {
    readonly batchIndex: number;
    readonly inputCount: number;
    readonly inputLovelace: bigint;
    readonly referenceScriptBytes: number;
    readonly estimatedTxBytes: number;
    readonly estimatedFee: bigint;
    readonly tokenOutputCount: number;
    readonly inputOutRefs: readonly string[];
  }[];
  readonly totals: {
    readonly batchCount: number;
    readonly inputCount: number;
    readonly inputLovelace: bigint;
    readonly referenceScriptBytes: number;
    readonly estimatedFee: bigint;
    readonly tokenOutputCount: number;
  };
};

export const summarizeOperatorWalletScriptRefStripPlan = (
  plan: OperatorWalletScriptRefStripPlan,
): OperatorWalletScriptRefStripPlanSummary => {
  const batches = plan.batches.map((batch) => ({
    batchIndex: batch.batchIndex,
    inputCount: batch.inputs.length,
    inputLovelace: batch.inputLovelace,
    referenceScriptBytes: batch.referenceScriptBytes,
    estimatedTxBytes: batch.estimatedTxBytes,
    estimatedFee: batch.estimatedFee,
    tokenOutputCount: batch.tokenOutputs.length,
    inputOutRefs: batch.inputs.map(outRefLabel),
  }));
  return {
    operatorAddress: plan.operatorAddress,
    budgets: plan.budgets,
    retainedUtxoCount: plan.retainedUtxoCount,
    batches,
    totals: {
      batchCount: batches.length,
      inputCount: batches.reduce((total, b) => total + b.inputCount, 0),
      inputLovelace: batches.reduce((total, b) => total + b.inputLovelace, 0n),
      referenceScriptBytes: batches.reduce(
        (total, b) => total + b.referenceScriptBytes,
        0,
      ),
      estimatedFee: batches.reduce((total, b) => total + b.estimatedFee, 0n),
      tokenOutputCount: batches.reduce(
        (total, b) => total + b.tokenOutputCount,
        0,
      ),
    },
  };
};
