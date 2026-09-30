import { type Assets, type UTxO } from "@lucid-evolution/lucid";

import { outRefLabel } from "../tx-context.js";
import {
  assertRetiredPolicyIsNotLive,
  ledgerMinimumFee,
  type LiveReferenceScriptDeployment,
  REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
  referenceScriptLedgerBytes,
  type ReferenceScriptSweepBatch,
  type ReferenceScriptSweepLimits,
  type ReferenceScriptSweepPlan,
  ReferenceScriptSweepRefusal,
  type RetiredAuthPolicyDisposition,
  TX_BASE_BYTES,
  TX_INPUT_BYTES,
  TX_MINT_ASSET_OVERHEAD_BYTES,
  TX_MINT_POLICY_BYTES,
  TX_NATIVE_WITNESS_OVERHEAD_BYTES,
  TX_OUTPUT_OVERHEAD_BYTES,
  withMargin,
} from "./reference-script-sweep.reference-script-fee.js";
import {
  packQuarantineOutputs,
  selectRetiredReferenceScriptUtxos,
  sortedTokenUnits,
  sumTokens,
  valueCborBytes,
} from "./reference-script-sweep.select-retired-reference-script-utxos.js";
import { lovelaceOf } from "./wallet-hygiene.js";

const estimateTxBytes = ({
  inputCount,
  quarantineOutputs,
  burnedUnits,
  disposition,
}: {
  readonly inputCount: number;
  readonly quarantineOutputs: readonly Readonly<Assets>[];
  readonly burnedUnits: readonly string[];
  readonly disposition: RetiredAuthPolicyDisposition;
}): number => {
  const outputBytes = quarantineOutputs.reduce(
    (total, output) =>
      total + TX_OUTPUT_OVERHEAD_BYTES + valueCborBytes(output),
    0,
  );
  const burnBytes =
    disposition.kind === "burn" && burnedUnits.length > 0
      ? TX_MINT_POLICY_BYTES +
        burnedUnits.reduce(
          (total, unit) =>
            total + (unit.length - 56) / 2 + TX_MINT_ASSET_OVERHEAD_BYTES,
          0,
        ) +
        disposition.policyScript.script.length / 2 +
        TX_NATIVE_WITNESS_OVERHEAD_BYTES
      : 0;
  return TX_BASE_BYTES + inputCount * TX_INPUT_BYTES + outputBytes + burnBytes;
};

export const negateAssets = (assets: Readonly<Assets>): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, -amount]),
  );

type BatchShape = Omit<ReferenceScriptSweepBatch, "batchIndex">;

const shapeBatch = ({
  inputs,
  disposition,
  quarantineAddress,
  limits,
  valueBytesPerOutput,
}: {
  readonly inputs: readonly UTxO[];
  readonly disposition: RetiredAuthPolicyDisposition;
  readonly quarantineAddress: string;
  readonly limits: ReferenceScriptSweepLimits;
  readonly valueBytesPerOutput: number;
}): BatchShape => {
  const tokens = sumTokens(inputs);
  const quarantineOutputs =
    disposition.kind === "burn"
      ? []
      : packQuarantineOutputs({
          tokens,
          quarantineAddress,
          valueBytesPerOutput,
          coinsPerUtxoByte: limits.coinsPerUtxoByte,
        });
  const burnedAssets = disposition.kind === "burn" ? tokens : {};
  const referenceScriptBytes = inputs.reduce(
    (total, utxo) =>
      total +
      (utxo.scriptRef === undefined || utxo.scriptRef === null
        ? 0
        : referenceScriptLedgerBytes(utxo.scriptRef)),
    0,
  );
  const estimatedTxBytes = estimateTxBytes({
    inputCount: inputs.length,
    quarantineOutputs,
    burnedUnits: sortedTokenUnits(inputs),
    disposition,
  });
  const inputLovelace = inputs.reduce(
    (total, utxo) => total + lovelaceOf(utxo),
    0n,
  );
  const estimatedFee = ledgerMinimumFee(
    limits,
    estimatedTxBytes,
    referenceScriptBytes,
  );
  const quarantineLovelace = quarantineOutputs.reduce(
    (total, output) => total + (output.lovelace ?? 0n),
    0n,
  );
  return {
    inputs,
    inputLovelace,
    referenceScriptBytes,
    estimatedTxBytes,
    estimatedFee,
    burnedAssets,
    quarantineOutputs,
    quarantineLovelace,
    netReclaimedLovelace: inputLovelace - estimatedFee - quarantineLovelace,
  };
};

export const buildReferenceScriptSweepPlan = ({
  utxos,
  referenceScriptsAddress,
  returnAddress,
  quarantineAddress,
  retiredAuthPolicyId,
  live,
  limits,
  disposition,
  maxReferenceScriptBytesPerBatch,
  maxInputsPerBatch = REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly quarantineAddress: string;
  readonly retiredAuthPolicyId: string;
  readonly live: LiveReferenceScriptDeployment;
  readonly limits: ReferenceScriptSweepLimits;
  readonly disposition: RetiredAuthPolicyDisposition;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
}): ReferenceScriptSweepPlan => {
  const retired = assertRetiredPolicyIsNotLive(retiredAuthPolicyId, live);
  const selected = selectRetiredReferenceScriptUtxos({
    utxos,
    referenceScriptsAddress,
    retiredAuthPolicyId: retired,
    live,
  });
  const protocolReferenceBudget = withMargin(
    limits.maxReferenceScriptBytesPerTx,
  );
  if (
    maxReferenceScriptBytesPerBatch !== undefined &&
    (!Number.isSafeInteger(maxReferenceScriptBytesPerBatch) ||
      maxReferenceScriptBytesPerBatch <= 0 ||
      maxReferenceScriptBytesPerBatch > protocolReferenceBudget)
  ) {
    throw new Error(
      `maxReferenceScriptBytesPerBatch must be a positive integer no larger than ${protocolReferenceBudget.toString()}`,
    );
  }
  if (!Number.isSafeInteger(maxInputsPerBatch) || maxInputsPerBatch <= 0) {
    throw new Error("maxInputsPerBatch must be a safe positive integer");
  }
  const budgets = {
    referenceScriptBytesPerBatch:
      maxReferenceScriptBytesPerBatch ?? protocolReferenceBudget,
    txBytesPerBatch: withMargin(limits.maxTxSize),
    valueBytesPerOutput: withMargin(limits.maxValueSize),
    inputsPerBatch: maxInputsPerBatch,
  };
  const shape = (inputs: readonly UTxO[]): BatchShape =>
    shapeBatch({
      inputs,
      disposition,
      quarantineAddress,
      limits,
      valueBytesPerOutput: budgets.valueBytesPerOutput,
    });
  const fits = (batch: BatchShape): boolean =>
    batch.inputs.length <= budgets.inputsPerBatch &&
    batch.referenceScriptBytes <= budgets.referenceScriptBytesPerBatch &&
    batch.estimatedTxBytes <= budgets.txBytesPerBatch;

  const shapes: BatchShape[] = [];
  let current: BatchShape | undefined;
  for (const utxo of selected) {
    const extended = shape([...(current?.inputs ?? []), utxo]);
    if (fits(extended)) {
      current = extended;
      continue;
    }
    const alone = shape([utxo]);
    if (!fits(alone)) {
      throw new ReferenceScriptSweepRefusal(
        "oversized-reference-script",
        `${outRefLabel(utxo)} alone exceeds the batch budget (reference_script_bytes=${alone.referenceScriptBytes.toString()},estimated_tx_bytes=${alone.estimatedTxBytes.toString()})`,
      );
    }
    if (current !== undefined) {
      shapes.push(current);
    }
    current = alone;
  }
  if (current !== undefined) {
    shapes.push(current);
  }
  const unfunded = shapes.filter((batch) => batch.netReclaimedLovelace <= 0n);
  if (unfunded.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "unfunded-batch",
      `batches cannot fund their fee and quarantine outputs: ${unfunded
        .map((batch) => batch.inputs.map(outRefLabel).join("+"))
        .join(",")}`,
    );
  }
  return {
    retiredAuthPolicyId: retired,
    referenceScriptsAddress,
    returnAddress,
    quarantineAddress,
    disposition,
    budgets,
    retainedUtxoCount: utxos.length - selected.length,
    batches: shapes.map((batch, batchIndex) => ({ batchIndex, ...batch })),
  };
};

export type ReferenceScriptSweepBatchSummary = {
  readonly batchIndex: number;
  readonly inputCount: number;
  readonly inputLovelace: bigint;
  readonly referenceScriptBytes: number;
  readonly estimatedTxBytes: number;
  readonly estimatedFee: bigint;
  readonly burnedAssetCount: number;
  readonly quarantineOutputCount: number;
  readonly quarantineLovelace: bigint;
  readonly netReclaimedLovelace: bigint;
  readonly inputOutRefs: readonly string[];
};

export type ReferenceScriptSweepPlanSummary = {
  readonly retiredAuthPolicyId: string;
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly quarantineAddress: string;
  readonly tokenDisposition: "burn" | "quarantine";
  readonly tokenDispositionReason: string;
  readonly budgets: ReferenceScriptSweepPlan["budgets"];
  readonly retainedUtxoCount: number;
  readonly batches: readonly ReferenceScriptSweepBatchSummary[];
  readonly totals: {
    readonly batchCount: number;
    readonly inputCount: number;
    readonly inputLovelace: bigint;
    readonly referenceScriptBytes: number;
    readonly estimatedFee: bigint;
    readonly burnedAssetCount: number;
    readonly quarantineOutputCount: number;
    readonly quarantineLovelace: bigint;
    readonly netReclaimedLovelace: bigint;
  };
};

const summarizeBatch = (
  batch: ReferenceScriptSweepBatch,
): ReferenceScriptSweepBatchSummary => ({
  batchIndex: batch.batchIndex,
  inputCount: batch.inputs.length,
  inputLovelace: batch.inputLovelace,
  referenceScriptBytes: batch.referenceScriptBytes,
  estimatedTxBytes: batch.estimatedTxBytes,
  estimatedFee: batch.estimatedFee,
  burnedAssetCount: Object.keys(batch.burnedAssets).length,
  quarantineOutputCount: batch.quarantineOutputs.length,
  quarantineLovelace: batch.quarantineLovelace,
  netReclaimedLovelace: batch.netReclaimedLovelace,
  inputOutRefs: batch.inputs.map(outRefLabel),
});

export const summarizeReferenceScriptSweepPlan = (
  plan: ReferenceScriptSweepPlan,
): ReferenceScriptSweepPlanSummary => {
  const batches = plan.batches.map(summarizeBatch);
  const sum = (pick: (batch: ReferenceScriptSweepBatchSummary) => bigint) =>
    batches.reduce((total, batch) => total + pick(batch), 0n);
  const count = (pick: (batch: ReferenceScriptSweepBatchSummary) => number) =>
    batches.reduce((total, batch) => total + pick(batch), 0);
  return {
    retiredAuthPolicyId: plan.retiredAuthPolicyId,
    referenceScriptsAddress: plan.referenceScriptsAddress,
    returnAddress: plan.returnAddress,
    quarantineAddress: plan.quarantineAddress,
    tokenDisposition: plan.disposition.kind,
    tokenDispositionReason: plan.disposition.reason,
    budgets: plan.budgets,
    retainedUtxoCount: plan.retainedUtxoCount,
    batches,
    totals: {
      batchCount: batches.length,
      inputCount: count((batch) => batch.inputCount),
      inputLovelace: sum((batch) => batch.inputLovelace),
      referenceScriptBytes: count((batch) => batch.referenceScriptBytes),
      estimatedFee: sum((batch) => batch.estimatedFee),
      burnedAssetCount: count((batch) => batch.burnedAssetCount),
      quarantineOutputCount: count((batch) => batch.quarantineOutputCount),
      quarantineLovelace: sum((batch) => batch.quarantineLovelace),
      netReclaimedLovelace: sum((batch) => batch.netReclaimedLovelace),
    },
  };
};
