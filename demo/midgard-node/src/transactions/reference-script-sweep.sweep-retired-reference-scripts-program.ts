import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  getAddressDetails,
  type LucidEvolution,
  type Script,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { outRefLabel } from "../tx-context.js";
import {
  buildReferenceScriptSweepPlan,
  negateAssets,
  type ReferenceScriptSweepPlanSummary,
  summarizeReferenceScriptSweepPlan,
} from "./reference-script-sweep.build-reference-script-sweep-plan.js";
import {
  assertRetiredPolicyIsNotLive,
  ledgerMinimumFee,
  type LiveReferenceScriptDeployment,
  type ReferenceScriptSweepBatch,
  type ReferenceScriptSweepLimits,
  type ReferenceScriptSweepPlan,
  SIGNED_TX_WITNESS_ALLOWANCE_BYTES,
} from "./reference-script-sweep.reference-script-fee.js";
import { decideRetiredAuthPolicyDisposition } from "./reference-script-sweep.select-retired-reference-script-utxos.js";
import {
  fetchReferenceScriptUtxosAt,
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
} from "./reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";

export type ReferenceScriptSweepOptions = {
  readonly retiredAuthPolicyId: string;
  readonly retiredAuthPolicyScript?: Script;
  readonly quarantineAddress?: string;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
  readonly execute: boolean;
  readonly acknowledgeRetirement: boolean;
};

export type ReferenceScriptSweepSubmittedBatch = {
  readonly txHash: string;
  readonly inputCount: number;
  readonly fee: bigint;
  readonly txBytes: number;
  readonly referenceScriptBytes: number;
  readonly tokenDisposition: "burn" | "quarantine";
};

export type ReferenceScriptSweepResult = {
  readonly dryRun: boolean;
  readonly plan: ReferenceScriptSweepPlanSummary;
  readonly submitted: readonly ReferenceScriptSweepSubmittedBatch[];
};

type ReferenceScriptSweepError =
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError;

const refusalToError = (cause: unknown): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message:
      cause instanceof Error
        ? cause.message
        : "Reference-script sweep planning failed",
    cause,
  });

const walletPaymentKeyHash = (address: string): string => {
  const { paymentCredential } = getAddressDetails(address);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Reference-script wallet must have a payment key");
  }
  return paymentCredential.hash;
};

const transactionInputOutRefs = (tx: CML.Transaction): readonly string[] => {
  const inputs = tx.body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();
};

const buildBatchTransaction = (
  lucid: LucidEvolution,
  plan: ReferenceScriptSweepPlan,
  batch: ReferenceScriptSweepBatch,
  limits: ReferenceScriptSweepLimits,
) =>
  Effect.gen(function* () {
    const unsigned = yield* Effect.tryPromise({
      try: () => {
        let tx = lucid.newTx().collectFrom([...batch.inputs]);
        if (plan.disposition.kind === "burn") {
          tx = tx
            .mintAssets(negateAssets(batch.burnedAssets))
            .attach.MintingPolicy(plan.disposition.policyScript)
            .validTo(lucid.slotToUnixTime(plan.disposition.validToSlot));
        }
        for (const output of batch.quarantineOutputs) {
          tx = tx.pay.ToAddress(plan.quarantineAddress, { ...output });
        }
        return tx.complete({
          coinSelection: false,
          localUPLCEval: true,
          changeAddress: plan.returnAddress,
          presetWalletInputs: [...batch.inputs],
        });
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build reference-script sweep batch: ${String(cause)}`,
          cause,
        }),
    });
    const tx = unsigned.toTransaction();
    const txBytes = unsigned.toCBOR().length / 2;
    const fee = tx.body().fee();
    const inputOutRefs = transactionInputOutRefs(tx);
    const expectedOutRefs = batch.inputs.map(outRefLabel).sort();
    const minimumFee = ledgerMinimumFee(
      limits,
      txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES,
      batch.referenceScriptBytes,
    );
    if (
      inputOutRefs.join(",") !== expectedOutRefs.join(",") ||
      txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES > limits.maxTxSize ||
      batch.referenceScriptBytes > limits.maxReferenceScriptBytesPerTx ||
      fee < minimumFee
    ) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Built reference-script sweep batch does not match its plan or the ledger limits",
          cause: `inputs=${inputOutRefs.length.toString()}/${expectedOutRefs.length.toString()},tx_bytes=${txBytes.toString()},max_tx_bytes=${limits.maxTxSize.toString()},reference_script_bytes=${batch.referenceScriptBytes.toString()},fee=${fee.toString()},ledger_minimum_fee=${minimumFee.toString()}`,
        }),
      );
    }
    return { unsigned, txBytes, fee };
  });

/**
 * Plans (dry run) or executes the retired reference-script sweep. Execution
 * submits one batch at a time, waits for its confirmation and re-plans from
 * chain before the next batch.
 */
export const sweepRetiredReferenceScriptsProgram = ({
  lucid,
  referenceScriptsAddress,
  live,
  limits,
  options,
}: {
  readonly lucid: LucidEvolution;
  readonly referenceScriptsAddress: string;
  readonly live: LiveReferenceScriptDeployment;
  readonly limits: ReferenceScriptSweepLimits;
  readonly options: ReferenceScriptSweepOptions;
}): Effect.Effect<ReferenceScriptSweepResult, ReferenceScriptSweepError> =>
  Effect.gen(function* () {
    const retiredAuthPolicyId = yield* Effect.try({
      try: () =>
        assertRetiredPolicyIsNotLive(options.retiredAuthPolicyId, live),
      catch: refusalToError,
    });
    const returnAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve the reference-script wallet address",
          cause,
        }),
    });
    const signerKeyHash = yield* Effect.try({
      try: () => walletPaymentKeyHash(returnAddress),
      catch: refusalToError,
    });
    const quarantineAddress = options.quarantineAddress ?? returnAddress;
    const spentOutRefs = new Set<string>();

    const planFromChain = Effect.gen(function* () {
      const utxos = yield* fetchReferenceScriptUtxosAt(
        lucid,
        referenceScriptsAddress,
        `reference-script sweep UTxO fetch at ${referenceScriptsAddress}`,
        `Failed to fetch reference-script sweep UTxOs at ${referenceScriptsAddress}`,
      );
      return yield* Effect.try({
        try: () =>
          buildReferenceScriptSweepPlan({
            utxos: utxos.filter((utxo) => !spentOutRefs.has(outRefLabel(utxo))),
            referenceScriptsAddress,
            returnAddress,
            quarantineAddress,
            retiredAuthPolicyId,
            live,
            limits,
            disposition: decideRetiredAuthPolicyDisposition({
              retiredAuthPolicyId,
              policyScript: options.retiredAuthPolicyScript,
              signerKeyHash,
              currentSlot: lucid.currentSlot(),
            }),
            maxReferenceScriptBytesPerBatch:
              options.maxReferenceScriptBytesPerBatch,
            maxInputsPerBatch: options.maxInputsPerBatch,
          }),
        catch: refusalToError,
      });
    });

    const initialPlan = yield* planFromChain;
    const plan = summarizeReferenceScriptSweepPlan(initialPlan);
    if (!options.execute || initialPlan.batches.length === 0) {
      return { dryRun: !options.execute, plan, submitted: [] };
    }
    if (!options.acknowledgeRetirement) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Refusing to execute reference-script sweep without retirement acknowledgement",
          cause:
            "Pass --i-am-retiring-reference-scripts to confirm the retired auth policy's reference scripts are no longer live.",
        }),
      );
    }

    const submitted: ReferenceScriptSweepSubmittedBatch[] = [];
    // Each confirmed batch removes at least one selected input, so the
    // initial input count bounds the number of rounds; the extra round is the
    // final re-plan that finds nothing left.
    const maxRounds = plan.totals.inputCount + 1;
    for (let round = 0; round < maxRounds; round += 1) {
      // A pinned wallet view from the previous batch would hide fresh inputs
      // from the signer.
      lucid.clearUTxOOverride();
      const current = round === 0 ? initialPlan : yield* planFromChain;
      const batch = current.batches[0];
      if (batch === undefined) {
        break;
      }
      const built = yield* buildBatchTransaction(lucid, current, batch, limits);
      yield* Effect.logInfo(
        `Submitting reference-script sweep batch ${(submitted.length + 1).toString()}: inputs=${batch.inputs.length.toString()},fee=${built.fee.toString()},tx_bytes=${built.txBytes.toString()},reference_script_bytes=${batch.referenceScriptBytes.toString()},token_disposition=${current.disposition.kind}`,
      );
      const txHash = yield* handleSignSubmit(lucid, built.unsigned, {
        confirmationTimeoutMs: REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
        confirmationRetries: 0,
      });
      for (const input of batch.inputs) {
        spentOutRefs.add(outRefLabel(input));
      }
      submitted.push({
        txHash,
        inputCount: batch.inputs.length,
        fee: built.fee,
        txBytes: built.txBytes,
        referenceScriptBytes: batch.referenceScriptBytes,
        tokenDisposition: current.disposition.kind,
      });
    }
    return { dryRun: false, plan, submitted };
  });
