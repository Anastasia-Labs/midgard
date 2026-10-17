/**
 * `strip-operator-wallet-script-refs`: spends every operator-wallet UTxO that
 * carries a reference script back to the operator address in outputs without
 * one. Lovelace and tokens stay in the operator wallet (no burn, no other
 * address); only the fee leaves. Batches follow the ledger's transaction-size,
 * value-size and reference-script byte limits like the reference-script
 * sweep, and are submitted and confirmed one at a time, each re-planned from
 * the wallet view. The fee comes from the batch's own inputs, with coin
 * selection over the operator wallet view adding another of its UTxOs only
 * when they do not cover it. The reference-script wallet is never touched,
 * and the deployment's reserved initialization nonces are left out of the
 * view before planning, so neither a strip input nor a fee input is one.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { IntentJournal } from "../services/intent-journal.js";
import { journaledIntent, openPlan } from "../services/intent-journal.js";
import type { WalletViewUnavailable } from "../services/intent-journal.wallet-view.js";
import { outRefLabel } from "../tx-context.js";
import {
  buildOperatorWalletScriptRefStripPlan,
  type OperatorWalletScriptRefStripBatch,
  type OperatorWalletScriptRefStripPlanSummary,
  scriptRefLedgerBytes,
  summarizeOperatorWalletScriptRefStripPlan,
} from "./operator-wallet-script-ref-strip.plan.js";
import {
  ledgerMinimumFee,
  type ReferenceScriptSweepLimits,
  SIGNED_TX_WITNESS_ALLOWANCE_BYTES,
} from "./reference-script-sweep.reference-script-fee.js";
import {
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  utxoOutRefKey,
} from "./reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";
import { readSelectedWalletView } from "./utils.wallet-view.js";

export type OperatorWalletScriptRefStripOptions = {
  readonly execute: boolean;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
};

export type OperatorWalletScriptRefStripSubmittedBatch = {
  readonly txHash: string;
  readonly inputCount: number;
  /** Operator-wallet UTxOs coin selection added to pay the fee. */
  readonly feeInputCount: number;
  readonly fee: bigint;
  readonly txBytes: number;
  readonly referenceScriptBytes: number;
};

export type OperatorWalletScriptRefStripResult = {
  readonly dryRun: boolean;
  readonly plan: OperatorWalletScriptRefStripPlanSummary;
  readonly submitted: readonly OperatorWalletScriptRefStripSubmittedBatch[];
  readonly message: string;
};

type OperatorWalletScriptRefStripError =
  | SDK.StateQueueError
  | SDK.LucidError
  | WalletViewUnavailable
  | TxConfirmError
  | TxSignError
  | TxSubmitError;

const planningError = (cause: unknown): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message:
      cause instanceof Error
        ? cause.message
        : "Operator-wallet script-ref strip planning failed",
    cause,
  });

const paymentKeyHashOf = (address: string): string | undefined =>
  getAddressDetails(address).paymentCredential?.hash;

const transactionInputOutRefs = (tx: CML.Transaction): readonly string[] => {
  const inputs = tx.body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();
};

/** Why a built batch is refused, or undefined when it keeps to its plan. */
const builtBatchMismatch = ({
  tx,
  txBytes,
  batch,
  spentUtxos,
  operatorAddress,
  limits,
}: {
  readonly tx: CML.Transaction;
  readonly txBytes: number;
  readonly batch: OperatorWalletScriptRefStripBatch;
  readonly spentUtxos: readonly UTxO[] | undefined;
  readonly operatorAddress: string;
  readonly limits: ReferenceScriptSweepLimits;
}): string | undefined => {
  if (spentUtxos === undefined) return "spends an output outside the view";
  const spent = new Set(spentUtxos.map(outRefLabel));
  if (batch.inputs.some((input) => !spent.has(outRefLabel(input))))
    return "leaves a planned input unspent";
  if (tx.body().mint() !== undefined) return "mints or burns";
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    if (output.script_ref() !== undefined)
      return "creates an output with a reference script";
    if (output.address().to_bech32(undefined) !== operatorAddress)
      return "pays outside the operator wallet";
  }
  const referenceScriptBytes = scriptRefLedgerBytes(spentUtxos);
  const fee = tx.body().fee();
  const minimumFee = ledgerMinimumFee(
    limits,
    txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES,
    referenceScriptBytes,
  );
  if (
    txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES > limits.maxTxSize ||
    referenceScriptBytes > limits.maxReferenceScriptBytesPerTx ||
    fee < minimumFee
  )
    return `exceeds a ledger limit (tx_bytes=${txBytes.toString()},reference_script_bytes=${referenceScriptBytes.toString()},fee=${fee.toString()},ledger_minimum_fee=${minimumFee.toString()})`;
  return undefined;
};

const buildBatchTransaction = (
  lucid: LucidEvolution,
  operatorAddress: string,
  viewUtxos: readonly UTxO[],
  batch: OperatorWalletScriptRefStripBatch,
  limits: ReferenceScriptSweepLimits,
) =>
  Effect.gen(function* () {
    const unsigned = yield* Effect.tryPromise({
      try: () => {
        let tx = lucid.newTx().collectFrom([...batch.inputs]);
        for (const output of batch.tokenOutputs)
          tx = tx.pay.ToAddress(operatorAddress, { ...output });
        // The rest of the lovelace returns as change. Coin selection draws on
        // the operator wallet view only when the batch cannot pay its fee.
        return tx.complete({
          coinSelection: true,
          localUPLCEval: true,
          changeAddress: operatorAddress,
          presetWalletInputs: [...viewUtxos],
        });
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build operator-wallet script-ref strip batch: ${String(cause)}`,
          cause,
        }),
    });
    const tx = unsigned.toTransaction();
    const txBytes = unsigned.toCBOR().length / 2;
    const byOutRef = new Map(
      viewUtxos.map((utxo) => [outRefLabel(utxo), utxo]),
    );
    const spentOutRefs = transactionInputOutRefs(tx);
    const spentUtxos = spentOutRefs.every((outRef) => byOutRef.has(outRef))
      ? spentOutRefs.map((outRef) => byOutRef.get(outRef)!)
      : undefined;
    const mismatch = builtBatchMismatch({
      tx,
      txBytes,
      batch,
      spentUtxos,
      operatorAddress,
      limits,
    });
    if (mismatch !== undefined || spentUtxos === undefined)
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Built operator-wallet script-ref strip batch does not match its plan or the ledger limits",
          cause: mismatch,
        }),
      );
    return {
      unsigned,
      txBytes,
      fee: tx.body().fee(),
      spentUtxos,
      referenceScriptBytes: scriptRefLedgerBytes(spentUtxos),
    };
  });

/**
 * Plans (dry run, the default) or executes the strip. `lucid` has the
 * operator wallet selected; the reference-script wallet is refused.
 * `reservedOutRefKeys` are the deployment's initialization nonces
 * (`deploymentInitializationNonceOutRefKeys`).
 */
export const stripOperatorWalletScriptRefsProgram = ({
  lucid,
  referenceScriptsWalletAddress,
  reservedOutRefKeys,
  limits,
  options,
}: {
  readonly lucid: LucidEvolution;
  readonly referenceScriptsWalletAddress: string;
  readonly reservedOutRefKeys: ReadonlySet<string>;
  readonly limits: ReferenceScriptSweepLimits;
  readonly options: OperatorWalletScriptRefStripOptions;
}): Effect.Effect<
  OperatorWalletScriptRefStripResult,
  OperatorWalletScriptRefStripError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const spentOutRefs = new Set<string>();

    // S5: each round's plan opens before the view its batch is built on.
    const planFromView = Effect.gen(function* () {
      const intentPlan = yield* openPlan;
      const view = yield* readSelectedWalletView(lucid);
      if (
        view.address === referenceScriptsWalletAddress ||
        paymentKeyHashOf(view.address) ===
          paymentKeyHashOf(referenceScriptsWalletAddress)
      )
        return yield* Effect.fail(
          planningError(
            new Error(
              "The selected wallet is the reference-script wallet; the strip spends only the operator wallet",
            ),
          ),
        );
      const utxos = view.utxos.filter(
        (utxo) =>
          !spentOutRefs.has(outRefLabel(utxo)) &&
          !reservedOutRefKeys.has(utxoOutRefKey(utxo)),
      );
      const plan = yield* Effect.try({
        try: () =>
          buildOperatorWalletScriptRefStripPlan({
            utxos,
            operatorAddress: view.address,
            limits,
            maxReferenceScriptBytesPerBatch:
              options.maxReferenceScriptBytesPerBatch,
            maxInputsPerBatch: options.maxInputsPerBatch,
          }),
        catch: planningError,
      });
      return { plan, utxos, intentPlan };
    });

    const initial = yield* planFromView;
    const summary = summarizeOperatorWalletScriptRefStripPlan(initial.plan);
    if (initial.plan.batches.length === 0)
      return {
        dryRun: !options.execute,
        plan: summary,
        submitted: [],
        message: `No operator-wallet UTxO at ${initial.plan.operatorAddress} carries a reference script; nothing to strip`,
      };
    if (!options.execute)
      return {
        dryRun: true,
        plan: summary,
        submitted: [],
        message: `Dry run: ${summary.totals.inputCount.toString()} UTxO(s) with a reference script in ${summary.totals.batchCount.toString()} batch(es); pass --execute to submit`,
      };

    const submitted: OperatorWalletScriptRefStripSubmittedBatch[] = [];
    // Each confirmed batch spends at least one selected input, so the initial
    // input count bounds the rounds; the extra round finds nothing left.
    const maxRounds = summary.totals.inputCount + 1;
    for (let round = 0; round < maxRounds; round += 1) {
      const { plan, utxos, intentPlan } =
        round === 0 ? initial : yield* planFromView;
      const batch = plan.batches[0];
      if (batch === undefined) break;
      const built = yield* buildBatchTransaction(
        lucid,
        plan.operatorAddress,
        utxos,
        batch,
        limits,
      );
      yield* Effect.logInfo(
        `Submitting operator-wallet script-ref strip batch ${(submitted.length + 1).toString()}: inputs=${built.spentUtxos.length.toString()},fee=${built.fee.toString()},tx_bytes=${built.txBytes.toString()},reference_script_bytes=${built.referenceScriptBytes.toString()}`,
      );
      const txHash = yield* handleSignSubmit(
        lucid,
        built.unsigned,
        journaledIntent(
          "reference_sweep",
          `operator_script_ref_strip:${outRefLabel(batch.inputs[0]!)}+${batch.inputs.length.toString()}`,
          intentPlan,
        ),
        {
          confirmationTimeoutMs: REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
          confirmationRetries: 0,
        },
      );
      for (const input of built.spentUtxos)
        spentOutRefs.add(outRefLabel(input));
      submitted.push({
        txHash,
        inputCount: batch.inputs.length,
        feeInputCount: built.spentUtxos.length - batch.inputs.length,
        fee: built.fee,
        txBytes: built.txBytes,
        referenceScriptBytes: built.referenceScriptBytes,
      });
    }
    return {
      dryRun: false,
      plan: summary,
      submitted,
      message: `Stripped ${submitted.reduce((total, batch) => total + batch.inputCount, 0).toString()} UTxO(s) with a reference script in ${submitted.length.toString()} transaction(s)`,
    };
  });
