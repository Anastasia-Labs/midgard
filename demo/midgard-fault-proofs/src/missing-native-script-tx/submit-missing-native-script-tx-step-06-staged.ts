import {
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep06SpendRedeemer,
  type MissingNativeScriptTxStep06State,
  MissingNativeScriptTxStep07Datum,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContracts } from "./contracts-v1.js";
import { prepareMissingNativeScriptTxStagedFieldOpening } from "./staged-field-opening-v1.js";
import {
  advanceMissingNativeScriptTxGrammarCheckpoint,
  encodeMissingNativeScriptTxGrammarCheckpoint,
  hashMissingNativeScriptTxGrammarCheckpoint,
  initialMissingNativeScriptTxGrammarCheckpoint,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
  missingNativeScriptTxGrammarCheckpointIsComplete,
} from "./staged-walk-v1.js";
import {
  missingNativeScriptTxStepLabel,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxStepState,
  requireMissingNativeScriptTxThreadUtxo,
} from "./submit-common-v1.js";

const STEP_INDEX = 5 as const;
const STEP_LABEL = missingNativeScriptTxStepLabel(STEP_INDEX);

export type SubmitMissingNativeScriptTxStep06StartGrammarResult = Readonly<{
  txHash: string;
  nextThreadOutRef: string;
  checkpointBytes: string;
  checkpointHash: string;
  carriageTier: string;
  inputIndex: number;
  outputIndex: number;
  awaitedConfirmation: boolean;
}>;

/** Starts the >64-witness grammar certification route at step 06. */
export const submitMissingNativeScriptTxStep06StartGrammar = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  itemBudget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly itemBudget?: number;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep06StartGrammarResult> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: STEP_INDEX,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep06State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep06Datum,
      stepIndex: STEP_INDEX,
    });
  if (state.phase !== "Ready") {
    throw missingNativeScriptTxSubmitError(
      "step-06 staged grammar certification requires the Ready phase.",
    );
  }
  if (
    scriptTxWitsItems.length <= MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT
  ) {
    throw missingNativeScriptTxSubmitError(
      `step-06 staged grammar certification requires more than ${MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT.toString()} witnesses.`,
    );
  }
  const initial = initialMissingNativeScriptTxGrammarCheckpoint({
    txId: state.bad_tx_id,
    items: scriptTxWitsItems,
  });
  const next = advanceMissingNativeScriptTxGrammarCheckpoint({
    checkpoint: initial,
    items: scriptTxWitsItems,
    budget: itemBudget,
  });
  if (missingNativeScriptTxGrammarCheckpointIsComplete(next)) {
    throw missingNativeScriptTxSubmitError(
      "step-06 grammar start must leave a non-terminal checkpoint for step-07.",
    );
  }
  const checkpointBytes =
    encodeMissingNativeScriptTxGrammarCheckpoint(next).toString("hex");
  const checkpointHash = hashMissingNativeScriptTxGrammarCheckpoint(next);
  const prepared = await prepareMissingNativeScriptTxStagedFieldOpening({
    lucid,
    contracts,
    signer,
    stepIndex: STEP_INDEX,
    nativeTxCompactCbor,
    witnessSet,
    scriptTxWitsItems,
    badTxId: state.bad_tx_id,
    badTxWitnessSetHash: state.bad_tx_witness_set_hash,
    publishCarriage,
    ...(publishedCarriageUtxos === undefined ? {} : { publishedCarriageUtxos }),
    ...(certificateUtxo === undefined ? {} : { certificateUtxo }),
    referenceScriptUtxo,
    ...(publicationPreSubmitBoundary === undefined
      ? {}
      : { publicationPreSubmitBoundary }),
    label: `${STEP_LABEL} staged script witnesses`,
  });
  const feeInput = selectFeeInput(prepared.usableWalletUtxos);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        ...state,
        phase: { GrammarCertification: { checkpoint_hash: checkpointHash } },
      },
    },
    MissingNativeScriptTxStep07Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[6].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} grammar output`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            StartGrammarCertification: {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              script_tx_wits_opening: prepared.opening,
              item_budget: BigInt(itemBudget),
            },
          },
        ],
      },
      MissingNativeScriptTxStep06SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...prepared.referenceInputs])
    .pay.ToContract(
      contracts.steps[6].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({
      localUPLCEval: true,
      presetWalletInputs: prepared.usableWalletUtxos as UTxO[],
    });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      "BuildTxWithRedeemer did not resolve step-06 staged layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-06",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[5].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-06 staged provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    checkpointBytes,
    checkpointHash,
    carriageTier: prepared.planned.plan.tier,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
