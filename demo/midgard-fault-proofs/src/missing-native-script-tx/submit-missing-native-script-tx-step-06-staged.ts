import {
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1,
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
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import { prepareMissingNativeScriptTxStagedFieldOpeningV1 } from "./staged-field-opening-v1.js";
import {
  advanceMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxGrammarCheckpointV1,
  hashMissingNativeScriptTxGrammarCheckpointV1,
  initialMissingNativeScriptTxGrammarCheckpointV1,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
  missingNativeScriptTxGrammarCheckpointIsCompleteV1,
} from "./staged-walk-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_INDEX = 5 as const;
const STEP_LABEL = missingNativeScriptTxStepLabelV1(STEP_INDEX);

export type SubmitMissingNativeScriptTxStep06StartGrammarResultV1 = Readonly<{
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
export const submitMissingNativeScriptTxStep06StartGrammarV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  itemBudget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContractsV1;
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
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep06StartGrammarResultV1> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: STEP_INDEX,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep06State =
    requireMissingNativeScriptTxStepStateV1({
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
    scriptTxWitsItems.length <= MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1
  ) {
    throw missingNativeScriptTxSubmitError(
      `step-06 staged grammar certification requires more than ${MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1.toString()} witnesses.`,
    );
  }
  const initial = initialMissingNativeScriptTxGrammarCheckpointV1({
    txId: state.bad_tx_id,
    items: scriptTxWitsItems,
  });
  const next = advanceMissingNativeScriptTxGrammarCheckpointV1({
    checkpoint: initial,
    items: scriptTxWitsItems,
    budget: itemBudget,
  });
  if (missingNativeScriptTxGrammarCheckpointIsCompleteV1(next)) {
    throw missingNativeScriptTxSubmitError(
      "step-06 grammar start must leave a non-terminal checkpoint for step-07.",
    );
  }
  const checkpointBytes =
    encodeMissingNativeScriptTxGrammarCheckpointV1(next).toString("hex");
  const checkpointHash = hashMissingNativeScriptTxGrammarCheckpointV1(next);
  const prepared = await prepareMissingNativeScriptTxStagedFieldOpeningV1({
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
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
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
