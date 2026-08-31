import {
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep07SpendRedeemer,
  type MissingNativeScriptTxStep07State,
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
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  decodeMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxGrammarCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  hashMissingNativeScriptTxGrammarCheckpointV1,
  hashMissingNativeScriptTxSemanticCheckpointV1,
  initialMissingNativeScriptTxSemanticCheckpointV1,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
  missingNativeScriptTxGrammarCheckpointIsCompleteV1,
  missingNativeScriptTxRequiredScriptPresentThroughV1,
  missingNativeScriptTxSemanticCheckpointIsCompleteV1,
  resolveMissingNativeScriptTxGrammarCheckpointV1,
} from "./staged-walk-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_INDEX = 6 as const;
const STEP_LABEL = missingNativeScriptTxStepLabelV1(STEP_INDEX);

export type SubmitMissingNativeScriptTxStep07ResultV1 = Readonly<{
  txHash: string;
  action: "ResumeGrammarCertification" | "StartSemanticScan";
  nextThreadOutRef: string;
  checkpointBytes: string;
  checkpointHash: string;
  requiredScriptIsPresent?: boolean;
  carriageTier: string;
  inputIndex: number;
  outputIndex: number;
  awaitedConfirmation: boolean;
}>;

/** Resumes grammar, or crosses to semantic scanning at a terminal checkpoint. */
export const submitMissingNativeScriptTxStep07V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  grammarCheckpointBytes,
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
  /** Optional restart hint; omitted bytes are reconstructed from the thread hash. */
  readonly grammarCheckpointBytes?: Uint8Array;
  readonly itemBudget?: number;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep07ResultV1> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: STEP_INDEX,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep07State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep07Datum,
      stepIndex: STEP_INDEX,
    });
  if (
    typeof state.phase !== "object" ||
    state.phase === null ||
    !("GrammarCertification" in state.phase)
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-07 requires the GrammarCertification phase.",
    );
  }
  const grammar =
    grammarCheckpointBytes === undefined
      ? resolveMissingNativeScriptTxGrammarCheckpointV1({
          txId: state.bad_tx_id,
          items: scriptTxWitsItems,
          committedHash: state.phase.GrammarCertification.checkpoint_hash,
          budget: itemBudget,
        })
      : decodeMissingNativeScriptTxGrammarCheckpointV1(grammarCheckpointBytes);
  const suppliedGrammarHash =
    hashMissingNativeScriptTxGrammarCheckpointV1(grammar);
  if (
    suppliedGrammarHash !== state.phase.GrammarCertification.checkpoint_hash
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-07 grammar checkpoint bytes do not match the on-chain checkpoint hash.",
    );
  }

  const startsSemantic =
    missingNativeScriptTxGrammarCheckpointIsCompleteV1(grammar);
  const nextGrammar = startsSemantic
    ? undefined
    : advanceMissingNativeScriptTxGrammarCheckpointV1({
        checkpoint: grammar,
        items: scriptTxWitsItems,
        budget: itemBudget,
      });
  const initialSemantic = startsSemantic
    ? initialMissingNativeScriptTxSemanticCheckpointV1({
        grammar,
        items: scriptTxWitsItems,
      })
    : undefined;
  const nextSemantic =
    initialSemantic === undefined
      ? undefined
      : advanceMissingNativeScriptTxSemanticCheckpointV1({
          checkpoint: initialSemantic,
          txId: state.bad_tx_id,
          items: scriptTxWitsItems,
          budget: itemBudget,
        });
  if (
    nextSemantic !== undefined &&
    missingNativeScriptTxSemanticCheckpointIsCompleteV1(nextSemantic)
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-07 semantic start must leave a non-terminal checkpoint for step-08.",
    );
  }
  const requiredScriptIsPresent =
    nextSemantic === undefined
      ? undefined
      : missingNativeScriptTxRequiredScriptPresentThroughV1({
          expectedScriptHash: state.expected_missing_script_hash,
          items: scriptTxWitsItems,
          nextItemIndex: nextSemantic.nextItemIndex,
        });
  const nextCheckpointBytes =
    nextSemantic === undefined
      ? encodeMissingNativeScriptTxGrammarCheckpointV1(nextGrammar!)
      : encodeMissingNativeScriptTxSemanticCheckpointV1(nextSemantic);
  const nextCheckpointHash =
    nextSemantic === undefined
      ? hashMissingNativeScriptTxGrammarCheckpointV1(nextGrammar!)
      : hashMissingNativeScriptTxSemanticCheckpointV1(nextSemantic);
  const nextStepIndex = startsSemantic ? (7 as const) : STEP_INDEX;
  const nextState: MissingNativeScriptTxStep07State = {
    ...state,
    phase:
      nextSemantic === undefined
        ? { GrammarCertification: { checkpoint_hash: nextCheckpointHash } }
        : {
            SemanticScan: {
              checkpoint_hash: nextCheckpointHash,
              required_script_is_present: requiredScriptIsPresent!,
            },
          },
  };
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
    { fraud_prover: signer.paymentKeyHash, data: nextState },
    MissingNativeScriptTxStep07Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
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
        `${STEP_LABEL} staged output`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          startsSemantic
            ? {
                StartSemanticScan: {
                  input_index: resolved.inputIndex,
                  output_index: resolved.outputIndex,
                  script_tx_wits_opening: prepared.opening,
                  grammar_checkpoint_bytes: Buffer.from(
                    encodeMissingNativeScriptTxGrammarCheckpointV1(grammar),
                  ).toString("hex"),
                  item_budget: BigInt(itemBudget),
                },
              }
            : {
                ResumeGrammarCertification: {
                  input_index: resolved.inputIndex,
                  output_index: resolved.outputIndex,
                  script_tx_wits_opening: prepared.opening,
                  checkpoint_bytes: Buffer.from(
                    encodeMissingNativeScriptTxGrammarCheckpointV1(grammar),
                  ).toString("hex"),
                  item_budget: BigInt(itemBudget),
                },
              },
        ],
      },
      MissingNativeScriptTxStep07SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...prepared.referenceInputs])
    .pay.ToContract(
      contracts.steps[nextStepIndex].spendingScriptAddress,
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
      "BuildTxWithRedeemer did not resolve step-07 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-07",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[6].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-07 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    action: startsSemantic ? "StartSemanticScan" : "ResumeGrammarCertification",
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    checkpointBytes: nextCheckpointBytes.toString("hex"),
    checkpointHash: nextCheckpointHash,
    ...(requiredScriptIsPresent === undefined
      ? {}
      : { requiredScriptIsPresent }),
    carriageTier: prepared.planned.plan.tier,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
