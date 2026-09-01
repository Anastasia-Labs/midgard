import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MissingNativeScriptTxStep08Datum,
  MissingNativeScriptTxStep08SpendRedeemer,
  type MissingNativeScriptTxStep08State,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import { prepareMissingNativeScriptTxStagedFieldOpeningV1 } from "./staged-field-opening-v1.js";
import {
  advanceMissingNativeScriptTxSemanticCheckpointV1,
  decodeMissingNativeScriptTxSemanticCheckpointV1,
  encodeMissingNativeScriptTxSemanticCheckpointV1,
  hashMissingNativeScriptTxSemanticCheckpointV1,
  MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
  missingNativeScriptTxRequiredScriptPresentThroughV1,
  missingNativeScriptTxSemanticCheckpointIsCompleteV1,
  resolveMissingNativeScriptTxSemanticCheckpointV1,
} from "./staged-walk-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_INDEX = 7 as const;
const STEP_LABEL = missingNativeScriptTxStepLabelV1(STEP_INDEX);

export type SubmitMissingNativeScriptTxStep08ResultV1 = Readonly<{
  txHash: string;
  action: "ResumeSemanticScan" | "FinalizeSemanticScan";
  nextThreadOutRef?: string;
  checkpointBytes: string;
  checkpointHash: string;
  requiredScriptIsPresent: boolean;
  fraudProofOutRef?: string;
  fraudProofUnit?: string;
  carriageTier: string;
  inputIndex: number;
  outputIndex: number;
  awaitedConfirmation: boolean;
}>;

/** Resumes step 08 or finalizes exactly when the bounded fold reaches terminal. */
export const submitMissingNativeScriptTxStep08V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  semanticCheckpointBytes,
  itemBudget = MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT_V1,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
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
  readonly semanticCheckpointBytes?: Uint8Array;
  readonly itemBudget?: number;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep08ResultV1> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: STEP_INDEX,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep08State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep08Datum,
      stepIndex: STEP_INDEX,
    });
  if (
    typeof state.phase !== "object" ||
    state.phase === null ||
    !("SemanticScan" in state.phase)
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-08 requires the SemanticScan phase.",
    );
  }
  const semantic =
    semanticCheckpointBytes === undefined
      ? resolveMissingNativeScriptTxSemanticCheckpointV1({
          txId: state.bad_tx_id,
          items: scriptTxWitsItems,
          committedHash: state.phase.SemanticScan.checkpoint_hash,
          budget: itemBudget,
        })
      : decodeMissingNativeScriptTxSemanticCheckpointV1(
          semanticCheckpointBytes,
        );
  if (
    hashMissingNativeScriptTxSemanticCheckpointV1(semantic) !==
    state.phase.SemanticScan.checkpoint_hash
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-08 semantic checkpoint bytes do not match the on-chain checkpoint hash.",
    );
  }
  const derivedCurrentFound =
    missingNativeScriptTxRequiredScriptPresentThroughV1({
      expectedScriptHash: state.expected_missing_script_hash,
      items: scriptTxWitsItems,
      nextItemIndex: semantic.nextItemIndex,
    });
  if (
    derivedCurrentFound !== state.phase.SemanticScan.required_script_is_present
  ) {
    throw missingNativeScriptTxSubmitError(
      "step-08 on-chain semantic accumulator does not match the exact authenticated witness prefix.",
    );
  }
  if (missingNativeScriptTxSemanticCheckpointIsCompleteV1(semantic)) {
    throw missingNativeScriptTxSubmitError(
      "step-08 cannot resume an already terminal semantic checkpoint.",
    );
  }
  const next = advanceMissingNativeScriptTxSemanticCheckpointV1({
    checkpoint: semantic,
    txId: state.bad_tx_id,
    items: scriptTxWitsItems,
    budget: itemBudget,
  });
  const finalizes = missingNativeScriptTxSemanticCheckpointIsCompleteV1(next);
  const nextFound = missingNativeScriptTxRequiredScriptPresentThroughV1({
    expectedScriptHash: state.expected_missing_script_hash,
    items: scriptTxWitsItems,
    nextItemIndex: next.nextItemIndex,
  });
  if (finalizes && nextFound) {
    throw missingNativeScriptTxSubmitError(
      "the accused native script is present in the authenticated field-6 preimage.",
    );
  }
  const checkpointBytes =
    encodeMissingNativeScriptTxSemanticCheckpointV1(next).toString("hex");
  const checkpointHash = hashMissingNativeScriptTxSemanticCheckpointV1(next);

  const computationThreadBurnCarriage = finalizes
    ? witnessMintingPolicyCarriageV1({
        script: contracts.computationThread.mintingScript,
        referenceUtxo: witnessReferenceScripts?.computationThreadMint,
        label: `${STEP_LABEL} computation-thread burn`,
      })
    : undefined;
  const fraudProofMintCarriage = finalizes
    ? witnessMintingPolicyCarriageV1({
        script: contracts.fraudProof.mintingScript,
        referenceUtxo: witnessReferenceScripts?.fraudProofMint,
        label: `${STEP_LABEL} fraud-proof mint`,
      })
    : undefined;
  const mintReferences = [
    ...(computationThreadBurnCarriage?.referenceInputs ?? []),
    ...(fraudProofMintCarriage?.referenceInputs ?? []),
  ];
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
    extraReferenceInputs: [...mintReferences],
    ...(publicationPreSubmitBoundary === undefined
      ? {}
      : { publicationPreSubmitBoundary }),
    label: `${STEP_LABEL} staged script witnesses`,
  });
  const feeInput = selectFeeInput(prepared.usableWalletUtxos);
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const nextDatum = finalizes
    ? undefined
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            ...state,
            phase: {
              SemanticScan: {
                checkpoint_hash: checkpointHash,
                required_script_is_present: nextFound,
              },
            },
          },
        },
        MissingNativeScriptTxStep08Datum,
      );
  const outputMatches = finalizes
    ? outputWithDatumAndUnitPredicate({
        address: contracts.fraudProof.spendingScriptAddress,
        datum: fraudProofDatum,
        unit: fraudProofUnit,
      })
    : computationThreadOutputPredicate({
        address: contracts.steps[7].spendingScriptAddress,
        datum: nextDatum!,
        unit: threadToken.unit,
      });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex?: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
      ...(finalizes
        ? {
            fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              `${STEP_LABEL} fraud-proof mint`,
            ),
          }
        : {}),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          finalizes
            ? {
                FinalizeSemanticScan: {
                  input_index: resolved.inputIndex,
                  output_index: resolved.outputIndex,
                  fraud_proof_mint_redeemer_index:
                    resolved.fraudProofMintRedeemerIndex!,
                  script_tx_wits_opening: prepared.opening,
                  checkpoint_bytes: Buffer.from(
                    encodeMissingNativeScriptTxSemanticCheckpointV1(semantic),
                  ).toString("hex"),
                  item_budget: BigInt(itemBudget),
                },
              }
            : {
                ResumeSemanticScan: {
                  input_index: resolved.inputIndex,
                  output_index: resolved.outputIndex,
                  script_tx_wits_opening: prepared.opening,
                  checkpoint_bytes: Buffer.from(
                    encodeMissingNativeScriptTxSemanticCheckpointV1(semantic),
                  ).toString("hex"),
                  item_budget: BigInt(itemBudget),
                },
              },
        ],
      },
      MissingNativeScriptTxStep08SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  let transaction = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer);
  if (finalizes) {
    transaction = transaction
      .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
      .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer);
  }
  transaction = transaction.readFrom([...prepared.referenceInputs]);
  transaction = finalizes
    ? transaction.pay.ToContract(
        contracts.fraudProof.spendingScriptAddress,
        { kind: "inline", value: fraudProofDatum },
        {
          lovelace: threadUtxo.assets.lovelace ?? 0n,
          [fraudProofUnit]: 1n,
        },
      )
    : transaction.pay.ToContract(
        contracts.steps[7].spendingScriptAddress,
        { kind: "inline", value: nextDatum! },
        {
          lovelace: threadUtxo.assets.lovelace ?? 0n,
          [threadToken.unit]: 1n,
        },
      );
  transaction = transaction.addSignerKey(signer.paymentKeyHash);
  const completed = finalizes
    ? fraudProofMintCarriage!.attach(
        computationThreadBurnCarriage!.attach(transaction),
      )
    : transaction;
  const unsigned = await completed.complete({
    localUPLCEval: true,
    presetWalletInputs: prepared.usableWalletUtxos as UTxO[],
  });
  if (
    layout === undefined ||
    (finalizes && computationThreadMintRedeemerIndex === undefined)
  ) {
    throw missingNativeScriptTxSubmitError(
      "BuildTxWithRedeemer did not resolve step-08 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-08",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[7].spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
        ...(finalizes ? [] : []),
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-08 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    action: finalizes ? "FinalizeSemanticScan" : "ResumeSemanticScan",
    ...(finalizes
      ? {
          fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
          fraudProofUnit,
        }
      : {
          nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
        }),
    checkpointBytes,
    checkpointHash,
    requiredScriptIsPresent: nextFound,
    carriageTier: prepared.planned.plan.tier,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
