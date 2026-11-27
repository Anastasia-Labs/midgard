import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  referenceInputNoIdxStep03StateFromBadInput,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type FaultProofTurn, isPrerequisiteTurn } from "./fault-proof-turn.js";
import {
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
} from "./field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening.publish-fault-proof-field-carriage.js";
import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "./runtime.js";
import { completeStep, finishStep, fundStep } from "./step-completion.js";
import { requireComputationThreadToken } from "./step-support.js";
import { submitFaultProofTurns } from "./submit-fault-proof-turns.js";
import {
  parseSubmitReferenceInputNoIdxReferenceInputsPreimage,
  type ReferenceInputNoIdxStep02Build,
  type ReferenceInputNoIdxStep02Layout,
  requireStep02Datum,
  type SubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxStep02CliConfig,
  type SubmitReferenceInputNoIdxStep02Result,
} from "./submit-reference-input-no-idx-step-02.parse-submit-reference-input-no-idx-reference-inputs-preimage.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriage } from "./witness-reference-scripts.js";
import type { ProofMaterialsAccess } from "./workflow/proof-materials-view.js";

export const buildReferenceInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  nativeTxCompactCbor,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputsPreimage: SubmitReferenceInputNoIdxReferenceInputsPreimage;
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<FaultProofTurn<ReferenceInputNoIdxStep02Build>> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.referenceInputNoIdx;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const verifiedTxId = inputDatum.data.verified_tx_id;

  // Re-run the door off-chain: these items must be the §5.1 preimage the
  // anchored transaction committed *at field 1*, and the compact bytes must
  // re-derive to the anchor the thread carries.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: verifiedTxId,
    nativeTxCompactCbor,
    itemCbors: referenceInputsPreimage.referenceInputsPreimage.map(
      encodeMidgardTxInputCanonical,
    ),
    owner: signer.paymentKeyHash,
    label: "Reference-input-no-idx step 02 reference-inputs",
  });
  const verifiedTxReferenceInputsHash = planned.commitment;

  // Publish tier-2 field carriage before selecting the final fee input and
  // resolving indices into the complete reference-input set.
  const published =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      materials,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: "Reference-input-no-idx step 02 reference-inputs field",
    }));
  if (isPrerequisiteTurn(published)) return published;
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "reference-input-no-idx step 02 validator",
  });
  // The reference inputs the transaction reads. Opening indices resolve
  // against the builder's final order inside the redeemer callback.
  const referenceInputs = [
    ...published,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
  ];
  const badReferenceInput =
    referenceInputsPreimage.referenceInputsPreimage[
      referenceInputsPreimage.badReferenceInputIndex
    ];
  if (badReferenceInput === undefined) {
    throw new Error(
      `--bad-reference-input-index ${referenceInputsPreimage.badReferenceInputIndex.toString()} is out of range for a ${referenceInputsPreimage.referenceInputsPreimage.length.toString()}-item preimage.`,
    );
  }

  const funding = await fundStep(lucid, { signer });
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: referenceInputNoIdxStep03StateFromBadInput(badReferenceInput),
    },
    ReferenceInputNoIdxStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: chain.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: ReferenceInputNoIdxStep02Layout | undefined;
  const redeemer = ((ctx) => {
    const referenceInputsOpening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
      label: "Reference-input-no-idx step 02 reference-inputs",
    });
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 02");
    const layout: ReferenceInputNoIdxStep02Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "reference-input-no-idx step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            reference_inputs_opening: referenceInputsOpening,
            bad_reference_input_index: BigInt(
              referenceInputsPreimage.badReferenceInputIndex,
            ),
          },
        ],
      },
      ReferenceInputNoIdxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const collected = funding.tx.collectFrom([threadUtxo], redeemer);
  // Without a published witness this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the carriage
  // produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  ).pay.ToContract(
    chain.steps[2].spendingScriptAddress,
    { kind: "inline", value: step03Datum },
    threadAssets,
  );
  const completedTx = stepScriptCarriage.attach(tx);

  const signed = await completeStep(completedTx, funding);
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 02 layout.",
    );
  }
  const transaction = finishStep(signed, {
    referenceScriptCandidates: [
      {
        role: "V1 fraud-proof reference-input-no-idx step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.referenceInputNoIdx.steps[1].spendingScript,
      },
    ],
  });

  return {
    transaction,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${transaction.txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: chain.steps[1].spendingScriptAddress,
    thirdStepAddress: chain.steps[2].spendingScriptAddress,
    verifiedTxReferenceInputsHash,
    verifiedTxId,
    referenceInputsPreimageItemCount:
      referenceInputsPreimage.referenceInputsPreimage.length,
    badReferenceInputIndex: referenceInputsPreimage.badReferenceInputIndex,
    badReferenceInputTxId: badReferenceInput.tx_id,
    badReferenceInputOutputIndex: Number(badReferenceInput.output_index),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
  };
};

export const submitReferenceInputNoIdxStep02FromFiles = async (
  config: SubmitReferenceInputNoIdxStep02CliConfig,
): Promise<SubmitReferenceInputNoIdxStep02Result> => {
  const [
    blueprint,
    deploymentInfo,
    referenceInputsPreimageJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.referenceInputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFaultProofTurns(
    buildReferenceInputNoIdxStep02,
    {
      lucid,
      blueprint,
      deploymentInfo,
      network: config.network,
      signer,
      threadOutRef: config.threadOutRef,
      referenceInputsPreimage:
        parseSubmitReferenceInputNoIdxReferenceInputsPreimage({
          value: referenceInputsPreimageJson,
          ...(config.badReferenceInputIndex === undefined
            ? {}
            : { badReferenceInputIndex: config.badReferenceInputIndex }),
        }),
      nativeTxCompactCbor: parseNativeTxCompactCbor(
        nativeTxCompactJson,
        "--native-tx-compact",
      ),
    },
    {
      label: "reference-input-no-idx-step02",
      awaitConfirmation: config.awaitConfirmation ?? true,
    },
  );
};
