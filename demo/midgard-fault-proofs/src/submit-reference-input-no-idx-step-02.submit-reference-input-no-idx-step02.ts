import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  referenceInputNoIdxStep03StateFromBadInput,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  parseSubmitReferenceInputNoIdxReferenceInputsPreimage,
  type ReferenceInputNoIdxStep02Layout,
  requireStep02Datum,
  type SubmitReferenceInputNoIdxReferenceInputsPreimage,
  type SubmitReferenceInputNoIdxStep02CliConfig,
  type SubmitReferenceInputNoIdxStep02Result,
} from "./submit-reference-input-no-idx-step-02.parse-submit-reference-input-no-idx-reference-inputs-preimage.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriage } from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const submitReferenceInputNoIdxStep02 = async ({
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
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
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
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitReferenceInputNoIdxStep02Result> => {
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

  signer.selectWallet(lucid);
  // Publish tier-2 field carriage before selecting the final fee input and
  // resolving indices into the complete reference-input set.
  const published =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Reference-input-no-idx step 02 reference-inputs field",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "reference-input-no-idx step 02 validator",
  });
  // The complete reference-input set the built transaction will declare, in
  // build order — the opening derivation must see all of it (bug fc635c8f).
  const referenceInputs = [
    ...published,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
  ];
  const referenceInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
    label: "Reference-input-no-idx step 02 reference-inputs",
  });
  const badReferenceInput =
    referenceInputsPreimage.referenceInputsPreimage[
      referenceInputsPreimage.badReferenceInputIndex
    ];
  if (badReferenceInput === undefined) {
    throw new Error(
      `--bad-reference-input-index ${referenceInputsPreimage.badReferenceInputIndex.toString()} is out of range for a ${referenceInputsPreimage.referenceInputsPreimage.length.toString()}-item preimage.`,
    );
  }

  // A tier-2 publication sits at the prover address under a large inline datum
  // (and its min-ADA), so it tops the fee selector's descending-lovelace sort;
  // exclude datum-carrying UTxOs so the referenced publication is never spent.
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
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

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  // Without a published witness this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the carriage
  // produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  ).pay
    .ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = stepScriptCarriage.attach(tx);

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof reference-input-no-idx step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.referenceInputNoIdx.steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `reference-input-no-idx step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
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
    awaitedConfirmation: awaitConfirmation,
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
  return await submitReferenceInputNoIdxStep02({
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
    awaitConfirmation: config.awaitConfirmation,
  });
};
