import {
  encodeMidgardTxOutputCanonical,
  type FieldOpening,
  FraudProofTokenDatum,
  isReferenceInputNoIdxViolation,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeReferenceInputNoIdxStep04SpendRedeemer,
  type ReferenceInputNoIdxStep04ResolvedLayout,
  type ReferenceInputNoIdxStep04SpendLayout,
  requireStep04Datum,
  type SubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxStep04Result,
} from "./submit-reference-input-no-idx-step-04.make-reference-input-no-idx-step04-spend-redeemer.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const submitReferenceInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
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
  readonly outputsPreimage: SubmitReferenceInputNoIdxOutputsPreimage;
  /** The **producing** transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitReferenceInputNoIdxStep04Result> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const chain = contracts.referenceInputNoIdx;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const producingTxId = inputDatum.data.producing_tx_id;
  const badReferenceInputOutputIndex =
    inputDatum.data.bad_reference_input_output_index;

  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producingTxId,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.outputsPreimage.map(
      encodeMidgardTxOutputCanonical,
    ),
    owner: signer.paymentKeyHash,
    label: "Reference-input-no-idx step 04 outputs",
  });
  const producingTxOutputsHash = planned.commitment;

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
      label: "Reference-input-no-idx step 04 outputs field",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "reference-input-no-idx step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "reference-input-no-idx step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "reference-input-no-idx step 04 fraud-proof mint",
  });
  // The complete reference-input set the built transaction will declare, in
  // build order — the opening derivation must see all of it (bug fc635c8f).
  const referenceInputs = [
    ...published,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const outputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
    label: "Reference-input-no-idx step 04 outputs",
  });
  // §5.2: the count the verdict rests on is the door's authenticated one, which
  // for tiers 1–2 is derived by walking the same preimage the door hashes.
  const producingTxOutputCount = planned.itemCount;
  if (
    !isReferenceInputNoIdxViolation({
      badReferenceInputOutputIndex,
      producingTxOutputCount,
    })
  ) {
    throw new Error(
      `Reference-input-no-idx step 04 cannot finalize: output index ${badReferenceInputOutputIndex.toString()} exists in a producing transaction with ${producingTxOutputCount.toString()} outputs; an existing transaction reference input cannot be proven non-existent.`,
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
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  let spendLayout: ReferenceInputNoIdxStep04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeReferenceInputNoIdxStep04SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        outputsOpening,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
    );
  // Without published witnesses this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the
  // carriages produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  )
    .mintAssets(
      { [threadToken.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: threadToken.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepScriptCarriage.attach(tx)),
  );

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 04 layout.",
    );
  }
  const resolvedLayout: ReferenceInputNoIdxStep04ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof reference-input-no-idx step-04",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.referenceInputNoIdx.steps[3].spendingScript,
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
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `reference-input-no-idx step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudProofOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: chain.steps[3].spendingScriptAddress,
    producingTxOutputsHash,
    producingTxId,
    producingTxOutputCount,
    badReferenceInputOutputIndex: Number(badReferenceInputOutputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      resolvedLayout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      resolvedLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
