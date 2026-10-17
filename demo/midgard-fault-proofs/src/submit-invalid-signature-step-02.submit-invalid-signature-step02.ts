import {
  encodeMidgardAddressWitnessCanonical,
  type FieldOpening,
  FraudProofTokenDatum,
  invalidSignatureTerminalContradiction,
  MIDGARD_FIELD_INDEX,
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
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
  resolveInvalidSignatureDeploymentContracts,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  type InvalidSignatureStep02ResolvedLayout,
  type InvalidSignatureStep02SpendLayout,
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeInvalidSignatureStep02SpendRedeemer,
  requireStep02Datum,
  type SubmitInvalidSignatureStep02Result,
} from "./submit-invalid-signature-step-02.make-invalid-signature-step02-spend-redeemer.js";
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

export const submitInvalidSignatureStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  addrTxWitsPreimage,
  nativeTxCompactCbor,
  witnessSetCompact,
  badAddrTxWitIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  certificatePolicyId,
  certificateUtxos = [],
  existingPublicationUtxos = [],
  publishMissingCarriage = true,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Complete positional address-witness list opened by this step. */
  readonly addrTxWitsPreimage: readonly MidgardAddressWitness[];
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** That transaction's compact witness set — §2.5's other half. */
  readonly witnessSetCompact: NativeTxWitnessSetCompact;
  readonly badAddrTxWitIndex: bigint;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** Manifest-bound certificate policy for a tier-3 field opening. */
  readonly certificatePolicyId?: string;
  /** Existing authenticated tier-3 certificate output. */
  readonly certificateUtxos?: readonly UTxO[];
  /** Existing authenticated tier-2/tier-3 field publications. */
  readonly existingPublicationUtxos?: readonly UTxO[];
  /**
   * Compatibility path for manual callers. Production workflows set this
   * false and journal every publication/certificate before this proof step.
   */
  readonly publishMissingCarriage?: boolean;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInvalidSignatureStep02Result> => {
  const { invalidSignatureCategory, contracts } =
    await resolveInvalidSignatureDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "invalid-signature step-02 computation-thread UTxO",
  });
  if (
    threadUtxo.address !==
    contracts.invalidSignature.steps[1].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at invalid-signature step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: invalidSignatureCategory.categoryId,
    categoryLabel: "invalid-signature",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const badTxId = inputDatum.data.subject.transaction_id;
  const badTxWitnessSetHash = inputDatum.data.bad_tx_witness_set_hash;

  // Mirror every check the door makes, in its order: the compact bytes
  // re-derive to the anchored id, the supplied witness set hashes to the
  // anchored `witness_set_hash`, and the supplied witness list is the §5.1
  // preimage that witness set commits at field 7.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: inputDatum.data.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: badTxId,
    nativeTxCompactCbor,
    itemCbors: addrTxWitsPreimage.map(encodeMidgardAddressWitnessCanonical),
    owner: signer.paymentKeyHash,
    witnessSet: witnessSetCompact,
    anchorWitnessSetHash: badTxWitnessSetHash,
    label: "Invalid-signature step 02 address-witnesses",
  });
  const badAddrTxWitsHash = planned.commitment;

  signer.selectWallet(lucid);
  // §8.4: publish tier-2 field carriage before the final transaction selects
  // fee inputs or resolves indices into the complete reference-input set.
  const published = [
    ...existingPublicationUtxos,
    ...(publishMissingCarriage
      ? await publishFaultProofFieldCarriage({
          lucid,
          signer,
          planned,
          publisherAddress: signer.address,
          label: "Invalid-signature step 02 address-witnesses field",
        })
      : []),
  ];
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: contracts.invalidSignature.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "invalid-signature step 02 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "invalid-signature step 02 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "invalid-signature step 02 fraud-proof mint",
  });
  // The complete reference-input set, built before the field opening derives
  // any carriage indices from it.
  const referenceInputs = [
    ...published,
    ...certificateUtxos,
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const addrTxWitsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    label: "Invalid-signature step 02 address-witnesses",
  });
  const badAddrTxWit = addrTxWitsPreimage[Number(badAddrTxWitIndex)];
  if (inputDatum.data.subject.direction === 0n && badAddrTxWit === undefined) {
    throw new Error(
      `--bad-addr-tx-wit-index ${badAddrTxWitIndex.toString()} is out of range for a ${addrTxWitsPreimage.length.toString()}-witness preimage.`,
    );
  }
  if (
    !invalidSignatureTerminalContradiction({
      subject: inputDatum.data.subject,
      witnessIndex: badAddrTxWitIndex,
      addressWitnesses: addrTxWitsPreimage,
    })
  ) {
    throw new Error(
      inputDatum.data.subject.direction === 0n
        ? `Address witness ${badAddrTxWitIndex.toString()} signs transaction ${badTxId} validly, so it does not violate the signature ledger rule.`
        : "invalidSignature: evidence does not contradict the authenticated verdict",
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
  let spendLayout: InvalidSignatureStep02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeInvalidSignatureStep02SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        addrTxWitsOpening,
        badAddrTxWitIndex,
        onLayout: (layout) => {
          spendLayout = layout;
        },
      }),
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
  // `readFrom([])` is an error rather than a no-op, so the branch is on
  // whether any witness published a reference script at all.
  const chained =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepScriptCarriage.attach(chained)),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve invalid-signature step 02 layout.",
    );
  }
  const resolvedLayout: InvalidSignatureStep02ResolvedLayout = {
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
          role: "V1 fraud-proof invalid-signature step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.invalidSignature.steps[1].spendingScript,
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
      `invalid-signature step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    secondStepAddress:
      contracts.invalidSignature.steps[1].spendingScriptAddress,
    badTxId,
    badAddrTxWitsHash,
    badTxWitnessSetHash,
    addrTxWitsPreimageItemCount: addrTxWitsPreimage.length,
    badAddrTxWitIndex: Number(badAddrTxWitIndex),
    badAddrTxWitVerificationKey: badAddrTxWit?.verification_key ?? null,
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
