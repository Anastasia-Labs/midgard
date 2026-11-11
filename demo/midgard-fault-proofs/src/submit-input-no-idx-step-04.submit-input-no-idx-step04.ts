import {
  encodeMidgardTxOutputCanonical,
  FraudProofTokenDatum,
  isInputNoIdxViolation,
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
  type FaultProofFieldGrammarDeployment,
  routeFaultProofFieldGrammar,
} from "./field-grammar-certificate.js";
import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening.js";
import { stepWalletFunding } from "./funding/plain-funding-output.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveInputNoIdxDeploymentContracts,
} from "./runtime.js";
import { requireComputationThreadToken } from "./step-support.js";
import {
  type InputNoIdxStep04ResolvedLayout,
  type InputNoIdxStep04SpendLayout,
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeInputNoIdxStep04SpendRedeemer,
  requireStep04Datum,
  type SubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxStep04Result,
} from "./submit-input-no-idx-step-04.make-input-no-idx-step04-spend-redeemer.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "./witness-reference-scripts.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "./workflow/own-outputs.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const submitInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  witnessSetCompactCbor = "",
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  publicationPreSubmitBoundary,
  fieldGrammar,
  preSubmitBoundary,
  awaitConfirmation = true,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outputsPreimage: SubmitInputNoIdxOutputsPreimage;
  /** The **producing** transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /**
   * The producing transaction's compact witness set, hashing to its compact
   * `witness_set_hash`. The field grammar certificate authenticates the field
   * against the whole compact transaction, so a certified count needs it.
   */
  readonly witnessSetCompactCbor?: string;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  /**
   * The field grammar certificate. The step counts field 2 through it past the
   * walked count limit or under tier 3, since the certified count also serves a
   * chunked field.
   */
  readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<SubmitInputNoIdxStep04Result> => {
  const { nonExistentInputNoIndexCategory, contracts } =
    await resolveInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const chain = contracts.nonExistentInputNoIndex;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "input-no-idx step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at input-no-idx step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputNoIndexCategory.categoryId,
    categoryLabel: "input-no-idx",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const producingTxId = inputDatum.data.producing_tx_id;
  const badInputOutputIndex = inputDatum.data.bad_input_output_index;

  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producingTxId,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.outputsPreimage.map(
      encodeMidgardTxOutputCanonical,
    ),
    owner: signer.paymentKeyHash,
    label: "Input-no-idx step 04 outputs",
  });
  const producingTxOutputsHash = planned.commitment;
  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain first. Tier 1
  // publishes nothing; tiers 2–3 publish raw carriage located by content
  // (§8.7), and a publication that already exists at this address is reused
  // rather than republished.
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Input-no-idx step 04 outputs",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const grammar = await routeFaultProofFieldGrammar({
    lucid,
    signer,
    plan: planned.plan,
    read: "count",
    itemCount: planned.itemCount,
    source: {
      sourceKind: 0n,
      compactCbor: nativeTxCompactCbor,
      witnessSetCompactCbor,
    },
    carriageUtxos: [
      ...carriageUtxos,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificate.policyId,
    fieldGrammar,
    label: "Input-no-idx step 04 outputs",
  });
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "input-no-idx step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "input-no-idx step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "input-no-idx step 04 fraud-proof mint",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...grammar.referenceInputs,
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  // §5.2: the count the verdict rests on is the door's authenticated one.
  const producingTxOutputCount = planned.itemCount;
  if (
    !isInputNoIdxViolation({
      badInputOutputIndex,
      producingTxOutputCount,
    })
  ) {
    throw new Error(
      `Input-no-idx step 04 cannot finalize: output index ${badInputOutputIndex.toString()} exists in a producing transaction with ${producingTxOutputCount.toString()} outputs; an existing transaction input cannot be proven non-existent.`,
    );
  }

  const { feeInput, walletInputs } = await stepWalletFunding(lucid, {
    excluding: carriageUtxos,
  });
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
  let spendLayout: InputNoIdxStep04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const txWithInputs = lucid.newTx().collectFrom([feeInput]);
  const txWithReferences =
    referenceInputs.length === 0
      ? txWithInputs
      : txWithInputs.readFrom([...referenceInputs]);
  const tx = txWithReferences
    .collectFrom(
      [threadUtxo],
      makeInputNoIdxStep04SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        outputsOpeningFor: (ctx) =>
          faultProofFieldOpening({
            planned,
            referenceInputs: ctx.referenceInputs,
            certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
            label: "Input-no-idx step 04 outputs",
          }),
        grammarRefInputIndex: grammar.grammarRefInputIndex,
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
  const completedTx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepScriptCarriage.attach(tx)),
  );

  const unsigned = await completedTx.complete({
    localUPLCEval: true,
    ...walletInputs,
  });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve input-no-idx step 04 layout.",
    );
  }
  const resolvedLayout: InputNoIdxStep04ResolvedLayout = {
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
          role: "V1 fraud-proof non-existent-input-no-index step-04",
          utxo: referenceScriptUtxo,
          expectedScript:
            contracts.nonExistentInputNoIndex.steps[3].spendingScript,
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
      `input-no-idx step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    carriageTier: planned.plan.tier,
    badInputOutputIndex: Number(badInputOutputIndex),
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
