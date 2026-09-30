import {
  type FieldOpening,
  FraudProofTokenDatum,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  excludeUtxo,
  spendInputsWitnessFromCbors,
} from "../spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../step-support.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeStep04SpendRedeemer,
  requireStep04Datum,
  sameMidgardTxInput,
  type Step04ResolvedLayout,
  type Step04SpendLayout,
  type SubmitStep04Result,
} from "./submit-step-04.make-step04-spend-redeemer.js";

export const submitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx2SpendInputCbors,
  nativeTxCompactCbor,
  doubleSpentInputIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  certificatePolicyId,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx2SpendInputCbors: readonly string[];
  /** tx2's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly doubleSpentInputIndex: bigint;
  /**
   * Force §8 tier 2 for tx2's field-0 preimage; see `submitStep03`'s
   * same-named option. Programmatic only — the retired CLI route never
   * sets it, and below the tier-1 bound the ladder would otherwise carry
   * the preimage inline in this step's redeemer (#612).
   */
  readonly publishCarriage?: boolean;
  /** Pre-observed tier-2/3 publications for journaled workflow use. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-minted §8.6 certificate, required when the plan selects tier 3. */
  readonly certificateUtxo?: UTxO;
  readonly certificatePolicyId?: string;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** Production workflow seam for carriage and proof-step submissions. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep04Result> => {
  const { doubleSpendCategory, contracts } =
    await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-04 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[3].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 04.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  // The door's own checks, run before a transaction is built.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: inputDatum.data.verified_tx2_id,
    nativeTxCompactCbor,
    itemCbors: tx2SpendInputCbors.map((inputCbor) =>
      Buffer.from(inputCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "Double-spend step 04 tx2 spend-inputs",
  });
  const tx2SpendInputsHash = planned.commitment;
  if (doubleSpentInputIndex >= BigInt(tx2SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx2SpendInputCbors.length.toString()} tx2 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx2SpendInputsWitness = spendInputsWitnessFromCbors(
    tx2SpendInputCbors,
    "--tx2-inputs",
  );
  const doubleSpentInputCbor =
    tx2SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx2SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;
  if (
    !sameMidgardTxInput(doubleSpentInput, inputDatum.data.double_spent_input)
  ) {
    throw new Error(
      `--tx2-inputs[${doubleSpentInputIndex.toString()}] does not match the double-spent input carried by step 04 datum.`,
    );
  }

  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Double-spend step 04 tx2 spend-inputs",
      preSubmitBoundary,
    }));
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: contracts.doubleSpend.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "double-spend step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "double-spend step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "double-spend step 04 fraud-proof mint",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      walletUtxos,
    ),
  );
  const tx2SpendInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    label: "Double-spend step 04 tx2 spend-inputs",
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
  let spendLayout: Step04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadSuccessRedeemer = makeComputationThreadSuccessRedeemer(
    {
      computationThreadPolicyId: contracts.computationThread.policyId,
      computationThreadAssetName: threadToken.assetName,
    },
  );

  const makeStep04Tx = (): TxBuilder => {
    const withInputs = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom(
        [threadUtxo],
        makeStep04SpendRedeemer({
          threadUtxo,
          fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
          fraudProofPolicyId: contracts.fraudProof.policyId,
          fraudProofUnit,
          fraudProofDatum,
          tx2SpendInputsOpening,
          doubleSpentInputIndex,
          onLayout: (layout) => {
            spendLayout = layout;
          },
        }),
      );
    // Tier 1 references nothing, and `readFrom([])` is an error rather than a
    // no-op, so the branch is on whether §8 produced carriage at all.
    const chained = (
      referenceInputs.length === 0
        ? withInputs
        : withInputs.readFrom([...referenceInputs])
    )
      .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
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
    return fraudProofMintCarriage.attach(
      computationThreadMintCarriage.attach(stepScriptCarriage.attach(chained)),
    );
  };

  const unsigned = await makeStep04Tx().complete({
    localUPLCEval: true,
    // With carriage published at the prover's own address, balancing must not
    // pick those UTxOs back up as wallet inputs while the redeemer references
    // them — same guard as `submitInputNoIdxStep02`.
    ...(referenceInputs.length === 0
      ? {}
      : {
          presetWalletInputs: referenceInputs.reduce<readonly UTxO[]>(
            (candidates, utxo) => excludeUtxo(candidates, utxo),
            walletUtxos,
          ) as UTxO[],
        }),
  });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error("BuildTxWithRedeemer did not resolve step 04 layout.");
  }
  const resolvedLayout: Step04ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof double-spend step-04",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.doubleSpend.steps[3].spendingScript,
      }),
      workflowReferenceScript({
        role: "V1 fraud-proof computation-thread minting",
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: contracts.computationThread.mintingScript,
      }),
      workflowReferenceScript({
        role: "V1 fraud-proof token minting",
        utxo: witnessReferenceScripts?.fraudProofMint,
        expectedScript: contracts.fraudProof.mintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
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
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx2Id: inputDatum.data.verified_tx2_id,
    verifiedTx2SpendInputsHash: tx2SpendInputsHash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx2SpendInputsCarriageTier: planned.plan.tier,
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
