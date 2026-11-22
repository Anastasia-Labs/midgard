import {
  type FieldOpening,
  FraudProofTokenDatum,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofTurn,
  isPrerequisiteTurn,
} from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
} from "../runtime.js";
import { spendInputsWitnessFromCbors } from "../spend-input-witness.js";
import { completeStep, finishStep, fundStep } from "../step-completion.js";
import { requireComputationThreadToken } from "../step-support.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../witness-reference-scripts.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowReferenceScript } from "../workflow/transaction-boundary.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeStep04SpendRedeemer,
  requireStep04Datum,
  sameMidgardTxInput,
  type Step04Build,
  type Step04ResolvedLayout,
  type Step04SpendLayout,
} from "./submit-step-04.make-step04-spend-redeemer.js";

export const buildStep04 = async ({
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
  materials,
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
   * Force §8 tier 2 for tx2's field-0 preimage; see `buildStep03`'s
   * same-named option. Programmatic only — the retired CLI route never
   * sets it, and below the tier-1 bound the ladder would otherwise carry
   * the preimage inline in this step's redeemer (#612).
   */
  readonly publishCarriage?: boolean;
  /** Pre-observed tier-2/3 publications for workflow use. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-minted §8.6 certificate, required when the plan selects tier 3. */
  readonly certificateUtxo?: UTxO;
  readonly certificatePolicyId?: string;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<FaultProofTurn<Step04Build>> => {
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

  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      materials,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: "Double-spend step 04 tx2 spend-inputs",
    }));
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
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
  const funding = await fundStep(lucid, {
    signer,
    excluding: referenceInputs,
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

  const withInputs = funding.tx.collectFrom(
    [threadUtxo],
    makeStep04SpendRedeemer({
      threadUtxo,
      fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      fraudProofUnit,
      fraudProofDatum,
      tx2SpendInputsOpeningFor: (ctx): FieldOpening =>
        faultProofFieldOpening({
          planned,
          referenceInputs: ctx.referenceInputs,
          ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
          label: "Double-spend step 04 tx2 spend-inputs",
        }),
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
    );
  const signed = await completeStep(
    fraudProofMintCarriage.attach(
      computationThreadMintCarriage.attach(stepScriptCarriage.attach(chained)),
    ),
    funding,
  );
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
  const transaction = finishStep(signed, {
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
  });

  return {
    transaction,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    fraudProofOutRef: `${transaction.txHash}#${resolvedLayout.outputIndex.toString()}`,
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
  };
};
