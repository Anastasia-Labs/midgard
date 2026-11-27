import {
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

import { planFaultProofFieldOpening } from "./field-opening.plan-fault-proof-field-opening.js";
import { faultProofFieldOpening } from "./field-opening.publish-fault-proof-field-carriage.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveZeroInputDeploymentContracts,
} from "./runtime.js";
import { finishStep } from "./step-completion.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
  makeZeroInputStep02SpendRedeemer,
  requireStep02Datum,
  type ZeroInputStep02Build,
  type ZeroInputStep02ResolvedLayout,
  type ZeroInputStep02SpendLayout,
} from "./submit-zero-input-step-02.make-zero-input-step02-spend-redeemer.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "./witness-reference-scripts.js";
import { workflowReferenceScriptsUsedByTransaction } from "./workflow/transaction-boundary.js";

export const buildZeroInputStep02V1 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}): Promise<ZeroInputStep02Build> => {
  const { zeroInputCategory, contracts } =
    await resolveZeroInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "zero-input step-02 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.zeroInput.steps[1].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at zero-input step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: zeroInputCategory.categoryId,
    categoryLabel: "zero-input",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const badTxId = inputDatum.data.subject.transaction_id;
  // The family's whole claim is that field 0 holds nothing, so the §5.1
  // preimage is the empty envelope and the prover supplies no items. Planning
  // it against the anchored transaction is what proves the claim off-chain:
  // `planFaultProofFieldOpening` refuses unless these bytes re-derive to
  // `badTxId` *and* the empty preimage matches the commitment that transaction
  // carries at field 0 specifically.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: inputDatum.data.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: badTxId,
    nativeTxCompactCbor,
    itemCbors: [],
    owner: signer.paymentKeyHash,
    label: "Zero-input step 02 spend-inputs",
  });
  // Mirrors the validator's only category-specific check (`field_item_count ==
  // 0`), so a thread that cannot conclude fails here instead of burning a
  // submission on-chain. Redundant against the empty `itemCbors` above by
  // construction, and kept because the assertion the validator makes is about
  // the count rather than about what the builder passed.
  if (planned.itemCount !== 0) {
    throw new Error(
      `Zero-input step 02 opens field 0 to ${planned.itemCount.toString()} items, so the challenged transaction does spend inputs.`,
    );
  }
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: contracts.zeroInput.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "zero-input step 02 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "zero-input step 02 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "zero-input step 02 fraud-proof mint",
  });
  const referenceInputs = [
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
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
  let spendLayout: ZeroInputStep02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeZeroInputStep02SpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        spendInputsOpeningFor: (ctx) =>
          faultProofFieldOpening({
            planned,
            referenceInputs: ctx.referenceInputs,
            label: "Zero-input step 02 spend-inputs",
          }),
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
      "BuildTxWithRedeemer did not resolve zero-input step 02 layout.",
    );
  }
  const resolvedLayout: ZeroInputStep02ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const transaction = finishStep(signed, {
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof zero-input step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.zeroInput.steps[1].spendingScript,
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
  });
  const { txHash } = transaction;

  return {
    transaction,
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
    secondStepAddress: contracts.zeroInput.steps[1].spendingScriptAddress,
    badTxId,
    spendInputsItemCount: planned.itemCount,
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
