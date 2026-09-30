import {
  encodeMidgardTxOutputCanonical,
  type FieldOpening,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX,
  type MidgardTxOutput,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, Data, toUnit } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireFaultProofStepReferenceScript,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "../../src/runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";
import { type RawStepConfig } from "./reference-input-no-idx-emulator.submit-raw-reference-input-no-idx-step02.js";
import { network } from "./submit-init-emulator-shared.js";

/**
 * A raw step-04 finalize: the honest submitter's thread-burn/token-mint
 * transaction with the local `isReferenceInputNoIdxViolationV1` twin removed,
 * so a challenged index that genuinely EXISTS in the producing transaction
 * reaches the validator's own
 * `bad_reference_input_output_index >= field_item_count(outputs_view)`.
 */
export const submitRawReferenceInputNoIdxStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  signer,
  threadOutRef,
  outputsPreimage,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: RawStepConfig & {
  readonly outputsPreimage: readonly MidgardTxOutput[];
  readonly nativeTxCompactCbor: string;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
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
    label: "raw reference-input-no-idx step-04 thread UTxO",
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
  const inputDatum = Data.from(
    threadUtxo.datum!,
    ReferenceInputNoIdxStep04Datum,
  );
  if (inputDatum.data === null) {
    throw new Error("raw step-04 thread carries no producing-tx anchor");
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: inputDatum.data.producing_tx_id,
    nativeTxCompactCbor,
    itemCbors: outputsPreimage.map(encodeMidgardTxOutputCanonical),
    owner: signer.paymentKeyHash,
    label: "Raw reference-input-no-idx step 04 outputs",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw reference-input-no-idx step 04 outputs field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: chain.steps[3].spendingScriptHash,
    label: "raw reference-input-no-idx step 04",
  });
  const referenceInputs = [...published, stepReference];
  const outputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw reference-input-no-idx step 04 outputs",
  });
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
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw reference-input-no-idx step 04",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw reference-input-no-idx step 04",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              fraudProofOutputMatches,
              "raw reference-input-no-idx step 04 fraud-proof",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw reference-input-no-idx step 04 fraud-proof",
            ),
            outputs_opening: outputsOpening,
          },
        ],
      },
      ReferenceInputNoIdxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw reference-input-no-idx step 04 thread burn",
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
      "raw reference-input-no-idx step 04 fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw reference-input-no-idx step 04 thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw reference-input-no-idx step 04 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw reference-input-no-idx step 04 fraud-proof mint",
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      ...referenceInputs,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
