import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core";
import { CML, createCostModels, type UTxO } from "@lucid-evolution/lucid";

import {
  type CardanoRedeemerBoundaryInput,
  compareCardanoRedeemerBoundaryInputs,
} from "./ordered-collection-boundary.build-signed-cardano-mint-native-policies-candidate.js";
import {
  CARDANO_BOUNDARY_TOTAL_COLLATERAL,
  type SignedCardanoCollectionCandidate,
} from "./ordered-collection-boundary.find-signed-cardano-collection-boundary.js";

export const buildSignedCardanoSpendRedeemersCandidate = async ({
  privateKeyBech32,
  feeFundingInput,
  collateralInput,
  availableScriptInputs,
  recipientAddress,
  plutusV3ScriptCborHex,
  redeemerDataCborHex,
  executionMemory,
  executionSteps,
  requestedRedeemerCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
  priceMem,
  priceStep,
  collateralPercentage,
  costModels,
}: {
  readonly privateKeyBech32: string;
  readonly feeFundingInput: UTxO;
  readonly collateralInput: UTxO;
  readonly availableScriptInputs: readonly UTxO[];
  readonly recipientAddress: string;
  readonly plutusV3ScriptCborHex: string;
  readonly redeemerDataCborHex: string;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly requestedRedeemerCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
  readonly priceMem: number;
  readonly priceStep: number;
  readonly collateralPercentage: number;
  readonly costModels: Parameters<typeof createCostModels>[0];
}): Promise<SignedCardanoCollectionCandidate> => {
  if (
    !Number.isSafeInteger(requestedRedeemerCount) ||
    requestedRedeemerCount <= 0
  ) {
    throw new Error("Requested Cardano spend-redeemer count must be positive");
  }
  if (requestedRedeemerCount > availableScriptInputs.length) {
    throw new Error(
      `Requested ${requestedRedeemerCount.toString()} Cardano spend redeemers, but only ${availableScriptInputs.length.toString()} real script UTxOs are available`,
    );
  }
  if (executionMemory < 0n || executionSteps < 0n) {
    throw new Error("Cardano redeemer execution units must be non-negative");
  }
  if (
    !Number.isSafeInteger(collateralPercentage) ||
    collateralPercentage <= 0
  ) {
    throw new Error(
      "Cardano collateral percentage must be a positive safe integer",
    );
  }
  if (costModels.PlutusV3.length === 0) {
    throw new Error("Cardano Plutus V3 cost model must not be empty");
  }

  const selectedScriptInputs = availableScriptInputs.slice(
    0,
    requestedRedeemerCount,
  );
  const selectedInputs: CardanoRedeemerBoundaryInput[] = [
    {
      txHash: feeFundingInput.txHash,
      outputIndex: feeFundingInput.outputIndex,
      lovelace: feeFundingInput.assets.lovelace ?? 0n,
      kind: "key" as const,
    },
    ...selectedScriptInputs.map(
      (input): CardanoRedeemerBoundaryInput => ({
        txHash: input.txHash,
        outputIndex: input.outputIndex,
        lovelace: input.assets.lovelace ?? 0n,
        kind: "script",
      }),
    ),
  ].sort(compareCardanoRedeemerBoundaryInputs);
  const distinctSpendOutRefs = new Set(
    selectedInputs.map(
      (input) => `${input.txHash}#${input.outputIndex.toString()}`,
    ),
  );
  if (distinctSpendOutRefs.size !== selectedInputs.length) {
    throw new Error(
      "Cardano spend-redeemer candidate contains a duplicate spend input",
    );
  }
  const collateralOutRef = `${collateralInput.txHash}#${collateralInput.outputIndex.toString()}`;
  if (distinctSpendOutRefs.has(collateralOutRef)) {
    throw new Error(
      "Cardano spend-redeemer collateral must not also be a spend input",
    );
  }

  const selectedLovelace = selectedInputs.reduce(
    (total, input) => total + input.lovelace,
    0n,
  );
  const collateralLovelace = collateralInput.assets.lovelace ?? 0n;
  if (collateralLovelace <= CARDANO_BOUNDARY_TOTAL_COLLATERAL) {
    throw new Error(
      "Cardano spend-redeemer collateral input cannot fund the fixed total collateral",
    );
  }

  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const plutusV3Script = CML.PlutusV3Script.from_cbor_hex(
    plutusV3ScriptCborHex,
  );
  const redeemerData = CML.PlutusData.from_cbor_hex(redeemerDataCborHex);
  if (
    aikenSerialisedPlutusDataCborPreservingMapOrder(redeemerDataCborHex) !==
    redeemerDataCborHex.toLowerCase()
  ) {
    throw new Error(
      "Cardano spend-redeemer Data must use exact Aiken serialiseData CBOR",
    );
  }
  const cmlCostModels = createCostModels(costModels);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const exUnitPrices = CML.ExUnitPrices.new(
    CML.SubCoin.from_base10_f32(priceMem),
    CML.SubCoin.from_base10_f32(priceStep),
  );
  const cmlInput = (
    input: Pick<CardanoRedeemerBoundaryInput, "txHash" | "outputIndex">,
  ): CML.TransactionInput =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.txHash),
      BigInt(input.outputIndex),
    );

  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    if (
      CARDANO_BOUNDARY_TOTAL_COLLATERAL * 100n <
      fee * BigInt(collateralPercentage)
    ) {
      throw new Error(
        `Cardano spend-redeemer fee ${fee.toString()} exceeds fixed collateral coverage`,
      );
    }
    const outputLovelace = selectedLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano spend-redeemer candidate ${requestedRedeemerCount.toString()} exhausts its selected inputs`,
      );
    }

    const inputs = CML.TransactionInputList.new();
    for (const input of selectedInputs) {
      inputs.add(cmlInput(input));
    }
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(CML.Value.from_coin(outputLovelace))
        .build()
        .output(),
    );
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    const collateralInputs = CML.TransactionInputList.new();
    collateralInputs.add(
      cmlInput({
        txHash: collateralInput.txHash,
        outputIndex: collateralInput.outputIndex,
      }),
    );
    body.set_collateral_inputs(collateralInputs);
    body.set_total_collateral(CARDANO_BOUNDARY_TOTAL_COLLATERAL);
    body.set_collateral_return(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(
          CML.Value.from_coin(
            collateralLovelace - CARDANO_BOUNDARY_TOTAL_COLLATERAL,
          ),
        )
        .build()
        .output(),
    );

    const redeemerMap = CML.MapRedeemerKeyToRedeemerVal.new();
    for (
      let inputIndex = 0;
      inputIndex < selectedInputs.length;
      inputIndex += 1
    ) {
      if (selectedInputs[inputIndex]!.kind !== "script") {
        continue;
      }
      redeemerMap.insert(
        CML.RedeemerKey.new(CML.RedeemerTag.Spend, BigInt(inputIndex)),
        CML.RedeemerVal.new(
          redeemerData,
          CML.ExUnits.new(executionMemory, executionSteps),
        ),
      );
    }
    const redeemers =
      CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemerMap);
    const scripts = CML.PlutusV3ScriptList.new();
    scripts.add(plutusV3Script);
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_plutus_v3_scripts(scripts);
    witnessSet.set_redeemers(redeemers);
    const usedLanguages = CML.LanguageList.new();
    usedLanguages.add(CML.Language.PlutusV3);
    const scriptDataHash = CML.calc_script_data_hash(
      redeemers,
      CML.PlutusDataList.new(),
      cmlCostModels,
      usedLanguages,
    );
    if (scriptDataHash === undefined) {
      throw new Error(
        "Cardano spend-redeemer script-data hash was not derived",
      );
    }
    body.set_script_data_hash(scriptDataHash);

    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_fee(
      signed.transaction,
      linearFee,
      exUnitPrices,
      0n,
    );
    if (nextFee === fee) {
      return {
        requestedItemCount: requestedRedeemerCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano spend-redeemer candidate ${requestedRedeemerCount.toString()} fee did not converge`,
  );
};
