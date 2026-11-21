import { CML } from "@lucid-evolution/lucid";

export const measureSignedCardanoOutputs = (
  signedCardanoCborHex: string,
): {
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  return {
    outputCount: transaction.body().outputs().len(),
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
  };
};

export const measureSignedCardanoInlineDatum = (
  signedCardanoCborHex: string,
): {
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputAddress: string;
  readonly outputLovelace: bigint;
  readonly datumCborHex: string;
  readonly datumCborBytes: number;
  readonly datumPayloadBytes: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const outputs = transaction.body().outputs();
  const datum = outputs.get(0).datum()?.as_datum();
  if (datum === undefined) {
    throw new Error(
      "Measured Cardano inline-datum transaction has no inline datum",
    );
  }
  const datumBytes = datum.as_bytes();
  if (datumBytes === undefined) {
    throw new Error("Measured Cardano inline datum is not a byte string");
  }
  const datumCbor = Buffer.from(datum.to_cbor_bytes());
  return {
    outputCount: outputs.len(),
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    outputAddress: outputs.get(0).address().to_bech32(),
    outputLovelace: outputs.get(0).amount().coin(),
    datumCborHex: datumCbor.toString("hex"),
    datumCborBytes: datumCbor.length,
    datumPayloadBytes: datumBytes.length,
  };
};

export const measureSignedCardanoMintNativePolicies = (
  signedCardanoCborHex: string,
): {
  readonly inputCount: number;
  readonly mintPolicyCount: number;
  readonly mintAssetCount: number;
  readonly nativeScriptWitnessCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
  readonly validityStart: bigint | undefined;
  readonly ttl: bigint | undefined;
  readonly mintPolicyHashHexes: readonly string[];
  readonly nativeScriptHashHexes: readonly string[];
  readonly policyAssetCounts: readonly number[];
  readonly mintQuantities: readonly bigint[];
  readonly outputValueByteLengths: readonly number[];
  readonly outputPolicyCounts: readonly number[];
  readonly outputAssetCount: number;
  readonly outputPolicyHashHexes: readonly string[];
  readonly outputAssetNameHexes: readonly string[];
  readonly outputAssetQuantities: readonly bigint[];
  readonly hasWithdrawals: boolean;
  readonly hasPlutusScripts: boolean;
  readonly hasRedeemers: boolean;
  readonly hasDatums: boolean;
  readonly collateralInputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const mint = body.mint();
  const nativeScripts = witnessSet.native_scripts();
  const mintPolicyHashHexes: string[] = [];
  const policyAssetCounts: number[] = [];
  const mintQuantities: bigint[] = [];
  if (mint !== undefined) {
    const policies = mint.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const assets = mint.get_assets(policy);
      if (assets === undefined) {
        throw new Error("Measured Cardano mint policy has no assets");
      }
      const assetNames = assets.keys();
      mintPolicyHashHexes.push(policy.to_hex());
      policyAssetCounts.push(assetNames.len());
      for (let assetIndex = 0; assetIndex < assetNames.len(); assetIndex += 1) {
        const quantity = assets.get(assetNames.get(assetIndex));
        if (quantity === undefined) {
          throw new Error("Measured Cardano mint policy asset has no quantity");
        }
        mintQuantities.push(quantity);
      }
    }
  }
  const nativeScriptHashHexes: string[] = [];
  if (nativeScripts !== undefined) {
    for (let index = 0; index < nativeScripts.len(); index += 1) {
      nativeScriptHashHexes.push(nativeScripts.get(index).hash().to_hex());
    }
  }
  const outputValueByteLengths: number[] = [];
  const outputPolicyCounts: number[] = [];
  const outputPolicyHashHexes: string[] = [];
  const outputAssetNameHexes: string[] = [];
  const outputAssetQuantities: bigint[] = [];
  const outputs = body.outputs();
  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const value = outputs.get(outputIndex).amount();
    outputValueByteLengths.push(value.to_cbor_bytes().length);
    const multiasset = value.multi_asset();
    if (multiasset === undefined) {
      throw new Error("Measured Cardano mint output has no assets");
    }
    const policies = multiasset.keys();
    outputPolicyCounts.push(policies.len());
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) {
        throw new Error("Measured Cardano mint output policy has no assets");
      }
      const assetNames = assets.keys();
      outputPolicyHashHexes.push(policy.to_hex());
      for (let assetIndex = 0; assetIndex < assetNames.len(); assetIndex += 1) {
        const assetName = assetNames.get(assetIndex);
        const quantity = assets.get(assetName);
        if (quantity === undefined) {
          throw new Error("Measured Cardano mint output asset has no quantity");
        }
        outputAssetNameHexes.push(
          Buffer.from(assetName.to_raw_bytes()).toString("hex"),
        );
        outputAssetQuantities.push(quantity);
      }
    }
  }
  return {
    inputCount: body.inputs().len(),
    mintPolicyCount: mint?.keys().len() ?? 0,
    mintAssetCount: mintQuantities.length,
    nativeScriptWitnessCount: nativeScripts?.len() ?? 0,
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    outputCount: outputs.len(),
    validityStart: body.validity_interval_start(),
    ttl: body.ttl(),
    mintPolicyHashHexes,
    nativeScriptHashHexes,
    policyAssetCounts,
    mintQuantities,
    outputValueByteLengths,
    outputPolicyCounts,
    outputAssetCount: outputAssetQuantities.length,
    outputPolicyHashHexes,
    outputAssetNameHexes,
    outputAssetQuantities,
    hasWithdrawals: body.withdrawals() !== undefined,
    hasPlutusScripts:
      (witnessSet.plutus_v1_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v2_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v3_scripts()?.len() ?? 0) > 0,
    hasRedeemers: witnessSet.redeemers() !== undefined,
    hasDatums: witnessSet.plutus_datums() !== undefined,
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
  };
};

export const measureSignedCardanoNestedDatum = (
  signedCardanoCborHex: string,
): {
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputAddress: string;
  readonly outputLovelace: bigint;
  readonly datumCborHex: string;
  readonly datumCborBytes: number;
  readonly hasWithdrawals: boolean;
  readonly hasMint: boolean;
  readonly hasPlutusScripts: boolean;
  readonly hasRedeemers: boolean;
  readonly collateralInputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const outputs = body.outputs();
  const datum = outputs.get(0).datum()?.as_datum();
  if (datum === undefined) {
    throw new Error(
      "Measured Cardano nested-datum transaction has no inline datum",
    );
  }
  const datumCbor = Buffer.from(datum.to_cbor_bytes());
  return {
    outputCount: outputs.len(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    outputAddress: outputs.get(0).address().to_bech32(),
    outputLovelace: outputs.get(0).amount().coin(),
    datumCborHex: datumCbor.toString("hex"),
    datumCborBytes: datumCbor.length,
    hasWithdrawals: body.withdrawals() !== undefined,
    hasMint: body.mint() !== undefined,
    hasPlutusScripts:
      (witnessSet.plutus_v1_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v2_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v3_scripts()?.len() ?? 0) > 0,
    hasRedeemers: witnessSet.redeemers() !== undefined,
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
  };
};

export const measureSignedCardanoNestedValue = (
  signedCardanoCborHex: string,
): {
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputAddress: string;
  readonly outputLovelace: bigint;
  readonly valueCborHex: string;
  readonly valueCborBytes: number;
  readonly policyHashHexes: readonly string[];
  readonly assetPolicyHashHexes: readonly string[];
  readonly assetNameHexes: readonly string[];
  readonly assetQuantities: readonly bigint[];
  readonly hasWithdrawals: boolean;
  readonly hasMint: boolean;
  readonly hasPlutusScripts: boolean;
  readonly hasRedeemers: boolean;
  readonly hasDatums: boolean;
  readonly collateralInputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const outputs = body.outputs();
  const output = outputs.get(0);
  const value = output.amount();
  const multiasset = value.multi_asset();
  if (multiasset === undefined) {
    throw new Error("Measured nested Cardano Value output has no assets");
  }
  const policyHashHexes: string[] = [];
  const assetPolicyHashHexes: string[] = [];
  const assetNameHexes: string[] = [];
  const assetQuantities: bigint[] = [];
  const policies = multiasset.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policy = policies.get(policyIndex);
    const assets = multiasset.get_assets(policy);
    if (assets === undefined) {
      throw new Error("Measured nested Cardano Value policy has no assets");
    }
    policyHashHexes.push(policy.to_hex());
    const assetNames = assets.keys();
    for (let assetIndex = 0; assetIndex < assetNames.len(); assetIndex += 1) {
      const assetName = assetNames.get(assetIndex);
      const quantity = assets.get(assetName);
      if (quantity === undefined) {
        throw new Error("Measured nested Cardano Value asset has no quantity");
      }
      assetPolicyHashHexes.push(policy.to_hex());
      assetNameHexes.push(
        Buffer.from(assetName.to_raw_bytes()).toString("hex"),
      );
      assetQuantities.push(quantity);
    }
  }
  const valueCbor = Buffer.from(value.to_cbor_bytes());
  return {
    outputCount: outputs.len(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    outputAddress: output.address().to_bech32(),
    outputLovelace: value.coin(),
    valueCborHex: valueCbor.toString("hex"),
    valueCborBytes: valueCbor.length,
    policyHashHexes,
    assetPolicyHashHexes,
    assetNameHexes,
    assetQuantities,
    hasWithdrawals: body.withdrawals() !== undefined,
    hasMint: body.mint() !== undefined,
    hasPlutusScripts:
      (witnessSet.plutus_v1_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v2_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v3_scripts()?.len() ?? 0) > 0,
    hasRedeemers: witnessSet.redeemers() !== undefined,
    hasDatums: witnessSet.plutus_datums() !== undefined,
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
  };
};

export const measureSignedCardanoSigners = (
  signedCardanoCborHex: string,
): {
  readonly requiredSignerCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  return {
    requiredSignerCount: transaction.body().required_signers()?.len() ?? 0,
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    outputCount: transaction.body().outputs().len(),
  };
};

export const measureSignedCardanoSpendInputs = (
  signedCardanoCborHex: string,
): {
  readonly inputCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  return {
    inputCount: transaction.body().inputs().len(),
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    outputCount: transaction.body().outputs().len(),
  };
};

export const measureSignedCardanoReferenceInputs = (
  signedCardanoCborHex: string,
): {
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  return {
    inputCount: transaction.body().inputs().len(),
    referenceInputCount: transaction.body().reference_inputs()?.len() ?? 0,
    vkeyWitnessCount: transaction.witness_set().vkeywitnesses()?.len() ?? 0,
    outputCount: transaction.body().outputs().len(),
  };
};

export const measureSignedCardanoObserverNativeScripts = (
  signedCardanoCborHex: string,
): {
  readonly inputCount: number;
  readonly withdrawalCount: number;
  readonly nativeScriptWitnessCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
  readonly validityStart: bigint | undefined;
  readonly ttl: bigint | undefined;
  readonly rewardAddressBech32s: readonly string[];
  readonly observerScriptHashHexes: readonly string[];
  readonly nativeScriptHashHexes: readonly string[];
  readonly withdrawalAmounts: readonly bigint[];
  readonly hasPlutusScripts: boolean;
  readonly hasRedeemers: boolean;
  readonly hasDatums: boolean;
  readonly collateralInputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const withdrawals = body.withdrawals();
  const nativeScripts = witnessSet.native_scripts();
  const rewardAddressBech32s: string[] = [];
  const observerScriptHashHexes: string[] = [];
  const withdrawalAmounts: bigint[] = [];
  if (withdrawals !== undefined) {
    const keys = withdrawals.keys();
    for (let index = 0; index < keys.len(); index += 1) {
      const rewardAddress = keys.get(index);
      const scriptHash = rewardAddress.payment().as_script();
      if (scriptHash === undefined) {
        throw new Error(
          "Measured Cardano observer withdrawal is not script-credentialed",
        );
      }
      const amount = withdrawals.get(rewardAddress);
      if (amount === undefined) {
        throw new Error("Measured Cardano observer withdrawal has no amount");
      }
      rewardAddressBech32s.push(rewardAddress.to_address().to_bech32());
      observerScriptHashHexes.push(scriptHash.to_hex());
      withdrawalAmounts.push(amount);
    }
  }
  const nativeScriptHashHexes: string[] = [];
  if (nativeScripts !== undefined) {
    for (let index = 0; index < nativeScripts.len(); index += 1) {
      nativeScriptHashHexes.push(nativeScripts.get(index).hash().to_hex());
    }
  }
  return {
    inputCount: body.inputs().len(),
    withdrawalCount: withdrawals?.len() ?? 0,
    nativeScriptWitnessCount: nativeScripts?.len() ?? 0,
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    outputCount: body.outputs().len(),
    validityStart: body.validity_interval_start(),
    ttl: body.ttl(),
    rewardAddressBech32s,
    observerScriptHashHexes,
    nativeScriptHashHexes,
    withdrawalAmounts,
    hasPlutusScripts:
      (witnessSet.plutus_v1_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v2_scripts()?.len() ?? 0) > 0 ||
      (witnessSet.plutus_v3_scripts()?.len() ?? 0) > 0,
    hasRedeemers: witnessSet.redeemers() !== undefined,
    hasDatums: witnessSet.plutus_datums() !== undefined,
    collateralInputCount: body.collateral_inputs()?.len() ?? 0,
  };
};
