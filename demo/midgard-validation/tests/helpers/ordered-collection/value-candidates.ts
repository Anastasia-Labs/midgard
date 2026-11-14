import { CML } from "@lucid-evolution/lucid";

import {
  CARDANO_BOUNDARY_MAX_VALUE_SIZE,
  CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE,
  CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES,
  type SignedCardanoCollectionCandidate,
} from "./parameters.js";

export const cardanoBoundaryNestedDataCbor = (
  nestedLeafCount: number,
): string => {
  if (!Number.isSafeInteger(nestedLeafCount) || nestedLeafCount <= 0) {
    throw new Error("Cardano nested Data leaf count must be positive");
  }
  const balancedList = (firstLeafIndex: number, leafCount: number): string => {
    if (leafCount === 1) {
      return firstLeafIndex === 0 ? "4101" : "00";
    }
    const leftCount = Math.floor(leafCount / 2);
    return [
      "9f",
      balancedList(firstLeafIndex, leftCount),
      balancedList(firstLeafIndex + leftCount, leafCount - leftCount),
      "ff",
    ].join("");
  };
  return [
    "d8668218809f",
    "a1",
    "d87980",
    balancedList(0, nestedLeafCount),
    "ff",
  ].join("");
};

export type CardanoBoundaryNestedValueAsset = {
  readonly policyIdHex: string;
  readonly assetNameHex: string;
  readonly quantity: bigint;
};

export const cardanoBoundaryNestedValueAssets = (
  requestedValueCborBytes: number,
): readonly CardanoBoundaryNestedValueAsset[] => {
  if (
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE &&
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1
  ) {
    throw new Error(
      "Nested Cardano Value boundary shape only supports 5,000 or 5,001 bytes",
    );
  }
  const adjacent =
    requestedValueCborBytes === CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1;
  return CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES.flatMap(
    (policyIdHex, policyIndex) => {
      const assetCount = policyIndex < 3 ? 228 : 227;
      return Array.from(
        { length: assetCount },
        (_, policyAssetIndex): CardanoBoundaryNestedValueAsset => ({
          policyIdHex,
          assetNameHex:
            policyAssetIndex === 0
              ? ""
              : Buffer.from([policyAssetIndex - 1]).toString("hex"),
          quantity:
            adjacent &&
            policyIndex + 1 ===
              CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES.length &&
            policyAssetIndex + 1 === assetCount
              ? 24n
              : 1n,
        }),
      );
    },
  );
};

/**
 * Builds a signed Cardano transaction with one fixed-lovelace multi-asset
 * output whose CML Value CBOR is exactly 5,000 or 5,001 bytes. A separate
 * ADA-only change output keeps the measured Value independent of the fee.
 */
export const buildSignedCardanoNestedValueCandidate = async ({
  privateKeyBech32,
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  requestedValueCborBytes,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedValueCborBytes: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidate> => {
  const valueAssets = cardanoBoundaryNestedValueAssets(requestedValueCborBytes);
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const multiasset = CML.MultiAsset.new();
  const policyAssets = new Map<string, CML.MapAssetNameToCoin>();
  for (const asset of valueAssets) {
    const assets =
      policyAssets.get(asset.policyIdHex) ?? CML.MapAssetNameToCoin.new();
    assets.insert(
      CML.AssetName.from_raw_bytes(Buffer.from(asset.assetNameHex, "hex")),
      asset.quantity,
    );
    policyAssets.set(asset.policyIdHex, assets);
  }
  for (const [policyIdHex, assets] of policyAssets) {
    multiasset.insert_assets(CML.ScriptHash.from_hex(policyIdHex), assets);
  }
  const boundaryValue = CML.Value.new(
    CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE,
    multiasset,
  );
  const actualValueCborBytes = boundaryValue.to_cbor_bytes().length;
  if (actualValueCborBytes !== requestedValueCborBytes) {
    throw new Error(
      `Nested Cardano Value shape encoded to ${actualValueCborBytes.toString()} bytes instead of ${requestedValueCborBytes.toString()}`,
    );
  }
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const changeLovelace =
      inputLovelace - CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE - fee;
    if (changeLovelace <= 0n) {
      throw new Error(
        `Nested Cardano Value candidate ${requestedValueCborBytes.toString()} exhausts its funding input`,
      );
    }
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(inputTransactionId),
        inputOutputIndex,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(boundaryValue)
        .build()
        .output(),
    );
    outputs.add(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(CML.Value.from_coin(changeLovelace))
        .build()
        .output(),
    );
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
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
    const nextFee = CML.min_no_script_fee(signed.transaction, linearFee);
    if (nextFee === fee) {
      return {
        requestedItemCount: requestedValueCborBytes,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Nested Cardano Value candidate ${requestedValueCborBytes.toString()} fee did not converge`,
  );
};
