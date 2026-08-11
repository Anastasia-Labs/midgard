import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  buildMidgardBoundedItemV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  commitMidgardBoundedItemV1,
  computeHash32,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldPreimages,
  encodeCbor,
  hashMidgardValidationWorkWitnessV1,
  midgardBoundedItemChunkCountV1,
  reconstructMidgardTransactionV1,
} from "@al-ft/midgard-core";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { deriveValidationProofItemPublicationV1 } from "@al-ft/midgard-sdk";
import {
  CML,
  createCostModels,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";

import { selectValidationCompleteItemCarriageV1 } from "../../../midgard-fault-proofs/src/validation-dispute/submit.js";
import { countedMachineTransactionChunkStepsV1 } from "../../src/validation-machine.js";
import { encodeValidationAuxiliaryWitnessCborV1 } from "../../src/validation-machine-data.js";

export const CARDANO_BOUNDARY_MAX_TX_SIZE_V1 = 16_384;
export const CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 = 5_000;
export const CARDANO_BOUNDARY_PROTOCOL_MAJOR_V1 = 11;
export const CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1 = 1_592;
export const CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE_V1 = 30_000_000n;
export const CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1 = Array.from(
  { length: 7 },
  (_, policyIndex) =>
    (0x11 + policyIndex).toString(16).padStart(2, "0").repeat(28),
);
export const cardanoBoundaryNestedDataCborV1 = (
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
export const CARDANO_BOUNDARY_OBSERVER_TTL_V1 = 10_000n;
export const CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 = 20_000n;
export const CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1 = 100_000_000n;
export const CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1 = 5_000_000n;
export const CARDANO_BOUNDARY_MINT_ASSET_NAME_V1 = Buffer.from(
  "MidgardV1",
  "utf8",
);
export const PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1 = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 44,
  minFeeB: 155_381,
  maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  maxValSize: CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  priceMem: 0.0577,
  priceStep: 0.0000721,
  coinsPerUtxoByte: 4_310n,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  minFeeRefScriptCostPerByte: 15,
} as const;

const CARDANO_BOUNDARY_SIGNER_KEY_DOMAIN_V1 = Buffer.from(
  "CardanoBoundarySignerKeyV1",
  "utf8",
);

export const deterministicCardanoBoundaryPrivateKeyV1 = (
  signerIndex: number,
): CML.PrivateKey => {
  if (
    !Number.isSafeInteger(signerIndex) ||
    signerIndex < 0 ||
    signerIndex > 0xffff_ffff
  ) {
    throw new Error("Deterministic Cardano signer index must fit uint32");
  }
  const encodedIndex = Buffer.alloc(4);
  encodedIndex.writeUInt32BE(signerIndex);
  return CML.PrivateKey.from_normal_bytes(
    computeHash32(
      Buffer.concat([CARDANO_BOUNDARY_SIGNER_KEY_DOMAIN_V1, encodedIndex]),
    ),
  );
};

export const deriveCardanoGenesisInputSupplyV1 = (
  maxTxSize: number,
): number => {
  if (!Number.isSafeInteger(maxTxSize) || maxTxSize <= 0) {
    throw new Error("Cardano maxTxSize must be a positive safe integer");
  }
  const transactionIdBytesPerInput = 32;
  const adjacentCandidateReserve = 2;
  return (
    Math.floor(maxTxSize / transactionIdBytesPerInput) +
    adjacentCandidateReserve
  );
};

export type SignedCardanoCollectionCandidateV1 = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
};

export type SignedCardanoCollectionBoundaryV1 = {
  readonly accepted: SignedCardanoCollectionCandidateV1;
  readonly adjacent: SignedCardanoCollectionCandidateV1;
  readonly adjacentFailure: string;
};

export type CardanoBoundaryNestedValueAssetV1 = {
  readonly policyIdHex: string;
  readonly assetNameHex: string;
  readonly quantity: bigint;
};

export const cardanoBoundaryNestedValueAssetsV1 = (
  requestedValueCborBytes: number,
): readonly CardanoBoundaryNestedValueAssetV1[] => {
  if (
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 &&
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1
  ) {
    throw new Error(
      "Nested Cardano Value boundary shape only supports 5,000 or 5,001 bytes",
    );
  }
  const adjacent =
    requestedValueCborBytes === CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1;
  return CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1.flatMap(
    (policyIdHex, policyIndex) => {
      const assetCount = policyIndex < 3 ? 228 : 227;
      return Array.from(
        { length: assetCount },
        (_, policyAssetIndex): CardanoBoundaryNestedValueAssetV1 => ({
          policyIdHex,
          assetNameHex:
            policyAssetIndex === 0
              ? ""
              : Buffer.from([policyAssetIndex - 1]).toString("hex"),
          quantity:
            adjacent &&
            policyIndex + 1 ===
              CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1.length &&
            policyAssetIndex + 1 === assetCount
              ? 24n
              : 1n,
        }),
      );
    },
  );
};

export type MidgardOrderedCollectionBoundaryMeasurementV1 = {
  readonly nativeCanonicalBytes: number;
  readonly fieldBytes: number;
  readonly fieldCommitmentHex: string;
  readonly fieldPreimageCborHex: string;
  readonly fieldPreimageHashHex: string;
  readonly itemCount: number;
  readonly revealStepCount: number;
  readonly completeFoldStepCount: number;
  readonly maxRevealBytes: number;
  readonly maxChunkBytes: number;
  readonly terminalFoldVector: {
    readonly transactionIdHex: string;
    readonly transactionCommitmentHex: string;
    readonly compactCborHex: string;
    readonly witnessSetCompactCborHex: string;
    readonly fieldPreimageLengthsCborHex: string;
    readonly validationContextCborHex: string;
    readonly workWitnessCborHex: string;
    readonly compactBindingWitnessCborHex: string;
    readonly successorPhase: "canonicalDecode" | "compactBinding";
    readonly successorWitnessCborHex: string;
    readonly preWorkRootHex: string;
    readonly postWorkRootHex: string;
    readonly encodedLengthBeforeItem: number;
    readonly collectionProof: {
      readonly fieldIndex: number;
      readonly itemCount: number;
      readonly itemIndex: number;
      readonly itemLength: number;
      readonly itemCommitmentHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
    readonly chunkProof: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
      readonly totalLength: number;
      readonly chunkIndex: number;
      readonly chunkHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
  };
};

type FindSignedCardanoCollectionBoundaryV1Options = {
  readonly buildSignedCandidate: (
    requestedItemCount: number,
  ) => Promise<SignedCardanoCollectionCandidateV1>;
  readonly maxTxSize: number;
};

/**
 * Finds a transaction-shape boundary without introducing a Midgard count cap.
 *
 * The shape builder produces fully signed Cardano CBOR on both sides of the
 * boundary. Exact signed bytes are compared with the preserved maxTxSize;
 * provider behavior and a Midgard count are deliberately not gate inputs.
 */
export const findSignedCardanoCollectionBoundaryV1 = async ({
  buildSignedCandidate,
  maxTxSize,
}: FindSignedCardanoCollectionBoundaryV1Options): Promise<SignedCardanoCollectionBoundaryV1> => {
  if (!Number.isSafeInteger(maxTxSize) || maxTxSize <= 0) {
    throw new Error("Cardano maxTxSize must be a positive safe integer");
  }

  const buildMeasured = async (
    requestedItemCount: number,
  ): Promise<SignedCardanoCollectionCandidateV1> => {
    const candidate = await buildSignedCandidate(requestedItemCount);
    if (candidate.requestedItemCount !== requestedItemCount) {
      throw new Error(
        `Cardano collection builder returned cardinality ${candidate.requestedItemCount.toString()} for requested cardinality ${requestedItemCount.toString()}`,
      );
    }
    return candidate;
  };

  let accepted = await buildMeasured(1);
  if (accepted.signedBytes > maxTxSize) {
    throw new Error("One-item signed Cardano collection exceeds maxTxSize");
  }
  let rejectedItemCount = 2;
  for (;;) {
    const candidate = await buildMeasured(rejectedItemCount);
    if (candidate.signedBytes > maxTxSize) {
      break;
    }
    accepted = candidate;
    if (rejectedItemCount > Math.floor(Number.MAX_SAFE_INTEGER / 2)) {
      throw new Error("Cardano collection boundary search overflowed");
    }
    rejectedItemCount *= 2;
  }

  let acceptedItemCount = accepted.requestedItemCount;
  while (acceptedItemCount + 1 < rejectedItemCount) {
    const midpoint = Math.floor((acceptedItemCount + rejectedItemCount) / 2);
    const candidate = await buildMeasured(midpoint);
    if (candidate.signedBytes <= maxTxSize) {
      accepted = candidate;
      acceptedItemCount = midpoint;
    } else {
      rejectedItemCount = midpoint;
    }
  }

  const adjacent = await buildMeasured(acceptedItemCount + 1);
  if (adjacent.signedBytes <= maxTxSize) {
    throw new Error(
      `Adjacent Cardano shape with ${adjacent.requestedItemCount.toString()} requested items unexpectedly fit maxTxSize`,
    );
  }

  return {
    accepted,
    adjacent,
    adjacentFailure:
      `Exact signed Cardano CBOR is ${adjacent.signedBytes.toString()} bytes, ` +
      `above snapshot maxTxSize ${maxTxSize.toString()}`,
  };
};

export const buildSignedCardanoOutputsCandidateV1 = async ({
  privateKeyBech32,
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  requestedOutputCount,
  lovelacePerOutput,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedOutputCount: number;
  readonly lovelacePerOutput: bigint;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (
    !Number.isSafeInteger(requestedOutputCount) ||
    requestedOutputCount <= 0
  ) {
    throw new Error("Requested Cardano output count must be positive");
  }
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeOutput = (lovelace: bigint): CML.TransactionOutput =>
    CML.TransactionOutputBuilder.new()
      .with_address(address)
      .next()
      .with_value(CML.Value.from_coin(lovelace))
      .build()
      .output();
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const paymentTotal = BigInt(requestedOutputCount) * lovelacePerOutput;
    const change = inputLovelace - paymentTotal - fee;
    if (change <= 0n) {
      throw new Error(
        `Cardano outputs candidate ${requestedOutputCount.toString()} exhausts its funding input`,
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
    for (let index = 0; index < requestedOutputCount; index += 1) {
      outputs.add(makeOutput(lovelacePerOutput));
    }
    outputs.add(makeOutput(change));
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
        requestedItemCount: requestedOutputCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano outputs candidate ${requestedOutputCount.toString()} fee did not converge`,
  );
};

const buildSignedCardanoInlineDataCandidateV1 = async ({
  privateKeyBech32,
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  datum,
  requestedItemCount,
  diagnosticKind,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly datum: CML.PlutusData;
  readonly requestedItemCount: number;
  readonly diagnosticKind: string;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (!Number.isSafeInteger(requestedItemCount) || requestedItemCount <= 0) {
    throw new Error(
      `Requested Cardano ${diagnosticKind} count must be positive`,
    );
  }
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = inputLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano ${diagnosticKind} candidate ${requestedItemCount.toString()} exhausts its funding input`,
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
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(outputLovelace),
        CML.DatumOption.new_datum(datum),
        undefined,
      ),
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
        requestedItemCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano ${diagnosticKind} candidate ${requestedItemCount.toString()} fee did not converge`,
  );
};

/**
 * Builds one signed Cardano output whose inline Plutus Data is a byte string
 * of the requested payload length. The shape isolates a single dynamic item
 * so the exact maxTxSize boundary also exercises multi-chunk item proofs.
 */
export const buildSignedCardanoInlineDatumCandidateV1 = async ({
  privateKeyBech32,
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  requestedDatumPayloadBytes,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedDatumPayloadBytes: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> =>
  buildSignedCardanoInlineDataCandidateV1({
    privateKeyBech32,
    inputTransactionId,
    inputOutputIndex,
    inputLovelace,
    recipientAddress,
    datum: CML.PlutusData.new_bytes(
      Buffer.alloc(requestedDatumPayloadBytes, 0x5a),
    ),
    requestedItemCount: requestedDatumPayloadBytes,
    diagnosticKind: "inline-datum payload",
    minFeeA,
    minFeeB,
    minFeeRefScriptCostPerByte,
  });

export const buildSignedCardanoNestedDatumCandidateV1 = async ({
  privateKeyBech32,
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  requestedNestedLeafCount,
  nestedDatumCborHex,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedNestedLeafCount: number;
  readonly nestedDatumCborHex: string;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  const normalized =
    aikenSerialisedPlutusDataCborPreservingMapOrder(nestedDatumCborHex);
  if (normalized !== nestedDatumCborHex.toLowerCase()) {
    throw new Error(
      "Cardano nested inline datum must use exact Aiken serialiseData CBOR",
    );
  }
  return buildSignedCardanoInlineDataCandidateV1({
    privateKeyBech32,
    inputTransactionId,
    inputOutputIndex,
    inputLovelace,
    recipientAddress,
    datum: CML.PlutusData.from_cbor_hex(normalized),
    requestedItemCount: requestedNestedLeafCount,
    diagnosticKind: "nested inline-datum leaf",
    minFeeA,
    minFeeB,
    minFeeRefScriptCostPerByte,
  });
};

/**
 * Builds a signed Cardano transaction with one fixed-lovelace multi-asset
 * output whose CML Value CBOR is exactly 5,000 or 5,001 bytes. A separate
 * ADA-only change output keeps the measured Value independent of the fee.
 */
export const buildSignedCardanoNestedValueCandidateV1 = async ({
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
}): Promise<SignedCardanoCollectionCandidateV1> => {
  const valueAssets = cardanoBoundaryNestedValueAssetsV1(
    requestedValueCborBytes,
  );
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
    CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE_V1,
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
      inputLovelace - CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE_V1 - fee;
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

export const buildSignedCardanoSignersCandidateV1 = async ({
  inputTransactionId,
  inputOutputIndex,
  inputLovelace,
  recipientAddress,
  requestedSignerCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly inputTransactionId: string;
  readonly inputOutputIndex: bigint;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedSignerCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (
    !Number.isSafeInteger(requestedSignerCount) ||
    requestedSignerCount <= 0
  ) {
    throw new Error("Requested Cardano signer count must be positive");
  }
  const privateKeys = Array.from(
    { length: requestedSignerCount },
    (_, signerIndex) => deterministicCardanoBoundaryPrivateKeyV1(signerIndex),
  );
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = inputLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano signer candidate ${requestedSignerCount.toString()} exhausts its funding input`,
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
        .with_value(CML.Value.from_coin(outputLovelace))
        .build()
        .output(),
    );
    const requiredSigners = CML.Ed25519KeyHashList.new();
    for (const privateKey of privateKeys) {
      requiredSigners.add(privateKey.to_public().hash());
    }
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_required_signers(requiredSigners);
    const bodyHash = CML.hash_transaction(body);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    for (const privateKey of privateKeys) {
      vkeyWitnesses.add(CML.make_vkey_witness(bodyHash, privateKey));
    }
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
        requestedItemCount: requestedSignerCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano signer candidate ${requestedSignerCount.toString()} fee did not converge`,
  );
};

export const buildSignedCardanoSpendInputsCandidateV1 = async ({
  privateKeyBech32,
  availableInputs,
  recipientAddress,
  requestedInputCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly availableInputs: readonly UTxO[];
  readonly recipientAddress: string;
  readonly requestedInputCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (!Number.isSafeInteger(requestedInputCount) || requestedInputCount <= 0) {
    throw new Error("Requested Cardano input count must be positive");
  }
  if (requestedInputCount > availableInputs.length) {
    throw new Error(
      `Requested ${requestedInputCount.toString()} Cardano inputs, but only ${availableInputs.length.toString()} real emulator UTxOs are available`,
    );
  }
  const selectedInputs = availableInputs.slice(0, requestedInputCount);
  const selectedLovelace = selectedInputs.reduce(
    (total, input) => total + (input.assets.lovelace ?? 0n),
    0n,
  );
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = selectedLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano input candidate ${requestedInputCount.toString()} exhausts its selected inputs`,
      );
    }
    const inputs = CML.TransactionInputList.new();
    for (const input of selectedInputs) {
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(input.txHash),
          BigInt(input.outputIndex),
        ),
      );
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
        requestedItemCount: requestedInputCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano input candidate ${requestedInputCount.toString()} fee did not converge`,
  );
};

export const buildSignedCardanoReferenceInputsCandidateV1 = async ({
  privateKeyBech32,
  availableInputs,
  recipientAddress,
  requestedReferenceInputCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly availableInputs: readonly UTxO[];
  readonly recipientAddress: string;
  readonly requestedReferenceInputCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (
    !Number.isSafeInteger(requestedReferenceInputCount) ||
    requestedReferenceInputCount <= 0
  ) {
    throw new Error("Requested Cardano reference-input count must be positive");
  }
  const requiredInputSupply = requestedReferenceInputCount + 1;
  if (requiredInputSupply > availableInputs.length) {
    throw new Error(
      `Requested one funding input and ${requestedReferenceInputCount.toString()} Cardano reference inputs, but only ${availableInputs.length.toString()} real emulator UTxOs are available`,
    );
  }
  const fundingInput = availableInputs[0]!;
  const referenceInputs = availableInputs.slice(1, requiredInputSupply);
  const fundingLovelace = fundingInput.assets.lovelace ?? 0n;
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const cmlInput = (input: UTxO): CML.TransactionInput =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.txHash),
      BigInt(input.outputIndex),
    );
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = fundingLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano reference-input candidate ${requestedReferenceInputCount.toString()} exhausts its funding input`,
      );
    }
    const inputs = CML.TransactionInputList.new();
    inputs.add(cmlInput(fundingInput));
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
    const cmlReferenceInputs = CML.TransactionInputList.new();
    for (const referenceInput of referenceInputs) {
      cmlReferenceInputs.add(cmlInput(referenceInput));
    }
    body.set_reference_inputs(cmlReferenceInputs);
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
        requestedItemCount: requestedReferenceInputCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano reference-input candidate ${requestedReferenceInputCount.toString()} fee did not converge`,
  );
};

const makeCardanoBoundaryNativeScriptV1 = ({
  signerHash,
  scriptIndex,
}: {
  readonly signerHash: CML.Ed25519KeyHash;
  readonly scriptIndex: number;
}): CML.NativeScript => {
  const clauses = CML.NativeScriptList.new();
  clauses.add(CML.NativeScript.new_script_pubkey(signerHash));
  clauses.add(
    CML.NativeScript.new_script_invalid_hereafter(
      CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 + BigInt(scriptIndex),
    ),
  );
  return CML.NativeScript.new_script_all(clauses);
};

export const buildSignedCardanoObserverNativeScriptsCandidateV1 = async ({
  privateKeyBech32,
  fundingInput,
  recipientAddress,
  requestedObserverCount,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly fundingInput: UTxO;
  readonly recipientAddress: string;
  readonly requestedObserverCount: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (
    !Number.isSafeInteger(requestedObserverCount) ||
    requestedObserverCount <= 0
  ) {
    throw new Error(
      "Requested Cardano observer/native-script count must be positive",
    );
  }
  const fundingLovelace = fundingInput.assets.lovelace ?? 0n;
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const signerHash = privateKey.to_public().hash();
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const makeObserverScript = (observerIndex: number): CML.NativeScript =>
    makeCardanoBoundaryNativeScriptV1({
      signerHash,
      scriptIndex: observerIndex,
    });
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const outputLovelace = fundingLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        `Cardano observer/native-script candidate ${requestedObserverCount.toString()} exhausts its funding input`,
      );
    }
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(fundingInput.txHash),
        BigInt(fundingInput.outputIndex),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(CML.Value.from_coin(outputLovelace))
        .build()
        .output(),
    );
    const withdrawals = CML.MapRewardAccountToCoin.new();
    const nativeScripts = CML.NativeScriptList.new();
    const observerScripts = Array.from(
      { length: requestedObserverCount },
      (_, observerIndex) => makeObserverScript(observerIndex),
    ).sort((left, right) =>
      Buffer.compare(
        Buffer.from(left.hash().to_raw_bytes()),
        Buffer.from(right.hash().to_raw_bytes()),
      ),
    );
    for (const script of observerScripts) {
      withdrawals.insert(
        CML.RewardAddress.new(0, CML.Credential.new_script(script.hash())),
        0n,
      );
      nativeScripts.add(script);
    }
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(CARDANO_BOUNDARY_OBSERVER_TTL_V1);
    body.set_withdrawals(withdrawals);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    witnessSet.set_native_scripts(nativeScripts);
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
        requestedItemCount: requestedObserverCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano observer/native-script candidate ${requestedObserverCount.toString()} fee did not converge`,
  );
};

export const buildSignedCardanoMintNativePoliciesCandidateV1 = async ({
  privateKeyBech32,
  fundingInput,
  recipientAddress,
  requestedPolicyCount,
  maxValueSize,
  minFeeA,
  minFeeB,
  minFeeRefScriptCostPerByte,
}: {
  readonly privateKeyBech32: string;
  readonly fundingInput: UTxO;
  readonly recipientAddress: string;
  readonly requestedPolicyCount: number;
  readonly maxValueSize: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly minFeeRefScriptCostPerByte: number;
}): Promise<SignedCardanoCollectionCandidateV1> => {
  if (
    !Number.isSafeInteger(requestedPolicyCount) ||
    requestedPolicyCount <= 0
  ) {
    throw new Error("Requested Cardano mint policy count must be positive");
  }
  if (!Number.isSafeInteger(maxValueSize) || maxValueSize <= 0) {
    throw new Error("Cardano maxValueSize must be a positive safe integer");
  }
  const fundingLovelace = fundingInput.assets.lovelace ?? 0n;
  const privateKey = CML.PrivateKey.from_bech32(privateKeyBech32);
  const signerHash = privateKey.to_public().hash();
  const address = CML.Address.from_bech32(recipientAddress);
  const linearFee = CML.LinearFee.new(
    BigInt(minFeeA),
    BigInt(minFeeB),
    BigInt(minFeeRefScriptCostPerByte),
  );
  const policyEntries = Array.from(
    { length: requestedPolicyCount },
    (_, scriptIndex) => ({
      scriptIndex,
      policyHashHex: makeCardanoBoundaryNativeScriptV1({
        signerHash,
        scriptIndex,
      })
        .hash()
        .to_hex(),
    }),
  ).sort((left, right) =>
    left.policyHashHex.localeCompare(right.policyHashHex),
  );
  type PolicyEntry = (typeof policyEntries)[number];
  const makeValue = (
    entries: readonly PolicyEntry[],
    lovelace: bigint,
  ): CML.Value => {
    const multiasset = CML.MultiAsset.new();
    for (const entry of entries) {
      const assets = CML.MapAssetNameToCoin.new();
      assets.insert(
        CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME_V1),
        1n,
      );
      multiasset.insert_assets(
        CML.ScriptHash.from_hex(entry.policyHashHex),
        assets,
      );
    }
    return CML.Value.new(lovelace, multiasset);
  };
  const packPolicyEntries = (
    fee: bigint,
  ): {
    readonly groups: readonly (readonly PolicyEntry[])[];
    readonly firstOutputLovelace: bigint;
  } => {
    let expectedOutputCount = 1;
    for (let attempt = 0; attempt < 10; attempt += 1) {
      const firstOutputLovelace =
        fundingLovelace -
        fee -
        BigInt(expectedOutputCount - 1) *
          CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1;
      if (firstOutputLovelace <= 0n) {
        throw new Error(
          `Cardano mint candidate ${requestedPolicyCount.toString()} exhausts its funding input while packing Values`,
        );
      }
      const groups: PolicyEntry[][] = [];
      for (const entry of policyEntries) {
        if (groups.length === 0) {
          groups.push([entry]);
          continue;
        }
        const groupIndex = groups.length - 1;
        const group = groups[groupIndex]!;
        const groupLovelace =
          groupIndex === 0
            ? firstOutputLovelace
            : CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1;
        const candidateGroup = [...group, entry];
        if (
          makeValue(candidateGroup, groupLovelace).to_cbor_bytes().length <=
          maxValueSize
        ) {
          groups[groupIndex] = candidateGroup;
          continue;
        }
        if (
          makeValue(
            [entry],
            CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1,
          ).to_cbor_bytes().length > maxValueSize
        ) {
          throw new Error("One Cardano mint policy entry exceeds maxValueSize");
        }
        groups.push([entry]);
      }
      if (groups.length === expectedOutputCount) {
        return { groups, firstOutputLovelace };
      }
      expectedOutputCount = groups.length;
    }
    throw new Error(
      `Cardano mint candidate ${requestedPolicyCount.toString()} Value packing did not converge`,
    );
  };
  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    const packed = packPolicyEntries(fee);
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(fundingInput.txHash),
        BigInt(fundingInput.outputIndex),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    for (const [outputIndex, entries] of packed.groups.entries()) {
      const lovelace =
        outputIndex === 0
          ? packed.firstOutputLovelace
          : CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1;
      const value = makeValue(entries, lovelace);
      if (value.to_cbor_bytes().length > maxValueSize) {
        throw new Error(
          `Packed Cardano output Value ${outputIndex.toString()} exceeds maxValueSize`,
        );
      }
      outputs.add(
        CML.TransactionOutputBuilder.new()
          .with_address(address)
          .next()
          .with_value(value)
          .build()
          .output(),
      );
    }
    const mint = CML.Mint.new();
    const nativeScripts = CML.NativeScriptList.new();
    for (const entry of policyEntries) {
      const script = makeCardanoBoundaryNativeScriptV1({
        signerHash,
        scriptIndex: entry.scriptIndex,
      });
      const assets = CML.MapAssetNameToNonZeroInt64.new();
      assets.insert(
        CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME_V1),
        1n,
      );
      mint.insert_assets(script.hash(), assets);
      nativeScripts.add(script);
    }
    const body = CML.TransactionBody.new(inputs, outputs, fee);
    body.set_ttl(CARDANO_BOUNDARY_OBSERVER_TTL_V1);
    body.set_mint(mint);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), privateKey),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    witnessSet.set_native_scripts(nativeScripts);
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
        requestedItemCount: requestedPolicyCount,
        cborHex: signed.cborHex,
        signedBytes: signed.cborHex.length / 2,
        fee,
      };
    }
    fee = nextFee;
  }
  throw new Error(
    `Cardano mint candidate ${requestedPolicyCount.toString()} fee did not converge`,
  );
};

type CardanoRedeemerBoundaryInputV1 = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly lovelace: bigint;
  readonly kind: "key" | "script";
};

const compareCardanoRedeemerBoundaryInputsV1 = (
  left: CardanoRedeemerBoundaryInputV1,
  right: CardanoRedeemerBoundaryInputV1,
): number =>
  left.txHash.localeCompare(right.txHash) ||
  left.outputIndex - right.outputIndex;

export const buildSignedCardanoSpendRedeemersCandidateV1 = async ({
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
}): Promise<SignedCardanoCollectionCandidateV1> => {
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
  const selectedInputs: CardanoRedeemerBoundaryInputV1[] = [
    {
      txHash: feeFundingInput.txHash,
      outputIndex: feeFundingInput.outputIndex,
      lovelace: feeFundingInput.assets.lovelace ?? 0n,
      kind: "key" as const,
    },
    ...selectedScriptInputs.map(
      (input): CardanoRedeemerBoundaryInputV1 => ({
        txHash: input.txHash,
        outputIndex: input.outputIndex,
        lovelace: input.assets.lovelace ?? 0n,
        kind: "script",
      }),
    ),
  ].sort(compareCardanoRedeemerBoundaryInputsV1);
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
  if (collateralLovelace <= CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1) {
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
    input: Pick<CardanoRedeemerBoundaryInputV1, "txHash" | "outputIndex">,
  ): CML.TransactionInput =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.txHash),
      BigInt(input.outputIndex),
    );

  const makeSigned = (
    fee: bigint,
  ): { readonly transaction: CML.Transaction; readonly cborHex: string } => {
    if (
      CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1 * 100n <
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
    body.set_total_collateral(CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1);
    body.set_collateral_return(
      CML.TransactionOutputBuilder.new()
        .with_address(address)
        .next()
        .with_value(
          CML.Value.from_coin(
            collateralLovelace - CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
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

export const buildCollateralFreeMidgardSchemaParallelCandidateV1 = ({
  collateralizedCardanoCborHex,
  privateKeyBech32,
}: {
  readonly collateralizedCardanoCborHex: string;
  readonly privateKeyBech32: string;
}): {
  readonly cborHex: string;
  readonly collateralizedRedeemersCborHex: string;
  readonly parallelRedeemersCborHex: string;
} => {
  const collateralized = CML.Transaction.from_cbor_hex(
    collateralizedCardanoCborHex,
  );
  const sourceBody = collateralized.body();
  const sourceWitnessSet = collateralized.witness_set();
  const sourceRedeemers = sourceWitnessSet.redeemers();
  if (sourceRedeemers === undefined) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no redeemers",
    );
  }
  if ((sourceBody.collateral_inputs()?.len() ?? 0) === 0) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no collateral input",
    );
  }
  if ((sourceWitnessSet.plutus_datums()?.len() ?? 0) > 0) {
    throw new Error(
      "Collateralized Cardano feasibility candidate unexpectedly uses datum witnesses",
    );
  }

  const parallelBody = CML.TransactionBody.new(
    sourceBody.inputs(),
    sourceBody.outputs(),
    sourceBody.fee(),
  );
  const referenceInputs = sourceBody.reference_inputs();
  if (referenceInputs !== undefined) {
    parallelBody.set_reference_inputs(referenceInputs);
  }
  const validityStart = sourceBody.validity_interval_start();
  if (validityStart !== undefined) {
    parallelBody.set_validity_interval_start(validityStart);
  }
  const ttl = sourceBody.ttl();
  if (ttl !== undefined) {
    parallelBody.set_ttl(ttl);
  }
  const withdrawals = sourceBody.withdrawals();
  if (withdrawals !== undefined) {
    parallelBody.set_withdrawals(withdrawals);
  }
  const requiredSigners = sourceBody.required_signers();
  if (requiredSigners !== undefined) {
    parallelBody.set_required_signers(requiredSigners);
  }
  const mint = sourceBody.mint();
  if (mint !== undefined) {
    parallelBody.set_mint(mint);
  }
  const scriptDataHash = sourceBody.script_data_hash();
  if (scriptDataHash !== undefined) {
    parallelBody.set_script_data_hash(scriptDataHash);
  }
  const auxiliaryDataHash = sourceBody.auxiliary_data_hash();
  if (auxiliaryDataHash !== undefined) {
    parallelBody.set_auxiliary_data_hash(auxiliaryDataHash);
  }
  const networkId = sourceBody.network_id();
  if (networkId !== undefined) {
    parallelBody.set_network_id(networkId);
  }

  const parallelWitnessSet = CML.TransactionWitnessSet.new();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.make_vkey_witness(
      CML.hash_transaction(parallelBody),
      CML.PrivateKey.from_bech32(privateKeyBech32),
    ),
  );
  parallelWitnessSet.set_vkeywitnesses(vkeyWitnesses);
  const nativeScripts = sourceWitnessSet.native_scripts();
  if (nativeScripts !== undefined) {
    parallelWitnessSet.set_native_scripts(nativeScripts);
  }
  const plutusV3Scripts = sourceWitnessSet.plutus_v3_scripts();
  if (plutusV3Scripts !== undefined) {
    parallelWitnessSet.set_plutus_v3_scripts(plutusV3Scripts);
  }
  parallelWitnessSet.set_redeemers(sourceRedeemers);
  const parallel = CML.Transaction.new(
    parallelBody,
    parallelWitnessSet,
    collateralized.is_valid(),
    collateralized.auxiliary_data(),
  );
  const parallelRedeemers = parallel.witness_set().redeemers();
  if (parallelRedeemers === undefined) {
    throw new Error(
      "Collateral-free Midgard-schema feasibility candidate lost its redeemers",
    );
  }
  return {
    cborHex: parallel.to_cbor_hex(),
    collateralizedRedeemersCborHex: Buffer.from(
      sourceRedeemers.to_cbor_bytes(),
    ).toString("hex"),
    parallelRedeemersCborHex: Buffer.from(
      parallelRedeemers.to_cbor_bytes(),
    ).toString("hex"),
  };
};

/**
 * Converts exact signed Cardano CBOR through the production bridge, verifies
 * every reveal for one typed field, and then runs the complete canonical
 * transaction chunk sequence through the exact terminal reconstruction fold.
 */
export const exerciseMidgardOrderedCollectionBoundaryV1 = ({
  signedCardanoCborHex,
  fieldIndex,
}: {
  readonly signedCardanoCborHex: string;
  readonly fieldIndex: number;
}): MidgardOrderedCollectionBoundaryMeasurementV1 => {
  const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
    Buffer.from(signedCardanoCborHex, "hex"),
  );
  const nativeTx =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeCanonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(nativeCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(nativeTx);
  const transactionCommitment = computeMidgardNativeTxProofCommitmentV1(source);
  const field = deriveMidgardV1TxFieldPreimages(nativeCanonicalCbor).find(
    (candidate) => candidate.fieldIndex === fieldIndex,
  );
  if (field === undefined) {
    throw new Error(
      `Canonical Midgard transaction does not contain field ${fieldIndex.toString()}`,
    );
  }
  const completeChunks =
    countedMachineTransactionChunkStepsV1(nativeCanonicalCbor);
  const fieldChunks = completeChunks.filter(
    (chunk) => chunk.fieldIndex === fieldIndex,
  );
  if (fieldChunks.length === 0) {
    throw new Error(
      `Canonical Midgard field ${fieldIndex.toString()} has no reveal steps`,
    );
  }

  // §4 authenticates a field once, over its whole preimage, against the hash the
  // compact structure carries. The retired counted chain verified each chunk
  // opening here instead; under §4 a per-item opening has nothing to be checked
  // against, so the single whole-field check is the authentication.
  const reconstructed = reconstructMidgardTransactionV1({
    transactionId,
    transactionCommitment,
    source,
    fieldPreimages: deriveMidgardV1TxFieldPreimages(nativeCanonicalCbor).map(
      (candidate) => candidate.preimageCbor,
    ),
  });
  if (!reconstructed.equals(nativeCanonicalCbor)) {
    throw new Error(
      "Canonical Midgard terminal fold did not reconstruct the exact transaction",
    );
  }
  const terminalFieldStep = fieldChunks.at(-1);
  if (
    terminalFieldStep === undefined ||
    terminalFieldStep.fieldEncodedSize !== field.preimageCbor.length
  ) {
    throw new Error(
      `Canonical Midgard field ${fieldIndex.toString()} did not terminate at its committed length`,
    );
  }
  const validationContextCbor = Buffer.from(
    "8701546d6964676172642d636f6e73656e7375732d763118640000001864",
    "hex",
  );
  const firstTerminalItemStepIndex = fieldChunks.findIndex(
    (step) =>
      step.chunkProof.itemIndex === terminalFieldStep.chunkProof.itemIndex,
  );
  const precedingItemStep =
    firstTerminalItemStepIndex > 0
      ? fieldChunks[firstTerminalItemStepIndex - 1]
      : undefined;
  const itemCount = terminalFieldStep.collectionProof.itemCount;
  const collectionHeaderBytes =
    itemCount < 24
      ? 1
      : itemCount <= 0xff
        ? 2
        : itemCount <= 0xffff
          ? 3
          : itemCount <= 0xffff_ffff
            ? 5
            : 9;
  const encodedLengthBeforeItem =
    precedingItemStep?.fieldEncodedSize ?? collectionHeaderBytes;
  const workWitnessCbor = encodeCbor([
    source.compactCbor,
    source.witnessSetCompactCbor,
    source.fieldPreimageLengthsCbor,
    validationContextCbor,
    BigInt(fieldIndex),
    BigInt(terminalFieldStep.chunkProof.itemIndex),
    BigInt(terminalFieldStep.chunkProof.chunkIndex),
    BigInt(terminalFieldStep.collectionProof.itemCount),
    BigInt(encodedLengthBeforeItem),
  ]);
  const compactBindingWitnessCbor = encodeCbor([
    transactionId,
    transactionCommitment,
    source.compactCbor,
    source.witnessSetCompactCbor,
    source.fieldPreimageLengthsCbor,
    validationContextCbor,
  ]);
  const successorPhase =
    fieldIndex === 8 ? "compactBinding" : "canonicalDecode";
  const successorWitnessCbor =
    fieldIndex === 8
      ? compactBindingWitnessCbor
      : encodeCbor([
          source.compactCbor,
          source.witnessSetCompactCbor,
          source.fieldPreimageLengthsCbor,
          validationContextCbor,
          BigInt(fieldIndex + 1),
          0n,
          0n,
          -1n,
          0n,
        ]);

  return {
    nativeCanonicalBytes: nativeCanonicalCbor.length,
    fieldBytes: field.preimageCbor.length,
    fieldCommitmentHex: field.expectedHash.toString("hex"),
    fieldPreimageCborHex: field.preimageCbor.toString("hex"),
    fieldPreimageHashHex: computeHash32(field.preimageCbor).toString("hex"),
    itemCount: fieldChunks[0]!.collectionProof.itemCount,
    revealStepCount: fieldChunks.length,
    completeFoldStepCount: completeChunks.length,
    maxRevealBytes: Math.max(
      ...fieldChunks.map(
        (chunk) =>
          encodeValidationAuxiliaryWitnessCborV1({
            kind: "transactionFieldChunk",
            collectionProof: chunk.collectionProof,
            chunkProof: chunk.chunkProof,
          }).length,
      ),
    ),
    maxChunkBytes: Math.max(
      ...fieldChunks.map((chunk) => chunk.chunkProof.chunk.length),
    ),
    terminalFoldVector: {
      transactionIdHex: transactionId.toString("hex"),
      transactionCommitmentHex: transactionCommitment.toString("hex"),
      compactCborHex: source.compactCbor.toString("hex"),
      witnessSetCompactCborHex: source.witnessSetCompactCbor.toString("hex"),
      fieldPreimageLengthsCborHex:
        source.fieldPreimageLengthsCbor.toString("hex"),
      validationContextCborHex: validationContextCbor.toString("hex"),
      workWitnessCborHex: workWitnessCbor.toString("hex"),
      compactBindingWitnessCborHex: compactBindingWitnessCbor.toString("hex"),
      successorPhase,
      successorWitnessCborHex: successorWitnessCbor.toString("hex"),
      preWorkRootHex: hashMidgardValidationWorkWitnessV1({
        phase: "canonicalDecode",
        programCounter: 40,
        witnessCbor: workWitnessCbor,
      }).toString("hex"),
      postWorkRootHex: hashMidgardValidationWorkWitnessV1({
        phase: successorPhase,
        programCounter: 41,
        witnessCbor: successorWitnessCbor,
      }).toString("hex"),
      encodedLengthBeforeItem,
      collectionProof: {
        fieldIndex: terminalFieldStep.collectionProof.fieldIndex,
        itemCount: terminalFieldStep.collectionProof.itemCount,
        itemIndex: terminalFieldStep.collectionProof.itemIndex,
        itemLength: terminalFieldStep.collectionProof.itemLength,
        itemCommitmentHex:
          terminalFieldStep.collectionProof.itemCommitment.toString("hex"),
        frontier: terminalFieldStep.collectionProof.frontier.peaks.map(
          (peak) => ({
            height: peak.height,
            hashHex: peak.hash.toString("hex"),
          }),
        ),
        siblingHexes: terminalFieldStep.collectionProof.siblings.map(
          (sibling) => sibling.toString("hex"),
        ),
      },
      chunkProof: {
        fieldIndex: terminalFieldStep.chunkProof.fieldIndex,
        itemIndex: terminalFieldStep.chunkProof.itemIndex,
        totalLength: terminalFieldStep.chunkProof.totalLength,
        chunkIndex: terminalFieldStep.chunkProof.chunkIndex,
        chunkHex: terminalFieldStep.chunkProof.chunk.toString("hex"),
        frontier: terminalFieldStep.chunkProof.frontier.peaks.map((peak) => ({
          height: peak.height,
          hashHex: peak.hash.toString("hex"),
        })),
        siblingHexes: terminalFieldStep.chunkProof.siblings.map((sibling) =>
          sibling.toString("hex"),
        ),
      },
    },
  };
};

export const measureSignedCardanoOutputsV1 = (
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

export const measureSignedCardanoInlineDatumV1 = (
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

export const measureSignedCardanoNestedDatumV1 = (
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

export const measureSignedCardanoNestedValueV1 = (
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

export const measureSignedCardanoSignersV1 = (
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

export const measureSignedCardanoSpendInputsV1 = (
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

export const measureSignedCardanoReferenceInputsV1 = (
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

export const measureSignedCardanoObserverNativeScriptsV1 = (
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

export const measureSignedCardanoMintNativePoliciesV1 = (
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

export type MidgardCompleteItemCarriageFitV1 = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: number;
  readonly commitmentHex: string;
  /** Production carriage decision for the complete item. */
  readonly carriage: "direct" | "reference";
  /** Chunks a bounded fallback would need if the complete item did not fit. */
  readonly boundedFallbackChunkCount: number;
  readonly maxReliableDirectCompleteItemBytes: number;
  readonly maxSinglePublicationCompleteItemBytes: number;
  readonly maxL1TransactionBytes: number;
  readonly publicationDatumBytes: number;
  readonly publicationTransactionBytes: number;
  readonly fitsDirectCarriage: boolean;
  readonly fitsSinglePublicationCarriage: boolean;
  /** True only when neither complete route admits the item. */
  readonly requiresBoundedFallback: boolean;
};

/**
 * Measures whether a complete canonical proof item fits direct carriage and
 * single-publication reference carriage, before any bounded fallback is
 * considered. This is the §3.2 complete-item-first ordering: a fallback is
 * necessary only when both complete routes are measured to overflow.
 *
 * The publication side builds a real signed Conway transaction carrying the
 * complete item as the published inline datum, using the same framing as
 * `complete-item-proof-fit-v1.test.ts`. The collection proof is sized at the
 * deepest admissible shape (14 frontier peaks and 14 siblings under the
 * 16,384-item guardrail), so a reported fit is conservative: real shallower
 * collections can only produce a smaller transaction.
 *
 * Direct carriage is decided by the production selector
 * `selectValidationCompleteItemCarriageV1`, whose bound is the applied
 * deployed-validator measurement pinned in `MIDGARD_V1_ENVELOPE_MEASUREMENTS`.
 */
export const measureMidgardCompleteItemCarriageFitV1 = ({
  fieldIndex,
  itemIndex,
  itemCbor,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemCbor: Buffer;
}): MidgardCompleteItemCarriageFitV1 => {
  const maxL1TransactionBytes =
    MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;
  const maxSinglePublicationCompleteItemBytes =
    MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
  const maxReliableDirectCompleteItemBytes =
    MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex,
    itemIndex,
    bytes: itemCbor,
  });
  const commitment = commitMidgardBoundedItemV1({
    fieldIndex,
    itemIndex,
    totalLength: itemCbor.length,
    frontier: bounded.frontier,
  });
  if (!commitment.equals(bounded.commitment)) {
    throw new Error(
      "Complete proof item commitment disagrees with its own bounded frontier",
    );
  }
  const fitsDirectCarriage =
    itemCbor.length <= maxReliableDirectCompleteItemBytes;
  const fitsItemPublicationBound =
    itemCbor.length <= maxSinglePublicationCompleteItemBytes;
  const publication = deriveValidationProofItemPublicationV1({
    transactionId: "44".repeat(32),
    transactionCommitment: "55".repeat(32),
    collectionProof: {
      version: 1n,
      field_index: BigInt(fieldIndex),
      item_count: 16_384n,
      item_index: BigInt(itemIndex),
      item_length: BigInt(itemCbor.length),
      item_commitment: commitment.toString("hex"),
      frontier: Array.from({ length: 14 }, (_, height) => ({
        height: BigInt(height),
        hash: "22".repeat(32),
      })),
      siblings: Array.from({ length: 14 }, () => "33".repeat(32)),
    },
    itemCbor: itemCbor.toString("hex"),
  });
  const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 4));
  const address = CML.Address.from_raw_bytes(
    Buffer.concat([
      Buffer.from([0x60]),
      Buffer.from(signingKey.to_public().hash().to_raw_bytes()),
    ]),
  );
  const scriptAddress = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x66)]),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
      0n,
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      scriptAddress,
      CML.Value.from_coin(70_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(publication.datumCbor),
      ),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      address,
      CML.Value.from_coin(1_000_000_000n),
      undefined,
      undefined,
    ),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(Buffer.alloc(32, 5)),
    ),
  );
  witnessSet.set_vkeywitnesses(vkeys);
  const publicationTransactionBytes = CML.Transaction.new(
    CML.TransactionBody.new(inputs, outputs, 1_000_000n),
    witnessSet,
    true,
    undefined,
  ).to_cbor_bytes().length;
  const fitsSinglePublicationCarriage =
    fitsItemPublicationBound &&
    publicationTransactionBytes <= maxL1TransactionBytes;
  return {
    fieldIndex,
    itemIndex,
    itemBytes: itemCbor.length,
    commitmentHex: commitment.toString("hex"),
    carriage: fitsItemPublicationBound
      ? selectValidationCompleteItemCarriageV1(itemCbor.length)
      : "reference",
    boundedFallbackChunkCount: midgardBoundedItemChunkCountV1(itemCbor.length),
    maxReliableDirectCompleteItemBytes,
    maxSinglePublicationCompleteItemBytes,
    maxL1TransactionBytes,
    publicationDatumBytes: publication.datumCbor.length / 2,
    publicationTransactionBytes,
    fitsDirectCarriage,
    fitsSinglePublicationCarriage,
    requiresBoundedFallback:
      !fitsDirectCarriage && !fitsSinglePublicationCarriage,
  };
};

export const measureCollateralizedPlutusFeasibilityCandidateV1 = (
  signedCardanoCborHex: string,
): {
  readonly signedBytes: number;
  readonly inputCount: number;
  readonly outputCount: number;
  readonly fee: bigint;
  readonly collateralInputOutRefs: readonly string[];
  readonly collateralReturnCborHex: string | undefined;
  readonly totalCollateral: bigint | undefined;
  readonly scriptDataHashHex: string | undefined;
  readonly vkeyWitnessCount: number;
  readonly plutusV3ScriptCount: number;
  readonly redeemerCount: number;
  readonly redeemersCborHex: string;
  readonly redeemerTags: readonly number[];
  readonly redeemerIndexes: readonly bigint[];
  readonly redeemerDataCborHexes: readonly string[];
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
} => {
  const transaction = CML.Transaction.from_cbor_hex(signedCardanoCborHex);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const collateralInputs = body.collateral_inputs();
  const collateralInputOutRefs: string[] = [];
  for (let index = 0; index < (collateralInputs?.len() ?? 0); index += 1) {
    const input = collateralInputs!.get(index);
    collateralInputOutRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const redeemers = witnessSet.redeemers();
  if (redeemers === undefined) {
    throw new Error(
      "Collateralized Cardano feasibility candidate has no redeemers",
    );
  }
  const flatRedeemers = redeemers.to_flat_format();
  const redeemerTags: number[] = [];
  const redeemerIndexes: bigint[] = [];
  const redeemerDataCborHexes: string[] = [];
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < flatRedeemers.len(); index += 1) {
    const redeemer = flatRedeemers.get(index);
    redeemerTags.push(redeemer.tag());
    redeemerIndexes.push(redeemer.index());
    redeemerDataCborHexes.push(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        redeemer.data().to_cbor_hex(),
      ),
    );
    executionMemory += redeemer.ex_units().mem();
    executionSteps += redeemer.ex_units().steps();
  }
  return {
    signedBytes: signedCardanoCborHex.length / 2,
    inputCount: body.inputs().len(),
    outputCount: body.outputs().len(),
    fee: body.fee(),
    collateralInputOutRefs,
    collateralReturnCborHex: body.collateral_return()?.to_cbor_hex(),
    totalCollateral: body.total_collateral(),
    scriptDataHashHex: body.script_data_hash()?.to_hex(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    plutusV3ScriptCount: witnessSet.plutus_v3_scripts()?.len() ?? 0,
    redeemerCount: flatRedeemers.len(),
    redeemersCborHex: Buffer.from(redeemers.to_cbor_bytes()).toString("hex"),
    redeemerTags,
    redeemerIndexes,
    redeemerDataCborHexes,
    executionMemory,
    executionSteps,
  };
};
