import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeHash32,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldChunks,
  deriveMidgardV1TxFieldPreimages,
  reconstructMidgardTransactionV1FromChunks,
  verifyMidgardV1TxFieldChunk,
} from "@al-ft/midgard-core";
import {
  CML,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";

import { encodeValidationAuxiliaryWitnessCborV1 } from "../../src/validation-machine-data.js";

export const CARDANO_BOUNDARY_MAX_TX_SIZE_V1 = 16_384;
export const CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 = 5_000;
export const CARDANO_BOUNDARY_OBSERVER_TTL_V1 = 10_000n;
export const CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 = 20_000n;
export const CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT_V1 =
  100_000_000n;
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
    throw new Error(
      "Deterministic Cardano signer index must fit uint32",
    );
  }
  const encodedIndex = Buffer.alloc(4);
  encodedIndex.writeUInt32BE(signerIndex);
  return CML.PrivateKey.from_normal_bytes(
    computeHash32(
      Buffer.concat([
        CARDANO_BOUNDARY_SIGNER_KEY_DOMAIN_V1,
        encodedIndex,
      ]),
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

export type MidgardOrderedCollectionBoundaryMeasurementV1 = {
  readonly nativeCanonicalBytes: number;
  readonly fieldBytes: number;
  readonly itemCount: number;
  readonly revealStepCount: number;
  readonly completeFoldStepCount: number;
  readonly maxRevealBytes: number;
  readonly maxChunkBytes: number;
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
    const midpoint = Math.floor(
      (acceptedItemCount + rejectedItemCount) / 2,
    );
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
    const paymentTotal =
      BigInt(requestedOutputCount) * lovelacePerOutput;
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
    (_, signerIndex) =>
      deterministicCardanoBoundaryPrivateKeyV1(signerIndex),
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
  if (
    !Number.isSafeInteger(requestedInputCount) ||
    requestedInputCount <= 0
  ) {
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
    throw new Error(
      "Requested Cardano reference-input count must be positive",
    );
  }
  const requiredInputSupply = requestedReferenceInputCount + 1;
  if (requiredInputSupply > availableInputs.length) {
    throw new Error(
      `Requested one funding input and ${requestedReferenceInputCount.toString()} Cardano reference inputs, but only ${availableInputs.length.toString()} real emulator UTxOs are available`,
    );
  }
  const fundingInput = availableInputs[0]!;
  const referenceInputs = availableInputs.slice(
    1,
    requiredInputSupply,
  );
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
      CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 +
        BigInt(scriptIndex),
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
  const makeObserverScript = (
    observerIndex: number,
  ): CML.NativeScript =>
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
    for (
      let observerIndex = 0;
      observerIndex < requestedObserverCount;
      observerIndex += 1
    ) {
      const script = makeObserverScript(observerIndex);
      withdrawals.insert(
        CML.RewardAddress.new(
          0,
          CML.Credential.new_script(script.hash()),
        ),
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
    throw new Error(
      "Requested Cardano mint policy count must be positive",
    );
  }
  if (!Number.isSafeInteger(maxValueSize) || maxValueSize <= 0) {
    throw new Error(
      "Cardano maxValueSize must be a positive safe integer",
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
        CML.AssetName.from_raw_bytes(
          CARDANO_BOUNDARY_MINT_ASSET_NAME_V1,
        ),
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
          makeValue(candidateGroup, groupLovelace).to_cbor_bytes()
            .length <= maxValueSize
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
          throw new Error(
            "One Cardano mint policy entry exceeds maxValueSize",
          );
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
        CML.AssetName.from_raw_bytes(
          CARDANO_BOUNDARY_MINT_ASSET_NAME_V1,
        ),
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
    const transaction = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    return {
      transaction,
      cborHex: transaction.to_cbor_hex(),
    };
  };

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const signed = makeSigned(fee);
    const nextFee = CML.min_no_script_fee(
      signed.transaction,
      linearFee,
    );
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
  const nativeCanonicalCbor =
    cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(signedCardanoCborHex, "hex"),
    );
  const nativeTx =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeCanonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
      nativeCanonicalCbor,
    );
  const transactionId = computeMidgardNativeTxIdV1(nativeTx);
  const transactionCommitment =
    computeMidgardNativeTxProofCommitmentV1(source);
  const field = deriveMidgardV1TxFieldPreimages(nativeCanonicalCbor).find(
    (candidate) => candidate.fieldIndex === fieldIndex,
  );
  if (field === undefined) {
    throw new Error(
      `Canonical Midgard transaction does not contain field ${fieldIndex.toString()}`,
    );
  }
  const completeChunks = deriveMidgardV1TxFieldChunks(nativeCanonicalCbor);
  const fieldChunks = completeChunks.filter(
    (chunk) => chunk.proof.fieldIndex === fieldIndex,
  );
  if (fieldChunks.length === 0) {
    throw new Error(
      `Canonical Midgard field ${fieldIndex.toString()} has no reveal steps`,
    );
  }

  for (const chunk of fieldChunks) {
    verifyMidgardV1TxFieldChunk({
      transactionId,
      transactionCommitment,
      source,
      collectionProof: chunk.collectionProof,
      proof: chunk.proof,
    });
  }

  const reconstructed = reconstructMidgardTransactionV1FromChunks({
    transactionId,
    transactionCommitment,
    source,
    chunkProofs: completeChunks,
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

  return {
    nativeCanonicalBytes: nativeCanonicalCbor.length,
    fieldBytes: field.preimageCbor.length,
    itemCount: fieldChunks[0]!.collectionProof.itemCount,
    revealStepCount: fieldChunks.length,
    completeFoldStepCount: completeChunks.length,
    maxRevealBytes: Math.max(
      ...fieldChunks.map((chunk) =>
        encodeValidationAuxiliaryWitnessCborV1({
          kind: "transactionFieldChunk",
          collectionProof: chunk.collectionProof,
          chunkProof: chunk.proof,
        }).length,
      ),
    ),
    maxChunkBytes: Math.max(
      ...fieldChunks.map((chunk) => chunk.proof.chunk.length),
    ),
  };
};

export const measureSignedCardanoOutputsV1 = (
  signedCardanoCborHex: string,
): {
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
  return {
    outputCount: transaction.body().outputs().len(),
    vkeyWitnessCount:
      transaction.witness_set().vkeywitnesses()?.len() ?? 0,
  };
};

export const measureSignedCardanoSignersV1 = (
  signedCardanoCborHex: string,
): {
  readonly requiredSignerCount: number;
  readonly vkeyWitnessCount: number;
  readonly outputCount: number;
} => {
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
  return {
    requiredSignerCount:
      transaction.body().required_signers()?.len() ?? 0,
    vkeyWitnessCount:
      transaction.witness_set().vkeywitnesses()?.len() ?? 0,
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
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
  return {
    inputCount: transaction.body().inputs().len(),
    vkeyWitnessCount:
      transaction.witness_set().vkeywitnesses()?.len() ?? 0,
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
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
  return {
    inputCount: transaction.body().inputs().len(),
    referenceInputCount:
      transaction.body().reference_inputs()?.len() ?? 0,
    vkeyWitnessCount:
      transaction.witness_set().vkeywitnesses()?.len() ?? 0,
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
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
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
        throw new Error(
          "Measured Cardano observer withdrawal has no amount",
        );
      }
      rewardAddressBech32s.push(
        rewardAddress.to_address().to_bech32(),
      );
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
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
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
      for (
        let assetIndex = 0;
        assetIndex < assetNames.len();
        assetIndex += 1
      ) {
        const quantity = assets.get(assetNames.get(assetIndex));
        if (quantity === undefined) {
          throw new Error(
            "Measured Cardano mint policy asset has no quantity",
          );
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
        throw new Error(
          "Measured Cardano mint output policy has no assets",
        );
      }
      const assetNames = assets.keys();
      outputPolicyHashHexes.push(policy.to_hex());
      for (
        let assetIndex = 0;
        assetIndex < assetNames.len();
        assetIndex += 1
      ) {
        const assetName = assetNames.get(assetIndex);
        const quantity = assets.get(assetName);
        if (quantity === undefined) {
          throw new Error(
            "Measured Cardano mint output asset has no quantity",
          );
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
  readonly redeemerDataCborHexes: readonly string[];
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
} => {
  const transaction = CML.Transaction.from_cbor_hex(
    signedCardanoCborHex,
  );
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const collateralInputs = body.collateral_inputs();
  const collateralInputOutRefs: string[] = [];
  for (
    let index = 0;
    index < (collateralInputs?.len() ?? 0);
    index += 1
  ) {
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
  const redeemerDataCborHexes: string[] = [];
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < flatRedeemers.len(); index += 1) {
    const redeemer = flatRedeemers.get(index);
    redeemerDataCborHexes.push(redeemer.data().to_cbor_hex());
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
    plutusV3ScriptCount:
      witnessSet.plutus_v3_scripts()?.len() ?? 0,
    redeemerCount: flatRedeemers.len(),
    redeemersCborHex: Buffer.from(
      redeemers.to_cbor_bytes(),
    ).toString("hex"),
    redeemerDataCborHexes,
    executionMemory,
    executionSteps,
  };
};
