import { CML, type UTxO } from "@lucid-evolution/lucid";

import {
  CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE,
  type SignedCardanoCollectionCandidate,
} from "./ordered-collection-boundary.find-signed-cardano-collection-boundary.js";

export const buildSignedCardanoSpendInputsCandidate = async ({
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
}): Promise<SignedCardanoCollectionCandidate> => {
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

export const buildSignedCardanoReferenceInputsCandidate = async ({
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
}): Promise<SignedCardanoCollectionCandidate> => {
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

export const makeCardanoBoundaryNativeScript = ({
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
      CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE + BigInt(scriptIndex),
    ),
  );
  return CML.NativeScript.new_script_all(clauses);
};
