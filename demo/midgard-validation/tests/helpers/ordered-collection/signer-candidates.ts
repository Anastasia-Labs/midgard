import { CML } from "@lucid-evolution/lucid";

import { type SignedCardanoCollectionCandidate } from "./parameters.js";
import { deterministicCardanoBoundaryPrivateKey } from "./signing-material.js";

export const buildSignedCardanoSignersCandidate = async ({
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
}): Promise<SignedCardanoCollectionCandidate> => {
  if (
    !Number.isSafeInteger(requestedSignerCount) ||
    requestedSignerCount <= 0
  ) {
    throw new Error("Requested Cardano signer count must be positive");
  }
  const privateKeys = Array.from(
    { length: requestedSignerCount },
    (_, signerIndex) => deterministicCardanoBoundaryPrivateKey(signerIndex),
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
