import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldChunks,
  deriveMidgardV1TxFieldPreimages,
  reconstructMidgardTransactionV1FromChunks,
  verifyMidgardV1TxFieldChunk,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import { encodeValidationAuxiliaryWitnessCborV1 } from "../../src/validation-machine-data.js";

export const CARDANO_BOUNDARY_MAX_TX_SIZE_V1 = 16_384;
export const CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 = 5_000;

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
