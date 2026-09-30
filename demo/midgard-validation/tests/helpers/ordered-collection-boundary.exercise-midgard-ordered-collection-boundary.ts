import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeHash32,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardTxFieldPreimages,
  encodeCbor,
  hashMidgardValidationWorkWitness,
  reconstructMidgardTransaction,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import { countedMachineTransactionChunkSteps } from "../../src/validation-machine/index.js";
import { encodeValidationAuxiliaryWitnessCbor } from "../../src/validation-machine-data.js";
import { admissibleFieldCarriage } from "./ordered-collection-boundary.build-collateral-free-midgard-schema-parallel-candidate.js";
import { type MidgardOrderedCollectionBoundaryMeasurement } from "./ordered-collection-boundary.find-signed-cardano-collection-boundary.js";

/**
 * Converts exact signed Cardano CBOR through the production bridge, verifies
 * every reveal for one typed field, and then runs the complete canonical
 * transaction chunk sequence through the exact terminal reconstruction fold.
 */
export const exerciseMidgardOrderedCollectionBoundary = ({
  signedCardanoCborHex,
  fieldIndex,
}: {
  readonly signedCardanoCborHex: string;
  readonly fieldIndex: number;
}): MidgardOrderedCollectionBoundaryMeasurement => {
  const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    Buffer.from(signedCardanoCborHex, "hex"),
  );
  const nativeTx =
    decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(nativeCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(nativeTx);
  const transactionCommitment = computeMidgardNativeTxProofCommitment(source);
  const field = deriveMidgardTxFieldPreimages(nativeCanonicalCbor).find(
    (candidate) => candidate.fieldIndex === fieldIndex,
  );
  if (field === undefined) {
    throw new Error(
      `Canonical Midgard transaction does not contain field ${fieldIndex.toString()}`,
    );
  }
  const completeChunks =
    countedMachineTransactionChunkSteps(nativeCanonicalCbor);
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
  const reconstructed = reconstructMidgardTransaction({
    transactionId,
    transactionCommitment,
    source,
    fieldPreimages: deriveMidgardTxFieldPreimages(nativeCanonicalCbor).map(
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
    // #597: a step's reveal is its §8 carriage, and which carriage is not a
    // choice — `selectMidgardFieldCarriageTier` is §8.4's partition, so a
    // preimage of this length has exactly one admissible tier. Measuring the
    // admitted tier is what makes this figure the reveal a prover actually
    // submits: tier 1 carries the preimage in the redeemer, tiers 2–3 carry
    // reference-input indices and are O(1) in field size, which is why every
    // field fits the envelope.
    //
    // The indices below are representative, because this is a *size*
    // measurement and a positional index is a small integer whichever UTxO it
    // names. Since #600 that shape is supplied the same way production supplies
    // a real one — as the carriage resolver `encodeValidationAuxiliaryWitnessCbor`
    // takes — so this measurement sits on the production seam rather than beside
    // it. Resolving *real* indices needs a concrete transaction (§8.7 addresses
    // carriage by content), which a size measurement has no reason to build.
    maxRevealBytes: Math.max(
      ...fieldChunks.map(
        (chunk) =>
          encodeValidationAuxiliaryWitnessCbor(
            {
              kind: "transactionFieldChunk",
              fieldIndex: chunk.collectionProof.fieldIndex,
              itemIndex: chunk.collectionProof.itemIndex,
              fieldPreimage: field.preimageCbor,
            },
            ({ fieldPreimage }) => admissibleFieldCarriage(fieldPreimage),
          ).length,
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
      preWorkRootHex: hashMidgardValidationWorkWitness({
        phase: "canonicalDecode",
        programCounter: 40,
        witnessCbor: workWitnessCbor,
      }).toString("hex"),
      postWorkRootHex: hashMidgardValidationWorkWitness({
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
