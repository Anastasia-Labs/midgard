import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeHash32,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
  deriveMidgardTxFieldPreimages,
  MIDGARD_CONSENSUS_LIMITS,
  reconstructMidgardTransaction,
} from "@al-ft/midgard-core";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  unwrapDaPayload,
  wrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { countedMachineTransactionChunkSteps } from "../../src/validation-machine/index.js";
import {
  appendBoundaryCorpusEntry,
  assertSourceMatches,
  makeRetainedPairPayload,
  requireEntry,
  type RetainedClassificationMeasurement,
  type RetainedDaAdmission,
  type RetainedDaBoundaryMeasurement,
} from "./retained-da-boundary.make-retained-pair-payload.js";

const reconstructRetainedClassification = ({
  sourceKind,
  sourceEntry,
  preimageEntry,
  transactionIdHex,
  transactionCommitmentHex,
}: {
  readonly sourceKind: "normal" | "forced";
  readonly sourceEntry: SDK.DaPayloadEntry;
  readonly preimageEntry: SDK.DaPayloadEntry;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
}): RetainedClassificationMeasurement => {
  const retainedSource =
    sourceKind === "normal"
      ? Data.from(sourceEntry[1], SDK.L2TransactionSource)
      : Data.from(sourceEntry[1], SDK.ForcedInclusionTxV1);
  if (
    "verdict" in retainedSource &&
    retainedSource.verdict !== "ForcedTxValid"
  ) {
    throw new Error("forced retained-DA source lost its operator verdict");
  }
  const exactSource: SDK.L2TransactionSource = {
    tx_id: retainedSource.tx_id,
    source:
      "verdict" in retainedSource
        ? retainedSource.submitted_source
        : retainedSource.source,
  };
  const retainedCanonicalCbor = Buffer.from(preimageEntry[1], "hex");
  const retainedTransaction = (
    sourceKind === "forced"
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(retainedCanonicalCbor);
  const retainedTransactionId = computeMidgardNativeTxId(retainedTransaction);
  const retainedProofSource =
    sourceKind === "forced"
      ? deriveMidgardForcedTxProofSource(
          decodeMidgardForcedTxFullFromCanonicalCbor(retainedCanonicalCbor),
        )
      : deriveMidgardNativeTxProofSource(
          decodeMidgardNativeTxFullFromCanonicalCbor(retainedCanonicalCbor),
        );
  const retainedTransactionCommitment = (
    sourceKind === "forced"
      ? computeMidgardForcedTxProofCommitment
      : computeMidgardNativeTxProofCommitment
  )(retainedProofSource);
  if (
    retainedTransactionId.toString("hex") !== transactionIdHex ||
    retainedTransactionCommitment.toString("hex") !== transactionCommitmentHex
  ) {
    throw new Error(
      `${sourceKind} retained-DA transaction identity or commitment changed`,
    );
  }
  assertSourceMatches({
    retainedSource: exactSource,
    transactionIdHex,
    compactCborHex: retainedProofSource.compactCbor.toString("hex"),
    witnessSetCompactCborHex:
      retainedProofSource.witnessSetCompactCbor.toString("hex"),
    fieldPreimageLengthsCborHex:
      retainedProofSource.fieldPreimageLengthsCbor.toString("hex"),
    fieldName: `${sourceKind} retained-DA source`,
  });

  // §4: each field authenticates once against the hash its compact structure
  // carries, and `reconstructMidgardTransaction` performs all nine checks. The
  // machine's chunk steps are still counted here, and still measured — they are
  // its trace, not a publication claim (see `countedMachineFieldChunkStepsV1`).
  const chunkProofs = countedMachineTransactionChunkSteps(
    retainedCanonicalCbor,
    sourceKind,
  );
  const reconstructed = reconstructMidgardTransaction({
    sourceKind,
    transactionId: retainedTransactionId,
    transactionCommitment: retainedTransactionCommitment,
    source: retainedProofSource,
    fieldPreimages: deriveMidgardTxFieldPreimages(
      retainedCanonicalCbor,
      sourceKind,
    ).map((field) => field.preimageCbor),
  });
  if (!reconstructed.equals(retainedCanonicalCbor)) {
    throw new Error(
      `${sourceKind} retained-DA terminal fold changed canonical transaction bytes`,
    );
  }
  return {
    sourceKind,
    retainedPreimageBytes: retainedCanonicalCbor.length,
    revealStepCount: chunkProofs.length,
    reconstructedCanonicalBytes: reconstructed.length,
    retainedPreimageDigestHex: computeHash32(retainedCanonicalCbor).toString(
      "hex",
    ),
    reconstructedCanonicalDigestHex:
      computeHash32(reconstructed).toString("hex"),
    transactionIdHex: retainedTransactionId.toString("hex"),
    transactionCommitmentHex: retainedTransactionCommitment.toString("hex"),
  };
};

/**
 * Stores one canonical maximum-shape transaction in both V1 DA
 * classification maps, passes it through the mandatory envelope and SDK
 * decoder, and independently executes every bounded reveal plus the terminal
 * reconstruction fold from each retained preimage.
 *
 * This is deliberately a boundary harness, not a full header/root/node test.
 * Strict payload-root and coverage validation remains exercised in the DA
 * committee package.
 */
export const exerciseMidgardRetainedDaCanonicalBoundary = async ({
  canonicalTransactionCbor,
  corpusLabel,
  productionAdmission: admission = "required",
  canonicalMaterialSidecarCbor,
  sourceRawScriptAuditHash,
  resolvedReferenceUtxos,
}: {
  readonly canonicalTransactionCbor: Uint8Array;
  readonly corpusLabel?: string;
  readonly productionAdmission?: RetainedDaAdmission;
  readonly canonicalMaterialSidecarCbor?: Uint8Array;
  readonly sourceRawScriptAuditHash?: string;
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
}): Promise<RetainedDaBoundaryMeasurement> => {
  if (
    (canonicalMaterialSidecarCbor === undefined) !==
    (sourceRawScriptAuditHash === undefined)
  ) {
    throw new Error(
      "retained-DA corpus program material and raw-source audit identity must be provided together",
    );
  }
  if (
    admission === "diagnostic-synthetic-script-witnesses" &&
    corpusLabel !== "mixed-size-balanced"
  ) {
    throw new Error(
      "diagnostic synthetic script witnesses are permitted only for mixed-size-balanced",
    );
  }
  if (
    resolvedReferenceUtxos !== undefined &&
    corpusLabel !== "maximum-reference-inputs"
  ) {
    throw new Error(
      "resolved reference UTxOs are permitted only for maximum-reference-inputs",
    );
  }
  const exactCanonicalTransactionCbor = Buffer.from(canonicalTransactionCbor);
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
    exactCanonicalTransactionCbor,
  );
  const transactionId = computeMidgardNativeTxId(transaction);
  const source = deriveMidgardNativeTxProofSource(transaction);
  const transactionCommitment = computeMidgardNativeTxProofCommitment(source);
  const transactionIdHex = transactionId.toString("hex");
  const forcedOrderIdHex = Data.to(
    {
      transactionId: transactionIdHex,
      outputIndex: 0n,
    },
    SDK.OutputReference,
  );
  const transactionCommitmentHex = transactionCommitment.toString("hex");
  appendBoundaryCorpusEntry({
    corpusLabel,
    productionAdmission: admission,
    transactionIdHex,
    transactionCommitmentHex,
    canonicalTransactionCbor: exactCanonicalTransactionCbor,
    canonicalMaterialSidecarCbor,
    sourceRawScriptAuditHash,
    resolvedReferenceUtxos,
  });
  const retainedSource: SDK.L2TransactionSource = {
    tx_id: transactionIdHex,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  const forcedTransaction =
    materializeMidgardForcedTxFromCanonical(transaction);
  const forcedProofSource = deriveMidgardForcedTxProofSource(forcedTransaction);
  const forcedTransactionCommitmentHex =
    computeMidgardForcedTxProofCommitment(forcedProofSource).toString("hex");
  const payload = makeRetainedPairPayload({
    transactionIdHex,
    forcedOrderIdHex,
    transactionCborHex: exactCanonicalTransactionCbor.toString("hex"),
    source: retainedSource,
    forcedTransactionCborHex:
      encodeMidgardForcedTxCanonical(forcedTransaction).toString("hex"),
    forcedSource: {
      tx_id: transactionIdHex,
      submitted_source: {
        compact_cbor: forcedProofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          forcedProofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          forcedProofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: "ForcedTxValid",
    },
  });
  const innerPayloadCbor = SDK.encodeDaPayload(payload);
  const storedPayloadCbor = await wrapDaPayload(innerPayloadCbor, {
    mode: "identity",
  });
  const unwrapped = await unwrapDaPayload(storedPayloadCbor, {
    maxPayloadBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
  });
  if (!unwrapped.innerBytes.equals(innerPayloadCbor)) {
    throw new Error("retained DA envelope changed the canonical inner payload");
  }
  const decoded = SDK.decodeDaPayload(unwrapped.innerBytes);
  const normalSourceEntry = requireEntry(
    decoded.block_body.transactions,
    transactionIdHex,
    "transactions",
  );
  const normalPreimageEntry = requireEntry(
    decoded.block_body.transaction_preimages,
    transactionIdHex,
    "transaction_preimages",
  );
  const forcedSourceEntry = requireEntry(
    decoded.block_body.forced_transactions,
    forcedOrderIdHex,
    "forced_transactions",
  );
  const forcedPreimageEntry = requireEntry(
    decoded.block_body.forced_transaction_preimages,
    forcedOrderIdHex,
    "forced_transaction_preimages",
  );
  const normal = reconstructRetainedClassification({
    sourceKind: "normal",
    sourceEntry: normalSourceEntry,
    preimageEntry: normalPreimageEntry,
    transactionIdHex,
    transactionCommitmentHex,
  });
  const forced = reconstructRetainedClassification({
    sourceKind: "forced",
    sourceEntry: forcedSourceEntry,
    preimageEntry: forcedPreimageEntry,
    transactionIdHex,
    transactionCommitmentHex: forcedTransactionCommitmentHex,
  });
  const measurement = {
    forcedTransactionCommitmentHex,
    transactionIdHex,
    transactionCommitmentHex,
    innerPayloadBytes: innerPayloadCbor.length,
    storedPayloadBytes: storedPayloadCbor.length,
    normal,
    forced,
  };
  if (process.env.MIDGARD_PRINT_RETAINED_DA === "1") {
    console.info(JSON.stringify({ retainedDaBoundaryV1: measurement }));
  }
  return measurement;
};

export const exerciseMidgardRetainedDaBoundary = ({
  signedCardanoCborHex,
  corpusLabel,
  resolvedReferenceUtxos,
}: {
  readonly signedCardanoCborHex: string;
  readonly corpusLabel?: string;
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
}): Promise<RetainedDaBoundaryMeasurement> =>
  exerciseMidgardRetainedDaCanonicalBoundary({
    canonicalTransactionCbor: cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(signedCardanoCborHex, "hex"),
    ),
    corpusLabel,
    resolvedReferenceUtxos,
  });
