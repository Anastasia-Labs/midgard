import {
  aikenSerialisedPlutusDataCbor,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardV1TxFieldChunks,
  MIDGARD_CONSENSUS_LIMITS_V1,
  reconstructMidgardTransactionV1FromChunks,
  verifyMidgardV1TxFieldChunk,
} from "@al-ft/midgard-core";
import {
  unwrapDaPayloadV1,
  wrapDaPayloadV1,
} from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const ZERO_HASH_28 = "00".repeat(28);
const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

type RetainedClassificationMeasurementV1 = {
  readonly sourceKind: "normal" | "forced";
  readonly retainedPreimageBytes: number;
  readonly revealStepCount: number;
  readonly reconstructedCanonicalBytes: number;
};

export type RetainedDaBoundaryMeasurementV1 = {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly innerPayloadBytes: number;
  readonly storedPayloadBytes: number;
  readonly normal: RetainedClassificationMeasurementV1;
  readonly forced: RetainedClassificationMeasurementV1;
};

const sourceValueHex = (
  source: SDK.L2TransactionSourceV1,
): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(
      source as never,
      SDK.L2TransactionSourceV1Schema as never,
    ),
  );

const forcedSourceValueHex = (
  source: SDK.L2TransactionSourceV1,
): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(
      {
        ...source,
        operator_validity: "TxIsValid",
      } as never,
      SDK.ForcedInclusionTxV1Schema as never,
    ),
  );

const makeRetainedPairPayloadV1 = ({
  transactionIdHex,
  forcedOrderIdHex,
  transactionCborHex,
  source,
}: {
  readonly transactionIdHex: string;
  readonly forcedOrderIdHex: string;
  readonly transactionCborHex: string;
  readonly source: SDK.L2TransactionSourceV1;
}): SDK.DaPayloadV1 => {
  const counts: SDK.DaPayloadCountsV1 = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 2n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot: EMPTY_ROOT,
    utxosRoot: EMPTY_ROOT,
    withdrawalsRoot: EMPTY_ROOT,
    forcedTransactionsRoot: EMPTY_ROOT,
    transactionsRoot: EMPTY_ROOT,
    depositsRoot: EMPTY_ROOT,
    transitionTraceRoot: EMPTY_ROOT,
    eventToStepRoot: EMPTY_ROOT,
    validationTracesRoot: EMPTY_ROOT,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    l2TransactionCount: counts.l2TransactionCount,
    depositCount: counts.depositCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
    validationTraceCount: counts.validationTraceCount,
    startTime: 0n,
    endTime: 1n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: ZERO_HASH_28,
    operatorVkey: ZERO_HASH_28,
    protocolVersion: 1n,
  };
  return {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: ZERO_HASH_28,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [
        [forcedOrderIdHex, forcedSourceValueHex(source)],
      ],
      transactions: [
        [transactionIdHex, sourceValueHex(source)],
      ],
      transaction_preimages: [
        [transactionIdHex, transactionCborHex],
      ],
      forced_transaction_preimages: [
        [forcedOrderIdHex, transactionCborHex],
      ],
      cek_program_material: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      counts,
    },
  };
};

const requireEntry = (
  entries: readonly SDK.DaPayloadEntry[],
  keyHex: string,
  fieldName: string,
): SDK.DaPayloadEntry => {
  const matches = entries.filter(
    ([entryKeyHex]) => entryKeyHex === keyHex,
  );
  if (matches.length !== 1) {
    throw new Error(
      `${fieldName} must retain exactly one entry for ${keyHex}`,
    );
  }
  return matches[0]!;
};

const assertSourceMatches = ({
  retainedSource,
  transactionIdHex,
  transactionCommitmentHex,
  compactCborHex,
  witnessSetCompactCborHex,
  fieldPreimageLengthsCborHex,
  fieldName,
}: {
  readonly retainedSource: SDK.L2TransactionSourceV1;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly fieldPreimageLengthsCborHex: string;
  readonly fieldName: string;
}): void => {
  if (
    retainedSource.tx_id !== transactionIdHex ||
    retainedSource.transaction_commitment !==
      transactionCommitmentHex ||
    retainedSource.source.compact_cbor !== compactCborHex ||
    retainedSource.source.witness_set_compact_cbor !==
      witnessSetCompactCborHex ||
    retainedSource.source.field_preimage_lengths_cbor !==
      fieldPreimageLengthsCborHex
  ) {
    throw new Error(
      `${fieldName} did not retain the exact transaction proof source`,
    );
  }
};

const reconstructRetainedClassificationV1 = ({
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
}): RetainedClassificationMeasurementV1 => {
  const retainedSource =
    sourceKind === "normal"
      ? Data.from(
          sourceEntry[1],
          SDK.L2TransactionSourceV1Schema as never,
        )
      : Data.from(
          sourceEntry[1],
          SDK.ForcedInclusionTxV1Schema as never,
        );
  const exactSource = retainedSource as SDK.L2TransactionSourceV1 & {
    readonly operator_validity?: string;
  };
  if (
    sourceKind === "forced" &&
    exactSource.operator_validity !== "TxIsValid"
  ) {
    throw new Error(
      "forced retained-DA source lost its operator validity",
    );
  }

  const retainedCanonicalCbor = Buffer.from(
    preimageEntry[1],
    "hex",
  );
  const retainedTransaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(
      retainedCanonicalCbor,
    );
  const retainedTransactionId =
    computeMidgardNativeTxIdV1(retainedTransaction);
  const retainedProofSource =
    deriveMidgardNativeTxProofSourceV1(retainedTransaction);
  const retainedTransactionCommitment =
    computeMidgardNativeTxProofCommitmentV1(
      retainedProofSource,
    );
  if (
    retainedTransactionId.toString("hex") !== transactionIdHex ||
    retainedTransactionCommitment.toString("hex") !==
      transactionCommitmentHex
  ) {
    throw new Error(
      `${sourceKind} retained-DA transaction identity or commitment changed`,
    );
  }
  assertSourceMatches({
    retainedSource: exactSource,
    transactionIdHex,
    transactionCommitmentHex,
    compactCborHex: retainedProofSource.compactCbor.toString("hex"),
    witnessSetCompactCborHex:
      retainedProofSource.witnessSetCompactCbor.toString("hex"),
    fieldPreimageLengthsCborHex:
      retainedProofSource.fieldPreimageLengthsCbor.toString("hex"),
    fieldName: `${sourceKind} retained-DA source`,
  });

  const chunkProofs = deriveMidgardV1TxFieldChunks(
    retainedCanonicalCbor,
  );
  for (const chunk of chunkProofs) {
    verifyMidgardV1TxFieldChunk({
      transactionId: retainedTransactionId,
      transactionCommitment: retainedTransactionCommitment,
      source: retainedProofSource,
      collectionProof: chunk.collectionProof,
      proof: chunk.proof,
    });
  }
  const reconstructed = reconstructMidgardTransactionV1FromChunks({
    transactionId: retainedTransactionId,
    transactionCommitment: retainedTransactionCommitment,
    source: retainedProofSource,
    chunkProofs,
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
export const exerciseMidgardRetainedDaCanonicalBoundaryV1 = async ({
  canonicalTransactionCbor,
}: {
  readonly canonicalTransactionCbor: Uint8Array;
}): Promise<RetainedDaBoundaryMeasurementV1> => {
  const exactCanonicalTransactionCbor = Buffer.from(
    canonicalTransactionCbor,
  );
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(
      exactCanonicalTransactionCbor,
    );
  const transactionId = computeMidgardNativeTxIdV1(transaction);
  const source = deriveMidgardNativeTxProofSourceV1(transaction);
  const transactionCommitment =
    computeMidgardNativeTxProofCommitmentV1(source);
  const transactionIdHex = transactionId.toString("hex");
  const forcedOrderIdHex = Data.to(
    {
      transactionId: transactionIdHex,
      outputIndex: 0n,
    },
    SDK.OutputReference,
  );
  const transactionCommitmentHex =
    transactionCommitment.toString("hex");
  const retainedSource: SDK.L2TransactionSourceV1 = {
    tx_id: transactionIdHex,
    transaction_commitment: transactionCommitmentHex,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  const payload = makeRetainedPairPayloadV1({
    transactionIdHex,
    forcedOrderIdHex,
    transactionCborHex:
      exactCanonicalTransactionCbor.toString("hex"),
    source: retainedSource,
  });
  const innerPayloadCbor = SDK.encodeDaPayloadV1(payload);
  const storedPayloadCbor = await wrapDaPayloadV1(
    innerPayloadCbor,
    { mode: "identity" },
  );
  const unwrapped = await unwrapDaPayloadV1(storedPayloadCbor, {
    maxPayloadBytes:
      MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes,
  });
  if (!unwrapped.innerBytes.equals(innerPayloadCbor)) {
    throw new Error(
      "retained DA envelope changed the canonical inner payload",
    );
  }
  const decoded = SDK.decodeDaPayloadV1(
    unwrapped.innerBytes,
  );
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
  const normal = reconstructRetainedClassificationV1({
    sourceKind: "normal",
    sourceEntry: normalSourceEntry,
    preimageEntry: normalPreimageEntry,
    transactionIdHex,
    transactionCommitmentHex,
  });
  const forced = reconstructRetainedClassificationV1({
    sourceKind: "forced",
    sourceEntry: forcedSourceEntry,
    preimageEntry: forcedPreimageEntry,
    transactionIdHex,
    transactionCommitmentHex,
  });
  return {
    transactionIdHex,
    transactionCommitmentHex,
    innerPayloadBytes: innerPayloadCbor.length,
    storedPayloadBytes: storedPayloadCbor.length,
    normal,
    forced,
  };
};

export const exerciseMidgardRetainedDaBoundaryV1 = ({
  signedCardanoCborHex,
}: {
  readonly signedCardanoCborHex: string;
}): Promise<RetainedDaBoundaryMeasurementV1> =>
  exerciseMidgardRetainedDaCanonicalBoundaryV1({
    canonicalTransactionCbor:
      cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(signedCardanoCborHex, "hex"),
      ),
  });
