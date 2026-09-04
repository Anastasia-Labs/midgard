import { appendFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  aikenSerialisedPlutusDataCbor,
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeHash32,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardTxFieldPreimages,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  mergeMidgardCekProgramMaterialSidecars,
  MIDGARD_CONSENSUS_LIMITS,
  midgardFieldCommitment,
  reconstructMidgardTransaction,
} from "@al-ft/midgard-core";
import {
  unwrapDaPayload,
  wrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { buildMidgardCanonicalScriptArtifact } from "../../src/cek-program.js";
import { countedMachineTransactionChunkSteps } from "../../src/validation-machine/index.js";

const ZERO_HASH_28 = "00".repeat(28);
const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

export type RetainedDaAdmission =
  | "required"
  | "diagnostic-synthetic-script-witnesses";

const appendBoundaryCorpusEntry = ({
  corpusLabel,
  productionAdmission: admission,
  transactionIdHex,
  transactionCommitmentHex,
  canonicalTransactionCbor,
  canonicalMaterialSidecarCbor,
  sourceRawScriptAuditHash,
  resolvedReferenceUtxos,
}: {
  readonly corpusLabel: string | undefined;
  readonly productionAdmission: RetainedDaAdmission;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalTransactionCbor: Buffer;
  readonly canonicalMaterialSidecarCbor: Uint8Array | undefined;
  readonly sourceRawScriptAuditHash: string | undefined;
  readonly resolvedReferenceUtxos: readonly SDK.DaPayloadEntry[] | undefined;
}): void => {
  const corpusPath = process.env.MIDGARD_BOUNDARY_CORPUS_JSONL;
  if (corpusPath === undefined || corpusLabel === undefined) {
    return;
  }
  if (!isAbsolute(corpusPath)) {
    throw new Error("MIDGARD_BOUNDARY_CORPUS_JSONL must be an absolute path");
  }
  appendFileSync(
    corpusPath,
    `${JSON.stringify({
      label: corpusLabel,
      productionAdmission: admission,
      transactionIdHex,
      transactionCommitmentHex,
      canonicalCborHex: canonicalTransactionCbor.toString("hex"),
      ...(canonicalMaterialSidecarCbor === undefined
        ? {}
        : {
            canonicalMaterialSidecarCborHex: Buffer.from(
              canonicalMaterialSidecarCbor,
            ).toString("hex"),
          }),
      ...(sourceRawScriptAuditHash === undefined
        ? {}
        : { sourceRawScriptAuditHash }),
      ...(resolvedReferenceUtxos === undefined
        ? {}
        : { resolvedReferenceUtxos }),
    })}\n`,
    "utf8",
  );
};

type RetainedClassificationMeasurement = {
  readonly sourceKind: "normal" | "forced";
  readonly retainedPreimageBytes: number;
  readonly revealStepCount: number;
  readonly reconstructedCanonicalBytes: number;
  /**
   * Blake2b-256 digests of the retained preimage and of the transaction
   * rebuilt by the terminal fold. Byte equality is already enforced inside
   * this harness; exposing both digests lets a boundary case assert canonical
   * signed-byte identity explicitly instead of only counting bytes.
   */
  readonly retainedPreimageDigestHex: string;
  readonly reconstructedCanonicalDigestHex: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
};

export type RetainedDaBoundaryMeasurement = {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly innerPayloadBytes: number;
  readonly storedPayloadBytes: number;
  readonly normal: RetainedClassificationMeasurement;
  readonly forced: RetainedClassificationMeasurement;
};

export type RetainedDaCanonicalScriptProjection = {
  readonly canonicalTransactionCbor: Buffer;
  readonly canonicalMaterialSidecarCbor: Buffer;
  readonly sourceRawScriptAuditHash: string;
};

const sourceValueHex = (source: SDK.L2TransactionSource): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(source as never, SDK.L2TransactionSourceSchema as never),
  );

const forcedSourceValueHex = (source: SDK.L2TransactionSource): string =>
  aikenSerialisedPlutusDataCbor(
    Data.to(
      {
        ...source,
        verdict: "ForcedTxValid",
      } as never,
      SDK.ForcedInclusionTxV1Schema as never,
    ),
  );

const makeRetainedPairPayload = ({
  transactionIdHex,
  forcedOrderIdHex,
  transactionCborHex,
  source,
}: {
  readonly transactionIdHex: string;
  readonly forcedOrderIdHex: string;
  readonly transactionCborHex: string;
  readonly source: SDK.L2TransactionSource;
}): SDK.DaPayload => {
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 2n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const header: SDK.Header = {
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
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: ZERO_HASH_28,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [[forcedOrderIdHex, forcedSourceValueHex(source)]],
      transactions: [[transactionIdHex, sourceValueHex(source)]],
      transaction_preimages: [[transactionIdHex, transactionCborHex]],
      forced_transaction_preimages: [[forcedOrderIdHex, transactionCborHex]],
      cek_program_material: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

const requireEntry = (
  entries: readonly SDK.DaPayloadEntry[],
  keyHex: string,
  fieldName: string,
): SDK.DaPayloadEntry => {
  const matches = entries.filter(([entryKeyHex]) => entryKeyHex === keyHex);
  if (matches.length !== 1) {
    throw new Error(`${fieldName} must retain exactly one entry for ${keyHex}`);
  }
  return matches[0]!;
};

const assertSourceMatches = ({
  retainedSource,
  transactionIdHex,
  compactCborHex,
  witnessSetCompactCborHex,
  fieldPreimageLengthsCborHex,
  fieldName,
}: {
  readonly retainedSource: SDK.L2TransactionSource;
  readonly transactionIdHex: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly fieldPreimageLengthsCborHex: string;
  readonly fieldName: string;
}): void => {
  if (
    retainedSource.tx_id !== transactionIdHex ||
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
      ? Data.from(sourceEntry[1], SDK.L2TransactionSourceSchema as never)
      : Data.from(sourceEntry[1], SDK.ForcedInclusionTxV1Schema as never);
  const exactSource = retainedSource as SDK.L2TransactionSource & {
    readonly verdict?: SDK.OperatorVerdict;
  };
  if (sourceKind === "forced" && exactSource.verdict !== "ForcedTxValid") {
    throw new Error("forced retained-DA source lost its operator verdict");
  }

  const retainedCanonicalCbor = Buffer.from(preimageEntry[1], "hex");
  const retainedTransaction = decodeMidgardNativeTxFullFromCanonicalCbor(
    retainedCanonicalCbor,
  );
  const retainedTransactionId = computeMidgardNativeTxId(retainedTransaction);
  const retainedProofSource =
    deriveMidgardNativeTxProofSource(retainedTransaction);
  const retainedTransactionCommitment =
    computeMidgardNativeTxProofCommitment(retainedProofSource);
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
  );
  const reconstructed = reconstructMidgardTransaction({
    transactionId: retainedTransactionId,
    transactionCommitment: retainedTransactionCommitment,
    source: retainedProofSource,
    fieldPreimages: deriveMidgardTxFieldPreimages(retainedCanonicalCbor).map(
      (field) => field.preimageCbor,
    ),
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
  const payload = makeRetainedPairPayload({
    transactionIdHex,
    forcedOrderIdHex,
    transactionCborHex: exactCanonicalTransactionCbor.toString("hex"),
    source: retainedSource,
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
    transactionCommitmentHex,
  });
  const measurement = {
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

/**
 * Builds the canonical Midgard schema projection used only for retained-DA
 * capability evidence when a Cardano-derived transaction carries one genuine
 * raw Flat spending program. The script-witness identity and script-integrity
 * commitment are replaced with the canonical CEK envelope identity, copied
 * vkey signatures are removed, and the source/raw hash remains audit metadata.
 *
 * This does not assert Cardano-ledger or Midgard Phase A/B validity.
 */
export const buildMidgardRetainedDaCanonicalScriptProjection = ({
  canonicalTransactionCbor,
}: {
  readonly canonicalTransactionCbor: Uint8Array;
}): RetainedDaCanonicalScriptProjection => {
  const source = decodeMidgardNativeTxFullFromCanonicalCbor(
    canonicalTransactionCbor,
  );
  if (
    !source.body.requiredObserversPreimageCbor.equals(Buffer.from([0x80])) ||
    !source.body.mintPreimageCbor.equals(Buffer.from([0x80]))
  ) {
    throw new Error(
      "retained-DA single-script projection does not remap observer or mint credentials",
    );
  }
  const scripts = decodeMidgardVersionedScriptListPreimage(
    source.witnessSet.scriptTxWitsPreimageCbor,
  );
  if (
    scripts.length !== 1 ||
    (scripts[0]!.language !== "PlutusV3" &&
      scripts[0]!.language !== "MidgardV1")
  ) {
    throw new Error(
      "retained-DA single-script projection requires exactly one genuine Flat program",
    );
  }
  const rawScript = scripts[0]!;
  const artifact = buildMidgardCanonicalScriptArtifact({
    language: rawScript.language,
    sourceRawFlatProgramBytes: rawScript.scriptBytes,
  });

  const projected = materializeMidgardNativeTxFromCanonical({
    version: source.version,
    validity: source.validity,
    body: {
      ...source.body,
      scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
        midgardFieldCommitment(source.witnessSet.redeemerTxWitsPreimageCbor),
        [rawScript.language],
      ),
    },
    witnessSet: {
      ...source.witnessSet,
      addrTxWitsPreimageCbor: Buffer.from([0x80]),
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
        artifact.canonicalMidgardCredentialScript,
      ]),
    },
  });
  const material = mergeMidgardCekProgramMaterialSidecars([
    artifact.canonicalMaterialSidecarCbor,
  ]);
  return {
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(projected),
    canonicalMaterialSidecarCbor:
      encodeMidgardCekProgramMaterialSidecar(material),
    sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
  };
};
