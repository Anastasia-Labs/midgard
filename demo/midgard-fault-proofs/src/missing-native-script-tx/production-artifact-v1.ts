import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  type MidgardTxInput,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatchV1,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import type { CanonicalBlockClassificationV1 } from "../workflow/classification-v1.js";
import type { JournalJsonObjectV1 } from "../workflow/journal-v1.js";
import type { ProductionHistoricalNativeScriptCorpusV1 } from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  admitProductionNativeInclusionArtifactV1,
  canonicalHexV1,
  canonicalNaturalStringV1,
  EVEN_HEX_V1,
  exactJournalRecordV1,
  HEX_28_V1,
  HEX_32_V1,
  type ProductionNativeInclusionArtifactV1,
} from "../workflow/production-native-index-artifact-v1.js";
import type { FraudProofRawL1PointV1 } from "../workflow/raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicyV1 } from "../workflow/release-finality-policy-v1.js";
import {
  buildMissingNativeScriptTxEvidenceV1,
  type MissingNativeScriptTxEvidenceV1,
} from "./evidence-v1.js";
import type {
  HistoricalNativeScriptEvidenceV1,
  HistoricalNativeScriptSourceRosterV1,
} from "./historical-script-v1.js";
import {
  admitProductionHistoricalNativeScriptPreimageV1,
  prepareProductionHistoricalNativeScriptPreimageV1,
  type ProductionHistoricalNativeScriptPreimageV1,
} from "./production-historical-preimage-v1.js";

export const PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1 =
  "midgard-production-missing-native-script-tx-artifact-v1" as const;

type InputJsonV1 = Readonly<{ tx_id: string; output_index: string }>;
type WitnessSetJsonV1 = Readonly<{
  addr_tx_wits_hash: string;
  script_tx_wits_hash: string;
  redeemer_tx_wits_hash: string;
}>;

export type ProductionMissingNativeScriptTxArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: ProductionNativeInclusionArtifactV1;
    badTxSpendInputs: readonly InputJsonV1[];
    badInputIndex: string;
    producingTx: ProductionNativeInclusionArtifactV1;
    producingOutputItemCbors: readonly string[];
    historicalPreimage: ProductionHistoricalNativeScriptPreimageV1;
    badTxWitnessSet: WitnessSetJsonV1;
    badTxScriptWitnessItemCbors: readonly string[];
    expectedMissingScriptHash: string;
  }>;

export type AdmittedProductionMissingNativeScriptTxArtifactV1 = Readonly<{
  artifact: ProductionMissingNativeScriptTxArtifactV1;
  evidence: MissingNativeScriptTxEvidenceV1;
}>;

const safeNatural = (value: string, label: string): number => {
  const parsed = BigInt(canonicalNaturalStringV1(value, label));
  if (parsed > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} exceeds the safe integer range`);
  }
  return Number(parsed);
};

const detectionCoordinates = ({
  detectionId,
  label,
}: {
  readonly detectionId: string;
  readonly label: string;
}) => {
  const fields = detectionId.split(":");
  if (
    fields.length !== 8 ||
    fields[0] !== MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1
  ) {
    throw new Error(`${label} has an invalid detection identity`);
  }
  return Object.freeze({
    transactionIndex: safeNatural(fields[1]!, `${label} transaction index`),
    inputIndex: safeNatural(fields[2]!, `${label} input index`),
    producerIndex: safeNatural(fields[3]!, `${label} producer index`),
    outputIndex: safeNatural(fields[4]!, `${label} output index`),
    badTxId: canonicalHexV1(fields[5], HEX_32_V1, `${label} bad tx id`),
    producerTxId: canonicalHexV1(
      fields[6],
      HEX_32_V1,
      `${label} producer tx id`,
    ),
    expectedScriptHash: canonicalHexV1(
      fields[7],
      HEX_28_V1,
      `${label} expected script hash`,
    ),
  });
};

const inclusionArtifact = ({
  material,
  transactionsPhasRoot,
  txMembershipProofCbor,
}: {
  readonly material: Awaited<ReturnType<typeof decodeTransactionMaterial>>;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
}): ProductionNativeInclusionArtifactV1 =>
  Object.freeze({
    nativeTxId: material.nodeTxId,
    nativeTxCompactCbor: material.nativeCompactCbor,
    l2TransactionSourceCbor: material.l2TransactionSourceCbor,
    transactionsPhasRoot,
    txMembershipProofCbor,
  });

const witnessSet = (
  material: Awaited<ReturnType<typeof decodeTransactionMaterial>>,
): WitnessSetJsonV1 => {
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(
    material.nativeTx.witnessSet,
  );
  return Object.freeze({
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  });
};

const witnessSetForEvidence = (
  value: WitnessSetJsonV1,
): NativeTxWitnessSetCompact => ({
  addr_tx_wits_hash: value.addr_tx_wits_hash,
  script_tx_wits_hash: value.script_tx_wits_hash,
  redeemer_tx_wits_hash: value.redeemer_tx_wits_hash,
});

export const prepareProductionMissingNativeScriptTxArtifactV1 = async ({
  evidence,
  classification,
  historicalNativeScriptCorpus,
  historicalL1Corroboration,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingNativeScriptTx" };
  readonly historicalNativeScriptCorpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly historicalL1Corroboration: HistoricalNativeScriptEvidenceV1;
}): Promise<ProductionMissingNativeScriptTxArtifactV1> => {
  const selected = classification.selected;
  if (
    classification.headerHash !== evidence.headerHash ||
    selected.headerHash !== evidence.headerHash ||
    selected.violationId !== MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID_V1 ||
    selected.position < 0n ||
    selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "missing-native-script-tx classification changed the authenticated header/fault",
    );
  }
  const coordinates = detectionCoordinates({
    detectionId: selected.detectionId,
    label: "missing-native-script-tx detection",
  });
  if (selected.position !== BigInt(coordinates.transactionIndex)) {
    throw new Error(
      "missing-native-script-tx classification position changed its detection coordinate",
    );
  }
  const materials = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const badTx = materials[coordinates.transactionIndex];
  const producer = materials[coordinates.producerIndex];
  if (
    badTx === undefined ||
    producer === undefined ||
    badTx.nodeTxId !== coordinates.badTxId ||
    producer.nodeTxId !== coordinates.producerTxId
  ) {
    throw new Error(
      "missing-native-script-tx detection does not identify exact retained transactions",
    );
  }
  const accusedInput = badTx.inputs[coordinates.inputIndex];
  if (
    accusedInput === undefined ||
    accusedInput.transactionId !== producer.nodeTxId ||
    accusedInput.outputIndex !== BigInt(coordinates.outputIndex)
  ) {
    throw new Error(
      "missing-native-script-tx detection changed the producer/input coordinate",
    );
  }
  const trie = await buildTrieView(materials.map(transactionSourceTrieItemV1));
  await requireTransactionsRootMatchV1({
    sourceRoot: trie.root,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
    count: BigInt(materials.length),
  });
  const badInclusion = inclusionArtifact({
    material: badTx,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: requireProof(
      trie,
      Buffer.from(badTx.nodeTxId, "hex"),
      "missing-native-script bad transaction",
    ),
  });
  const producerInclusion = inclusionArtifact({
    material: producer,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: requireProof(
      trie,
      Buffer.from(producer.nodeTxId, "hex"),
      "missing-native-script producer transaction",
    ),
  });
  const historicalPreimage = prepareProductionHistoricalNativeScriptPreimageV1({
    corpus: historicalNativeScriptCorpus,
    expectedHeaderHash: evidence.headerHash,
    expectedScriptHash: coordinates.expectedScriptHash,
    corroboration: historicalL1Corroboration,
  });
  const producingOutputItemCbors = decodeMidgardNativeByteListPreimage(
    producer.nativeTx.body.outputsPreimageCbor,
    "missing-native-script producer outputs",
  ).map((item) => Buffer.from(item).toString("hex"));
  if (producingOutputItemCbors[coordinates.outputIndex] === undefined) {
    throw new Error(
      "missing-native-script producer output coordinate is outside the committed field",
    );
  }
  const badTxScriptWitnessItemCbors = decodeMidgardNativeByteListPreimage(
    badTx.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    "missing-native-script witness field",
  ).map((item) => Buffer.from(item).toString("hex"));
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: selected.detectionId,
    position: Number(selected.position),
    badTx: badInclusion,
    badTxSpendInputs: Object.freeze(
      badTx.inputs.map((input) =>
        Object.freeze({
          tx_id: input.transactionId,
          output_index: input.outputIndex.toString(),
        }),
      ),
    ),
    badInputIndex: coordinates.inputIndex.toString(),
    producingTx: producerInclusion,
    producingOutputItemCbors: Object.freeze(producingOutputItemCbors),
    historicalPreimage,
    badTxWitnessSet: witnessSet(badTx),
    badTxScriptWitnessItemCbors: Object.freeze(badTxScriptWitnessItemCbors),
    expectedMissingScriptHash: coordinates.expectedScriptHash,
  }) satisfies ProductionMissingNativeScriptTxArtifactV1;
  admitPreparedArtifact(artifact, historicalPreimage.scriptBytesHex);
  return artifact;
};

const inputList = (value: unknown, label: string): readonly InputJsonV1[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) => {
      const parsed = exactJournalRecordV1(
        item,
        ["tx_id", "output_index"],
        `${label}[${index.toString()}]`,
      );
      return Object.freeze({
        tx_id: canonicalHexV1(
          parsed.tx_id,
          HEX_32_V1,
          `${label}[${index.toString()}].tx_id`,
        ),
        output_index: canonicalNaturalStringV1(
          parsed.output_index,
          `${label}[${index.toString()}].output_index`,
        ),
      });
    }),
  );
};

const hexList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) =>
      canonicalHexV1(item, EVEN_HEX_V1, `${label}[${index.toString()}]`),
    ),
  );
};

/**
 * Strict persisted-shape route selector used only by the claim prerequisite.
 * The transaction port still performs full live corpus/L1 admission before it
 * captures any body; this function never grants artifact authority.
 */
export const productionMissingNativeScriptTxArtifactUsesDirectRouteV1 = (
  value: unknown,
): boolean => {
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "badTxSpendInputs",
      "badInputIndex",
      "producingTx",
      "producingOutputItemCbors",
      "historicalPreimage",
      "badTxWitnessSet",
      "badTxScriptWitnessItemCbors",
      "expectedMissingScriptHash",
    ],
    "production missing-native-script-tx route artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1
  ) {
    throw new Error(
      "production missing-native-script-tx route artifact schema changed",
    );
  }
  return (
    hexList(
      parsed.badTxScriptWitnessItemCbors,
      "production missing-native-script-tx route witnesses",
    ).length <= MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT_V1
  );
};

const admitWitnessSet = (value: unknown): WitnessSetJsonV1 => {
  const parsed = exactJournalRecordV1(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "missing-native-script witness set",
  );
  return Object.freeze({
    addr_tx_wits_hash: canonicalHexV1(
      parsed.addr_tx_wits_hash,
      HEX_32_V1,
      "missing-native-script address witness hash",
    ),
    script_tx_wits_hash: canonicalHexV1(
      parsed.script_tx_wits_hash,
      HEX_32_V1,
      "missing-native-script script witness hash",
    ),
    redeemer_tx_wits_hash: canonicalHexV1(
      parsed.redeemer_tx_wits_hash,
      HEX_32_V1,
      "missing-native-script redeemer witness hash",
    ),
  });
};

const admitPreparedArtifact = (
  artifact: ProductionMissingNativeScriptTxArtifactV1,
  scriptBytesHex: string,
): MissingNativeScriptTxEvidenceV1 => {
  const badTx = admitProductionNativeInclusionArtifactV1(
    artifact.badTx,
    "missing-native-script bad transaction",
  );
  const producer = admitProductionNativeInclusionArtifactV1(
    artifact.producingTx,
    "missing-native-script producer transaction",
  );
  return buildMissingNativeScriptTxEvidenceV1({
    badTxInclusion: badTx.inclusion,
    badTxSpendInputs: artifact.badTxSpendInputs.map(
      (input): MidgardTxInput => ({
        tx_id: input.tx_id,
        output_index: BigInt(input.output_index),
      }),
    ),
    badInputIndex: BigInt(artifact.badInputIndex),
    producingTxInclusion: producer.inclusion,
    producingOutputItemCbors: artifact.producingOutputItemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    missingNativeScriptBytes: Buffer.from(scriptBytesHex, "hex"),
    badTxWitnessSet: witnessSetForEvidence(artifact.badTxWitnessSet),
    badTxScriptWitnessItemCbors: artifact.badTxScriptWitnessItemCbors.map(
      (item) => Buffer.from(item, "hex"),
    ),
    owner: "production missing-native-script-tx artifact",
  });
};

export const admitProductionMissingNativeScriptTxArtifactV1 = async ({
  value,
  historicalNativeScriptCorpus,
  historicalSourceRoster,
  historicalThroughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly historicalNativeScriptCorpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly historicalSourceRoster: HistoricalNativeScriptSourceRosterV1;
  readonly historicalThroughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): Promise<AdmittedProductionMissingNativeScriptTxArtifactV1> => {
  const parsed = exactJournalRecordV1(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "badTxSpendInputs",
      "badInputIndex",
      "producingTx",
      "producingOutputItemCbors",
      "historicalPreimage",
      "badTxWitnessSet",
      "badTxScriptWitnessItemCbors",
      "expectedMissingScriptHash",
    ],
    "production missing-native-script-tx artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1
  ) {
    throw new Error(
      "production missing-native-script-tx artifact schema changed",
    );
  }
  if (
    !Number.isSafeInteger(parsed.position) ||
    (parsed.position as number) < 0
  ) {
    throw new Error("production missing-native-script-tx position is invalid");
  }
  const headerHash = canonicalHexV1(
    parsed.headerHash,
    HEX_28_V1,
    "production missing-native-script-tx header hash",
  );
  if (typeof parsed.detectionId !== "string") {
    throw new Error("production missing-native-script-tx detection is invalid");
  }
  const coordinates = detectionCoordinates({
    detectionId: parsed.detectionId,
    label: "production missing-native-script-tx detection",
  });
  if (coordinates.transactionIndex !== parsed.position) {
    throw new Error("production missing-native-script-tx position changed");
  }
  const historicalPreimage =
    await admitProductionHistoricalNativeScriptPreimageV1({
      value: parsed.historicalPreimage,
      corpus: historicalNativeScriptCorpus,
      expectedHeaderHash: headerHash,
      expectedScriptHash: coordinates.expectedScriptHash,
      roster: historicalSourceRoster,
      throughPoint: historicalThroughPoint,
      releaseFinality,
    });
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_MISSING_NATIVE_SCRIPT_TX_ARTIFACT_V1,
    headerHash,
    detectionId: parsed.detectionId,
    position: parsed.position as number,
    badTx: admitProductionNativeInclusionArtifactV1(
      parsed.badTx,
      "production missing-native-script bad transaction",
    ).artifact,
    badTxSpendInputs: inputList(
      parsed.badTxSpendInputs,
      "production missing-native-script spend inputs",
    ),
    badInputIndex: canonicalNaturalStringV1(
      parsed.badInputIndex,
      "production missing-native-script bad input index",
    ),
    producingTx: admitProductionNativeInclusionArtifactV1(
      parsed.producingTx,
      "production missing-native-script producer transaction",
    ).artifact,
    producingOutputItemCbors: hexList(
      parsed.producingOutputItemCbors,
      "production missing-native-script producer outputs",
    ),
    historicalPreimage: historicalPreimage.artifact,
    badTxWitnessSet: admitWitnessSet(parsed.badTxWitnessSet),
    badTxScriptWitnessItemCbors: hexList(
      parsed.badTxScriptWitnessItemCbors,
      "production missing-native-script witnesses",
    ),
    expectedMissingScriptHash: canonicalHexV1(
      parsed.expectedMissingScriptHash,
      HEX_28_V1,
      "production missing-native-script expected hash",
    ),
  }) satisfies ProductionMissingNativeScriptTxArtifactV1;
  if (
    artifact.headerHash !== historicalNativeScriptCorpus.throughHeaderHash ||
    artifact.expectedMissingScriptHash !== coordinates.expectedScriptHash ||
    artifact.badTx.nativeTxId !== coordinates.badTxId ||
    artifact.producingTx.nativeTxId !== coordinates.producerTxId ||
    artifact.badInputIndex !== coordinates.inputIndex.toString()
  ) {
    throw new Error(
      "production missing-native-script artifact changed its detection/history identity",
    );
  }
  const evidence = admitPreparedArtifact(
    artifact,
    historicalPreimage.artifact.scriptBytesHex,
  );
  return Object.freeze({ artifact, evidence });
};
