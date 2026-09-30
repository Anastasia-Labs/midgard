import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  type MidgardTxInput,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  admitNativeInclusionArtifact,
  canonicalHex,
  canonicalNaturalString,
  exactJournalRecord,
  HEX_28,
  HEX_32,
  type NativeInclusionArtifact,
} from "../workflow/native-index-artifact.js";
import {
  buildMissingNativeScriptTxEvidence,
  type MissingNativeScriptTxEvidence,
} from "./evidence.js";
import {
  type HistoricalNativeScriptPreimage,
  prepareHistoricalNativeScriptPreimage,
} from "./historical-preimage.js";
import type { HistoricalNativeScriptEvidence } from "./historical-script.js";

export const MISSING_NATIVE_SCRIPT_TX_ARTIFACT =
  "midgard-production-missing-native-script-tx-artifact-v1" as const;

type InputJson = Readonly<{ tx_id: string; output_index: string }>;

export type WitnessSetJson = Readonly<{
  addr_tx_wits_hash: string;
  script_tx_wits_hash: string;
  redeemer_tx_wits_hash: string;
}>;

export type MissingNativeScriptTxArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MISSING_NATIVE_SCRIPT_TX_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: NativeInclusionArtifact;
    badTxSpendInputs: readonly InputJson[];
    badInputIndex: string;
    producingTx: NativeInclusionArtifact;
    producingOutputItemCbors: readonly string[];
    historicalPreimage: HistoricalNativeScriptPreimage;
    badTxWitnessSet: WitnessSetJson;
    badTxScriptWitnessItemCbors: readonly string[];
    expectedMissingScriptHash: string;
  }>;

export type AdmittedMissingNativeScriptTxArtifact = Readonly<{
  artifact: MissingNativeScriptTxArtifact;
  evidence: MissingNativeScriptTxEvidence;
}>;

const safeNatural = (value: string, label: string): number => {
  const parsed = BigInt(canonicalNaturalString(value, label));
  if (parsed > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} exceeds the safe integer range`);
  }
  return Number(parsed);
};

export const detectionCoordinates = ({
  detectionId,
  label,
}: {
  readonly detectionId: string;
  readonly label: string;
}) => {
  const fields = detectionId.split(":");
  if (
    fields.length !== 8 ||
    fields[0] !== MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID
  ) {
    throw new Error(`${label} has an invalid detection identity`);
  }
  return Object.freeze({
    transactionIndex: safeNatural(fields[1]!, `${label} transaction index`),
    inputIndex: safeNatural(fields[2]!, `${label} input index`),
    producerIndex: safeNatural(fields[3]!, `${label} producer index`),
    outputIndex: safeNatural(fields[4]!, `${label} output index`),
    badTxId: canonicalHex(fields[5], HEX_32, `${label} bad tx id`),
    producerTxId: canonicalHex(fields[6], HEX_32, `${label} producer tx id`),
    expectedScriptHash: canonicalHex(
      fields[7],
      HEX_28,
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
}): NativeInclusionArtifact =>
  Object.freeze({
    nativeTxId: material.nodeTxId,
    nativeTxCompactCbor: material.nativeCompactCbor,
    l2TransactionSourceCbor: material.l2TransactionSourceCbor,
    transactionsPhasRoot,
    txMembershipProofCbor,
  });

const witnessSet = (
  material: Awaited<ReturnType<typeof decodeTransactionMaterial>>,
): WitnessSetJson => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    material.nativeTx.witnessSet,
  );
  return Object.freeze({
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  });
};

const witnessSetForEvidence = (
  value: WitnessSetJson,
): NativeTxWitnessSetCompact => ({
  addr_tx_wits_hash: value.addr_tx_wits_hash,
  script_tx_wits_hash: value.script_tx_wits_hash,
  redeemer_tx_wits_hash: value.redeemer_tx_wits_hash,
});

export const prepareMissingNativeScriptTxArtifact = async ({
  evidence,
  classification,
  historicalNativeScriptCorpus,
  historicalL1Corroboration,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingNativeScriptTx" };
  readonly historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
  readonly historicalL1Corroboration: HistoricalNativeScriptEvidence;
}): Promise<MissingNativeScriptTxArtifact> => {
  const selected = classification.selected;
  if (
    classification.headerHash !== evidence.headerHash ||
    selected.headerHash !== evidence.headerHash ||
    selected.violationId !== MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID ||
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
  const trie = await buildTrieView(materials.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
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
  const historicalPreimage = prepareHistoricalNativeScriptPreimage({
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
    schemaVersion: MISSING_NATIVE_SCRIPT_TX_ARTIFACT,
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
  }) satisfies MissingNativeScriptTxArtifact;
  admitPreparedArtifact(artifact, historicalPreimage.scriptBytesHex);
  return artifact;
};

export const inputList = (
  value: unknown,
  label: string,
): readonly InputJson[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  return Object.freeze(
    value.map((item, index) => {
      const parsed = exactJournalRecord(
        item,
        ["tx_id", "output_index"],
        `${label}[${index.toString()}]`,
      );
      return Object.freeze({
        tx_id: canonicalHex(
          parsed.tx_id,
          HEX_32,
          `${label}[${index.toString()}].tx_id`,
        ),
        output_index: canonicalNaturalString(
          parsed.output_index,
          `${label}[${index.toString()}].output_index`,
        ),
      });
    }),
  );
};

export const admitPreparedArtifact = (
  artifact: MissingNativeScriptTxArtifact,
  scriptBytesHex: string,
): MissingNativeScriptTxEvidence => {
  const badTx = admitNativeInclusionArtifact(
    artifact.badTx,
    "missing-native-script bad transaction",
  );
  const producer = admitNativeInclusionArtifact(
    artifact.producingTx,
    "missing-native-script producer transaction",
  );
  return buildMissingNativeScriptTxEvidence({
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
