import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import { COMMITTED_FIELD_SHAPE_VIOLATION_ID } from "@al-ft/midgard-sdk";

import {
  prepareCommittedFieldShapeFromCanonicalTx,
  type PreparedCommittedFieldShape,
} from "../committed-field-shape/prepare-committed-field-shape.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { type JournalJsonObject } from "./journal.js";

export const COMMITTED_FIELD_SHAPE_ARTIFACT =
  "midgard-production-committed-field-shape-artifact-v1" as const;

type ArtifactTransaction = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type CommittedFieldShapeArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof COMMITTED_FIELD_SHAPE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly transactionsPhasRoot: string;
  readonly selectedTransactionIndex: number;
  readonly selectedFieldIndex: number;
  readonly txMembershipProofCbor: string;
  readonly transactions: readonly ArtifactTransaction[];
};

const HEX_28 = /^[0-9a-f]{56}$/u;

const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  expected: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const canonical = [...expected].sort();
  if (
    actual.length !== canonical.length ||
    actual.some((key, index) => key !== canonical[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

export const artifactFields = [
  "schemaVersion",
  "headerHash",
  "committedTransactionsRoot",
  "l2TransactionCount",
  "transactionsPhasRoot",
  "selectedTransactionIndex",
  "selectedFieldIndex",
  "txMembershipProofCbor",
  "transactions",
] as const;

const parseArtifact = (value: unknown): CommittedFieldShapeArtifact => {
  const artifact = record(value, "committed-field-shape artifact");
  exactKeys(artifact, artifactFields, "committed-field-shape artifact");
  if (artifact.schemaVersion !== COMMITTED_FIELD_SHAPE_ARTIFACT) {
    throw new Error("committed-field-shape artifact version changed");
  }
  if (
    !Array.isArray(artifact.transactions) ||
    artifact.transactions.length === 0
  ) {
    throw new Error("committed-field-shape artifact has no transactions");
  }
  const transactions = Object.freeze(
    artifact.transactions.map((value, index) => {
      const transaction = record(
        value,
        `committed-field-shape transaction ${index.toString()}`,
      );
      exactKeys(
        transaction,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `committed-field-shape transaction ${index.toString()}`,
      );
      return Object.freeze({
        nodeTxId: canonicalHex(
          transaction.nodeTxId,
          HEX_32,
          `transaction ${index.toString()} id`,
        ),
        txCbor: canonicalHex(
          transaction.txCbor,
          EVEN_HEX,
          `transaction ${index.toString()} CBOR`,
        ),
        l2TransactionSourceCbor: canonicalHex(
          transaction.l2TransactionSourceCbor,
          EVEN_HEX,
          `transaction ${index.toString()} source CBOR`,
        ),
      });
    }),
  );
  const l2TransactionCount = natural(
    artifact.l2TransactionCount,
    "committed-field-shape transaction count",
  );
  if (l2TransactionCount !== transactions.length) {
    throw new Error(
      "committed-field-shape artifact transaction count differs from its leaves",
    );
  }
  return Object.freeze({
    schemaVersion: COMMITTED_FIELD_SHAPE_ARTIFACT,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "committed-field-shape header",
    ),
    committedTransactionsRoot: canonicalHex(
      artifact.committedTransactionsRoot,
      HEX_32,
      "committed transactions root",
    ),
    l2TransactionCount,
    transactionsPhasRoot: canonicalHex(
      artifact.transactionsPhasRoot,
      HEX_32,
      "transactions PHAS root",
    ),
    selectedTransactionIndex: natural(
      artifact.selectedTransactionIndex,
      "selected transaction index",
    ),
    selectedFieldIndex: natural(
      artifact.selectedFieldIndex,
      "selected field index",
    ),
    txMembershipProofCbor: canonicalHex(
      artifact.txMembershipProofCbor,
      EVEN_HEX,
      "transaction membership proof",
    ),
    transactions,
  });
};

type AdmittedCommittedFieldShapeArtifact = Readonly<{
  artifact: CommittedFieldShapeArtifact;
  prepared: PreparedCommittedFieldShape;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

/** Strictly reopens every source leaf and reproduces the selected proof. */
export const admitCommittedFieldShapeArtifact = async (
  value: unknown,
): Promise<AdmittedCommittedFieldShapeArtifact> => {
  const artifact = parseArtifact(value);
  const decoded = await Promise.all(
    artifact.transactions.map((transaction) =>
      decodeTransactionMaterial(transaction),
    ),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  if (trie.root !== artifact.transactionsPhasRoot) {
    throw new Error(
      "committed-field-shape artifact transactions PHAS root changed",
    );
  }
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(artifact.l2TransactionCount),
  });
  const transaction = decoded[artifact.selectedTransactionIndex];
  if (transaction === undefined) {
    throw new Error("committed-field-shape selected transaction is absent");
  }
  const proof = requireProof(
    trie,
    Buffer.from(transaction.nodeTxId, "hex"),
    "committed-field-shape transaction",
  );
  if (proof !== artifact.txMembershipProofCbor) {
    throw new Error(
      "committed-field-shape transaction proof differs from leaf re-derivation",
    );
  }
  const canonical = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(
      artifact.transactions[artifact.selectedTransactionIndex]!.txCbor,
      "hex",
    ),
  );
  const prepared = prepareCommittedFieldShapeFromCanonicalTx({
    tx: canonical,
    fieldIndex: artifact.selectedFieldIndex,
  });
  if (prepared.evidence.badTxId !== transaction.nodeTxId) {
    throw new Error(
      "committed-field-shape selected transaction id changed on re-derivation",
    );
  }
  const txInclusion = parseSubmitStep01TxInclusion({
    nativeTxId: transaction.nodeTxId,
    nativeTx: transaction.nativeTxCompact,
    nativeTxCompactCbor: transaction.nativeCompactCbor,
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: proof,
  });
  return Object.freeze({ artifact, prepared, txInclusion });
};

export const fieldIndexFromClassification = ({
  classification,
  transactionIndex,
  nodeTxId,
}: {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
  readonly transactionIndex: number;
  readonly nodeTxId: string;
}): number => {
  if (
    classification.selected.violationId !==
      COMMITTED_FIELD_SHAPE_VIOLATION_ID ||
    classification.selected.position !== BigInt(transactionIndex)
  ) {
    throw new Error(
      "committed-field-shape classification does not bind its transaction position",
    );
  }
  const prefix = `${COMMITTED_FIELD_SHAPE_VIOLATION_ID}:${transactionIndex.toString()}:${nodeTxId}:`;
  if (!classification.selected.detectionId.startsWith(prefix)) {
    throw new Error(
      "committed-field-shape classification does not bind its canonical transaction",
    );
  }
  const suffix = classification.selected.detectionId.slice(prefix.length);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(suffix)) {
    throw new Error(
      "committed-field-shape classification has a malformed field index",
    );
  }
  return Number(suffix);
};
