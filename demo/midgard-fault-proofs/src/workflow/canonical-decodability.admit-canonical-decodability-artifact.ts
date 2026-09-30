import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  canonicalDecodabilityEvidenceFromCommittedField,
  type L2TransactionSource,
  L2TransactionSource as L2TransactionSourceCodec,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  CANONICAL_DECODABILITY_ARTIFACT,
  canonicalDecodabilityArtifactFromRawEvidence,
  type CanonicalDecodabilityRawArtifact,
  type CanonicalDecodabilityRawBlockEvidence,
} from "../evidence/canonical-decodability-raw-evidence.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { type LinearFamilyAssemblyContext } from "./family-definition.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";

export type CanonicalDecodabilityArtifact = JournalJsonObject &
  CanonicalDecodabilityRawArtifact;

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
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
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

export const parseArtifact = (
  value: unknown,
): CanonicalDecodabilityArtifact => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "committedTransactionsRoot",
      "l2TransactionCount",
      "transactionsPhasRoot",
      "selectedTransactionIndex",
      "selectedFieldIndex",
      "selectedVerdict",
      "txMembershipProofCbor",
      "transactions",
    ],
    "canonical-decodability artifact",
  );
  if (
    parsed.schemaVersion !== CANONICAL_DECODABILITY_ARTIFACT ||
    !Array.isArray(parsed.transactions) ||
    parsed.transactions.length === 0
  ) {
    throw new Error(
      "canonical-decodability artifact version or leaves changed",
    );
  }
  const transactions = Object.freeze(
    parsed.transactions.map((value, index) => {
      const transaction = exact(
        value,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `canonical-decodability transaction ${index.toString()}`,
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
    parsed.l2TransactionCount,
    "canonical-decodability transaction count",
  );
  if (transactions.length !== l2TransactionCount) {
    throw new Error("canonical-decodability transaction count changed");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_DECODABILITY_ARTIFACT,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "header hash"),
    committedTransactionsRoot: canonicalHex(
      parsed.committedTransactionsRoot,
      HEX_32,
      "committed transactions root",
    ),
    l2TransactionCount,
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "transactions PHAS root",
    ),
    selectedTransactionIndex: natural(
      parsed.selectedTransactionIndex,
      "selected transaction index",
    ),
    selectedFieldIndex: natural(
      parsed.selectedFieldIndex,
      "selected field index",
    ),
    selectedVerdict: natural(parsed.selectedVerdict, "selected verdict"),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "transaction membership proof",
    ),
    transactions,
  });
};

type AdmittedCanonicalDecodabilityArtifact = Readonly<{
  artifact: CanonicalDecodabilityArtifact;
  committedPreimage: Buffer;
  witnessSet?: NativeTxWitnessSetCompact;
  witnessSetCompactCbor?: string;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

export const admitCanonicalDecodabilityArtifact = async (
  value: unknown,
): Promise<AdmittedCanonicalDecodabilityArtifact> => {
  const artifact = parseArtifact(value);
  const transactions = artifact.transactions.map((transaction, index) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    let source: L2TransactionSource;
    try {
      source = Data.from(
        transaction.l2TransactionSourceCbor,
        L2TransactionSourceCodec,
      );
    } catch (cause) {
      throw new Error(
        `canonical-decodability transaction ${index.toString()} source does not decode: ${String(cause)}`,
      );
    }
    const expected: L2TransactionSource = {
      tx_id: material.transactionId.toString("hex"),
      source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    if (
      expected.tx_id !== transaction.nodeTxId ||
      Data.to(source, L2TransactionSourceCodec) !==
        transaction.l2TransactionSourceCbor ||
      Data.to(source, L2TransactionSourceCodec) !==
        Data.to(expected, L2TransactionSourceCodec)
    ) {
      throw new Error(
        `canonical-decodability transaction ${index.toString()} changed its committed source identity`,
      );
    }
    return Object.freeze({ transaction, material });
  });
  const trie = await buildTrieView(
    transactions.map(({ transaction }) => ({
      key: Buffer.from(transaction.nodeTxId, "hex"),
      value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
    })),
  );
  if (trie.root !== artifact.transactionsPhasRoot) {
    throw new Error("canonical-decodability transactions PHAS root changed");
  }
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(artifact.l2TransactionCount),
  });
  const selected = transactions[artifact.selectedTransactionIndex];
  if (selected === undefined) {
    throw new Error("canonical-decodability selected transaction is absent");
  }
  const proof = requireProof(
    trie,
    Buffer.from(selected.transaction.nodeTxId, "hex"),
    "canonical-decodability transaction",
  );
  if (proof !== artifact.txMembershipProofCbor) {
    throw new Error("canonical-decodability transaction proof changed");
  }
  const committedPreimage =
    selected.material.fieldPreimages[artifact.selectedFieldIndex];
  if (committedPreimage === undefined) {
    throw new Error("canonical-decodability selected field is absent");
  }
  const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(
    selected.material.canonical.witnessSet,
  );
  const witnessSet: NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: witnessCompact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witnessCompact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witnessCompact.redeemerTxWitsHash.toString("hex"),
  };
  const fieldEvidence = canonicalDecodabilityEvidenceFromCommittedField({
    badTxId: selected.transaction.nodeTxId,
    fieldIndex: artifact.selectedFieldIndex,
    committedPreimage,
  });
  if (
    !fieldEvidence.isViolation ||
    fieldEvidence.verdict !== artifact.selectedVerdict
  ) {
    throw new Error("canonical-decodability selected verdict changed");
  }
  return Object.freeze({
    artifact,
    committedPreimage,
    ...(artifact.selectedFieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX
      ? {}
      : {
          witnessSet,
          witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
            addrTxWitsHash: witnessCompact.addrTxWitsHash,
            scriptTxWitsHash: witnessCompact.scriptTxWitsHash,
            redeemerTxWitsHash: witnessCompact.redeemerTxWitsHash,
          }).toString("hex"),
        }),
    txInclusion: parseSubmitStep01TxInclusion({
      nativeTxId: selected.transaction.nodeTxId,
      nativeTx: nativeTxFromCoreCompact(selected.material.compact),
      nativeTxCompactCbor:
        selected.material.proofSource.compactCbor.toString("hex"),
      l2TransactionSourceCbor: selected.transaction.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: proof,
    }),
  });
};

export const prepareCanonicalDecodabilityArtifact = async (
  evidence: CanonicalDecodabilityRawBlockEvidence,
): Promise<CanonicalDecodabilityArtifact> => {
  const artifact = normalizeJournalJson({
    ...canonicalDecodabilityArtifactFromRawEvidence(evidence),
  }) as CanonicalDecodabilityArtifact;
  await admitCanonicalDecodabilityArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "canonicalDecodability",
  (typeof WITNESS_ROLES)[number],
  true
>;
