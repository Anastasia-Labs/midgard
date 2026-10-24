import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  type MidgardAddressWitness,
  missingSignatureVkeyHash,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { type SubmitStep01TxInclusion } from "../step-support.js";
import { type JournalJsonObject } from "./journal.js";

export const MISSING_SIGNATURE_ARTIFACT =
  "midgard-production-missing-signature-artifact-v1" as const;

type MissingSignatureArtifactTransaction = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type MissingSignatureArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof MISSING_SIGNATURE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly selectedTransactionIndex: number;
  readonly accusedRequiredSignerIndex: number;
  readonly accusedRequiredSignerHash: string;
  readonly resolvedVkey: string;
  readonly transactions: readonly MissingSignatureArtifactTransaction[];
};

export type AdmittedMissingSignatureArtifact = Readonly<{
  artifact: MissingSignatureArtifact;
  txInclusion: SubmitStep01TxInclusion;
  nativeTxCompactCbor: string;
  requiredSignerHashes: readonly string[];
  addrTxWits: readonly MidgardAddressWitness[];
  witnessSetCompact: NativeTxWitnessSetCompact;
  accusedRequiredSignerIndex: bigint;
  resolvedVkey: string;
}>;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

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

export const parseArtifact = (value: unknown): MissingSignatureArtifact => {
  const artifact = record(value, "missing-signature artifact");
  exactKeys(
    artifact,
    [
      "schemaVersion",
      "headerHash",
      "committedTransactionsRoot",
      "selectedTransactionIndex",
      "accusedRequiredSignerIndex",
      "accusedRequiredSignerHash",
      "resolvedVkey",
      "transactions",
    ],
    "missing-signature artifact",
  );
  if (artifact.schemaVersion !== MISSING_SIGNATURE_ARTIFACT) {
    throw new Error("missing-signature artifact version changed");
  }
  if (
    !Array.isArray(artifact.transactions) ||
    artifact.transactions.length === 0
  ) {
    throw new Error("missing-signature artifact has no committed transactions");
  }
  const transactions = Object.freeze(
    artifact.transactions.map((value, index) => {
      const transaction = record(
        value,
        `missing-signature transaction ${index.toString()}`,
      );
      exactKeys(
        transaction,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `missing-signature transaction ${index.toString()}`,
      );
      return Object.freeze({
        nodeTxId: canonicalHex(
          transaction.nodeTxId,
          HEX_32,
          `missing-signature transaction ${index.toString()} id`,
        ),
        txCbor: canonicalHex(
          transaction.txCbor,
          EVEN_HEX,
          `missing-signature transaction ${index.toString()} CBOR`,
        ),
        l2TransactionSourceCbor: canonicalHex(
          transaction.l2TransactionSourceCbor,
          EVEN_HEX,
          `missing-signature transaction ${index.toString()} source`,
        ),
      });
    }),
  );
  return Object.freeze({
    schemaVersion: MISSING_SIGNATURE_ARTIFACT,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "missing-signature header",
    ),
    committedTransactionsRoot: canonicalHex(
      artifact.committedTransactionsRoot,
      HEX_32,
      "missing-signature transactions root",
    ),
    selectedTransactionIndex: natural(
      artifact.selectedTransactionIndex,
      "missing-signature selected transaction index",
    ),
    accusedRequiredSignerIndex: natural(
      artifact.accusedRequiredSignerIndex,
      "missing-signature accused signer index",
    ),
    accusedRequiredSignerHash: canonicalHex(
      artifact.accusedRequiredSignerHash,
      HEX_28,
      "missing-signature accused signer hash",
    ),
    resolvedVkey: canonicalHex(
      artifact.resolvedVkey,
      HEX_32,
      "missing-signature resolved verification key",
    ),
    transactions,
  });
};

export const signerHashes = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes, index) => {
      if (bytes.length !== 28) {
        throw new Error(
          `${label}[${index.toString()}] is not a 28-byte signer hash`,
        );
      }
      return Buffer.from(bytes).toString("hex");
    },
  );

export const witnessSetCompact = (
  witnessSet: Parameters<typeof deriveMidgardNativeTxWitnessSetCompact>[0],
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(witnessSet);
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

export const publicCommittedVkeyFor = ({
  hash,
  witnesses,
}: {
  readonly hash: string;
  readonly witnesses: readonly (readonly MidgardAddressWitness[])[];
}): string | undefined => {
  for (const list of witnesses) {
    for (const witness of list) {
      const verificationKey = witness.verification_key.toLowerCase();
      if (missingSignatureVkeyHash(verificationKey) === hash) {
        return verificationKey;
      }
    }
  }
  return undefined;
};
