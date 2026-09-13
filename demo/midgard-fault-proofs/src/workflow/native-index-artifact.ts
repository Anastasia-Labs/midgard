import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutputCanonical,
  type MidgardTxInput,
  type MidgardTxOutput,
} from "@al-ft/midgard-sdk";

import { midgardTxOutputFromCanonicalCbor } from "../prepare-input-no-idx.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";

export const NATIVE_INDEX_ARTIFACT_HELPER =
  "midgard-production-native-index-artifact-helper-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;
export const HEX_32 = /^[0-9a-f]{64}$/u;
export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
export const NATURAL_DECIMAL = /^(?:0|[1-9][0-9]*)$/u;

export type NativeInclusionArtifact = Readonly<{
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  transactionsPhasRoot: string;
  txMembershipProofCbor: string;
}>;

export const exactJournalRecord = (
  value: unknown,
  keys: readonly string[],
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
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return record;
};

export const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

export const canonicalNaturalString = (
  value: unknown,
  label: string,
): string => {
  if (typeof value !== "string" || !NATURAL_DECIMAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

export const safeNaturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

export const admitNativeInclusionArtifact = (
  value: unknown,
  label: string,
): Readonly<{
  artifact: NativeInclusionArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const parsed = exactJournalRecord(
    value,
    [
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
    ],
    label,
  );
  const artifact = Object.freeze({
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, `${label} tx id`),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      `${label} compact transaction`,
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      `${label} transaction source`,
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      `${label} PHAS root`,
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      `${label} membership proof`,
    ),
  });
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompact(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error(`${label} membership proof cannot be replayed`);
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(`${label} membership proof does not open its PHAS root`);
  }
  return Object.freeze({ artifact, inclusion });
};

export const admitTxInputList = (
  value: unknown,
  label: string,
): Readonly<{
  json: readonly Readonly<{ tx_id: string; output_index: string }>[];
  inputs: readonly MidgardTxInput[];
}> => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array`);
  }
  const json = Object.freeze(
    value.map((entry, index) => {
      const parsed = exactJournalRecord(
        entry,
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
  return Object.freeze({
    json,
    inputs: Object.freeze(
      json.map((input) => ({
        tx_id: input.tx_id,
        output_index: BigInt(input.output_index),
      })),
    ),
  });
};

export const admitOutputCborList = (
  value: unknown,
  label: string,
): Readonly<{
  json: readonly string[];
  outputs: readonly MidgardTxOutput[];
}> => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array`);
  }
  const json = Object.freeze(
    value.map((item, index) =>
      canonicalHex(item, EVEN_HEX, `${label}[${index.toString()}]`),
    ),
  );
  const outputs = Object.freeze(
    json.map((item, index) => {
      const bytes = Buffer.from(item, "hex");
      const output = midgardTxOutputFromCanonicalCbor(bytes);
      if (!encodeMidgardTxOutputCanonical(output).equals(bytes)) {
        throw new Error(`${label}[${index.toString()}] is not canonical`);
      }
      return output;
    }),
  );
  return Object.freeze({ json, outputs });
};
