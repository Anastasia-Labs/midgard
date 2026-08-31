import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompactV1 } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutputCanonicalV1,
  type MidgardTxInput,
  type MidgardTxOutput,
} from "@al-ft/midgard-sdk";

import { midgardTxOutputFromCanonicalCborV1 } from "../prepare-input-no-idx.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";

export const PRODUCTION_NATIVE_INDEX_ARTIFACT_HELPER_V1 =
  "midgard-production-native-index-artifact-helper-v1" as const;

export const HEX_28_V1 = /^[0-9a-f]{56}$/u;
export const HEX_32_V1 = /^[0-9a-f]{64}$/u;
export const EVEN_HEX_V1 = /^(?:[0-9a-f]{2})+$/u;
export const NATURAL_DECIMAL_V1 = /^(?:0|[1-9][0-9]*)$/u;

export type ProductionNativeInclusionArtifactV1 = Readonly<{
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  transactionsPhasRoot: string;
  txMembershipProofCbor: string;
}>;

export const exactJournalRecordV1 = (
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

export const canonicalHexV1 = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

export const canonicalNaturalStringV1 = (
  value: unknown,
  label: string,
): string => {
  if (typeof value !== "string" || !NATURAL_DECIMAL_V1.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

export const safeNaturalNumberV1 = (value: unknown, label: string): number => {
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

export const admitProductionNativeInclusionArtifactV1 = (
  value: unknown,
  label: string,
): Readonly<{
  artifact: ProductionNativeInclusionArtifactV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const parsed = exactJournalRecordV1(
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
    nativeTxId: canonicalHexV1(parsed.nativeTxId, HEX_32_V1, `${label} tx id`),
    nativeTxCompactCbor: canonicalHexV1(
      parsed.nativeTxCompactCbor,
      EVEN_HEX_V1,
      `${label} compact transaction`,
    ),
    l2TransactionSourceCbor: canonicalHexV1(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX_V1,
      `${label} transaction source`,
    ),
    transactionsPhasRoot: canonicalHexV1(
      parsed.transactionsPhasRoot,
      HEX_32_V1,
      `${label} PHAS root`,
    ),
    txMembershipProofCbor: canonicalHexV1(
      parsed.txMembershipProofCbor,
      EVEN_HEX_V1,
      `${label} membership proof`,
    ),
  });
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompactV1(
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

export const admitProductionTxInputListV1 = (
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
      const parsed = exactJournalRecordV1(
        entry,
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

export const admitProductionOutputCborListV1 = (
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
      canonicalHexV1(item, EVEN_HEX_V1, `${label}[${index.toString()}]`),
    ),
  );
  const outputs = Object.freeze(
    json.map((item, index) => {
      const bytes = Buffer.from(item, "hex");
      const output = midgardTxOutputFromCanonicalCborV1(bytes);
      if (!encodeMidgardTxOutputCanonicalV1(output).equals(bytes)) {
        throw new Error(`${label}[${index.toString()}] is not canonical`);
      }
      return output;
    }),
  );
  return Object.freeze({ json, outputs });
};
