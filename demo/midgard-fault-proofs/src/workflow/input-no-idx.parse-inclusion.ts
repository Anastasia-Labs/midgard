import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import {
  parseSubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxInputsPreimage,
} from "../submit-input-no-idx-step-02.js";
import {
  parseSubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxOutputsPreimage,
} from "../submit-input-no-idx-step-04.js";
import { type JournalJsonObject } from "./journal.js";

export const INPUT_NO_IDX_ARTIFACT =
  "midgard-production-input-no-idx-artifact-v1" as const;

type InclusionJson = Readonly<{
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  transactionsPhasRoot: string;
  txMembershipProofCbor: string;
}>;

export type InputNoIdxArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_NO_IDX_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: InclusionJson;
    producingTx: InclusionJson;
    inputs: readonly Readonly<{ tx_id: string; output_index: string }>[];
    badInputsIndex: number;
    outputsPreimageCbor: readonly string[];
    badInputOutputIndex: string;
  }>;

export type AdmittedArtifact = Readonly<{
  artifact: InputNoIdxArtifact;
  badInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  producingInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  inputs: SubmitInputNoIdxInputsPreimage;
  outputs: SubmitInputNoIdxOutputsPreimage;
  inputFieldPlan: FaultProofFieldOpeningPlan;
  outputFieldPlan: FaultProofFieldOpeningPlan;
}>;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

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

export const exact = (
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

export const hex = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

export const naturalString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

export const safeNatural = (value: unknown, label: string): number => {
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

export const parseInclusion = (
  value: unknown,
  label: string,
): Readonly<{
  artifact: InclusionJson;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const parsed = exact(
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
    nativeTxId: hex(parsed.nativeTxId, HEX_32, `${label} tx id`),
    nativeTxCompactCbor: hex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      `${label} compact tx`,
    ),
    l2TransactionSourceCbor: hex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      `${label} source`,
    ),
    transactionsPhasRoot: hex(
      parsed.transactionsPhasRoot,
      HEX_32,
      `${label} root`,
    ),
    txMembershipProofCbor: hex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      `${label} proof`,
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

export const parseInputs = (
  value: unknown,
  badInputsIndex: number,
): Readonly<{
  json: readonly Readonly<{ tx_id: string; output_index: string }>[];
  parsed: SubmitInputNoIdxInputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact inputs must be an array");
  }
  const json = Object.freeze(
    value.map((entry, index) => {
      const parsed = exact(
        entry,
        ["tx_id", "output_index"],
        `input-no-idx artifact input ${index.toString()}`,
      );
      return Object.freeze({
        tx_id: hex(parsed.tx_id, HEX_32, "input transaction id"),
        output_index: naturalString(parsed.output_index, "input output index"),
      });
    }),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxInputsPreimage({
      inputsPreimage: json,
      badInputsIndex,
    }),
  });
};

export const parseOutputCbors = (
  value: unknown,
): Readonly<{
  json: readonly string[];
  parsed: SubmitInputNoIdxOutputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact outputs must be an array");
  }
  const json = Object.freeze(
    value.map((item, index) =>
      hex(item, EVEN_HEX, `input-no-idx output ${index.toString()}`),
    ),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxOutputsPreimage({
      outputsPreimageCbor: json,
    }),
  });
};
