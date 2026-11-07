import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { type NativeTxWitnessSetCompact } from "@al-ft/midgard-sdk";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { type MinFeeFieldItemCbors } from "../submit-min-fee-step-02.js";
import { type JournalJsonObject } from "./journal.js";

export const MIN_FEE_ARTIFACT =
  "midgard-production-min-fee-artifact-v1" as const;

export type MinFeeArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MIN_FEE_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProofCbor: string;
    witnessSet: Readonly<{
      addr_tx_wits_hash: string;
      script_tx_wits_hash: string;
      redeemer_tx_wits_hash: string;
    }>;
    fieldItemCbors: readonly (readonly string[])[];
    minFeeA: string;
    minFeeB: string;
    fee: string;
    canonicalTxSize: string;
    minimumFee: string;
    shortfall: string;
  }>;

export type AdmittedMinFeeArtifact = Readonly<{
  artifact: MinFeeArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  fieldItemCbors: MinFeeFieldItemCbors;
  fieldPlans: readonly FaultProofFieldOpeningPlan[];
}>;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const FIELD_COUNT = 9;

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

const naturalString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

const naturalNumber = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

export const witnessSetCore = (witnessSet: NativeTxWitnessSetCompact) => ({
  addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
  scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
  redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
});

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

const parseWitnessSet = (value: unknown): NativeTxWitnessSetCompact => {
  const parsed = exact(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "min-fee witness set",
  );
  return {
    addr_tx_wits_hash: canonicalHex(
      parsed.addr_tx_wits_hash,
      HEX_32,
      "address-witness hash",
    ),
    script_tx_wits_hash: canonicalHex(
      parsed.script_tx_wits_hash,
      HEX_32,
      "script-witness hash",
    ),
    redeemer_tx_wits_hash: canonicalHex(
      parsed.redeemer_tx_wits_hash,
      HEX_32,
      "redeemer-witness hash",
    ),
  };
};

export const parseFieldItems = (value: unknown): MinFeeFieldItemCbors => {
  if (!Array.isArray(value) || value.length !== FIELD_COUNT) {
    throw new Error("min-fee artifact requires exactly nine field item lists");
  }
  return value.map((items, fieldIndex) => {
    if (!Array.isArray(items)) {
      throw new Error(
        `min-fee field ${fieldIndex.toString()} items must be an array`,
      );
    }
    return items.map((item, itemIndex) =>
      Buffer.from(
        canonicalHex(
          item,
          EVEN_HEX,
          `min-fee field ${fieldIndex.toString()} item ${itemIndex.toString()}`,
        ),
        "hex",
      ),
    );
  }) as unknown as MinFeeFieldItemCbors;
};

export const parseArtifact = (
  value: unknown,
): Omit<AdmittedMinFeeArtifact, "fieldPlans"> => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
      "witnessSet",
      "fieldItemCbors",
      "minFeeA",
      "minFeeB",
      "fee",
      "canonicalTxSize",
      "minimumFee",
      "shortfall",
    ],
    "min-fee production artifact",
  );
  if (
    parsed.schemaVersion !== MIN_FEE_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("min-fee production artifact identity changed");
  }
  const artifact: MinFeeArtifact = Object.freeze({
    schemaVersion: MIN_FEE_ARTIFACT,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "min-fee header"),
    detectionId: parsed.detectionId,
    position: naturalNumber(parsed.position, "min-fee position"),
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, "min-fee tx id"),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "min-fee compact tx",
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "min-fee transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "min-fee transactions PHAS root",
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "min-fee transaction proof",
    ),
    witnessSet: parseWitnessSet(parsed.witnessSet),
    fieldItemCbors: Array.isArray(parsed.fieldItemCbors)
      ? parsed.fieldItemCbors.map((items) =>
          Array.isArray(items) ? Object.freeze([...items] as string[]) : [],
        )
      : [],
    minFeeA: naturalString(parsed.minFeeA, "minFeeA"),
    minFeeB: naturalString(parsed.minFeeB, "minFeeB"),
    fee: naturalString(parsed.fee, "fee"),
    canonicalTxSize: naturalString(parsed.canonicalTxSize, "canonicalTxSize"),
    minimumFee: naturalString(parsed.minimumFee, "minimumFee"),
    shortfall: naturalString(parsed.shortfall, "shortfall"),
  });
  const fieldItemCbors = parseFieldItems(parsed.fieldItemCbors);
  const compact = decodeMidgardNativeTxCompact(
    Buffer.from(artifact.nativeTxCompactCbor, "hex"),
  );
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
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
    throw new Error("min-fee transaction proof cannot be replayed");
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error("min-fee transaction proof does not open its PHAS root");
  }
  return Object.freeze({
    artifact,
    inclusion,
    witnessSet: artifact.witnessSet,
    fieldItemCbors,
  });
};
