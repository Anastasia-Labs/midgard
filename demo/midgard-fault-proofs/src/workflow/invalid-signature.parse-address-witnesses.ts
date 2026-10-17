import { encodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import {
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { type JournalJsonObject } from "./journal.js";

export const INVALID_SIGNATURE_ARTIFACT =
  "midgard-production-invalid-signature-artifact-v1" as const;

export type InvalidSignatureArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INVALID_SIGNATURE_ARTIFACT;
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
    addressWitnesses: readonly Readonly<{
      verification_key: string;
      signature: string;
    }>[];
    badWitnessIndex: number;
  }>;

export type AdmittedArtifact = Readonly<{
  artifact: InvalidSignatureArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  addressWitnesses: readonly MidgardAddressWitness[];
  fieldPlan: FaultProofFieldOpeningPlan;
}>;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const HEX_64 = /^[0-9a-f]{128}$/u;

export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

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

export const safeNatural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

export const parseWitnessSet = (value: unknown): NativeTxWitnessSetCompact => {
  const parsed = exact(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "invalid-signature witness set",
  );
  return Object.freeze({
    addr_tx_wits_hash: hex(
      parsed.addr_tx_wits_hash,
      HEX_32,
      "address-witness hash",
    ),
    script_tx_wits_hash: hex(
      parsed.script_tx_wits_hash,
      HEX_32,
      "script-witness hash",
    ),
    redeemer_tx_wits_hash: hex(
      parsed.redeemer_tx_wits_hash,
      HEX_32,
      "redeemer-witness hash",
    ),
  });
};

export const parseAddressWitnesses = (
  value: unknown,
): readonly MidgardAddressWitness[] => {
  if (!Array.isArray(value)) {
    throw new Error("invalid-signature address witnesses must be an array");
  }
  return Object.freeze(
    value.map((item, index) => {
      const parsed = exact(
        item,
        ["verification_key", "signature"],
        `invalid-signature address witness ${index.toString()}`,
      );
      return Object.freeze({
        verification_key: hex(
          parsed.verification_key,
          HEX_32,
          `address witness ${index.toString()} verification key`,
        ),
        signature: hex(
          parsed.signature,
          HEX_64,
          `address witness ${index.toString()} signature`,
        ),
      });
    }),
  );
};

export const proofSteps = (
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

export const witnessSetCbor = (witnessSet: NativeTxWitnessSetCompact): string =>
  encodeMidgardNativeTxWitnessSetCompact({
    addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
    scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
    redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
  }).toString("hex");
