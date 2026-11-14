import { createHash } from "node:crypto";

import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";

import {
  type DeriveStateQueueCorrectionTransitionInput,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
} from "../src/index.js";

const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};

export const rehash = <T extends { readonly transitionDigest: string }>(
  value: T,
): T => {
  const { transitionDigest: _ignored, ...canonical } = value;
  return {
    ...canonical,
    transitionDigest: createHash("sha256")
      .update(canonicalJson(canonical))
      .digest("hex"),
  } as T;
};

export const outRef = (byte: number, index: number): string =>
  `${h32(byte)}#${index.toString()}`;

export const timeoutRedeemer = (
  value: StateQueueRedeemerType,
): DeriveStateQueueCorrectionTransitionInput["redeemers"] => [
  {
    purpose: "mint",
    index: "0",
    cborHex: Data.to(value, StateQueueRedeemer),
  },
];

export const common = {
  deploymentIdentityDigest: h32(0xaa),
  stateQueuePolicyId: h28(0xbb),
  transactionHash: h32(0xcc),
  blockHash: h32(0xdd),
  slot: "100",
  blockNo: "90",
  chainPointId: h32(0xee),
  finalityDepth: "2160",
  mintPolicyIds: [h28(0xbb)],
} as const;

export const timeoutLock = (target: string, terminal: boolean) => ({
  referenceInputOutRefs: [],
  correctionLockWitness: {
    kind: "correction_transition" as const,
    consumedOutRef: outRef(0xff, 0),
    continuedOutRef: outRef(0xcc, 9),
    targetHeaderHash: target,
    correctionIdentity: "AttestationTimeout" as const,
    previousDatum: "Idle" as const,
    nextDatum: terminal
      ? ("Idle" as const)
      : ({
          Locked: {
            target_header_hash: target,
            correction_identity: "AttestationTimeout" as const,
          },
        } as const),
  },
});

export const idleLockReference = {
  referenceInputOutRefs: [outRef(0xff, 0)],
  correctionLockWitness: {
    kind: "idle_reference" as const,
    referenceOutRef: outRef(0xff, 0),
    datum: "Idle" as const,
  },
};

export const fraudLock = (target: string, terminal: boolean) => {
  const correctionIdentity = {
    FraudProof: { fraud_proof_asset_name: `00000001${target}` },
  } as const;
  return {
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition" as const,
      consumedOutRef: outRef(0xff, 0),
      continuedOutRef: outRef(0xcc, 9),
      targetHeaderHash: target,
      correctionIdentity,
      previousDatum: "Idle" as const,
      nextDatum: terminal
        ? ("Idle" as const)
        : ({
            Locked: {
              target_header_hash: target,
              correction_identity: correctionIdentity,
            },
          } as const),
    },
  };
};
