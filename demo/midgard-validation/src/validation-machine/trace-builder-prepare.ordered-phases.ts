import {
  encodeCbor,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";

import type { RejectedTx } from "../types.js";
import { RejectCodes } from "../types.js";
import { type ValidationMachineLedgerOp } from "./ledger-mutation.js";

/** Canonical rejection has a direct proof, but no validation-machine trace. */
export class DirectValidationTraceUnavailable extends Error {
  constructor(
    readonly rejectionCode:
      | typeof RejectCodes.InvalidFieldType
      | typeof RejectCodes.IsValidFalseForbidden,
  ) {
    super(`Canonical rejection ${rejectionCode} requires its direct proof`);
    this.name = "DirectValidationTraceUnavailable";
  }
}

export const exactHash32 = (hex: string, field: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(hex)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return Buffer.from(hex, "hex");
};

const canonicalLedgerOps = (
  operations: readonly ValidationMachineLedgerOp[],
): Buffer =>
  encodeCbor(
    operations.map((operation) =>
      operation.type === "delete"
        ? [0n, operation.key]
        : [1n, operation.key, operation.value],
    ),
  );

export const sameLedgerOps = (
  left: readonly ValidationMachineLedgerOp[],
  right: readonly ValidationMachineLedgerOp[],
): boolean => canonicalLedgerOps(left).equals(canonicalLedgerOps(right));

export const rejectionPhase = (
  rejection: RejectedTx,
): MidgardValidationPhaseName => {
  if (rejection.consensusPhase === undefined) {
    throw new Error(
      `V1 rejection ${rejection.code} is missing its exact consensus phase`,
    );
  }
  return rejection.consensusPhase;
};

export const orderedPhases: readonly MidgardValidationPhaseName[] = [
  "canonicalDecode",
  "compactBinding",
  "staticLedgerRules",
  "inputSets",
  "signatures",
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
  "resolveInputs",
  "scriptSources",
  "nativeScripts",
  "scriptIntegrity",
  "cek",
  "valueAndMint",
  "ledgerDelta",
];

export const safeBlockEndTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockEndTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};
