import {
  encodeCbor,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";

import type { RejectedTx } from "../types.js";
import { RejectCodes } from "../types.js";
import { type ValidationMachineLedgerOp } from "./ledger-mutation.js";

/**
 * A rejection with a direct proof but no validation-machine trace: it is not
 * provable by trace. A canonical-decode refusal, and an undecodable Plutus
 * item (arm 48, a witness or an output reference script), which the machine
 * has no successor for and the script-decode family proves.
 */
export class DirectValidationTraceUnavailable extends Error {
  constructor(
    readonly rejectionCode:
      | typeof RejectCodes.InvalidFieldType
      | typeof RejectCodes.IsValidFalseForbidden
      | typeof RejectCodes.ScriptProgramEncoding,
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
];

export const safeBlockTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};
