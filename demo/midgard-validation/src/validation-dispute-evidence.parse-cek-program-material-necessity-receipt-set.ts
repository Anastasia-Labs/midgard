import {
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardValidationDispute,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";

import {
  CEK_PROGRAM_MATERIAL_ROUTE_ORDER,
  type CekProgramMaterialNecessityReceiptSet,
  decimal,
  exactHex,
  exactObject,
  positiveCount,
  RECEIPT_SET_KEYS,
  TARGET_PROTOCOL_PARAMETER_KEYS,
  VALIDATOR_IDENTITY_KEYS,
} from "./validation-dispute-evidence.cek-program-material-necessity-receipt-set.js";
import { routeAttempt } from "./validation-dispute-evidence.route-attempt.js";
import { type ValidationOneStepArgument } from "./validation-machine-data.js";

/**
 * Parses the exact JSON-safe C28 necessity ABI. Complete routes are measured
 * concrete rejections, incremental traversal is a separate measured fit, and
 * every claimed margin is recomputed against the bound target. Unknown keys
 * and omitted identity or transaction fields are rejected without defaults.
 */
export const parseCekProgramMaterialNecessityReceiptSet = (
  value: unknown,
): CekProgramMaterialNecessityReceiptSet => {
  const receiptSet = exactObject(
    value,
    RECEIPT_SET_KEYS,
    "CEK program-material necessity receipt set",
  );
  if (receiptSet.schemaVersion !== 1) {
    throw new Error("CEK program-material necessity receipt set must be V1");
  }
  const validatorIdentityValues = receiptSet.validatorIdentities;
  if (
    !Array.isArray(validatorIdentityValues) ||
    validatorIdentityValues.length === 0
  ) {
    throw new Error(
      "CEK program-material necessity receipt set requires validator identities",
    );
  }
  const validatorIdentities = Object.freeze(
    validatorIdentityValues.map((value, index) => {
      const identity = exactObject(
        value,
        VALIDATOR_IDENTITY_KEYS,
        `validatorIdentities[${index.toString()}]`,
      );
      if (
        typeof identity.title !== "string" ||
        identity.title.length === 0 ||
        identity.title.trim() !== identity.title
      ) {
        throw new Error(
          `validatorIdentities[${index.toString()}].title must be a non-empty exact title`,
        );
      }
      return Object.freeze({
        title: identity.title,
        generatedHash: exactHex(
          identity.generatedHash,
          28,
          `validatorIdentities[${index.toString()}].generatedHash`,
        ),
        appliedHash: exactHex(
          identity.appliedHash,
          28,
          `validatorIdentities[${index.toString()}].appliedHash`,
        ),
      });
    }),
  );
  for (let index = 1; index < validatorIdentities.length; index += 1) {
    if (
      validatorIdentities[index - 1]!.title >= validatorIdentities[index]!.title
    ) {
      throw new Error(
        "validator identity titles must be strictly sorted without duplicates",
      );
    }
  }
  const targetValue = exactObject(
    receiptSet.targetProtocolParameters,
    TARGET_PROTOCOL_PARAMETER_KEYS,
    "target protocol parameters",
  );
  const targetProtocolParameters = Object.freeze({
    digest: exactHex(targetValue.digest, 32, "targetProtocolParameters.digest"),
    maxTxSize: positiveCount(
      targetValue.maxTxSize,
      "targetProtocolParameters.maxTxSize",
    ),
    maxValueSize: positiveCount(
      targetValue.maxValueSize,
      "targetProtocolParameters.maxValueSize",
    ),
    maxExecutionMemoryUnits: decimal(
      targetValue.maxExecutionMemoryUnits,
      "targetProtocolParameters.maxExecutionMemoryUnits",
    ),
    maxExecutionCpuUnits: decimal(
      targetValue.maxExecutionCpuUnits,
      "targetProtocolParameters.maxExecutionCpuUnits",
    ),
    coinsPerUtxoByte: decimal(
      targetValue.coinsPerUtxoByte,
      "targetProtocolParameters.coinsPerUtxoByte",
    ),
    maturityWindowMilliseconds: positiveCount(
      targetValue.maturityWindowMilliseconds,
      "targetProtocolParameters.maturityWindowMilliseconds",
    ),
  });
  if (
    BigInt(targetProtocolParameters.maxExecutionMemoryUnits) === 0n ||
    BigInt(targetProtocolParameters.maxExecutionCpuUnits) === 0n ||
    BigInt(targetProtocolParameters.coinsPerUtxoByte) === 0n
  ) {
    throw new Error(
      "target protocol parameter decimal limits must be positive",
    );
  }
  const routeAttemptValues = receiptSet.routeAttempts;
  if (
    !Array.isArray(routeAttemptValues) ||
    routeAttemptValues.length !== CEK_PROGRAM_MATERIAL_ROUTE_ORDER.length
  ) {
    throw new Error(
      "CEK program-material necessity receipt set requires exactly four ordered route attempts",
    );
  }
  const routeAttempts = CEK_PROGRAM_MATERIAL_ROUTE_ORDER.map((route, index) =>
    routeAttempt({
      value: routeAttemptValues[index],
      route,
      expectedFit: route === "incrementalTraversal",
      target: targetProtocolParameters,
      label: `routeAttempts[${index.toString()}]`,
    }),
  ) as unknown as CekProgramMaterialNecessityReceiptSet["routeAttempts"];
  const signedTransactionHashes = new Set<string>();
  const transactionIds = new Set<string>();
  for (const attempt of routeAttempts) {
    for (const transaction of attempt.transactions) {
      if (
        signedTransactionHashes.has(transaction.signedTxSha256) ||
        transactionIds.has(transaction.txId)
      ) {
        throw new Error(
          "CEK program-material necessity receipts contain duplicate transaction identities",
        );
      }
      signedTransactionHashes.add(transaction.signedTxSha256);
      transactionIds.add(transaction.txId);
    }
  }
  return Object.freeze({
    schemaVersion: 1,
    sourceRevision: exactHex(receiptSet.sourceRevision, 20, "sourceRevision"),
    programEnvelopeHash: exactHex(
      receiptSet.programEnvelopeHash,
      32,
      "programEnvelopeHash",
    ),
    validatorIdentities,
    targetProtocolParameters,
    routeAttempts,
  });
};

export const CekProgramMaterialNecessityReceiptSetSchema = Object.freeze({
  parse: parseCekProgramMaterialNecessityReceiptSet,
});

export type ValidationDisputeEvidenceMove = {
  readonly role: "operator" | "challenger";
  readonly disputeBefore: MidgardValidationDispute;
  readonly proof: MidgardValidationTraceProof;
  readonly proofCbor: Buffer;
  readonly disputeAfter: MidgardValidationDispute;
  readonly disputeAfterCbor: Buffer;
};

export type ValidationDisputeEvidenceBundle = {
  readonly operatorDescriptorCbor: Buffer;
  readonly challengerDescriptorCbor: Buffer;
  readonly openingDispute: MidgardValidationDispute;
  readonly openingDisputeCbor: Buffer;
  readonly moves: readonly ValidationDisputeEvidenceMove[];
  readonly finalDispute: MidgardValidationDispute;
  readonly finalDisputeCbor: Buffer;
  readonly boundaryEvidenceCbor: Buffer;
  readonly oneStepArgument: ValidationOneStepArgument;
};

export const requireProofEnvelope = (
  bytes: Uint8Array,
  label: string,
): void => {
  if (bytes.length >= MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes) {
    throw new Error(
      `${label} exceeds the strict L1 proof envelope: ${bytes.length.toString()} bytes`,
    );
  }
};
