import {
  CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES,
  type CekProgramMaterialConcreteTransactionReceipt,
  type CekProgramMaterialLimitingConstraintType,
  type CekProgramMaterialNecessityReceiptSet,
  type CekProgramMaterialRoute,
  type CekProgramMaterialTransactionRole,
  CONCRETE_TRANSACTION_RECEIPT_KEYS,
  confirmationMillisecondsV1,
  decimal,
  exactHex,
  exactObject,
  outRefV1,
  type ParsedOutRef,
  positiveCount,
  safeCount,
  safeSignedInteger,
  signedDecimal,
} from "./validation-dispute-evidence.cek-program-material-necessity-receipt-set.js";

const outRefList = (value: unknown, label: string): readonly ParsedOutRef[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array`);
  }
  const outRefs = Object.freeze(
    value.map((candidate, index) =>
      outRefV1(candidate, `${label}[${index.toString()}]`),
    ),
  );
  const identities = new Set<string>();
  for (const outRef of outRefs) {
    if (identities.has(outRef.canonical)) {
      throw new Error(`${label} must not contain duplicate outrefs`);
    }
    identities.add(outRef.canonical);
  }
  return outRefs;
};

export type ParsedTargetProtocolParameters =
  CekProgramMaterialNecessityReceiptSet["targetProtocolParameters"];

export const transactionReceipt = ({
  value,
  target,
  label,
}: {
  readonly value: unknown;
  readonly target: ParsedTargetProtocolParameters;
  readonly label: string;
}): CekProgramMaterialConcreteTransactionReceipt => {
  const receipt = exactObject(value, CONCRETE_TRANSACTION_RECEIPT_KEYS, label);
  if (
    typeof receipt.role !== "string" ||
    !CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES.includes(
      receipt.role as CekProgramMaterialTransactionRole,
    )
  ) {
    throw new Error(`${label}.role is invalid`);
  }
  const transactionBytes = positiveCount(
    receipt.transactionBytes,
    `${label}.transactionBytes`,
  );
  const transactionByteMargin = safeSignedInteger(
    receipt.transactionByteMargin,
    `${label}.transactionByteMargin`,
  );
  const maximumValueBytes = safeCount(
    receipt.maximumValueBytes,
    `${label}.maximumValueBytes`,
  );
  const maximumValueByteMargin = safeSignedInteger(
    receipt.maximumValueByteMargin,
    `${label}.maximumValueByteMargin`,
  );
  const executionMemoryUnits = decimal(
    receipt.executionMemoryUnits,
    `${label}.executionMemoryUnits`,
  );
  const executionMemoryMargin = signedDecimal(
    receipt.executionMemoryMargin,
    `${label}.executionMemoryMargin`,
  );
  const executionCpuUnits = decimal(
    receipt.executionCpuUnits,
    `${label}.executionCpuUnits`,
  );
  const executionCpuMargin = signedDecimal(
    receipt.executionCpuMargin,
    `${label}.executionCpuMargin`,
  );
  const inputCount = safeCount(receipt.inputCount, `${label}.inputCount`);
  const referenceInputCount = safeCount(
    receipt.referenceInputCount,
    `${label}.referenceInputCount`,
  );
  const outputCount = safeCount(receipt.outputCount, `${label}.outputCount`);
  const programMaterialInputCount = safeCount(
    receipt.programMaterialInputCount,
    `${label}.programMaterialInputCount`,
  );
  const programMaterialReferenceInputCount = safeCount(
    receipt.programMaterialReferenceInputCount,
    `${label}.programMaterialReferenceInputCount`,
  );
  const txId = exactHex(receipt.txId, 32, `${label}.txId`);
  const programMaterialOutputOutRefs = outRefList(
    receipt.programMaterialOutputOutRefs,
    `${label}.programMaterialOutputOutRefs`,
  );
  const programMaterialConsumedInputOutRefs = outRefList(
    receipt.programMaterialConsumedInputOutRefs,
    `${label}.programMaterialConsumedInputOutRefs`,
  );
  const programMaterialReferenceInputOutRefs = outRefList(
    receipt.programMaterialReferenceInputOutRefs,
    `${label}.programMaterialReferenceInputOutRefs`,
  );
  const confirmationMilliseconds = confirmationMillisecondsV1(
    receipt.confirmationMilliseconds,
    `${label}.confirmationMilliseconds`,
  );
  if (
    transactionByteMargin !== target.maxTxSize - transactionBytes ||
    maximumValueByteMargin !== target.maxValueSize - maximumValueBytes ||
    BigInt(executionMemoryMargin) !==
      (BigInt(target.maxExecutionMemoryUnits) * 4n) / 5n -
        BigInt(executionMemoryUnits) ||
    BigInt(executionCpuMargin) !==
      (BigInt(target.maxExecutionCpuUnits) * 4n) / 5n -
        BigInt(executionCpuUnits)
  ) {
    throw new Error(`${label} contains a target-inconsistent measured margin`);
  }
  if (
    programMaterialInputCount !== programMaterialConsumedInputOutRefs.length ||
    programMaterialReferenceInputCount !==
      programMaterialReferenceInputOutRefs.length ||
    programMaterialInputCount > inputCount ||
    programMaterialReferenceInputCount > referenceInputCount
  ) {
    throw new Error(`${label} contains invalid program-material input counts`);
  }
  const consumedMaterialOutRefs = new Set(
    programMaterialConsumedInputOutRefs.map((outRef) => outRef.canonical),
  );
  if (
    programMaterialReferenceInputOutRefs.some((outRef) =>
      consumedMaterialOutRefs.has(outRef.canonical),
    )
  ) {
    throw new Error(
      `${label} program-material consumed and reference inputs must be disjoint`,
    );
  }
  for (let index = 0; index < programMaterialOutputOutRefs.length; index += 1) {
    const outRef = programMaterialOutputOutRefs[index]!;
    if (
      outRef.txId !== txId ||
      outRef.outputIndex >= outputCount ||
      (index > 0 &&
        programMaterialOutputOutRefs[index - 1]!.outputIndex >=
          outRef.outputIndex)
    ) {
      throw new Error(
        `${label} program-material output outrefs must bind increasing output indices of its txId`,
      );
    }
  }
  return Object.freeze({
    role: receipt.role as CekProgramMaterialTransactionRole,
    signedTxSha256: exactHex(
      receipt.signedTxSha256,
      32,
      `${label}.signedTxSha256`,
    ),
    txId,
    transactionBytes,
    transactionByteMargin,
    maximumValueBytes,
    maximumValueByteMargin,
    feeLovelace: decimal(receipt.feeLovelace, `${label}.feeLovelace`),
    minAdaLovelace: decimal(receipt.minAdaLovelace, `${label}.minAdaLovelace`),
    executionMemoryUnits,
    executionMemoryMargin,
    executionCpuUnits,
    executionCpuMargin,
    inputCount,
    referenceInputCount,
    outputCount,
    programMaterialInputCount,
    programMaterialReferenceInputCount,
    programMaterialOutputOutRefs: Object.freeze(
      programMaterialOutputOutRefs.map((outRef) => outRef.canonical),
    ),
    programMaterialConsumedInputOutRefs: Object.freeze(
      programMaterialConsumedInputOutRefs.map((outRef) => outRef.canonical),
    ),
    programMaterialReferenceInputOutRefs: Object.freeze(
      programMaterialReferenceInputOutRefs.map((outRef) => outRef.canonical),
    ),
    confirmationMilliseconds,
  });
};

const minimumNumber = (values: readonly number[]): number =>
  Math.min(...values);

const minimumBigInt = (values: readonly string[]): bigint =>
  values.reduce(
    (minimum, value) => (BigInt(value) < minimum ? BigInt(value) : minimum),
    BigInt(values[0]!),
  );

export const measuredConstraintMargin = ({
  constraint,
  transactions,
  maturityWindowMarginMilliseconds,
}: {
  readonly constraint: CekProgramMaterialLimitingConstraintType;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceipt[];
  readonly maturityWindowMarginMilliseconds: number;
}): string => {
  switch (constraint) {
    case "maxTxSize":
      return minimumNumber(
        transactions.map((receipt) => receipt.transactionByteMargin),
      ).toString();
    case "maxValueSize":
      return minimumNumber(
        transactions.map((receipt) => receipt.maximumValueByteMargin),
      ).toString();
    case "maxExecutionMemoryUnits":
      return minimumBigInt(
        transactions.map((receipt) => receipt.executionMemoryMargin),
      ).toString();
    case "maxExecutionCpuUnits":
      return minimumBigInt(
        transactions.map((receipt) => receipt.executionCpuMargin),
      ).toString();
    case "maturityWindowMilliseconds":
      return maturityWindowMarginMilliseconds.toString();
  }
};

export const exactOutRefSequence = (
  actual: readonly string[],
  expected: readonly string[],
): boolean =>
  actual.length === expected.length &&
  actual.every((outRef, index) => outRef === expected[index]);

export const materialSourceOutRefs = (
  receipt: CekProgramMaterialConcreteTransactionReceipt,
): readonly string[] => [
  ...receipt.programMaterialConsumedInputOutRefs,
  ...receipt.programMaterialReferenceInputOutRefs,
];

export const validateRouteTransactionGrammar = ({
  route,
  transactions,
  label,
}: {
  readonly route: CekProgramMaterialRoute;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceipt[];
  readonly label: string;
}): void => {
  const roles = transactions.map((receipt) => receipt.role);
  let valid = false;
  switch (route) {
    case "directProof":
      valid = roles.length === 1 && roles[0] === "proof";
      break;
    case "completeSinglePublicationReference":
      valid =
        roles.length === 2 &&
        roles[0] === "publication" &&
        roles[1] === "proofConsumption";
      break;
    case "minimumMultiOutputReconstruction":
      valid =
        roles.length >= 2 &&
        roles.at(-1) === "proofConsumption" &&
        roles.slice(0, -1).every((role) => role === "publication");
      break;
    case "incrementalTraversal": {
      const consumptionIndex = roles.indexOf("proofConsumption");
      valid =
        consumptionIndex >= 1 &&
        consumptionIndex < roles.length - 1 &&
        roles
          .slice(0, consumptionIndex)
          .every((role) => role === "publication") &&
        roles
          .slice(consumptionIndex + 1)
          .every((role) => role === "proofContinuation");
      break;
    }
  }
  if (!valid) {
    throw new Error(
      `${label}.transactions has invalid transaction-role grammar`,
    );
  }
};
