export const CEK_PROGRAM_MATERIAL_ROUTE_ORDER = Object.freeze([
  "directProof",
  "completeSinglePublicationReference",
  "minimumMultiOutputReconstruction",
  "incrementalTraversal",
] as const);

export type CekProgramMaterialRoute =
  (typeof CEK_PROGRAM_MATERIAL_ROUTE_ORDER)[number];

export const CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES = Object.freeze([
  "publication",
  "proof",
  "proofConsumption",
  "proofContinuation",
] as const);

export type CekProgramMaterialTransactionRole =
  (typeof CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES)[number];

export const CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS = Object.freeze([
  "maxTxSize",
  "maxValueSize",
  "maxExecutionMemoryUnits",
  "maxExecutionCpuUnits",
  "maturityWindowMilliseconds",
] as const);

export type CekProgramMaterialLimitingConstraintType =
  (typeof CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS)[number];

export type CekProgramMaterialConcreteTransactionReceipt<
  Role extends
    CekProgramMaterialTransactionRole = CekProgramMaterialTransactionRole,
> = {
  readonly role: Role;
  readonly signedTxSha256: string;
  readonly txId: string;
  readonly transactionBytes: number;
  readonly transactionByteMargin: number;
  readonly maximumValueBytes: number;
  readonly maximumValueByteMargin: number;
  readonly feeLovelace: string;
  readonly minAdaLovelace: string;
  readonly executionMemoryUnits: string;
  readonly executionMemoryMargin: string;
  readonly executionCpuUnits: string;
  readonly executionCpuMargin: string;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly programMaterialInputCount: number;
  readonly programMaterialReferenceInputCount: number;
  readonly programMaterialOutputOutRefs: readonly string[];
  readonly programMaterialConsumedInputOutRefs: readonly string[];
  readonly programMaterialReferenceInputOutRefs: readonly string[];
  readonly confirmationMilliseconds: number;
};

export type CekProgramMaterialLimitingConstraint = {
  readonly type: CekProgramMaterialLimitingConstraintType;
  readonly measuredMargin: string;
};

export type CekProgramMaterialRouteAttempt<
  Route extends CekProgramMaterialRoute,
  Transactions extends readonly CekProgramMaterialConcreteTransactionReceipt[],
  MinimumMultiOutputCount extends number | null,
> = {
  readonly route: Route;
  readonly transactions: Transactions;
  readonly dataAvailabilityFetchMilliseconds: number;
  readonly evidenceConstructionMilliseconds: number;
  readonly retryMilliseconds: number;
  readonly rollbackAllowanceMilliseconds: number;
  readonly settlementMilliseconds: number;
  readonly removalMilliseconds: number;
  readonly maturityWindowMarginMilliseconds: number;
  readonly fit: boolean;
  readonly limitingConstraint: CekProgramMaterialLimitingConstraint | null;
  readonly minimumMultiOutputCount: MinimumMultiOutputCount;
};

export type CekProgramMaterialNecessityReceiptSet = {
  readonly schemaVersion: 1;
  readonly sourceRevision: string;
  readonly programEnvelopeHash: string;
  readonly validatorIdentities: readonly {
    readonly title: string;
    readonly generatedHash: string;
    readonly appliedHash: string;
  }[];
  readonly targetProtocolParameters: {
    readonly digest: string;
    readonly maxTxSize: number;
    readonly maxValueSize: number;
    readonly maxExecutionMemoryUnits: string;
    readonly maxExecutionCpuUnits: string;
    readonly coinsPerUtxoByte: string;
    readonly maturityWindowMilliseconds: number;
  };
  readonly routeAttempts: readonly [
    CekProgramMaterialRouteAttempt<
      "directProof",
      readonly [CekProgramMaterialConcreteTransactionReceipt<"proof">],
      null
    >,
    CekProgramMaterialRouteAttempt<
      "completeSinglePublicationReference",
      readonly [
        CekProgramMaterialConcreteTransactionReceipt<"publication">,
        CekProgramMaterialConcreteTransactionReceipt<"proofConsumption">,
      ],
      null
    >,
    CekProgramMaterialRouteAttempt<
      "minimumMultiOutputReconstruction",
      readonly [
        CekProgramMaterialConcreteTransactionReceipt<"publication">,
        ...CekProgramMaterialConcreteTransactionReceipt<"publication">[],
        CekProgramMaterialConcreteTransactionReceipt<"proofConsumption">,
      ],
      number
    >,
    CekProgramMaterialRouteAttempt<
      "incrementalTraversal",
      readonly [
        CekProgramMaterialConcreteTransactionReceipt<"publication">,
        ...CekProgramMaterialConcreteTransactionReceipt[],
        CekProgramMaterialConcreteTransactionReceipt<"proofContinuation">,
      ],
      null
    >,
  ];
};

export const RECEIPT_SET_KEYS = Object.freeze([
  "schemaVersion",
  "sourceRevision",
  "programEnvelopeHash",
  "validatorIdentities",
  "targetProtocolParameters",
  "routeAttempts",
] as const);

export const VALIDATOR_IDENTITY_KEYS = Object.freeze([
  "title",
  "generatedHash",
  "appliedHash",
] as const);

export const TARGET_PROTOCOL_PARAMETER_KEYS = Object.freeze([
  "digest",
  "maxTxSize",
  "maxValueSize",
  "maxExecutionMemoryUnits",
  "maxExecutionCpuUnits",
  "coinsPerUtxoByte",
  "maturityWindowMilliseconds",
] as const);

export const CONCRETE_TRANSACTION_RECEIPT_KEYS = Object.freeze([
  "role",
  "signedTxSha256",
  "txId",
  "transactionBytes",
  "transactionByteMargin",
  "maximumValueBytes",
  "maximumValueByteMargin",
  "feeLovelace",
  "minAdaLovelace",
  "executionMemoryUnits",
  "executionMemoryMargin",
  "executionCpuUnits",
  "executionCpuMargin",
  "inputCount",
  "referenceInputCount",
  "outputCount",
  "programMaterialInputCount",
  "programMaterialReferenceInputCount",
  "programMaterialOutputOutRefs",
  "programMaterialConsumedInputOutRefs",
  "programMaterialReferenceInputOutRefs",
  "confirmationMilliseconds",
] as const);

export const ROUTE_ATTEMPT_KEYS = Object.freeze([
  "route",
  "transactions",
  "dataAvailabilityFetchMilliseconds",
  "evidenceConstructionMilliseconds",
  "retryMilliseconds",
  "rollbackAllowanceMilliseconds",
  "settlementMilliseconds",
  "removalMilliseconds",
  "maturityWindowMarginMilliseconds",
  "fit",
  "limitingConstraint",
  "minimumMultiOutputCount",
] as const);

export const LIMITING_CONSTRAINT_KEYS = Object.freeze([
  "type",
  "measuredMargin",
] as const);

export const exactObject = <Keys extends readonly string[]>(
  value: unknown,
  keys: Keys,
  label: string,
): Record<Keys[number], unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} must contain exactly ${keys.join(", ")}`);
  }
  return value as Record<Keys[number], unknown>;
};

export const exactHex = (
  value: unknown,
  bytes: number,
  label: string,
): string => {
  if (
    typeof value !== "string" ||
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
  ) {
    throw new Error(`${label} must be ${bytes.toString()}-byte lowercase hex`);
  }
  return value;
};

export const decimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical non-negative decimal string`);
  }
  return value;
};

export const signedDecimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^-?(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical signed decimal string`);
  }
  return value;
};

export const safeCount = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

export const positiveCount = (value: unknown, label: string): number => {
  const count = safeCount(value, label);
  if (count === 0) {
    throw new Error(`${label} must be positive`);
  }
  return count;
};

export const safeSignedInteger = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value)) {
    throw new Error(`${label} must be a safe integer`);
  }
  return value as number;
};

export const confirmationMillisecondsV1 = (
  value: unknown,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

export type ParsedOutRef = {
  readonly canonical: string;
  readonly txId: string;
  readonly outputIndex: number;
};

export const outRefV1 = (value: unknown, label: string): ParsedOutRef => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a canonical transaction outref`);
  }
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(value);
  if (match === null) {
    throw new Error(`${label} must be canonical lowercase txid#index`);
  }
  const outputIndex = Number(match[2]);
  if (!Number.isSafeInteger(outputIndex) || outputIndex > 65_535) {
    throw new Error(`${label} output index must be a canonical uint16`);
  }
  return Object.freeze({
    canonical: value,
    txId: match[1]!,
    outputIndex,
  });
};
