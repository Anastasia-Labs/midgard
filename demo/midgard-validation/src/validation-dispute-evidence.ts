import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  type MidgardValidationDisputeV1,
  type MidgardValidationTraceProofV1,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";

import type { DeterministicValidationMachineTrace } from "./validation-machine.js";
import {
  buildValidationOneStepArgumentV1,
  type ValidationOneStepArgumentV1,
} from "./validation-machine-data.js";
import {
  encodeValidationBoundaryEvidenceCborV1,
  encodeValidationDisputeDataCborV1,
  encodeValidationTraceDescriptorDataCborV1,
  encodeValidationTraceProofDataCborV1,
} from "./validation-one-step-data.js";

export const CEK_PROGRAM_MATERIAL_ROUTE_ORDER_V1 = Object.freeze([
  "directProof",
  "completeSinglePublicationReference",
  "minimumMultiOutputReconstruction",
  "incrementalTraversal",
] as const);

export type CekProgramMaterialRouteV1 =
  (typeof CEK_PROGRAM_MATERIAL_ROUTE_ORDER_V1)[number];

export const CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES_V1 = Object.freeze([
  "publication",
  "proof",
  "proofConsumption",
  "proofContinuation",
] as const);

export type CekProgramMaterialTransactionRoleV1 =
  (typeof CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES_V1)[number];

export const CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS_V1 = Object.freeze([
  "maxTxSize",
  "maxValueSize",
  "maxExecutionMemoryUnits",
  "maxExecutionCpuUnits",
  "maturityWindowMilliseconds",
] as const);

export type CekProgramMaterialLimitingConstraintTypeV1 =
  (typeof CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS_V1)[number];

export type CekProgramMaterialConcreteTransactionReceiptV1<
  Role extends
    CekProgramMaterialTransactionRoleV1 = CekProgramMaterialTransactionRoleV1,
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

export type CekProgramMaterialLimitingConstraintV1 = {
  readonly type: CekProgramMaterialLimitingConstraintTypeV1;
  readonly measuredMargin: string;
};

export type CekProgramMaterialRouteAttemptV1<
  Route extends CekProgramMaterialRouteV1,
  Transactions extends
    readonly CekProgramMaterialConcreteTransactionReceiptV1[],
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
  readonly limitingConstraint: CekProgramMaterialLimitingConstraintV1 | null;
  readonly minimumMultiOutputCount: MinimumMultiOutputCount;
};

export type CekProgramMaterialNecessityReceiptSetV1 = {
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
    CekProgramMaterialRouteAttemptV1<
      "directProof",
      readonly [CekProgramMaterialConcreteTransactionReceiptV1<"proof">],
      null
    >,
    CekProgramMaterialRouteAttemptV1<
      "completeSinglePublicationReference",
      readonly [
        CekProgramMaterialConcreteTransactionReceiptV1<"publication">,
        CekProgramMaterialConcreteTransactionReceiptV1<"proofConsumption">,
      ],
      null
    >,
    CekProgramMaterialRouteAttemptV1<
      "minimumMultiOutputReconstruction",
      readonly [
        CekProgramMaterialConcreteTransactionReceiptV1<"publication">,
        ...CekProgramMaterialConcreteTransactionReceiptV1<"publication">[],
        CekProgramMaterialConcreteTransactionReceiptV1<"proofConsumption">,
      ],
      number
    >,
    CekProgramMaterialRouteAttemptV1<
      "incrementalTraversal",
      readonly [
        CekProgramMaterialConcreteTransactionReceiptV1<"publication">,
        ...CekProgramMaterialConcreteTransactionReceiptV1[],
        CekProgramMaterialConcreteTransactionReceiptV1<"proofContinuation">,
      ],
      null
    >,
  ];
};

const RECEIPT_SET_KEYS_V1 = Object.freeze([
  "schemaVersion",
  "sourceRevision",
  "programEnvelopeHash",
  "validatorIdentities",
  "targetProtocolParameters",
  "routeAttempts",
] as const);
const VALIDATOR_IDENTITY_KEYS_V1 = Object.freeze([
  "title",
  "generatedHash",
  "appliedHash",
] as const);
const TARGET_PROTOCOL_PARAMETER_KEYS_V1 = Object.freeze([
  "digest",
  "maxTxSize",
  "maxValueSize",
  "maxExecutionMemoryUnits",
  "maxExecutionCpuUnits",
  "coinsPerUtxoByte",
  "maturityWindowMilliseconds",
] as const);
const CONCRETE_TRANSACTION_RECEIPT_KEYS_V1 = Object.freeze([
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
const ROUTE_ATTEMPT_KEYS_V1 = Object.freeze([
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
const LIMITING_CONSTRAINT_KEYS_V1 = Object.freeze([
  "type",
  "measuredMargin",
] as const);

const exactObjectV1 = <Keys extends readonly string[]>(
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

const exactHexV1 = (value: unknown, bytes: number, label: string): string => {
  if (
    typeof value !== "string" ||
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
  ) {
    throw new Error(`${label} must be ${bytes.toString()}-byte lowercase hex`);
  }
  return value;
};

const decimalV1 = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical non-negative decimal string`);
  }
  return value;
};

const signedDecimalV1 = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^-?(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical signed decimal string`);
  }
  return value;
};

const safeCountV1 = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const positiveCountV1 = (value: unknown, label: string): number => {
  const count = safeCountV1(value, label);
  if (count === 0) {
    throw new Error(`${label} must be positive`);
  }
  return count;
};

const safeSignedIntegerV1 = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value)) {
    throw new Error(`${label} must be a safe integer`);
  }
  return value as number;
};

const confirmationMillisecondsV1 = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

type ParsedOutRefV1 = {
  readonly canonical: string;
  readonly txId: string;
  readonly outputIndex: number;
};

const outRefV1 = (value: unknown, label: string): ParsedOutRefV1 => {
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

const outRefListV1 = (
  value: unknown,
  label: string,
): readonly ParsedOutRefV1[] => {
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

type ParsedTargetProtocolParametersV1 =
  CekProgramMaterialNecessityReceiptSetV1["targetProtocolParameters"];

const transactionReceiptV1 = ({
  value,
  target,
  label,
}: {
  readonly value: unknown;
  readonly target: ParsedTargetProtocolParametersV1;
  readonly label: string;
}): CekProgramMaterialConcreteTransactionReceiptV1 => {
  const receipt = exactObjectV1(
    value,
    CONCRETE_TRANSACTION_RECEIPT_KEYS_V1,
    label,
  );
  if (
    typeof receipt.role !== "string" ||
    !CEK_PROGRAM_MATERIAL_TRANSACTION_ROLES_V1.includes(
      receipt.role as CekProgramMaterialTransactionRoleV1,
    )
  ) {
    throw new Error(`${label}.role is invalid`);
  }
  const transactionBytes = positiveCountV1(
    receipt.transactionBytes,
    `${label}.transactionBytes`,
  );
  const transactionByteMargin = safeSignedIntegerV1(
    receipt.transactionByteMargin,
    `${label}.transactionByteMargin`,
  );
  const maximumValueBytes = safeCountV1(
    receipt.maximumValueBytes,
    `${label}.maximumValueBytes`,
  );
  const maximumValueByteMargin = safeSignedIntegerV1(
    receipt.maximumValueByteMargin,
    `${label}.maximumValueByteMargin`,
  );
  const executionMemoryUnits = decimalV1(
    receipt.executionMemoryUnits,
    `${label}.executionMemoryUnits`,
  );
  const executionMemoryMargin = signedDecimalV1(
    receipt.executionMemoryMargin,
    `${label}.executionMemoryMargin`,
  );
  const executionCpuUnits = decimalV1(
    receipt.executionCpuUnits,
    `${label}.executionCpuUnits`,
  );
  const executionCpuMargin = signedDecimalV1(
    receipt.executionCpuMargin,
    `${label}.executionCpuMargin`,
  );
  const inputCount = safeCountV1(receipt.inputCount, `${label}.inputCount`);
  const referenceInputCount = safeCountV1(
    receipt.referenceInputCount,
    `${label}.referenceInputCount`,
  );
  const outputCount = safeCountV1(receipt.outputCount, `${label}.outputCount`);
  const programMaterialInputCount = safeCountV1(
    receipt.programMaterialInputCount,
    `${label}.programMaterialInputCount`,
  );
  const programMaterialReferenceInputCount = safeCountV1(
    receipt.programMaterialReferenceInputCount,
    `${label}.programMaterialReferenceInputCount`,
  );
  const txId = exactHexV1(receipt.txId, 32, `${label}.txId`);
  const programMaterialOutputOutRefs = outRefListV1(
    receipt.programMaterialOutputOutRefs,
    `${label}.programMaterialOutputOutRefs`,
  );
  const programMaterialConsumedInputOutRefs = outRefListV1(
    receipt.programMaterialConsumedInputOutRefs,
    `${label}.programMaterialConsumedInputOutRefs`,
  );
  const programMaterialReferenceInputOutRefs = outRefListV1(
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
    role: receipt.role as CekProgramMaterialTransactionRoleV1,
    signedTxSha256: exactHexV1(
      receipt.signedTxSha256,
      32,
      `${label}.signedTxSha256`,
    ),
    txId,
    transactionBytes,
    transactionByteMargin,
    maximumValueBytes,
    maximumValueByteMargin,
    feeLovelace: decimalV1(receipt.feeLovelace, `${label}.feeLovelace`),
    minAdaLovelace: decimalV1(
      receipt.minAdaLovelace,
      `${label}.minAdaLovelace`,
    ),
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

const measuredConstraintMarginV1 = ({
  constraint,
  transactions,
  maturityWindowMarginMilliseconds,
}: {
  readonly constraint: CekProgramMaterialLimitingConstraintTypeV1;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceiptV1[];
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

const exactOutRefSequenceV1 = (
  actual: readonly string[],
  expected: readonly string[],
): boolean =>
  actual.length === expected.length &&
  actual.every((outRef, index) => outRef === expected[index]);

const materialSourceOutRefsV1 = (
  receipt: CekProgramMaterialConcreteTransactionReceiptV1,
): readonly string[] => [
  ...receipt.programMaterialConsumedInputOutRefs,
  ...receipt.programMaterialReferenceInputOutRefs,
];

const validateRouteTransactionGrammarV1 = ({
  route,
  transactions,
  label,
}: {
  readonly route: CekProgramMaterialRouteV1;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceiptV1[];
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

const validateRouteMaterialLinkageV1 = ({
  route,
  transactions,
  minimumMultiOutputCount,
  label,
}: {
  readonly route: CekProgramMaterialRouteV1;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceiptV1[];
  readonly minimumMultiOutputCount: number | null;
  readonly label: string;
}): void => {
  const publications = transactions.filter(
    (receipt) => receipt.role === "publication",
  );
  for (const publication of publications) {
    if (
      materialSourceOutRefsV1(publication).length !== 0 ||
      publication.programMaterialOutputOutRefs.length === 0
    ) {
      throw new Error(
        `${label} publications must create material without sourcing prior material`,
      );
    }
  }
  for (const receipt of transactions) {
    if (
      receipt.role !== "publication" &&
      receipt.programMaterialOutputOutRefs.length !== 0
    ) {
      throw new Error(
        `${label} only publication receipts may create program-material outputs`,
      );
    }
  }
  if (route === "directProof") {
    const proof = transactions[0]!;
    if (
      proof.programMaterialInputCount !== 0 ||
      proof.programMaterialReferenceInputCount !== 0 ||
      materialSourceOutRefsV1(proof).length !== 0 ||
      proof.programMaterialOutputOutRefs.length !== 0
    ) {
      throw new Error(`${label} direct proof must not source program material`);
    }
    return;
  }
  const publicationOutRefs = publications.flatMap(
    (publication) => publication.programMaterialOutputOutRefs,
  );
  if (new Set(publicationOutRefs).size !== publicationOutRefs.length) {
    throw new Error(`${label} contains duplicate published material outrefs`);
  }
  if (route === "completeSinglePublicationReference") {
    const consumption = transactions[1]!;
    if (
      publicationOutRefs.length !== 1 ||
      !exactOutRefSequenceV1(
        materialSourceOutRefsV1(consumption),
        publicationOutRefs,
      )
    ) {
      throw new Error(
        `${label} single publication and consumption material outrefs do not match`,
      );
    }
    return;
  }
  if (route === "minimumMultiOutputReconstruction") {
    const consumption = transactions.at(-1)!;
    if (
      minimumMultiOutputCount === null ||
      publicationOutRefs.length !== minimumMultiOutputCount ||
      !exactOutRefSequenceV1(
        materialSourceOutRefsV1(consumption),
        publicationOutRefs,
      )
    ) {
      throw new Error(
        `${label} multi-output publication and reconstruction sources do not match the exact minimum`,
      );
    }
    return;
  }
  const published = new Set(publicationOutRefs);
  const traversed = new Set<string>();
  for (const receipt of transactions.slice(publications.length)) {
    const sources = materialSourceOutRefsV1(receipt);
    if (
      sources.length === 0 ||
      sources.some((outRef) => !published.has(outRef))
    ) {
      throw new Error(
        `${label} incremental proof receipt contains an empty or unknown material source`,
      );
    }
    for (const source of sources) traversed.add(source);
  }
  if (
    traversed.size !== published.size ||
    publicationOutRefs.some((outRef) => !traversed.has(outRef))
  ) {
    throw new Error(
      `${label} incremental proof receipts omit published material sources`,
    );
  }
};

const routeAttemptV1 = ({
  value,
  route,
  expectedFit,
  target,
  label,
}: {
  readonly value: unknown;
  readonly route: CekProgramMaterialRouteV1;
  readonly expectedFit: boolean;
  readonly target: ParsedTargetProtocolParametersV1;
  readonly label: string;
}): CekProgramMaterialRouteAttemptV1<
  CekProgramMaterialRouteV1,
  readonly CekProgramMaterialConcreteTransactionReceiptV1[],
  number | null
> => {
  const attempt = exactObjectV1(value, ROUTE_ATTEMPT_KEYS_V1, label);
  if (attempt.route !== route || attempt.fit !== expectedFit) {
    throw new Error(
      `${label} must be the ${route} ${expectedFit ? "fit" : "rejected"} attempt`,
    );
  }
  if (
    !Array.isArray(attempt.transactions) ||
    attempt.transactions.length === 0
  ) {
    throw new Error(`${label}.transactions has an invalid receipt count`);
  }
  const transactions = Object.freeze(
    attempt.transactions.map((receipt, index) =>
      transactionReceiptV1({
        value: receipt,
        target,
        label: `${label}.transactions[${index.toString()}]`,
      }),
    ),
  );
  validateRouteTransactionGrammarV1({ route, transactions, label });
  const timing = Object.freeze({
    dataAvailabilityFetchMilliseconds: confirmationMillisecondsV1(
      attempt.dataAvailabilityFetchMilliseconds,
      `${label}.dataAvailabilityFetchMilliseconds`,
    ),
    evidenceConstructionMilliseconds: confirmationMillisecondsV1(
      attempt.evidenceConstructionMilliseconds,
      `${label}.evidenceConstructionMilliseconds`,
    ),
    retryMilliseconds: confirmationMillisecondsV1(
      attempt.retryMilliseconds,
      `${label}.retryMilliseconds`,
    ),
    rollbackAllowanceMilliseconds: confirmationMillisecondsV1(
      attempt.rollbackAllowanceMilliseconds,
      `${label}.rollbackAllowanceMilliseconds`,
    ),
    settlementMilliseconds: confirmationMillisecondsV1(
      attempt.settlementMilliseconds,
      `${label}.settlementMilliseconds`,
    ),
    removalMilliseconds: confirmationMillisecondsV1(
      attempt.removalMilliseconds,
      `${label}.removalMilliseconds`,
    ),
  });
  const maturityWindowMarginMilliseconds = safeSignedIntegerV1(
    attempt.maturityWindowMarginMilliseconds,
    `${label}.maturityWindowMarginMilliseconds`,
  );
  const totalConfirmationMilliseconds = transactions.reduce(
    (total, receipt) => total + receipt.confirmationMilliseconds,
    0,
  );
  const totalCorrectionPathMilliseconds = Object.values(timing).reduce(
    (total, component) => total + component,
    totalConfirmationMilliseconds,
  );
  if (
    !Number.isSafeInteger(totalCorrectionPathMilliseconds) ||
    maturityWindowMarginMilliseconds !==
      Math.floor(target.maturityWindowMilliseconds / 2) -
        totalCorrectionPathMilliseconds
  ) {
    throw new Error(`${label} contains an invalid maturity-window margin`);
  }
  let minimumMultiOutputCount: number | null;
  if (route === "minimumMultiOutputReconstruction") {
    minimumMultiOutputCount = positiveCountV1(
      attempt.minimumMultiOutputCount,
      `${label}.minimumMultiOutputCount`,
    );
    if (minimumMultiOutputCount < 2) {
      throw new Error(`${label}.minimumMultiOutputCount must be at least two`);
    }
  } else if (attempt.minimumMultiOutputCount !== null) {
    throw new Error(
      `${label}.minimumMultiOutputCount is invalid for the selected route`,
    );
  } else {
    minimumMultiOutputCount = null;
  }
  validateRouteMaterialLinkageV1({
    route,
    transactions,
    minimumMultiOutputCount,
    label,
  });
  let limitingConstraint: CekProgramMaterialLimitingConstraintV1 | null = null;
  if (expectedFit) {
    if (
      attempt.limitingConstraint !== null ||
      maturityWindowMarginMilliseconds < 0 ||
      transactions.some(
        (receipt) =>
          receipt.transactionByteMargin < 0 ||
          receipt.maximumValueByteMargin < 0 ||
          BigInt(receipt.executionMemoryMargin) < 0n ||
          BigInt(receipt.executionCpuMargin) < 0n,
      )
    ) {
      throw new Error(`${label} fit attempt contains a failed constraint`);
    }
  } else {
    const constraint = exactObjectV1(
      attempt.limitingConstraint,
      LIMITING_CONSTRAINT_KEYS_V1,
      `${label}.limitingConstraint`,
    );
    if (
      typeof constraint.type !== "string" ||
      !CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS_V1.includes(
        constraint.type as CekProgramMaterialLimitingConstraintTypeV1,
      )
    ) {
      throw new Error(`${label}.limitingConstraint.type is invalid`);
    }
    const measuredMargin = signedDecimalV1(
      constraint.measuredMargin,
      `${label}.limitingConstraint.measuredMargin`,
    );
    if (
      measuredMargin !==
        measuredConstraintMarginV1({
          constraint:
            constraint.type as CekProgramMaterialLimitingConstraintTypeV1,
          transactions,
          maturityWindowMarginMilliseconds,
        }) ||
      BigInt(measuredMargin) >= 0n
    ) {
      throw new Error(`${label} contains an invalid limiting measured margin`);
    }
    limitingConstraint = Object.freeze({
      type: constraint.type as CekProgramMaterialLimitingConstraintTypeV1,
      measuredMargin,
    });
  }
  return Object.freeze({
    route,
    transactions,
    ...timing,
    maturityWindowMarginMilliseconds,
    fit: expectedFit,
    limitingConstraint,
    minimumMultiOutputCount,
  });
};

/**
 * Parses the exact JSON-safe C28 necessity ABI. Complete routes are measured
 * concrete rejections, incremental traversal is a separate measured fit, and
 * every claimed margin is recomputed against the bound target. Unknown keys
 * and omitted identity or transaction fields are rejected without defaults.
 */
export const parseCekProgramMaterialNecessityReceiptSetV1 = (
  value: unknown,
): CekProgramMaterialNecessityReceiptSetV1 => {
  const receiptSet = exactObjectV1(
    value,
    RECEIPT_SET_KEYS_V1,
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
      const identity = exactObjectV1(
        value,
        VALIDATOR_IDENTITY_KEYS_V1,
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
        generatedHash: exactHexV1(
          identity.generatedHash,
          28,
          `validatorIdentities[${index.toString()}].generatedHash`,
        ),
        appliedHash: exactHexV1(
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
  const targetValue = exactObjectV1(
    receiptSet.targetProtocolParameters,
    TARGET_PROTOCOL_PARAMETER_KEYS_V1,
    "target protocol parameters",
  );
  const targetProtocolParameters = Object.freeze({
    digest: exactHexV1(
      targetValue.digest,
      32,
      "targetProtocolParameters.digest",
    ),
    maxTxSize: positiveCountV1(
      targetValue.maxTxSize,
      "targetProtocolParameters.maxTxSize",
    ),
    maxValueSize: positiveCountV1(
      targetValue.maxValueSize,
      "targetProtocolParameters.maxValueSize",
    ),
    maxExecutionMemoryUnits: decimalV1(
      targetValue.maxExecutionMemoryUnits,
      "targetProtocolParameters.maxExecutionMemoryUnits",
    ),
    maxExecutionCpuUnits: decimalV1(
      targetValue.maxExecutionCpuUnits,
      "targetProtocolParameters.maxExecutionCpuUnits",
    ),
    coinsPerUtxoByte: decimalV1(
      targetValue.coinsPerUtxoByte,
      "targetProtocolParameters.coinsPerUtxoByte",
    ),
    maturityWindowMilliseconds: positiveCountV1(
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
    routeAttemptValues.length !== CEK_PROGRAM_MATERIAL_ROUTE_ORDER_V1.length
  ) {
    throw new Error(
      "CEK program-material necessity receipt set requires exactly four ordered route attempts",
    );
  }
  const routeAttempts = CEK_PROGRAM_MATERIAL_ROUTE_ORDER_V1.map(
    (route, index) =>
      routeAttemptV1({
        value: routeAttemptValues[index],
        route,
        expectedFit: route === "incrementalTraversal",
        target: targetProtocolParameters,
        label: `routeAttempts[${index.toString()}]`,
      }),
  ) as unknown as CekProgramMaterialNecessityReceiptSetV1["routeAttempts"];
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
    sourceRevision: exactHexV1(receiptSet.sourceRevision, 20, "sourceRevision"),
    programEnvelopeHash: exactHexV1(
      receiptSet.programEnvelopeHash,
      32,
      "programEnvelopeHash",
    ),
    validatorIdentities,
    targetProtocolParameters,
    routeAttempts,
  });
};

export const CekProgramMaterialNecessityReceiptSetV1Schema = Object.freeze({
  parse: parseCekProgramMaterialNecessityReceiptSetV1,
});

export type ValidationDisputeEvidenceMoveV1 = {
  readonly role: "operator" | "challenger";
  readonly disputeBefore: MidgardValidationDisputeV1;
  readonly proof: MidgardValidationTraceProofV1;
  readonly proofCbor: Buffer;
  readonly disputeAfter: MidgardValidationDisputeV1;
  readonly disputeAfterCbor: Buffer;
};

export type ValidationDisputeEvidenceBundleV1 = {
  readonly operatorDescriptorCbor: Buffer;
  readonly challengerDescriptorCbor: Buffer;
  readonly openingDispute: MidgardValidationDisputeV1;
  readonly openingDisputeCbor: Buffer;
  readonly moves: readonly ValidationDisputeEvidenceMoveV1[];
  readonly finalDispute: MidgardValidationDisputeV1;
  readonly finalDisputeCbor: Buffer;
  readonly boundaryEvidenceCbor: Buffer;
  readonly oneStepArgument: ValidationOneStepArgumentV1;
};

const requireProofEnvelope = (bytes: Uint8Array, label: string): void => {
  if (bytes.length >= MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes) {
    throw new Error(
      `${label} exceeds the strict L1 proof envelope: ${bytes.length.toString()} bytes`,
    );
  }
};

/**
 * Constructs every authenticated bisection reveal and the terminal one-step
 * argument from two complete local traces. The routine never guesses missing
 * trace nodes: an absent proof, mismatched endpoint, excessive round count, or
 * oversized independently submitted preimage fails before transaction
 * construction.
 */
export const buildValidationDisputeEvidenceBundleV1 = ({
  operatorTrace,
  challengerTrace,
  currentTime,
}: {
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly currentTime: number;
}): ValidationDisputeEvidenceBundleV1 => {
  const openingDispute = openMidgardValidationDispute({
    operatorDescriptor: operatorTrace.tree.descriptor,
    challengerDescriptor: challengerTrace.tree.descriptor,
    currentTime,
  });
  let dispute = openingDispute;
  const moves: ValidationDisputeEvidenceMoveV1[] = [];
  const maximumMoves =
    2 * MIDGARD_CONSENSUS_LIMITS_V1.maxValidationBisectionRounds;

  while (dispute.turn.type !== "readyForOneStep") {
    if (moves.length >= maximumMoves) {
      throw new Error("validation dispute exceeded its compiled move bound");
    }
    const disputeBefore = dispute;
    const proof =
      dispute.turn.type === "awaitingOperator"
        ? operatorTrace.tree.proofs[dispute.turn.midpoint]
        : challengerTrace.tree.proofs[dispute.turn.midpoint];
    if (proof === undefined) {
      throw new Error(
        `validation trace is missing midpoint ${dispute.turn.midpoint.toString()}`,
      );
    }
    const role =
      dispute.turn.type === "awaitingOperator"
        ? ("operator" as const)
        : ("challenger" as const);
    dispute =
      role === "operator"
        ? revealMidgardValidationOperatorMidpoint({
            dispute,
            proof,
            currentTime,
          })
        : revealMidgardValidationChallengerMidpoint({
            dispute,
            proof,
            currentTime,
          });
    const proofCbor = encodeValidationTraceProofDataCborV1(proof);
    const disputeAfterCbor = encodeValidationDisputeDataCborV1(dispute);
    requireProofEnvelope(proofCbor, `${role} midpoint proof`);
    requireProofEnvelope(disputeAfterCbor, "continued validation dispute");
    moves.push({
      role,
      disputeBefore,
      proof,
      proofCbor,
      disputeAfter: dispute,
      disputeAfterCbor,
    });
  }

  const operatorDescriptorCbor = encodeValidationTraceDescriptorDataCborV1(
    operatorTrace.tree.descriptor,
  );
  const challengerDescriptorCbor = encodeValidationTraceDescriptorDataCborV1(
    challengerTrace.tree.descriptor,
  );
  const openingDisputeCbor = encodeValidationDisputeDataCborV1(openingDispute);
  const finalDisputeCbor = encodeValidationDisputeDataCborV1(dispute);
  const boundaryEvidenceCbor = encodeValidationBoundaryEvidenceCborV1({
    dispute,
    operatorTrace,
    challengerTrace,
  });
  const oneStepArgument = buildValidationOneStepArgumentV1({
    trace: challengerTrace,
    stateIndex: dispute.lowIndex,
  });
  requireProofEnvelope(operatorDescriptorCbor, "operator descriptor");
  requireProofEnvelope(challengerDescriptorCbor, "challenger descriptor");
  requireProofEnvelope(openingDisputeCbor, "opening validation dispute");
  requireProofEnvelope(finalDisputeCbor, "final validation dispute");
  requireProofEnvelope(
    boundaryEvidenceCbor,
    "validation one-step boundary evidence",
  );

  return {
    operatorDescriptorCbor,
    challengerDescriptorCbor,
    openingDispute,
    openingDisputeCbor,
    moves,
    finalDispute: dispute,
    finalDisputeCbor,
    boundaryEvidenceCbor,
    oneStepArgument,
  };
};
