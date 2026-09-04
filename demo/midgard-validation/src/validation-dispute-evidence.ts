import {
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardValidationDispute,
  type MidgardValidationTraceProof,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";

import type { DeterministicValidationMachineTrace } from "./validation-machine/index.js";
import {
  buildValidationOneStepArgument,
  type ValidationOneStepArgument,
} from "./validation-machine-data.js";
import {
  encodeValidationBoundaryEvidenceCbor,
  encodeValidationDisputeDataCbor,
  encodeValidationTraceDescriptorDataCbor,
  encodeValidationTraceProofDataCbor,
} from "./validation-one-step-data.js";

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

const RECEIPT_SET_KEYS = Object.freeze([
  "schemaVersion",
  "sourceRevision",
  "programEnvelopeHash",
  "validatorIdentities",
  "targetProtocolParameters",
  "routeAttempts",
] as const);
const VALIDATOR_IDENTITY_KEYS = Object.freeze([
  "title",
  "generatedHash",
  "appliedHash",
] as const);
const TARGET_PROTOCOL_PARAMETER_KEYS = Object.freeze([
  "digest",
  "maxTxSize",
  "maxValueSize",
  "maxExecutionMemoryUnits",
  "maxExecutionCpuUnits",
  "coinsPerUtxoByte",
  "maturityWindowMilliseconds",
] as const);
const CONCRETE_TRANSACTION_RECEIPT_KEYS = Object.freeze([
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
const ROUTE_ATTEMPT_KEYS = Object.freeze([
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
const LIMITING_CONSTRAINT_KEYS = Object.freeze([
  "type",
  "measuredMargin",
] as const);

const exactObject = <Keys extends readonly string[]>(
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

const exactHex = (value: unknown, bytes: number, label: string): string => {
  if (
    typeof value !== "string" ||
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
  ) {
    throw new Error(`${label} must be ${bytes.toString()}-byte lowercase hex`);
  }
  return value;
};

const decimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical non-negative decimal string`);
  }
  return value;
};

const signedDecimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^-?(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${label} must be a canonical signed decimal string`);
  }
  return value;
};

const safeCount = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const positiveCount = (value: unknown, label: string): number => {
  const count = safeCount(value, label);
  if (count === 0) {
    throw new Error(`${label} must be positive`);
  }
  return count;
};

const safeSignedInteger = (value: unknown, label: string): number => {
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

type ParsedOutRef = {
  readonly canonical: string;
  readonly txId: string;
  readonly outputIndex: number;
};

const outRefV1 = (value: unknown, label: string): ParsedOutRef => {
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

type ParsedTargetProtocolParameters =
  CekProgramMaterialNecessityReceiptSet["targetProtocolParameters"];

const transactionReceipt = ({
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

const measuredConstraintMargin = ({
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

const exactOutRefSequence = (
  actual: readonly string[],
  expected: readonly string[],
): boolean =>
  actual.length === expected.length &&
  actual.every((outRef, index) => outRef === expected[index]);

const materialSourceOutRefs = (
  receipt: CekProgramMaterialConcreteTransactionReceipt,
): readonly string[] => [
  ...receipt.programMaterialConsumedInputOutRefs,
  ...receipt.programMaterialReferenceInputOutRefs,
];

const validateRouteTransactionGrammar = ({
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

const validateRouteMaterialLinkage = ({
  route,
  transactions,
  minimumMultiOutputCount,
  label,
}: {
  readonly route: CekProgramMaterialRoute;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceipt[];
  readonly minimumMultiOutputCount: number | null;
  readonly label: string;
}): void => {
  const publications = transactions.filter(
    (receipt) => receipt.role === "publication",
  );
  for (const publication of publications) {
    if (
      materialSourceOutRefs(publication).length !== 0 ||
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
      materialSourceOutRefs(proof).length !== 0 ||
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
      !exactOutRefSequence(
        materialSourceOutRefs(consumption),
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
      !exactOutRefSequence(
        materialSourceOutRefs(consumption),
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
    const sources = materialSourceOutRefs(receipt);
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

const routeAttempt = ({
  value,
  route,
  expectedFit,
  target,
  label,
}: {
  readonly value: unknown;
  readonly route: CekProgramMaterialRoute;
  readonly expectedFit: boolean;
  readonly target: ParsedTargetProtocolParameters;
  readonly label: string;
}): CekProgramMaterialRouteAttempt<
  CekProgramMaterialRoute,
  readonly CekProgramMaterialConcreteTransactionReceipt[],
  number | null
> => {
  const attempt = exactObject(value, ROUTE_ATTEMPT_KEYS, label);
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
      transactionReceipt({
        value: receipt,
        target,
        label: `${label}.transactions[${index.toString()}]`,
      }),
    ),
  );
  validateRouteTransactionGrammar({ route, transactions, label });
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
  const maturityWindowMarginMilliseconds = safeSignedInteger(
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
    minimumMultiOutputCount = positiveCount(
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
  validateRouteMaterialLinkage({
    route,
    transactions,
    minimumMultiOutputCount,
    label,
  });
  let limitingConstraint: CekProgramMaterialLimitingConstraint | null = null;
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
    const constraint = exactObject(
      attempt.limitingConstraint,
      LIMITING_CONSTRAINT_KEYS,
      `${label}.limitingConstraint`,
    );
    if (
      typeof constraint.type !== "string" ||
      !CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS.includes(
        constraint.type as CekProgramMaterialLimitingConstraintType,
      )
    ) {
      throw new Error(`${label}.limitingConstraint.type is invalid`);
    }
    const measuredMargin = signedDecimal(
      constraint.measuredMargin,
      `${label}.limitingConstraint.measuredMargin`,
    );
    if (
      measuredMargin !==
        measuredConstraintMargin({
          constraint:
            constraint.type as CekProgramMaterialLimitingConstraintType,
          transactions,
          maturityWindowMarginMilliseconds,
        }) ||
      BigInt(measuredMargin) >= 0n
    ) {
      throw new Error(`${label} contains an invalid limiting measured margin`);
    }
    limitingConstraint = Object.freeze({
      type: constraint.type as CekProgramMaterialLimitingConstraintType,
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

const requireProofEnvelope = (bytes: Uint8Array, label: string): void => {
  if (bytes.length >= MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes) {
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
export const buildValidationDisputeEvidenceBundle = ({
  operatorTrace,
  challengerTrace,
  currentTime,
}: {
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly currentTime: number;
}): ValidationDisputeEvidenceBundle => {
  const openingDispute = openMidgardValidationDispute({
    operatorDescriptor: operatorTrace.tree.descriptor,
    challengerDescriptor: challengerTrace.tree.descriptor,
    currentTime,
  });
  let dispute = openingDispute;
  const moves: ValidationDisputeEvidenceMove[] = [];
  const maximumMoves =
    2 * MIDGARD_CONSENSUS_LIMITS.maxValidationBisectionRounds;

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
    const proofCbor = encodeValidationTraceProofDataCbor(proof);
    const disputeAfterCbor = encodeValidationDisputeDataCbor(dispute);
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

  const operatorDescriptorCbor = encodeValidationTraceDescriptorDataCbor(
    operatorTrace.tree.descriptor,
  );
  const challengerDescriptorCbor = encodeValidationTraceDescriptorDataCbor(
    challengerTrace.tree.descriptor,
  );
  const openingDisputeCbor = encodeValidationDisputeDataCbor(openingDispute);
  const finalDisputeCbor = encodeValidationDisputeDataCbor(dispute);
  const boundaryEvidenceCbor = encodeValidationBoundaryEvidenceCbor({
    dispute,
    operatorTrace,
    challengerTrace,
  });
  const oneStepArgument = buildValidationOneStepArgument({
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
