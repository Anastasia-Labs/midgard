import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
} from "./consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "./da-transport.js";

const FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT_V1 = 4;
const FRAUD_PROOF_CATALOGUE_SCRIPT_HASH_BYTE_COUNT_V1 = 28;
const FRAUD_PROOF_CATALOGUE_NULL_ROOT_V1 = Buffer.alloc(32);

const FraudProofCatalogueNeighborV1Schema = Data.Object({
  nibble: Data.Integer(),
  prefix: Data.Bytes(),
  root: Data.Bytes(),
});
const FraudProofCatalogueProofStepV1Schema = Data.Enum([
  Data.Object({
    Branch: Data.Object({
      skip: Data.Integer(),
      neighbors: Data.Bytes(),
    }),
  }),
  Data.Object({
    Fork: Data.Object({
      skip: Data.Integer(),
      neighbor: FraudProofCatalogueNeighborV1Schema,
    }),
  }),
  Data.Object({
    Leaf: Data.Object({
      skip: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
]);
const FraudProofCatalogueProofV1Schema = Data.Array(
  FraudProofCatalogueProofStepV1Schema,
);
type FraudProofCatalogueProofStepV1 = Data.Static<
  typeof FraudProofCatalogueProofStepV1Schema
>;
type FraudProofCatalogueProofV1 = Data.Static<
  typeof FraudProofCatalogueProofV1Schema
>;

type FraudProofCatalogueEntryV1 = {
  readonly path: string;
  readonly valueDigest: Buffer;
};

export const DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES = Object.freeze([
  "referenceScriptAuthMint",
  "hubOracleMint",
  "daParamsGovernorSpend",
  "daParamsGovernorMint",
  "daAttestationSpend",
  "daAttestationMint",
  "stateQueueSpend",
  "stateQueueMint",
  "schedulerSpend",
  "schedulerMint",
  "registeredOperatorsSpend",
  "registeredOperatorsMint",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "escapeHatchSpend",
  "escapeHatchMint",
  "fraudProofCatalogueSpend",
  "fraudProofCatalogueMint",
  "fraudProofSpend",
  "fraudProofMint",
  "depositSpend",
  "depositMint",
  "withdrawalSpend",
  "withdrawalMint",
  "txOrderSpend",
  "txOrderMint",
  "txOrderFieldPreimageSpend",
  "txOrderFieldReceiptSpend",
  "txOrderFieldReceiptMint",
  "cekProgramMaterialSpend",
  "settlementSpend",
  "settlementMint",
  "payoutSpend",
  "payoutMint",
  "reserveSpend",
  "reserveWithdraw",
  "phasMembershipWithdraw",
  "fraudProofDoubleSpend",
  "fraudProofNonExistentInput",
  "fraudProofNonExistentInputNoIndex",
  "fraudProofInvalidRange",
  "fraudProofTransitionTrace",
  "fraudProofZeroInput",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
] as const);

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "reference-script-auth minting": "referenceScriptAuthMint",
    "hub-oracle minting": "hubOracleMint",
    "da-params-governor spending": "daParamsGovernorSpend",
    "da-params-governor minting": "daParamsGovernorMint",
    "da-attestation spending": "daAttestationSpend",
    "da-attestation minting": "daAttestationMint",
    "state-queue spending": "stateQueueSpend",
    "state-queue minting": "stateQueueMint",
    "scheduler spending": "schedulerSpend",
    "scheduler minting": "schedulerMint",
    "registered-operators spending": "registeredOperatorsSpend",
    "registered-operators minting": "registeredOperatorsMint",
    "active-operators spending": "activeOperatorsSpend",
    "active-operators minting": "activeOperatorsMint",
    "retired-operators spending": "retiredOperatorsSpend",
    "retired-operators minting": "retiredOperatorsMint",
    "fraud-proof-catalogue minting": "fraudProofCatalogueMint",
    "deposit spending": "depositSpend",
    "deposit minting": "depositMint",
    "withdrawal spending": "withdrawalSpend",
    "withdrawal minting": "withdrawalMint",
    "settlement minting": "settlementMint",
    "payout spending": "payoutSpend",
    "payout minting": "payoutMint",
    "reserve spending": "reserveSpend",
    "reserve observer": "reserveWithdraw",
    "membership proof withdrawal": "phasMembershipWithdraw",
    "V1 transaction-field preimage publication": "txOrderFieldPreimageSpend",
    "V1 transaction-field receipt": "txOrderFieldReceiptSpend",
    "V1 transaction-field receipt minting": "txOrderFieldReceiptMint",
    "V1 immutable CEK program-material publication": "cekProgramMaterialSpend",
    "V1 validation-trace dispute": "validationTraceDispute",
    "V1 validation-trace source": "validationTraceDisputeSource",
    "V1 validation-trace game": "validationTraceDisputeGame",
    "V1 validation-trace boundary": "validationTraceDisputeBoundary",
    "V1 validation-trace timeout": "validationTraceDisputeTimeout",
    "V1 validation-trace award": "validationTraceDisputeAward",
  } as const);

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES =
  Object.freeze({
    "reference-script-auth minting": "ReferenceScriptAuthMint",
    "hub-oracle minting": "HubOracleMint",
    "da-params-governor spending": "DaParamsGovernorSpend",
    "da-params-governor minting": "DaParamsGovernorMint",
    "da-attestation spending": "DaAttestationSpend",
    "da-attestation minting": "DaAttestationMint",
    "state-queue spending": "StateQueueSpend",
    "state-queue minting": "StateQueueMint",
    "scheduler spending": "SchedulerSpend",
    "scheduler minting": "SchedulerMint",
    "registered-operators spending": "RegisteredOperatorsSpend",
    "registered-operators minting": "RegisteredOperatorsMint",
    "active-operators spending": "ActiveOperatorsSpend",
    "active-operators minting": "ActiveOperatorsMint",
    "retired-operators spending": "RetiredOperatorsSpend",
    "retired-operators minting": "RetiredOperatorsMint",
    "fraud-proof-catalogue minting": "FraudProofCatalogueMint",
    "deposit spending": "DepositSpend",
    "deposit minting": "DepositMint",
    "withdrawal spending": "WithdrawalSpend",
    "withdrawal minting": "WithdrawalMint",
    "settlement minting": "SettlementMint",
    "payout spending": "PayoutSpend",
    "payout minting": "PayoutMint",
    "reserve spending": "ReserveSpend",
    "reserve observer": "ReserveObserver",
    "membership proof withdrawal": "MembershipProofWithdraw",
    "V1 transaction-field preimage publication": "V1TxFieldPreimageSpend",
    "V1 transaction-field receipt": "V1TxFieldReceiptSpend",
    "V1 transaction-field receipt minting": "V1TxFieldReceiptMint",
    "V1 immutable CEK program-material publication":
      "V1CekProgramMaterialSpend",
    "V1 validation-trace dispute": "V1ValidationTraceDispute",
    "V1 validation-trace source": "V1ValidationTraceSource",
    "V1 validation-trace game": "V1ValidationTraceGame",
    "V1 validation-trace boundary": "V1ValidationTraceBoundary",
    "V1 validation-trace timeout": "V1ValidationTraceTimeout",
    "V1 validation-trace award": "V1ValidationTraceAward",
  } as const);

export const DEPLOYMENT_MANIFEST_V1_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

export const DEPLOYMENT_MANIFEST_V1_ROOT_KEYS = Object.freeze([
  "schemaVersion",
  "manifestId",
  "consensusProfile",
  "consensusProfileDigest",
  "network",
  "cardanoProtocolParameters",
  "genesis",
  "createdAt",
  "updatedAt",
  "referenceScriptDeployAddress",
  "hubOracleOneShot",
  "referenceScriptAuthPolicy",
  "contracts",
  "referenceScripts",
  "da",
  "proofEvidence",
  "steps",
  "validationDispute",
] as const);

export type DeploymentManifestV1JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly DeploymentManifestV1JsonValue[]
  | { readonly [key: string]: DeploymentManifestV1JsonValue };

export const MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION =
  "midgard-deployment-marker-v1" as const;

export type DeploymentMarkerV1 = {
  readonly schemaVersion: typeof MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION;
  readonly manifestId: string;
};

const requireRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must be a plain object`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return value as Record<string, unknown>;
};

const requireDeploymentManifestIdV1 = (
  value: unknown,
  field: string,
): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return value;
};

export const parseDeploymentMarkerV1 = (value: unknown): DeploymentMarkerV1 => {
  const candidate = requireRecord(value, "Deployment marker V1");
  const keys = Object.keys(candidate);
  if (
    keys.length !== 2 ||
    !Object.prototype.hasOwnProperty.call(candidate, "schemaVersion") ||
    !Object.prototype.hasOwnProperty.call(candidate, "manifestId")
  ) {
    throw new Error(
      "Deployment marker V1 must contain exactly schemaVersion and manifestId",
    );
  }
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION) {
    throw new Error(
      `Deployment marker V1 schemaVersion must be ${MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION}`,
    );
  }
  return {
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
    manifestId: requireDeploymentManifestIdV1(
      candidate.manifestId,
      "Deployment marker V1 manifestId",
    ),
  };
};

export const makeDeploymentMarkerV1 = (
  manifestId: string,
): DeploymentMarkerV1 =>
  parseDeploymentMarkerV1({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
    manifestId,
  });

export const assertDeploymentMarkerV1Matches = (
  expected: DeploymentMarkerV1,
  actual: unknown,
  boundary = "deployment boundary",
): DeploymentMarkerV1 => {
  const canonicalExpected = parseDeploymentMarkerV1(expected);
  const canonicalActual = parseDeploymentMarkerV1(actual);
  if (canonicalActual.manifestId !== canonicalExpected.manifestId) {
    throw new Error(
      `${boundary} deployment marker mismatch: expected ${canonicalExpected.manifestId}, found ${canonicalActual.manifestId}`,
    );
  }
  return canonicalActual;
};

const normalizeDeploymentManifestV1JsonValueInternal = (
  value: unknown,
  field: string,
  stringifyBigInt: boolean,
): DeploymentManifestV1JsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "bigint") {
    if (stringifyBigInt) {
      return value.toString(10);
    }
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      throw new Error(`${field} must contain only finite numbers`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeDeploymentManifestV1JsonValueInternal(
        entry,
        `${field}[${index.toString()}]`,
        stringifyBigInt,
      ),
    );
  }
  if (typeof value !== "object" || value === null) {
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must contain only plain records`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return Object.fromEntries(
    Object.entries(value as Record<string, unknown>).map(([key, entry]) => {
      if (entry === undefined) {
        throw new Error(`${field}.${key} must not be undefined`);
      }
      return [
        key,
        normalizeDeploymentManifestV1JsonValueInternal(
          entry,
          `${field}.${key}`,
          stringifyBigInt,
        ),
      ];
    }),
  );
};

export const normalizeDeploymentManifestV1JsonValue = (
  value: unknown,
  field = "value",
): DeploymentManifestV1JsonValue =>
  normalizeDeploymentManifestV1JsonValueInternal(
    value,
    `Deployment manifest ${field}`,
    true,
  );

const stableJson = (value: DeploymentManifestV1JsonValue): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, entry]) => `${JSON.stringify(key)}:${stableJson(entry)}`)
    .join(",")}}`;
};

export const computeDeploymentManifestV1JsonDigest = (
  value: unknown,
): string => {
  const normalized = normalizeDeploymentManifestV1JsonValueInternal(
    value,
    "Deployment manifest JSON digest input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

const exactRoot = (candidate: Record<string, unknown>): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_V1_ROOT_KEYS);
  for (const key of Object.keys(candidate)) {
    if (!expected.has(key)) {
      throw new Error(`Deployment manifest value.${key} is unexpected`);
    }
  }
  for (const key of DEPLOYMENT_MANIFEST_V1_ROOT_KEYS) {
    if (!Object.prototype.hasOwnProperty.call(candidate, key)) {
      throw new Error(`Deployment manifest value.${key} is required`);
    }
  }
};

export const computeDeploymentManifestV1Id = (
  identityInput: Record<string, unknown>,
): string => {
  if (Object.prototype.hasOwnProperty.call(identityInput, "manifestId")) {
    throw new Error("Deployment manifest identity input must omit manifestId");
  }
  const normalized = normalizeDeploymentManifestV1JsonValueInternal(
    identityInput,
    "Deployment manifest identity input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

export const verifyDeploymentManifestV1Identity = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = requireRecord(value, "Deployment manifest value");
  if (
    candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION
  ) {
    throw new Error(
      `Deployment manifest schemaVersion must be ${MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION}`,
    );
  }
  exactRoot(candidate);
  if (!isMidgardConsensusProfileV1(candidate.consensusProfile)) {
    throw new Error(
      "Deployment manifest consensusProfile must exactly match canonical V1",
    );
  }
  if (
    candidate.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST
  ) {
    throw new Error(
      "Deployment manifest consensusProfileDigest must exactly match canonical V1",
    );
  }
  if (
    typeof candidate.manifestId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(candidate.manifestId)
  ) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const { manifestId, ...identityInput } = candidate;
  const expectedManifestId = computeDeploymentManifestV1Id(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return candidate;
};

const requireExactKeys = (
  value: Record<string, unknown>,
  required: readonly string[],
  optional: readonly string[] = [],
  field: string,
): void => {
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of required) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

const requireString = (value: unknown, field: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Deployment manifest ${field} must be a non-empty string`);
  }
  return value;
};

const requireHex = (
  value: unknown,
  bytes: number | undefined,
  field: string,
): string => {
  const text = requireString(value, field);
  const pattern =
    bytes === undefined
      ? /^(?:[0-9a-f]{2})+$/u
      : new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u");
  if (!pattern.test(text)) {
    throw new Error(
      `Deployment manifest ${field} must be lowercase canonical hex`,
    );
  }
  return text;
};

const requireInteger = (value: unknown, field: string, minimum = 0): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum
  ) {
    throw new Error(
      `Deployment manifest ${field} must be an integer >= ${minimum.toString()}`,
    );
  }
  return value;
};

const requireFinalOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireInteger(outRef.outputIndex, `${field}.outputIndex`),
  };
};

const requireIsoTimestamp = (value: unknown, field: string): string => {
  const text = requireString(value, field);
  const milliseconds = Date.parse(text);
  if (
    !Number.isFinite(milliseconds) ||
    new Date(milliseconds).toISOString() !== text
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return text;
};

const fraudProofCatalogueDigestV1 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const fraudProofCatalogueNibblesV1 = (
  hexDigits: string,
  field: string,
): Buffer =>
  Buffer.from(
    [...hexDigits].map((digit) => {
      const nibble = Number.parseInt(digit, 16);
      if (!Number.isInteger(nibble) || nibble < 0 || nibble > 15) {
        throw new Error(`Deployment manifest ${field} has invalid nibble`);
      }
      return nibble;
    }),
  );

const fraudProofCatalogueLeafHashV1 = (
  prefix: string,
  valueDigest: Buffer,
): Buffer => {
  const head =
    prefix.length % 2 > 0
      ? Buffer.concat([
          Buffer.from([0]),
          fraudProofCatalogueNibblesV1(
            prefix.slice(0, 1),
            "fraud-proof catalogue leaf prefix",
          ),
        ])
      : Buffer.from([255]);
  const tail = Buffer.from(
    prefix.length % 2 > 0 ? prefix.slice(1) : prefix,
    "hex",
  );
  return fraudProofCatalogueDigestV1(Buffer.concat([head, tail, valueDigest]));
};

const fraudProofCatalogueBranchHashV1 = (
  prefix: string,
  root: Buffer,
): Buffer =>
  fraudProofCatalogueDigestV1(
    Buffer.concat([
      fraudProofCatalogueNibblesV1(
        prefix,
        "fraud-proof catalogue branch prefix",
      ),
      root,
    ]),
  );

const fraudProofCataloguePairHashV1 = (left: Buffer, right: Buffer): Buffer =>
  fraudProofCatalogueDigestV1(Buffer.concat([left, right]));

const fraudProofCatalogueMerkleRoot16V1 = (
  nodesByNibble: Readonly<Record<number, Buffer>>,
): Buffer => {
  let nodes = Array.from(
    { length: 16 },
    (_, index) => nodesByNibble[index] ?? FRAUD_PROOF_CATALOGUE_NULL_ROOT_V1,
  );
  while (nodes.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < nodes.length; index += 2) {
      next.push(
        fraudProofCataloguePairHashV1(nodes[index]!, nodes[index + 1]!),
      );
    }
    nodes = next;
  }
  return nodes[0]!;
};

const fraudProofCatalogueBranchRootFromNeighborsV1 = (
  nibble: number,
  root: Buffer,
  neighbors: Buffer,
): Buffer => {
  if (neighbors.length !== 128) {
    throw new Error(
      `Deployment manifest fraud-proof catalogue branch proof must contain 128 neighbor bytes, got ${neighbors.length.toString()}`,
    );
  }
  const siblings = [
    neighbors.subarray(96, 128),
    neighbors.subarray(64, 96),
    neighbors.subarray(32, 64),
    neighbors.subarray(0, 32),
  ];
  return siblings.reduce(
    (current, sibling, level) =>
      ((nibble >> level) & 1) === 0
        ? fraudProofCataloguePairHashV1(current, sibling)
        : fraudProofCataloguePairHashV1(sibling, current),
    root,
  );
};

const fraudProofCatalogueProofIntegerV1 = (
  value: bigint,
  field: string,
  maximum?: number,
): number => {
  const parsed = Number(value);
  if (
    !Number.isSafeInteger(parsed) ||
    parsed < 0 ||
    (maximum !== undefined && parsed > maximum)
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a non-negative safe integer${maximum === undefined ? "" : ` <= ${maximum.toString()}`}`,
    );
  }
  return parsed;
};

const fraudProofCatalogueProofBytesV1 = (
  value: string,
  field: string,
  byteLength?: number,
): Buffer => {
  if (
    !/^(?:[0-9a-f]{2})*$/u.test(value) ||
    (byteLength !== undefined && value.length !== byteLength * 2)
  ) {
    throw new Error(
      `Deployment manifest ${field} must be lowercase canonical hex${byteLength === undefined ? "" : ` of ${byteLength.toString()} bytes`}`,
    );
  }
  return Buffer.from(value, "hex");
};

const fraudProofCataloguePathNibbleV1 = (
  path: string,
  index: number,
  field: string,
): number => {
  if (index < 0 || index >= path.length) {
    throw new Error(`Deployment manifest ${field} exceeds its 32-byte path`);
  }
  const nibble = Number.parseInt(path[index]!, 16);
  if (!Number.isInteger(nibble) || nibble < 0 || nibble > 15) {
    throw new Error(`Deployment manifest ${field} has invalid path nibble`);
  }
  return nibble;
};

const fraudProofCatalogueRootFromEntriesV1 = (
  entries: readonly FraudProofCatalogueEntryV1[],
  cursor = 0,
): Buffer => {
  if (entries.length === 0) {
    return FRAUD_PROOF_CATALOGUE_NULL_ROOT_V1;
  }
  if (entries.length === 1) {
    return fraudProofCatalogueLeafHashV1(
      entries[0]!.path.slice(cursor),
      entries[0]!.valueDigest,
    );
  }
  let branchCursor = cursor;
  while (
    branchCursor < entries[0]!.path.length &&
    entries.every(
      (entry) => entry.path[branchCursor] === entries[0]!.path[branchCursor],
    )
  ) {
    branchCursor += 1;
  }
  if (branchCursor >= entries[0]!.path.length) {
    throw new Error(
      "Deployment manifest fraud-proof catalogue contains colliding keys",
    );
  }
  const groups = new Map<number, FraudProofCatalogueEntryV1[]>();
  for (const entry of entries) {
    const nibble = fraudProofCataloguePathNibbleV1(
      entry.path,
      branchCursor,
      "fraud-proof catalogue entry",
    );
    const group = groups.get(nibble);
    if (group === undefined) {
      groups.set(nibble, [entry]);
    } else {
      group.push(entry);
    }
  }
  const children: Record<number, Buffer> = {};
  for (const [nibble, group] of groups) {
    children[nibble] = fraudProofCatalogueRootFromEntriesV1(
      group,
      branchCursor + 1,
    );
  }
  return fraudProofCatalogueBranchHashV1(
    entries[0]!.path.slice(cursor, branchCursor),
    fraudProofCatalogueMerkleRoot16V1(children),
  );
};

const fraudProofCatalogueRootFromProofV1 = (
  path: string,
  valueDigest: Buffer,
  proof: readonly FraudProofCatalogueProofStepV1[],
  cursor = 0,
  index = 0,
): Buffer => {
  const step = proof[index];
  if (step === undefined) {
    return fraudProofCatalogueLeafHashV1(path.slice(cursor), valueDigest);
  }
  if ("Branch" in step) {
    const skip = fraudProofCatalogueProofIntegerV1(
      step.Branch.skip,
      "fraud-proof catalogue branch skip",
      path.length,
    );
    const nextCursor = cursor + 1 + skip;
    const thisNibble = fraudProofCataloguePathNibbleV1(
      path,
      nextCursor - 1,
      "fraud-proof catalogue branch proof",
    );
    const root = fraudProofCatalogueRootFromProofV1(
      path,
      valueDigest,
      proof,
      nextCursor,
      index + 1,
    );
    return fraudProofCatalogueBranchHashV1(
      path.slice(cursor, nextCursor - 1),
      fraudProofCatalogueBranchRootFromNeighborsV1(
        thisNibble,
        root,
        fraudProofCatalogueProofBytesV1(
          step.Branch.neighbors,
          "fraud-proof catalogue branch neighbors",
          128,
        ),
      ),
    );
  }
  if ("Fork" in step) {
    const skip = fraudProofCatalogueProofIntegerV1(
      step.Fork.skip,
      "fraud-proof catalogue fork skip",
      path.length,
    );
    const nextCursor = cursor + 1 + skip;
    const thisNibble = fraudProofCataloguePathNibbleV1(
      path,
      nextCursor - 1,
      "fraud-proof catalogue fork proof",
    );
    const neighborNibble = fraudProofCatalogueProofIntegerV1(
      step.Fork.neighbor.nibble,
      "fraud-proof catalogue fork neighbor nibble",
      15,
    );
    if (neighborNibble === thisNibble) {
      throw new Error(
        "Deployment manifest fraud-proof catalogue fork neighbor uses the proven path nibble",
      );
    }
    const root = fraudProofCatalogueRootFromProofV1(
      path,
      valueDigest,
      proof,
      nextCursor,
      index + 1,
    );
    const children: Record<number, Buffer> = {
      [thisNibble]: root,
      [neighborNibble]: fraudProofCatalogueDigestV1(
        Buffer.concat([
          fraudProofCatalogueProofBytesV1(
            step.Fork.neighbor.prefix,
            "fraud-proof catalogue fork neighbor prefix",
          ),
          fraudProofCatalogueProofBytesV1(
            step.Fork.neighbor.root,
            "fraud-proof catalogue fork neighbor root",
            32,
          ),
        ]),
      ),
    };
    return fraudProofCatalogueBranchHashV1(
      path.slice(cursor, nextCursor - 1),
      fraudProofCatalogueMerkleRoot16V1(children),
    );
  }
  const neighborPath = fraudProofCatalogueProofBytesV1(
    step.Leaf.key,
    "fraud-proof catalogue leaf neighbor key",
    32,
  ).toString("hex");
  const skip = fraudProofCatalogueProofIntegerV1(
    step.Leaf.skip,
    "fraud-proof catalogue leaf skip",
    path.length,
  );
  const nextCursor = cursor + 1 + skip;
  const thisNibble = fraudProofCataloguePathNibbleV1(
    path,
    nextCursor - 1,
    "fraud-proof catalogue leaf proof",
  );
  if (neighborPath.slice(0, cursor) !== path.slice(0, cursor)) {
    throw new Error(
      "Deployment manifest fraud-proof catalogue leaf neighbor is outside the expected prefix",
    );
  }
  const neighborNibble = fraudProofCataloguePathNibbleV1(
    neighborPath,
    nextCursor - 1,
    "fraud-proof catalogue leaf neighbor",
  );
  if (neighborNibble === thisNibble) {
    throw new Error(
      "Deployment manifest fraud-proof catalogue leaf neighbor uses the proven path nibble",
    );
  }
  const root = fraudProofCatalogueRootFromProofV1(
    path,
    valueDigest,
    proof,
    nextCursor,
    index + 1,
  );
  const children: Record<number, Buffer> = {
    [thisNibble]: root,
    [neighborNibble]: fraudProofCatalogueLeafHashV1(
      neighborPath.slice(nextCursor),
      fraudProofCatalogueProofBytesV1(
        step.Leaf.value,
        "fraud-proof catalogue leaf neighbor value",
        32,
      ),
    ),
  };
  return fraudProofCatalogueBranchHashV1(
    path.slice(cursor, nextCursor - 1),
    fraudProofCatalogueMerkleRoot16V1(children),
  );
};

const fraudProofCatalogueDataBytesV1 = (hex: string): Buffer =>
  Buffer.from(Data.to(hex as never, Data.Bytes()), "hex");

const parseFraudProofCatalogueProofV1 = (
  cborHex: string,
  field: string,
): FraudProofCatalogueProofV1 => {
  let proof: FraudProofCatalogueProofV1;
  try {
    proof = Data.from(
      cborHex,
      FraudProofCatalogueProofV1Schema as never,
    ) as FraudProofCatalogueProofV1;
  } catch (cause) {
    throw new Error(
      `Deployment manifest ${field} is not valid fraud-proof catalogue membership proof CBOR: ${String(cause)}`,
    );
  }
  const canonicalCbor = Data.to(
    proof as never,
    FraudProofCatalogueProofV1Schema,
  );
  if (canonicalCbor !== cborHex) {
    throw new Error(
      `Deployment manifest ${field} must use canonical fraud-proof catalogue proof CBOR`,
    );
  }
  return proof;
};

const validateFinalizedContracts = (
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
    [],
    "contracts",
  );
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const scriptHashByName = new Map<string, string>();
  for (const contractName of DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireRecord(contracts[contractName], field);
    requireExactKeys(
      entry,
      ["refScriptUTxO", "contract", "scriptHash"],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    if (referenceScriptContractNames.has(contractName)) {
      requireFinalOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    } else if (entry.refScriptUTxO !== null) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be null because the contract has no reference-script role`,
      );
    }
    const contract = requireRecord(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    if (
      contract.type !== "Native" &&
      contract.type !== "PlutusV1" &&
      contract.type !== "PlutusV2" &&
      contract.type !== "PlutusV3"
    ) {
      throw new Error(
        `Deployment manifest ${field}.contract.type is unsupported`,
      );
    }
    const cborHex = requireHex(
      contract.cborHex,
      undefined,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireHex(entry.scriptHash, 28, `${field}.scriptHash`);
    let derivedScriptHash: string;
    try {
      derivedScriptHash = validatorToScriptHash({
        type: contract.type,
        script: cborHex,
      });
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.contract.cborHex is invalid: ${String(cause)}`,
      );
    }
    if (derivedScriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash mismatch: expected ${derivedScriptHash}`,
      );
    }
    scriptHashByName.set(contractName, scriptHash);
  }

  const catalogueMint = requireRecord(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  const catalogue = requireRecord(
    catalogueMint.fraudProofCatalogue,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  const declaredCatalogueRoot = requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireRecord(
    catalogue.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const contractByCategory = {
    doubleSpend: "fraudProofDoubleSpend",
    nonExistentInput: "fraudProofNonExistentInput",
    nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
    invalidRange: "fraudProofInvalidRange",
    transitionTrace: "fraudProofTransitionTrace",
    zeroInput: "fraudProofZeroInput",
    validationTraceDispute: "validationTraceDispute",
  } as const;
  requireExactKeys(
    categories,
    Object.keys(contractByCategory),
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const catalogueEntries: Array<
    FraudProofCatalogueEntryV1 & {
      readonly categoryName: string;
      readonly membershipProofCbor: string;
    }
  > = [];
  const seenCategoryIds = new Set<string>();
  for (const [categoryIndex, [categoryName, contractName]] of Object.entries(
    contractByCategory,
  ).entries()) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireRecord(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireHex(
      category.categoryId,
      FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT_V1,
      `${field}.categoryId`,
    );
    const expectedCategoryId = Buffer.alloc(
      FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT_V1,
    );
    expectedCategoryId.writeUInt32BE(categoryIndex);
    if (categoryId !== expectedCategoryId.toString("hex")) {
      throw new Error(
        `Deployment manifest ${field}.categoryId must be ${expectedCategoryId.toString("hex")} for canonical V1 chronology`,
      );
    }
    if (seenCategoryIds.has(categoryId)) {
      throw new Error(
        `Deployment manifest ${field}.categoryId duplicates another fraud-proof catalogue category`,
      );
    }
    seenCategoryIds.add(categoryId);
    const scriptHash = requireHex(
      category.scriptHash,
      FRAUD_PROOF_CATALOGUE_SCRIPT_HASH_BYTE_COUNT_V1,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireHex(
      category.membershipProofCbor,
      undefined,
      `${field}.membershipProofCbor`,
    );
    if (scriptHash !== scriptHashByName.get(contractName)) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const encodedCategoryId = fraudProofCatalogueDataBytesV1(categoryId);
    const encodedScriptHash = fraudProofCatalogueDataBytesV1(scriptHash);
    catalogueEntries.push({
      categoryName,
      membershipProofCbor,
      path: fraudProofCatalogueDigestV1(encodedCategoryId).toString("hex"),
      valueDigest: fraudProofCatalogueDigestV1(encodedScriptHash),
    });
  }

  const expectedCatalogueRoot =
    fraudProofCatalogueRootFromEntriesV1(catalogueEntries).toString("hex");
  if (declaredCatalogueRoot !== expectedCatalogueRoot) {
    throw new Error(
      `Deployment manifest contracts.fraudProofCatalogueMint.fraudProofCatalogue.root mismatch: expected ${expectedCatalogueRoot}`,
    );
  }
  for (const entry of catalogueEntries) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${entry.categoryName}.membershipProofCbor`;
    const proof = parseFraudProofCatalogueProofV1(
      entry.membershipProofCbor,
      field,
    );
    let proofRoot: string;
    try {
      proofRoot = fraudProofCatalogueRootFromProofV1(
        entry.path,
        entry.valueDigest,
        proof,
      ).toString("hex");
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field} is invalid: ${String(cause)}`,
      );
    }
    if (proofRoot !== expectedCatalogueRoot) {
      throw new Error(
        `Deployment manifest ${field} does not open the canonical fraud-proof catalogue root`,
      );
    }
  }
};

const validateFinalizedReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  requireExactKeys(referenceScripts, roles, [], "referenceScripts");
  const policyId = requireHex(
    referenceScriptAuthPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of roles) {
    const field = `referenceScripts.${role}`;
    const reference = requireRecord(referenceScripts[role], field);
    requireExactKeys(
      reference,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (reference.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const tokenName =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    const expectedRoleUnit =
      policyId + bytesToHex(new TextEncoder().encode(tokenName));
    if (reference.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireRecord(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    const scriptHash = requireHex(
      reference.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    if (scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireFinalOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const expectedOutRef = `${contractOutRef.txHash}#${contractOutRef.outputIndex.toString()}`;
    if (reference.outRef !== expectedOutRef) {
      throw new Error(
        `Deployment manifest ${field}.outRef must equal ${expectedOutRef}`,
      );
    }
  }
};

const validateFinalizedDa = (value: unknown): void => {
  const da = requireRecord(value, "Deployment manifest da");
  requireExactKeys(
    da,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (!Array.isArray(da.committeeVkeys) || da.committeeVkeys.length === 0) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = da.committeeVkeys.map((entry, index) =>
    requireHex(entry, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error("Deployment manifest da.committeeVkeys must be unique");
  }
  const committeeSignersHash = requireHex(
    da.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash = bytesToHex(
    blake2b(hexToBytes(committeeVkeys.join("")), { dkLen: 32 }),
  );
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requireInteger(da.threshold, "da.threshold", 1);
  if (threshold > committeeVkeys.length) {
    throw new Error("Deployment manifest da.threshold exceeds committee size");
  }
  const transport = requireRecord(
    da.transportProfile,
    "Deployment manifest da.transportProfile",
  );
  requireExactKeys(
    transport,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transport.protocolVersion !== DA_TRANSPORT_V1_PROTOCOL_VERSION) {
    throw new Error(
      "Deployment manifest da.transportProfile.protocolVersion is unsupported",
    );
  }
  if (
    transport.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.runtimeManifestSchemaVersion is unsupported",
    );
  }
  if (
    transport.envelopeEncoding !== "identity" &&
    transport.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding is unsupported",
    );
  }
  requireInteger(transport.zstdLevel, "da.transportProfile.zstdLevel", 1);
  if (
    stableJson(
      normalizeDeploymentManifestV1JsonValueInternal(
        transport.limits,
        "Deployment manifest da.transportProfile.limits",
        false,
      ),
    ) !== stableJson(DA_TRANSPORT_LIMITS_V1)
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
    );
  }
  const retentionDays = requireInteger(
    transport.retentionDays,
    "da.transportProfile.retentionDays",
    1,
  );
  if (retentionDays < DA_TRANSPORT_LIMITS_V1.minimumRetentionDays) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays is too short",
    );
  }
};

export const verifyFinalizedDeploymentManifestV1 = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = verifyDeploymentManifestV1Identity(value);
  if (
    candidate.network !== "Mainnet" &&
    candidate.network !== "Preprod" &&
    candidate.network !== "Preview" &&
    candidate.network !== "Custom"
  ) {
    throw new Error("Deployment manifest network is unsupported");
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );

  const cardano = requireRecord(
    candidate.cardanoProtocolParameters,
    "Deployment manifest cardanoProtocolParameters",
  );
  requireExactKeys(
    cardano,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoDigest = requireHex(
    cardano.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  const expectedCardanoDigest = computeDeploymentManifestV1JsonDigest(
    cardano.snapshot,
  );
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }

  const genesis = requireRecord(
    candidate.genesis,
    "Deployment manifest genesis",
  );
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireHex(genesis.headerHash, 28, "genesis.headerHash");
  requireHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const oneShot = requireRecord(
    candidate.hubOracleOneShot,
    "Deployment manifest hubOracleOneShot",
  );
  requireExactKeys(
    oneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const oneShotTxHash = requireHex(
    oneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const oneShotOutputIndex = requireInteger(
    oneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOneShotOutRef = `${oneShotTxHash}#${oneShotOutputIndex.toString()}`;
  if (oneShot.outRef !== expectedOneShotOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef must equal ${expectedOneShotOutRef}`,
    );
  }
  if (oneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }

  const authPolicy = requireRecord(
    candidate.referenceScriptAuthPolicy,
    "Deployment manifest referenceScriptAuthPolicy",
  );
  requireExactKeys(
    authPolicy,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireHex(
    authPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireRecord(
    authPolicy.nativeScript,
    "Deployment manifest referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireHex(
    nativeScript.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  requireInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requireInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
    1,
  );
  const derivedPolicyId = validatorToScriptHash({
    type: "Native",
    script: nativeScriptCbor,
  });
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    authPolicy.tokenNames,
    "Deployment manifest referenceScriptAuthPolicy.tokenNames",
  );
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  );
  requireExactKeys(
    tokenNames,
    roles,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const role of roles) {
    const expected =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    if (tokenNames[role] !== expected) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expected}`,
      );
    }
  }
  const audit = requireRecord(
    authPolicy.postTimelockAudit,
    "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    audit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (audit.required !== true) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be true",
    );
  }
  requireString(audit.rule, "referenceScriptAuthPolicy.postTimelockAudit.rule");

  const contracts = requireRecord(
    candidate.contracts,
    "Deployment manifest contracts",
  );
  validateFinalizedContracts(contracts);
  const authContract = requireRecord(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (authContract.scriptHash !== policyId) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateFinalizedReferenceScripts(
    requireRecord(
      candidate.referenceScripts,
      "Deployment manifest referenceScripts",
    ),
    authPolicy,
    contracts,
  );
  validateFinalizedDa(candidate.da);

  const proofEvidence = requireRecord(
    candidate.proofEvidence,
    "Deployment manifest proofEvidence",
  );
  requireExactKeys(
    proofEvidence,
    ["digest", "blueprintHash"],
    [],
    "proofEvidence",
  );
  if (proofEvidence.digest !== MIDGARD_V1_RELEASE_EVIDENCE_DIGEST) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must match compiled canonical V1 evidence",
    );
  }
  requireHex(proofEvidence.blueprintHash, 32, "proofEvidence.blueprintHash");

  const steps = requireRecord(candidate.steps, "Deployment manifest steps");
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_V1_STEP_NAMES, [], "steps");
  const supportedStepStatuses = new Set([
    "pending",
    "in_progress",
    "submitted",
    "complete",
    "attached",
    "failed",
    "blocked_requires_fresh_redeploy",
  ]);
  for (const stepName of DEPLOYMENT_MANIFEST_V1_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireRecord(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (!supportedStepStatuses.has(String(step.status))) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireHex(step.txHash, 32, `${field}.txHash`);
    }
  }
  for (const requiredStep of [
    "prepareHubOracleNonce",
    "deployNodeRuntimeReferenceScripts",
    "initProtocol",
  ]) {
    const step = requireRecord(steps[requiredStep], `steps.${requiredStep}`);
    if (step.status !== "complete") {
      throw new Error(
        `Deployment manifest steps.${requiredStep}.status must be complete`,
      );
    }
  }

  const dispute = requireRecord(
    candidate.validationDispute,
    "Deployment manifest validationDispute",
  );
  requireExactKeys(
    dispute,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  const expectedDispute = {
    version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
    responseWindowMs:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
    maxBisectionRounds:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
    maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedDispute)) {
    if (dispute[key] !== expected) {
      throw new Error(
        `Deployment manifest validationDispute.${key} must equal ${expected.toString()}`,
      );
    }
  }
  return candidate;
};
