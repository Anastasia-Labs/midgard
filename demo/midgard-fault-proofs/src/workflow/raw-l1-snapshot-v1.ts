import { createHash } from "node:crypto";

import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy-v1.js";

export const FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION =
  "midgard-fraud-proof-raw-l1-snapshot-v1" as const;
export const FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY =
  "midgard-fraud-proof-raw-l1-snapshot-authority-v1" as const;

export type FraudProofRawL1ComputationStepRole =
  | "computation_thread_step_01"
  | "computation_thread_step_02"
  | "computation_thread_step_03"
  | "computation_thread_step_04"
  | "computation_thread_step_05"
  | "computation_thread_step_06"
  | "computation_thread_step_07"
  | "computation_thread_step_08"
  | "computation_thread_step_09";

export type FraudProofRawL1ScopeRole =
  | "state_queue"
  | FraudProofRawL1ComputationStepRole
  | "permanent_proof_token"
  | "active_operator_directory"
  | "retired_operator_directory"
  | "scheduler"
  | "proof_chunk"
  | "field_publication"
  | "field_certificate";

export type FraudProofRawL1SnapshotRequest = {
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly finalityPolicyDigest: string;
  readonly headerHash: string;
  readonly scopes: readonly {
    readonly role: FraudProofRawL1ScopeRole;
    readonly address: string;
  }[];
  /** Exact units whose create/spend history must be returned. */
  readonly historyUnits: readonly string[];
};

export type FraudProofRawL1Point = {
  readonly slot: string;
  readonly blockHash: string;
  readonly blockNo: string;
  readonly pointId: string;
};

export type FraudProofRawL1Utxo = {
  readonly outRef: string;
  readonly outputCbor: string;
  readonly datumCbor: string | null;
  readonly referenceScriptCbor: string | null;
};

export type FraudProofRawL1Transaction = {
  readonly txHash: string;
  readonly bodyCbor: string;
  readonly witnessSetCbor: string;
  readonly redeemersCbor: string | null;
  readonly isValid: true;
  readonly inclusionPoint: FraudProofRawL1Point;
  readonly confirmationDepth: number;
  /** Every ordinary input resolved to the exact output bytes it consumed. */
  readonly resolvedInputs: readonly FraudProofRawL1Utxo[];
  /** Every reference input resolved to the exact output bytes it referenced. */
  readonly resolvedReferenceInputs: readonly FraudProofRawL1Utxo[];
};

export type FraudProofRawL1UnitHistory = {
  readonly unit: string;
  /** Kupo's matcher was scanned from origin rather than an arbitrary cursor. */
  readonly fromGenesis: true;
  readonly completeThroughPointId: string;
  readonly transactionHashes: readonly string[];
};

export type FraudProofRawL1Snapshot = {
  readonly schemaVersion: typeof FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly finalityPolicyDigest: string;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance & {
    readonly trustClass: "authenticated_cardano_l1";
    readonly sourceMode: "local_kupo_ogmios";
    readonly kupoCheckpoint: FraudProofRawL1Point;
    readonly ogmiosTip: FraudProofRawL1Point;
  };
  readonly cursor: {
    readonly point: FraudProofRawL1Point;
    readonly tip: FraudProofRawL1Point;
    readonly confirmationDepth: number;
    readonly rollbackCursor: string;
  };
  readonly scopes: readonly {
    readonly role: FraudProofRawL1ScopeRole;
    readonly address: string;
    readonly utxos: readonly FraudProofRawL1Utxo[];
  }[];
  readonly historyUnits: readonly string[];
  readonly history: readonly FraudProofRawL1UnitHistory[];
  readonly transactions: readonly FraudProofRawL1Transaction[];
};

/**
 * Provider-specific implementations return untrusted bytes. Admission and all
 * stage/terminal derivation stay in this package.
 */
export interface FraudProofRawL1SnapshotAuthority {
  readonly authorityVersion: typeof FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY;
  capture(request: FraudProofRawL1SnapshotRequest): Promise<unknown>;
}

const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;
const NATURAL = /^(0|[1-9][0-9]*)$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;
const MAX_COLLECTION_SIZE = 100_000;
const RAW_L1_SCOPE_ROLES = new Set<FraudProofRawL1ScopeRole>([
  "state_queue",
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
  "computation_thread_step_06",
  "computation_thread_step_07",
  "computation_thread_step_08",
  "computation_thread_step_09",
  "permanent_proof_token",
  "active_operator_directory",
  "retired_operator_directory",
  "scheduler",
  "proof_chunk",
  "field_publication",
  "field_certificate",
]);

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
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

const string = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

const digest = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!HEX_32.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

const assetUnit = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!UNIT.test(parsed))
    throw new Error(`${label} must be a canonical asset unit`);
  return parsed;
};

const address = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  try {
    if (CML.Address.from_bech32(parsed).to_bech32() !== parsed) {
      throw new Error("non-canonical address");
    }
  } catch {
    throw new Error(`${label} must be a canonical Cardano bech32 address`);
  }
  return parsed;
};

const cbor = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!EVEN_HEX.test(parsed))
    throw new Error(`${label} must be lowercase CBOR`);
  return parsed;
};

const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value) || value.length > MAX_COLLECTION_SIZE) {
    throw new Error(`${label} must be a bounded array`);
  }
  return value;
};

export const computeFraudProofRawL1PointId = ({
  slot,
  blockHash,
  blockNo,
}: Omit<FraudProofRawL1Point, "pointId">): string =>
  createHash("sha256").update(`${slot}:${blockHash}:${blockNo}`).digest("hex");

const point = (value: unknown, label: string): FraudProofRawL1Point => {
  const parsed = exact(
    value,
    ["slot", "blockHash", "blockNo", "pointId"],
    label,
  );
  const slot = string(parsed.slot, `${label}.slot`);
  const blockNo = string(parsed.blockNo, `${label}.blockNo`);
  if (!NATURAL.test(slot) || !NATURAL.test(blockNo)) {
    throw new Error(`${label} slot/blockNo must be canonical naturals`);
  }
  const result = {
    slot,
    blockNo,
    blockHash: digest(parsed.blockHash, `${label}.blockHash`),
    pointId: digest(parsed.pointId, `${label}.pointId`),
  };
  if (result.pointId !== computeFraudProofRawL1PointId(result)) {
    throw new Error(`${label}.pointId does not commit to the chain point`);
  }
  return result;
};

export const admitFraudProofRawL1Point = (
  value: unknown,
  label = "raw L1 point",
): FraudProofRawL1Point => point(value, label);

export const computeFraudProofRawL1RollbackCursor = ({
  deploymentIdentityDigest,
  releaseIdentityDigest,
  finalityPolicyDigest,
  sourceId,
  pointId,
}: {
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly finalityPolicyDigest: string;
  readonly sourceId: string;
  readonly pointId: string;
}): string =>
  createHash("sha256")
    .update(
      `${deploymentIdentityDigest}:${releaseIdentityDigest}:${finalityPolicyDigest}:${sourceId}:${pointId}`,
    )
    .digest("hex");

const outRefOf = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!OUT_REF.test(parsed))
    throw new Error(`${label} must be a canonical outRef`);
  return parsed;
};

const utxo = (value: unknown, label: string): FraudProofRawL1Utxo => {
  const parsed = exact(
    value,
    ["outRef", "outputCbor", "datumCbor", "referenceScriptCbor"],
    label,
  );
  const outputCbor = cbor(parsed.outputCbor, `${label}.outputCbor`);
  let output: CML.TransactionOutput;
  try {
    output = CML.TransactionOutput.from_cbor_hex(outputCbor);
  } catch {
    throw new Error(`${label}.outputCbor is not a Cardano output`);
  }
  if (output.to_canonical_cbor_hex() !== outputCbor) {
    throw new Error(`${label}.outputCbor is not canonical`);
  }
  const actualDatum =
    output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null;
  const actualScript = output.script_ref()?.to_canonical_cbor_hex() ?? null;
  const datumCbor =
    parsed.datumCbor === null
      ? null
      : cbor(parsed.datumCbor, `${label}.datumCbor`);
  const referenceScriptCbor =
    parsed.referenceScriptCbor === null
      ? null
      : cbor(parsed.referenceScriptCbor, `${label}.referenceScriptCbor`);
  if (actualDatum !== datumCbor || actualScript !== referenceScriptCbor) {
    throw new Error(
      `${label} datum/reference-script bytes differ from output CBOR`,
    );
  }
  return {
    outRef: outRefOf(parsed.outRef, `${label}.outRef`),
    outputCbor,
    datumCbor,
    referenceScriptCbor,
  };
};

export const admitFraudProofRawL1Utxo = (
  value: unknown,
  label = "raw L1 UTxO",
): FraudProofRawL1Utxo => utxo(value, label);

const outputAddress = (candidate: FraudProofRawL1Utxo): string =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(candidate.outputCbor))
    .address;

const bodyInputOutRefs = (body: CML.TransactionBody): readonly string[] => {
  const result: string[] = [];
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

const bodyReferenceInputOutRefs = (
  body: CML.TransactionBody,
): readonly string[] => {
  const result: string[] = [];
  const inputs = body.reference_inputs();
  if (inputs === undefined) return result;
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

export const admitFraudProofRawL1Transaction = (
  value: unknown,
  label: string,
  minimumConfirmationDepth: number,
): FraudProofRawL1Transaction => {
  const parsed = exact(
    value,
    [
      "txHash",
      "bodyCbor",
      "witnessSetCbor",
      "redeemersCbor",
      "isValid",
      "inclusionPoint",
      "confirmationDepth",
      "resolvedInputs",
      "resolvedReferenceInputs",
    ],
    label,
  );
  if (parsed.isValid !== true)
    throw new Error(`${label} must be a valid transaction`);
  const txHash = digest(parsed.txHash, `${label}.txHash`);
  const bodyCbor = cbor(parsed.bodyCbor, `${label}.bodyCbor`);
  const witnessSetCbor = cbor(parsed.witnessSetCbor, `${label}.witnessSetCbor`);
  let body: CML.TransactionBody;
  let witnesses: CML.TransactionWitnessSet;
  try {
    body = CML.TransactionBody.from_cbor_hex(bodyCbor);
    witnesses = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  } catch {
    throw new Error(`${label} contains invalid transaction CBOR`);
  }
  if (
    body.to_canonical_cbor_hex() !== bodyCbor ||
    witnesses.to_canonical_cbor_hex() !== witnessSetCbor ||
    CML.hash_transaction(body).to_hex() !== txHash
  ) {
    throw new Error(
      `${label} transaction bytes are non-canonical or hash-mismatched`,
    );
  }
  const actualRedeemers =
    witnesses.redeemers()?.to_canonical_cbor_hex() ?? null;
  const redeemersCbor =
    parsed.redeemersCbor === null
      ? null
      : cbor(parsed.redeemersCbor, `${label}.redeemersCbor`);
  if (actualRedeemers !== redeemersCbor) {
    throw new Error(`${label}.redeemersCbor differs from the witness set`);
  }
  if (
    !Number.isSafeInteger(parsed.confirmationDepth) ||
    (parsed.confirmationDepth as number) < minimumConfirmationDepth
  ) {
    throw new Error(`${label} is below release finality`);
  }
  const resolvedInputs = array(
    parsed.resolvedInputs,
    `${label}.resolvedInputs`,
  ).map((candidate, index) =>
    utxo(candidate, `${label}.resolvedInputs[${index.toString()}]`),
  );
  const resolvedReferenceInputs = array(
    parsed.resolvedReferenceInputs,
    `${label}.resolvedReferenceInputs`,
  ).map((candidate, index) =>
    utxo(candidate, `${label}.resolvedReferenceInputs[${index.toString()}]`),
  );
  const expectedInputs = [...bodyInputOutRefs(body)].sort();
  const actualInputs = resolvedInputs.map((input) => input.outRef).sort();
  if (
    expectedInputs.length !== actualInputs.length ||
    expectedInputs.some((outRef, index) => outRef !== actualInputs[index])
  ) {
    throw new Error(
      `${label}.resolvedInputs do not exactly resolve body inputs`,
    );
  }
  const expectedReferenceInputs = [...bodyReferenceInputOutRefs(body)].sort();
  const actualReferenceInputs = resolvedReferenceInputs
    .map((input) => input.outRef)
    .sort();
  if (
    expectedReferenceInputs.length !== actualReferenceInputs.length ||
    expectedReferenceInputs.some(
      (outRef, index) => outRef !== actualReferenceInputs[index],
    )
  ) {
    throw new Error(
      `${label}.resolvedReferenceInputs do not exactly resolve body reference inputs`,
    );
  }
  return {
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor,
    isValid: true,
    inclusionPoint: point(parsed.inclusionPoint, `${label}.inclusionPoint`),
    confirmationDepth: parsed.confirmationDepth as number,
    resolvedInputs,
    resolvedReferenceInputs,
  };
};

const outputContainsUnit = (
  output: CML.TransactionOutput,
  unit: string,
): boolean => (coreToTxOutput(output).assets[unit] ?? 0n) !== 0n;

const transactionTouchesUnit = (
  candidate: FraudProofRawL1Transaction,
  unit: string,
): boolean => {
  if (
    candidate.resolvedInputs.some((input) =>
      outputContainsUnit(
        CML.TransactionOutput.from_cbor_hex(input.outputCbor),
        unit,
      ),
    )
  ) {
    return true;
  }
  const body = CML.TransactionBody.from_cbor_hex(candidate.bodyCbor);
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputContainsUnit(outputs.get(index), unit)) return true;
  }
  const mint = body.mint();
  if (mint === undefined) return false;
  const policy = CML.ScriptHash.from_hex(unit.slice(0, 56));
  const minted = mint.get_assets(policy);
  if (minted === undefined) return false;
  return (minted.get(CML.AssetName.from_hex(unit.slice(56))) ?? 0n) !== 0n;
};

const historyEntry = (
  value: unknown,
  label: string,
): FraudProofRawL1UnitHistory => {
  const parsed = exact(
    value,
    ["unit", "fromGenesis", "completeThroughPointId", "transactionHashes"],
    label,
  );
  if (parsed.fromGenesis !== true) {
    throw new Error(`${label} must cover unit history from genesis`);
  }
  const transactionHashes = array(
    parsed.transactionHashes,
    `${label}.transactionHashes`,
  ).map((candidate, index) =>
    digest(candidate, `${label}.transactionHashes[${index.toString()}]`),
  );
  if (new Set(transactionHashes).size !== transactionHashes.length) {
    throw new Error(`${label} contains duplicate transaction hashes`);
  }
  return {
    unit: assetUnit(parsed.unit, `${label}.unit`),
    fromGenesis: true,
    completeThroughPointId: digest(
      parsed.completeThroughPointId,
      `${label}.completeThroughPointId`,
    ),
    transactionHashes,
  };
};

const sameStringSet = (
  left: readonly string[],
  right: readonly string[],
): boolean => {
  if (left.length !== right.length) return false;
  const sortedLeft = [...left].sort();
  const sortedRight = [...right].sort();
  return sortedLeft.every((value, index) => value === sortedRight[index]);
};

export const admitFraudProofRawL1Snapshot = ({
  value,
  request,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly request: FraudProofRawL1SnapshotRequest;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): FraudProofRawL1Snapshot => {
  const root = exact(
    value,
    [
      "schemaVersion",
      "deploymentIdentityDigest",
      "releaseIdentityDigest",
      "finalityPolicyDigest",
      "headerHash",
      "provenance",
      "cursor",
      "scopes",
      "historyUnits",
      "history",
      "transactions",
    ],
    "raw L1 snapshot",
  );
  if (root.schemaVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION) {
    throw new Error("raw L1 snapshot has an unsupported schema");
  }
  const deploymentIdentityDigest = digest(
    root.deploymentIdentityDigest,
    "raw L1 snapshot deploymentIdentityDigest",
  );
  const releaseIdentityDigest = digest(
    root.releaseIdentityDigest,
    "raw L1 snapshot releaseIdentityDigest",
  );
  const finalityPolicyDigest = digest(
    root.finalityPolicyDigest,
    "raw L1 snapshot finalityPolicyDigest",
  );
  const headerHash = string(root.headerHash, "raw L1 snapshot headerHash");
  if (!HEX_28.test(headerHash))
    throw new Error("raw L1 snapshot headerHash must be 28-byte hex");
  if (
    request.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    request.releaseIdentityDigest !== releaseFinality.releaseIdentityDigest ||
    request.finalityPolicyDigest !== releaseFinality.policyDigest ||
    !HEX_28.test(request.headerHash)
  ) {
    throw new Error(
      "raw L1 request is not bound to the verified release identity",
    );
  }
  if (
    deploymentIdentityDigest !== request.deploymentIdentityDigest ||
    deploymentIdentityDigest !== releaseFinality.deploymentIdentityDigest ||
    releaseIdentityDigest !== request.releaseIdentityDigest ||
    releaseIdentityDigest !== releaseFinality.releaseIdentityDigest ||
    finalityPolicyDigest !== request.finalityPolicyDigest ||
    finalityPolicyDigest !== releaseFinality.policyDigest ||
    headerHash !== request.headerHash
  ) {
    throw new Error(
      "raw L1 snapshot changed a deployment/finality/header identity",
    );
  }
  const provenanceRecord = exact(
    root.provenance,
    [
      "trustClass",
      "sourceId",
      "grade",
      "sourceMode",
      "kupoCheckpoint",
      "ogmiosTip",
    ],
    "raw L1 snapshot provenance",
  );
  if (
    provenanceRecord.trustClass !== "authenticated_cardano_l1" ||
    provenanceRecord.grade !== "security" ||
    provenanceRecord.sourceMode !== "local_kupo_ogmios"
  ) {
    throw new Error(
      "raw L1 snapshot lacks security-grade Kupo/Ogmios provenance",
    );
  }
  const provenance = {
    trustClass: "authenticated_cardano_l1" as const,
    sourceId: string(provenanceRecord.sourceId, "raw L1 snapshot sourceId"),
    grade: "security" as const,
    sourceMode: "local_kupo_ogmios" as const,
    kupoCheckpoint: point(
      provenanceRecord.kupoCheckpoint,
      "raw L1 Kupo checkpoint",
    ),
    ogmiosTip: point(provenanceRecord.ogmiosTip, "raw L1 Ogmios tip"),
  };
  const cursorRecord = exact(
    root.cursor,
    ["point", "tip", "confirmationDepth", "rollbackCursor"],
    "raw L1 snapshot cursor",
  );
  if (
    !Number.isSafeInteger(cursorRecord.confirmationDepth) ||
    (cursorRecord.confirmationDepth as number) <
      releaseFinality.policy.confirmationDepth
  ) {
    throw new Error("raw L1 snapshot cursor is below release finality");
  }
  const cursor = {
    point: point(cursorRecord.point, "raw L1 cursor point"),
    tip: point(cursorRecord.tip, "raw L1 cursor tip"),
    confirmationDepth: cursorRecord.confirmationDepth as number,
    rollbackCursor: digest(
      cursorRecord.rollbackCursor,
      "raw L1 rollback cursor",
    ),
  };
  if (
    provenance.kupoCheckpoint.pointId !== cursor.point.pointId ||
    provenance.ogmiosTip.pointId !== cursor.tip.pointId
  ) {
    throw new Error(
      "raw L1 provider checkpoints disagree with the rollback cursor",
    );
  }
  const expectedCursorDepth =
    BigInt(cursor.tip.blockNo) - BigInt(cursor.point.blockNo) + 1n;
  if (
    expectedCursorDepth <= 0n ||
    expectedCursorDepth !== BigInt(cursor.confirmationDepth) ||
    BigInt(cursor.point.slot) > BigInt(cursor.tip.slot)
  ) {
    throw new Error(
      "raw L1 cursor confirmation depth disagrees with chain points",
    );
  }
  if (
    cursor.rollbackCursor !==
    computeFraudProofRawL1RollbackCursor({
      deploymentIdentityDigest,
      releaseIdentityDigest,
      finalityPolicyDigest,
      sourceId: provenance.sourceId,
      pointId: cursor.point.pointId,
    })
  ) {
    throw new Error(
      "raw L1 rollback cursor does not bind the release and chain point",
    );
  }
  const requestedScopes = new Map(
    request.scopes.map((scope, index) => {
      if (!RAW_L1_SCOPE_ROLES.has(scope.role)) {
        throw new Error(
          `raw L1 request scopes[${index.toString()}].role is unsupported`,
        );
      }
      return [
        scope.role,
        address(
          scope.address,
          `raw L1 request scopes[${index.toString()}].address`,
        ),
      ] as const;
    }),
  );
  if (requestedScopes.size !== request.scopes.length) {
    throw new Error("raw L1 request contains duplicate scope roles");
  }
  const seenRoles = new Set<string>();
  const scopes = array(root.scopes, "raw L1 snapshot scopes").map(
    (candidate, index) => {
      const label = `raw L1 snapshot scopes[${index.toString()}]`;
      const parsed = exact(candidate, ["role", "address", "utxos"], label);
      const role = string(
        parsed.role,
        `${label}.role`,
      ) as FraudProofRawL1ScopeRole;
      const scopedAddress = address(parsed.address, `${label}.address`);
      if (
        !RAW_L1_SCOPE_ROLES.has(role) ||
        seenRoles.has(role) ||
        requestedScopes.get(role) !== scopedAddress
      ) {
        throw new Error(`${label} is duplicate or was not exactly requested`);
      }
      seenRoles.add(role);
      const utxos = array(parsed.utxos, `${label}.utxos`).map(
        (entry, utxoIndex) =>
          utxo(entry, `${label}.utxos[${utxoIndex.toString()}]`),
      );
      if (new Set(utxos.map((entry) => entry.outRef)).size !== utxos.length) {
        throw new Error(`${label} contains duplicate outRefs`);
      }
      if (utxos.some((entry) => outputAddress(entry) !== scopedAddress)) {
        throw new Error(`${label} contains an output from a different address`);
      }
      return { role, address: scopedAddress, utxos };
    },
  );
  if (seenRoles.size !== requestedScopes.size) {
    throw new Error("raw L1 snapshot omitted an address scope");
  }
  const historyUnits = array(
    root.historyUnits,
    "raw L1 snapshot historyUnits",
  ).map((unit, index) =>
    assetUnit(unit, `raw L1 snapshot historyUnits[${index.toString()}]`),
  );
  const requestedHistoryUnits = request.historyUnits.map((unit, index) =>
    assetUnit(unit, `raw L1 request historyUnits[${index.toString()}]`),
  );
  if (
    new Set(requestedHistoryUnits).size !== requestedHistoryUnits.length ||
    new Set(historyUnits).size !== historyUnits.length ||
    !sameStringSet(historyUnits, requestedHistoryUnits)
  ) {
    throw new Error("raw L1 snapshot changed the requested history units");
  }
  const transactions = array(
    root.transactions,
    "raw L1 snapshot transactions",
  ).map((candidate, index) =>
    admitFraudProofRawL1Transaction(
      candidate,
      `raw L1 snapshot transactions[${index.toString()}]`,
      releaseFinality.policy.confirmationDepth,
    ),
  );
  if (
    new Set(transactions.map((entry) => entry.txHash)).size !==
    transactions.length
  ) {
    throw new Error("raw L1 snapshot contains duplicate transactions");
  }
  const transactionHashes = new Set(transactions.map((entry) => entry.txHash));
  const transactionByHash = new Map(
    transactions.map((entry) => [entry.txHash, entry] as const),
  );
  for (const [index, entry] of transactions.entries()) {
    const expectedDepth =
      BigInt(cursor.tip.blockNo) - BigInt(entry.inclusionPoint.blockNo) + 1n;
    if (
      expectedDepth <= 0n ||
      expectedDepth !== BigInt(entry.confirmationDepth) ||
      BigInt(entry.inclusionPoint.blockNo) > BigInt(cursor.point.blockNo) ||
      BigInt(entry.inclusionPoint.slot) > BigInt(cursor.point.slot)
    ) {
      throw new Error(
        `raw L1 snapshot transactions[${index.toString()}] has inconsistent inclusion finality`,
      );
    }
  }
  const history = array(root.history, "raw L1 snapshot history").map(
    (candidate, index) =>
      historyEntry(candidate, `raw L1 snapshot history[${index.toString()}]`),
  );
  if (
    new Set(history.map((entry) => entry.unit)).size !== history.length ||
    !sameStringSet(
      history.map((entry) => entry.unit),
      requestedHistoryUnits,
    )
  ) {
    throw new Error(
      "raw L1 snapshot omitted or duplicated unit history coverage",
    );
  }
  for (const [index, entry] of history.entries()) {
    const actualHashes = transactions
      .filter((candidate) => transactionTouchesUnit(candidate, entry.unit))
      .map((candidate) => candidate.txHash);
    if (
      entry.completeThroughPointId !== cursor.point.pointId ||
      entry.transactionHashes.some(
        (txHash) => !transactionHashes.has(txHash),
      ) ||
      !sameStringSet(entry.transactionHashes, actualHashes)
    ) {
      throw new Error(
        `raw L1 snapshot history[${index.toString()}] is incomplete or references a transaction that does not touch its unit`,
      );
    }
  }
  for (const [scopeIndex, scope] of scopes.entries()) {
    for (const [utxoIndex, candidate] of scope.utxos.entries()) {
      const touchesRequestedHistoryUnit = historyUnits.some((unit) =>
        outputContainsUnit(
          CML.TransactionOutput.from_cbor_hex(candidate.outputCbor),
          unit,
        ),
      );
      if (!touchesRequestedHistoryUnit) continue;
      const match = OUT_REF.exec(candidate.outRef);
      const creation =
        match === null ? undefined : transactionByHash.get(match[1]!);
      const outputIndex = match === null ? -1 : Number(match[2]);
      const createdOutputs =
        creation?.bodyCbor === undefined
          ? undefined
          : CML.TransactionBody.from_cbor_hex(creation.bodyCbor).outputs();
      const createdOutput =
        createdOutputs === undefined || outputIndex >= createdOutputs.len()
          ? undefined
          : createdOutputs.get(outputIndex);
      if (
        createdOutput === undefined ||
        createdOutput.to_canonical_cbor_hex() !== candidate.outputCbor
      ) {
        throw new Error(
          `raw L1 snapshot scopes[${scopeIndex.toString()}].utxos[${utxoIndex.toString()}] lacks its exact admitted creation transaction`,
        );
      }
    }
  }
  return {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest,
    releaseIdentityDigest,
    finalityPolicyDigest,
    headerHash,
    provenance,
    cursor,
    scopes,
    historyUnits,
    history,
    transactions,
  };
};
