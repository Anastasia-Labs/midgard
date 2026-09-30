import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import {
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  admittedHeaders,
  admittedObservations,
  exactRecord,
  HEX_28,
  HEX_32,
  NATURAL,
  OUT_REF,
  parsePersistedHeader,
  parseQueueNodes,
  RELEASE_FINALITY_DEPTH,
  WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherCorrectionLockObservation,
  type WatcherStateQueueHeaderObservation,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";

const parsePersistedLock = (
  value: unknown,
): WatcherCorrectionLockObservation | null => {
  const record = exactRecord(value, [
    "outRef",
    "datum",
    "observedTransactionHash",
    "observedBlockHash",
    "observedSlot",
    "observedBlockNo",
    "observedChainPointId",
    "finalityDepth",
  ]);
  const datum = SDK.parseStateQueueCorrectionLockDatum(record?.datum);
  if (
    record === null ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    datum === null ||
    typeof record.observedTransactionHash !== "string" ||
    !HEX_32.test(record.observedTransactionHash) ||
    typeof record.observedBlockHash !== "string" ||
    !HEX_32.test(record.observedBlockHash) ||
    typeof record.observedSlot !== "string" ||
    !NATURAL.test(record.observedSlot) ||
    typeof record.observedBlockNo !== "string" ||
    !NATURAL.test(record.observedBlockNo) ||
    typeof record.observedChainPointId !== "string" ||
    !HEX_32.test(record.observedChainPointId) ||
    typeof record.finalityDepth !== "string" ||
    !NATURAL.test(record.finalityDepth) ||
    BigInt(record.finalityDepth) < BigInt(RELEASE_FINALITY_DEPTH)
  ) {
    return null;
  }
  return Object.freeze({
    outRef: record.outRef,
    datum,
    observedTransactionHash: record.observedTransactionHash,
    observedBlockHash: record.observedBlockHash,
    observedSlot: record.observedSlot,
    observedBlockNo: record.observedBlockNo,
    observedChainPointId: record.observedChainPointId,
    finalityDepth: record.finalityDepth,
  });
};

export const parsePersistedObservation = (
  value: unknown,
): WatcherAuthenticatedStateQueueObservation | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "deploymentIdentityDigest",
    "protocolScriptAuthorityDigest",
    "stateQueuePolicyId",
    "hubOraclePolicyId",
    "nativePoint",
    "sourceId",
    "previousObservationDigest",
    "checkpoints",
    "finalizedQueue",
    "finalizedHeaders",
    "finalizedCorrectionLock",
    "correctionLockWitnesses",
    "observationDigest",
  ]);
  const nativePoint = exactRecord(record?.nativePoint, [
    "blockHash",
    "parentBlockHash",
    "slot",
    "blockNo",
    "chainPointId",
    "finalityDepth",
  ]);
  const checkpoints = Array.isArray(record?.checkpoints)
    ? record.checkpoints.map(SDK.parseStateQueueAuthenticatedReplayCheckpoint)
    : null;
  const queue = parseQueueNodes(record?.finalizedQueue);
  const headers = Array.isArray(record?.finalizedHeaders)
    ? record.finalizedHeaders.map(parsePersistedHeader)
    : null;
  const lock =
    record?.finalizedCorrectionLock === null
      ? null
      : parsePersistedLock(record?.finalizedCorrectionLock);
  const witnesses = Array.isArray(record?.correctionLockWitnesses)
    ? record.correctionLockWitnesses.map(
        SDK.parseStateQueueCorrectionLockWitness,
      )
    : null;
  if (
    record === null ||
    nativePoint === null ||
    record.schemaVersion !==
      WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION ||
    typeof record.deploymentIdentityDigest !== "string" ||
    !HEX_32.test(record.deploymentIdentityDigest) ||
    typeof record.protocolScriptAuthorityDigest !== "string" ||
    !HEX_32.test(record.protocolScriptAuthorityDigest) ||
    typeof record.stateQueuePolicyId !== "string" ||
    !HEX_28.test(record.stateQueuePolicyId) ||
    typeof record.hubOraclePolicyId !== "string" ||
    !HEX_28.test(record.hubOraclePolicyId) ||
    typeof nativePoint.blockHash !== "string" ||
    !HEX_32.test(nativePoint.blockHash) ||
    (nativePoint.parentBlockHash !== null &&
      (typeof nativePoint.parentBlockHash !== "string" ||
        !HEX_32.test(nativePoint.parentBlockHash))) ||
    typeof nativePoint.slot !== "string" ||
    !NATURAL.test(nativePoint.slot) ||
    typeof nativePoint.blockNo !== "string" ||
    !NATURAL.test(nativePoint.blockNo) ||
    typeof nativePoint.chainPointId !== "string" ||
    !HEX_32.test(nativePoint.chainPointId) ||
    typeof nativePoint.finalityDepth !== "string" ||
    !NATURAL.test(nativePoint.finalityDepth) ||
    BigInt(nativePoint.finalityDepth) < BigInt(RELEASE_FINALITY_DEPTH) ||
    typeof record.sourceId !== "string" ||
    record.sourceId.length === 0 ||
    (record.previousObservationDigest !== null &&
      (typeof record.previousObservationDigest !== "string" ||
        !HEX_32.test(record.previousObservationDigest))) ||
    checkpoints === null ||
    checkpoints.some((checkpoint) => checkpoint === null) ||
    queue === null ||
    headers === null ||
    headers.some((header) => header === null) ||
    (record.finalizedCorrectionLock !== null && lock === null) ||
    witnesses === null ||
    witnesses.some((witness) => witness === null) ||
    typeof record.observationDigest !== "string" ||
    !HEX_32.test(record.observationDigest)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: record.schemaVersion,
    deploymentIdentityDigest: record.deploymentIdentityDigest,
    protocolScriptAuthorityDigest: record.protocolScriptAuthorityDigest,
    stateQueuePolicyId: record.stateQueuePolicyId,
    hubOraclePolicyId: record.hubOraclePolicyId,
    nativePoint: Object.freeze({
      blockHash: nativePoint.blockHash as string,
      parentBlockHash: nativePoint.parentBlockHash as string | null,
      slot: nativePoint.slot as string,
      blockNo: nativePoint.blockNo as string,
      chainPointId: nativePoint.chainPointId as string,
      finalityDepth: nativePoint.finalityDepth as string,
    }),
    sourceId: record.sourceId,
    previousObservationDigest: record.previousObservationDigest as
      | string
      | null,
    checkpoints: Object.freeze(
      checkpoints as SDK.StateQueueAuthenticatedReplayCheckpoint[],
    ),
    finalizedQueue: queue,
    finalizedHeaders: Object.freeze(
      headers as WatcherStateQueueHeaderObservation[],
    ),
    finalizedCorrectionLock: lock,
    correctionLockWitnesses: Object.freeze(
      witnesses as SDK.StateQueueCorrectionLockWitness[],
    ),
  };
  return watcherSha256CanonicalJson(canonical) === record.observationDigest &&
    watcherSameCanonicalJson(
      canonical.correctionLockWitnesses,
      canonical.checkpoints.map(
        ({ correctionLockWitness }) => correctionLockWitness,
      ),
    )
    ? Object.freeze({
        ...canonical,
        observationDigest: record.observationDigest,
      })
    : null;
};

/**
 * Narrow test-only opaque admission. It first runs the exact persisted parser,
 * then independently re-derives every contained Header hash and queue link.
 * Production code cannot call this helper and no structural clone is admitted.
 */
export const unsafeAdmitWatcherStateQueueObservationForReplayTest = (
  value: unknown,
): WatcherAuthenticatedStateQueueObservation => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error("unsafe state-queue replay admission is test-only");
  }
  const parsed = parsePersistedObservation(value);
  if (parsed === null) {
    throw new Error("test state-queue replay observation is not canonical");
  }
  for (const header of parsed.finalizedHeaders) {
    const decoded = Data.from(header.headerCborHex, SDK.Header);
    if (
      Data.to(decoded, SDK.Header) !== header.headerCborHex ||
      computeHash28(Buffer.from(header.headerCborHex, "hex")).toString(
        "hex",
      ) !== header.headerHash ||
      !parsed.finalizedQueue.some(
        (node) =>
          node.headerHash === header.headerHash &&
          node.outRef === header.queueOutRef,
      ) ||
      header.observedBlockHash !== parsed.nativePoint.blockHash ||
      header.observedSlot !== parsed.nativePoint.slot ||
      header.observedBlockNo !== parsed.nativePoint.blockNo ||
      header.observedChainPointId !== parsed.nativePoint.chainPointId
    ) {
      throw new Error(
        "test state-queue replay HeaderV1 differs from its observation",
      );
    }
  }
  return admitObservation(parsed);
};

export const outputReferences = (
  inputs: CML.TransactionInputList | undefined,
): readonly string[] => {
  if (inputs === undefined) return [];
  const result: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

export const mintPolicyIds = (body: CML.TransactionBody): readonly string[] => {
  const mint = body.mint();
  if (mint === undefined) return [];
  const keys = mint.keys();
  const policies: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    policies.push(keys.get(index).to_hex());
  }
  return policies.sort();
};

export const outputHasPolicy = (
  output: CML.TransactionOutput,
  policyId: string,
): boolean =>
  Object.entries(coreToTxOutput(output).assets).some(
    ([unit, quantity]) => unit.startsWith(policyId) && quantity !== 0n,
  );

export const outputHasUnit = (
  output: CML.TransactionOutput,
  unit: string,
): boolean => (coreToTxOutput(output).assets[unit] ?? 0n) !== 0n;

export const rawOutputHasPolicy = (
  outputCbor: string,
  policyId: string,
): boolean =>
  outputHasPolicy(CML.TransactionOutput.from_cbor_hex(outputCbor), policyId);

export const admitObservation = (
  observation: WatcherAuthenticatedStateQueueObservation,
): WatcherAuthenticatedStateQueueObservation => {
  admittedObservations.add(observation);
  for (const header of observation.finalizedHeaders) {
    admittedHeaders.add(header);
  }
  return observation;
};
