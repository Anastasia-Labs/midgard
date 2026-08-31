import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  computeFraudProofRawL1PointIdV1,
  type FraudProofRawL1TransactionV1,
  type FraudProofRawL1UtxoV1,
  LocalKupmiosExactPointNotCanonicalV1Error,
  type LocalKupmiosFraudProofRawSourceV1,
  localKupmiosHttpOgmiosRawSourceDetailsV1,
  type LocalKupmiosRawBlockAtPointV1,
  readAdmittedLocalKupmiosAddressUtxosAtPointV1,
  readAdmittedLocalKupmiosBoundaryV1,
  readAdmittedLocalKupmiosRawBlockAtPointV1,
  readAdmittedLocalKupmiosRawTransactionV1,
  readAdmittedLocalKupmiosUnitHistoryAtPointV1,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentityV1,
  assertWatcherDeploymentProtocolScriptAuthorityV1,
  type VerifiedWatcherDeploymentIdentityV1,
  watcherDeploymentProtocolScriptAuthorityV1,
} from "./deployment-identity.js";
import {
  watcherSameCanonicalJsonV1,
  watcherSha256CanonicalJsonV1,
} from "./durable-store.js";
import { watcherL1TransportAttestationDetailsV1 } from "./l1-adapter.js";
import {
  assertWatcherLocalKupmiosNativeObservationV1,
  type WatcherLocalKupmiosNativeObservationV1,
} from "./local-kupmios-native-observation-v1.js";
import type { WatcherNativeBlockAdmissionV1 } from "./native-block-admission-v1.js";

export const WATCHER_PRODUCTION_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-production-state-queue-observation-v1" as const;

const RELEASE_FINALITY_DEPTH = 30;
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

type QueueNode = SDK.StateQueueTransitionNodeV1;
type DecodedQueueHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  daAvailability: SDK.DaAvailabilityStateQueueStatusV1;
}>;
type QueueOutput = Readonly<{
  node: QueueNode;
  nextHeaderHash: string | null;
  header: DecodedQueueHeader | null;
}>;
type LockOutput = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;

export type WatcherProductionStateQueueObservationV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION;
  deploymentIdentityDigest: string;
  protocolScriptAuthorityDigest: string;
  stateQueuePolicyId: string;
  hubOraclePolicyId: string;
  nativePoint: Readonly<{
    blockHash: string;
    parentBlockHash: string | null;
    slot: string;
    blockNo: string;
    chainPointId: string;
    finalityDepth: string;
  }>;
  sourceId: string;
  previousObservationDigest: string | null;
  checkpoints: readonly SDK.StateQueueAuthenticatedReplayCheckpointV1[];
  finalizedQueue: readonly QueueNode[];
  finalizedHeaders: readonly WatcherProductionStateQueueHeaderObservationV1[];
  finalizedCorrectionLock: WatcherProductionCorrectionLockObservationV1 | null;
  correctionLockWitnesses: readonly SDK.StateQueueCorrectionLockWitnessV1[];
  observationDigest: string;
}>;

export type WatcherProductionCorrectionLockObservationV1 = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  observedChainPointId: string;
  finalityDepth: string;
}>;

export type WatcherProductionStateQueueHeaderObservationV1 = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  daAvailability: SDK.DaAvailabilityStateQueueStatusV1;
  queueOutRef: string;
  nextHeaderHash: string | null;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  observedChainPointId: string;
  finalityDepth: string;
}>;

export type WatcherProductionStateQueueObservationSourceV1 = Readonly<{
  observe(
    input: Readonly<{
      nativeBlock: WatcherNativeBlockAdmissionV1;
      localObservation: WatcherLocalKupmiosNativeObservationV1;
      previous: WatcherProductionStateQueueObservationV1 | null;
    }>,
  ): Promise<WatcherProductionStateQueueObservationV1 | null>;
  bootstrap(): Promise<WatcherProductionStateQueueRecoveryV1>;
  restore(
    input: Readonly<{
      persistedObservations: readonly unknown[];
    }>,
  ): Promise<WatcherProductionStateQueueRecoveryV1>;
  resolveRetainedHeader(
    input: Readonly<{ headerHash: string }>,
  ): Promise<WatcherProductionStateQueueHeaderObservationV1>;
}>;

export type WatcherProductionStateQueueRecoveryV1 = Readonly<{
  previous: WatcherProductionStateQueueObservationV1;
  /** Newest durable records rejected by raw-L1 prefix re-admission. */
  discardedObservationCount: number;
  replayIntersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
  }>;
  catchupBoundary: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
    finalityDepth: string;
    ogmiosTipBlockNo: string;
  }>;
}>;

const admittedObservations = new WeakSet<object>();
const admittedHeaders = new WeakSet<object>();
const admittedSources = new WeakSet<object>();

export const assertWatcherProductionStateQueueObservationV1 = (
  observation: WatcherProductionStateQueueObservationV1,
): void => {
  if (!admittedObservations.has(observation)) {
    throw new Error(
      "state-queue observation was not admitted by the production source",
    );
  }
};

export const assertWatcherProductionStateQueueHeaderObservationV1 = (
  header: WatcherProductionStateQueueHeaderObservationV1,
): void => {
  if (!admittedHeaders.has(header)) {
    throw new Error(
      "state-queue HeaderV1 observation was not admitted by the production source",
    );
  }
};

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  const expected = new Set(keys);
  return actual.length === keys.length &&
    actual.every((key) => typeof key === "string" && expected.has(key))
    ? (value as Record<string, unknown>)
    : null;
};

const parseQueueNodes = (value: unknown): readonly QueueNode[] | null => {
  if (!Array.isArray(value)) return null;
  const nodes: QueueNode[] = [];
  for (const [index, candidate] of value.entries()) {
    const node = exactRecord(candidate, ["headerHash", "outRef"]);
    if (
      node === null ||
      typeof node.outRef !== "string" ||
      !OUT_REF.test(node.outRef) ||
      (index === 0
        ? node.headerHash !== null
        : typeof node.headerHash !== "string" || !HEX_28.test(node.headerHash))
    ) {
      return null;
    }
    nodes.push(
      Object.freeze({
        headerHash: node.headerHash as string | null,
        outRef: node.outRef,
      }),
    );
  }
  return new Set(nodes.map(({ headerHash }) => headerHash)).size ===
    nodes.length &&
    new Set(nodes.map(({ outRef }) => outRef)).size === nodes.length
    ? Object.freeze(nodes)
    : null;
};

const parsePersistedHeader = (
  value: unknown,
): WatcherProductionStateQueueHeaderObservationV1 | null => {
  const record = exactRecord(value, [
    "headerHash",
    "headerCborHex",
    "stateQueueNodeCborHex",
    "linkedListDatumCborHex",
    "daAvailability",
    "queueOutRef",
    "nextHeaderHash",
    "observedTransactionHash",
    "observedBlockHash",
    "observedSlot",
    "observedBlockNo",
    "observedChainPointId",
    "finalityDepth",
  ]);
  if (
    record === null ||
    typeof record.headerHash !== "string" ||
    !HEX_28.test(record.headerHash) ||
    typeof record.headerCborHex !== "string" ||
    !EVEN_HEX.test(record.headerCborHex) ||
    typeof record.stateQueueNodeCborHex !== "string" ||
    !EVEN_HEX.test(record.stateQueueNodeCborHex) ||
    typeof record.linkedListDatumCborHex !== "string" ||
    !EVEN_HEX.test(record.linkedListDatumCborHex) ||
    typeof record.queueOutRef !== "string" ||
    !OUT_REF.test(record.queueOutRef) ||
    (record.nextHeaderHash !== null &&
      (typeof record.nextHeaderHash !== "string" ||
        !HEX_28.test(record.nextHeaderHash))) ||
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
  try {
    const encoded = Data.to(
      record.daAvailability as SDK.DaAvailabilityStateQueueStatusV1,
      SDK.DaAvailabilityStateQueueStatusV1,
    );
    const daAvailability = Data.from(
      encoded,
      SDK.DaAvailabilityStateQueueStatusV1,
    );
    if (!watcherSameCanonicalJsonV1(record.daAvailability, daAvailability)) {
      return null;
    }
    return Object.freeze({
      ...(record as Omit<
        WatcherProductionStateQueueHeaderObservationV1,
        "daAvailability"
      >),
      daAvailability,
    });
  } catch {
    return null;
  }
};

const parsePersistedLock = (
  value: unknown,
): WatcherProductionCorrectionLockObservationV1 | null => {
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
  const datum = SDK.parseStateQueueCorrectionLockDatumV1(record?.datum);
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

const parsePersistedObservation = (
  value: unknown,
): WatcherProductionStateQueueObservationV1 | null => {
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
    ? record.checkpoints.map(SDK.parseStateQueueAuthenticatedReplayCheckpointV1)
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
        SDK.parseStateQueueCorrectionLockWitnessV1,
      )
    : null;
  if (
    record === null ||
    nativePoint === null ||
    record.schemaVersion !==
      WATCHER_PRODUCTION_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION ||
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
      checkpoints as SDK.StateQueueAuthenticatedReplayCheckpointV1[],
    ),
    finalizedQueue: queue,
    finalizedHeaders: Object.freeze(
      headers as WatcherProductionStateQueueHeaderObservationV1[],
    ),
    finalizedCorrectionLock: lock,
    correctionLockWitnesses: Object.freeze(
      witnesses as SDK.StateQueueCorrectionLockWitnessV1[],
    ),
  };
  return watcherSha256CanonicalJsonV1(canonical) === record.observationDigest &&
    watcherSameCanonicalJsonV1(
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

/** Pure structural parser only; it never grants production observation authority. */
export const unsafeParsePersistedWatcherProductionStateQueueObservationForTest =
  parsePersistedObservation;

/**
 * Narrow test-only opaque admission. It first runs the exact persisted parser,
 * then independently re-derives every contained HeaderV1 hash and queue link.
 * Production code cannot call this helper and no structural clone is admitted.
 */
export const unsafeAdmitWatcherProductionStateQueueObservationForReplayTestV1 =
  (value: unknown): WatcherProductionStateQueueObservationV1 => {
    if (process.env.NODE_ENV !== "test") {
      throw new Error("unsafe state-queue replay admission is test-only");
    }
    const parsed = parsePersistedObservation(value);
    if (parsed === null) {
      throw new Error("test state-queue replay observation is not canonical");
    }
    for (const header of parsed.finalizedHeaders) {
      const decoded = Data.from(header.headerCborHex, SDK.HeaderV1);
      if (
        Data.to(decoded, SDK.HeaderV1) !== header.headerCborHex ||
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

const outputReferences = (
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

const mintPolicyIds = (body: CML.TransactionBody): readonly string[] => {
  const mint = body.mint();
  if (mint === undefined) return [];
  const keys = mint.keys();
  const policies: string[] = [];
  for (let index = 0; index < keys.len(); index += 1) {
    policies.push(keys.get(index).to_hex());
  }
  return policies.sort();
};

const outputHasPolicy = (
  output: CML.TransactionOutput,
  policyId: string,
): boolean =>
  Object.entries(coreToTxOutput(output).assets).some(
    ([unit, quantity]) => unit.startsWith(policyId) && quantity !== 0n,
  );

const outputHasUnit = (output: CML.TransactionOutput, unit: string): boolean =>
  (coreToTxOutput(output).assets[unit] ?? 0n) !== 0n;

const rawOutputHasPolicy = (outputCbor: string, policyId: string): boolean =>
  outputHasPolicy(CML.TransactionOutput.from_cbor_hex(outputCbor), policyId);

const candidateRawBlockTransactions = ({
  rawBlock,
  queue,
  currentLock,
  stateQueuePolicyId,
  hubOraclePolicyId,
}: {
  rawBlock: LocalKupmiosRawBlockAtPointV1;
  queue: readonly QueueNode[];
  currentLock: WatcherProductionCorrectionLockObservationV1 | null;
  stateQueuePolicyId: string;
  hubOraclePolicyId: string;
}): readonly Readonly<{ txHash: string; transactionIndex: number }>[] => {
  const cursorOutRefs = new Set(queue.map(({ outRef }) => outRef));
  if (currentLock !== null) cursorOutRefs.add(currentLock.outRef);
  const lockUnit = SDK.correctionLockUnit(hubOraclePolicyId);
  return Object.freeze(
    rawBlock.transactions.flatMap((transaction, transactionIndex) => {
      const body = CML.Transaction.from_cbor_hex(
        transaction.transactionCbor,
      ).body();
      const inputOutRefs = [
        ...outputReferences(body.inputs()),
        ...outputReferences(body.reference_inputs()),
      ];
      const outputs = body.outputs();
      let outputCandidate = false;
      for (let index = 0; index < outputs.len(); index += 1) {
        outputCandidate ||=
          outputHasPolicy(outputs.get(index), stateQueuePolicyId) ||
          outputHasUnit(outputs.get(index), lockUnit);
      }
      return mintPolicyIds(body).includes(stateQueuePolicyId) ||
        outputCandidate ||
        inputOutRefs.some((outRef) => cursorOutRefs.has(outRef))
        ? [Object.freeze({ txHash: transaction.txHash, transactionIndex })]
        : [];
    }),
  );
};

/** Pure candidate-selection test seam; it resolves or admits no transaction. */
export const unsafeSelectWatcherStateQueueRawCandidatesForTest =
  candidateRawBlockTransactions;

const queueOutput = ({
  output,
  outRef,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  output: CML.TransactionOutput;
  outRef: string;
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): QueueOutput | null => {
  const core = coreToTxOutput(output);
  const assets = Object.entries(core.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(stateQueuePolicyId) && quantity !== 0n,
  );
  if (assets.length === 0) return null;
  if (
    core.address !== stateQueueAddress ||
    assets.length !== 1 ||
    assets[0]![1] !== 1n ||
    output.script_ref() !== undefined
  ) {
    throw new Error(
      "state-queue policy output has invalid address/value/script topology",
    );
  }
  const assetName = assets[0]![0].slice(stateQueuePolicyId.length);
  const headerHash =
    assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
      ? null
      : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
          HEX_28.test(
            assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
          )
        ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
        : undefined;
  const datum = output.datum()?.as_datum();
  if (headerHash === undefined || datum === undefined) {
    throw new Error(
      "state-queue output has an unknown asset or non-inline datum",
    );
  }
  const linkedListDatumCborHex = datum.to_canonical_cbor_hex();
  const view = SDK.linkedListDatumToNodeView(
    Data.from(linkedListDatumCborHex, SDK.LinkedListDatum),
    assetName,
  );
  const datumHeaderHash = view.key === "Empty" ? null : view.key.Key.key;
  if (datumHeaderHash !== headerHash) {
    throw new Error("state-queue output asset and datum identities differ");
  }
  let header: DecodedQueueHeader | null = null;
  if (headerHash !== null) {
    const stateQueueNode = Data.castFrom(
      view.data,
      SDK.StateQueueNodeV1,
    ) as SDK.StateQueueNodeV1;
    const stateQueueNodeCborHex = Data.to(stateQueueNode, SDK.StateQueueNodeV1);
    const headerCborHex = Data.to(stateQueueNode.header, SDK.HeaderV1);
    const computedHeaderHash = computeHash28(
      Buffer.from(headerCborHex, "hex"),
    ).toString("hex");
    if (computedHeaderHash !== headerHash) {
      throw new Error(
        "state-queue header bytes or DA-attestation identity differ from the node asset",
      );
    }
    header = Object.freeze({
      headerHash,
      headerCborHex,
      stateQueueNodeCborHex,
      linkedListDatumCborHex,
      daAvailability: stateQueueNode.da_attestation,
    });
  }
  return Object.freeze({
    node: Object.freeze({ headerHash, outRef }),
    nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    header,
  });
};

const lockOutput = ({
  output,
  outRef,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  output: CML.TransactionOutput;
  outRef: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
}): LockOutput | null => {
  const core = coreToTxOutput(output);
  const lockUnit = SDK.correctionLockUnit(hubOraclePolicyId);
  if ((core.assets[lockUnit] ?? 0n) === 0n) return null;
  const nonAda = Object.entries(core.assets).filter(
    ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
  );
  const datum = output.datum()?.as_datum();
  if (
    core.address !== correctionLockAddress ||
    nonAda.length !== 1 ||
    nonAda[0]![0] !== lockUnit ||
    nonAda[0]![1] !== 1n ||
    output.script_ref() !== undefined ||
    datum === undefined
  ) {
    throw new Error(
      "CorrectionLock output has invalid address/value/datum topology",
    );
  }
  const parsed = SDK.parseStateQueueCorrectionLockDatumV1(
    Data.from(datum.to_canonical_cbor_hex(), SDK.CorrectionLockDatum),
  );
  if (parsed === null) throw new Error("CorrectionLock datum is non-canonical");
  return Object.freeze({ outRef, datum: parsed });
};

const admitObservation = (
  observation: WatcherProductionStateQueueObservationV1,
): WatcherProductionStateQueueObservationV1 => {
  admittedObservations.add(observation);
  for (const header of observation.finalizedHeaders) {
    admittedHeaders.add(header);
  }
  return observation;
};

const decodedQueueOutputs = ({
  body,
  transactionHash,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  body: CML.TransactionBody;
  transactionHash: string;
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): readonly QueueOutput[] => {
  const result: QueueOutput[] = [];
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const decoded = queueOutput({
      output: outputs.get(index),
      outRef: `${transactionHash}#${index.toString()}`,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    if (decoded !== null) result.push(decoded);
  }
  if (
    new Set(result.map(({ node }) => node.headerHash)).size !== result.length ||
    new Set(result.map(({ node }) => node.outRef)).size !== result.length
  ) {
    throw new Error("state-queue transaction produced duplicate identities");
  }
  return result;
};

const reconstructQueue = ({
  previousQueue,
  transactionHash,
  spentInputOutRefs,
  resolvedInputs,
  outputs,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  previousQueue: readonly QueueNode[];
  transactionHash: string;
  spentInputOutRefs: readonly string[];
  resolvedInputs: FraudProofRawL1TransactionV1["resolvedInputs"];
  outputs: readonly QueueOutput[];
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): readonly QueueNode[] => {
  const spent = new Set(spentInputOutRefs);
  const consumed = resolvedInputs.flatMap((input) => {
    const decoded = queueOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  if (
    consumed.length === 0 ||
    consumed.some(
      ({ node }) =>
        !previousQueue.some(
          (prior) =>
            prior.outRef === node.outRef &&
            prior.headerHash === node.headerHash,
        ),
    )
  ) {
    throw new Error(
      "state-queue inputs do not extend the authenticated queue cursor",
    );
  }
  const previousIdentities = new Set(
    previousQueue.map(({ headerHash }) => headerHash),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.some(
      ({ node }) =>
        previousIdentities.has(node.headerHash) &&
        !previousQueue.some(
          (prior) =>
            prior.headerHash === node.headerHash && spent.has(prior.outRef),
        ),
    )
  ) {
    throw new Error(
      "state-queue continuation was not backed by its exact input",
    );
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousIdentities.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new Error(
      "state-queue transaction introduced a non-canonical identity set",
    );
  }
  const nextQueue: readonly QueueNode[] = Object.freeze(
    [...retained, ...introduced].map((node) =>
      Object.freeze({ headerHash: node.headerHash, outRef: node.outRef }),
    ),
  );
  if (nextQueue.length > 0 && nextQueue[0]!.headerHash !== null) {
    throw new Error("state-queue transaction removed or misplaced the root");
  }
  const expectedLinks = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedLinks.get(node.headerHash) !== nextHeaderHash,
    ) ||
    (nextQueue.length > 0 &&
      nextQueue.every(
        ({ outRef }) => !outRef.startsWith(`${transactionHash}#`),
      ))
  ) {
    throw new Error("state-queue output links or continuation are invalid");
  }
  return nextQueue;
};

const decodeLockOutputs = ({
  body,
  transactionHash,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  body: CML.TransactionBody;
  transactionHash: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
}): readonly LockOutput[] => {
  const result: LockOutput[] = [];
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const decoded = lockOutput({
      output: outputs.get(index),
      outRef: `${transactionHash}#${index.toString()}`,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    if (decoded !== null) result.push(decoded);
  }
  return result;
};

const orderedResolved = (
  labels: readonly string[],
  values: FraudProofRawL1TransactionV1["resolvedInputs"],
): readonly FraudProofRawL1TransactionV1["resolvedInputs"][number][] => {
  const byRef = new Map(values.map((value) => [value.outRef, value]));
  return labels.map((label) => {
    const value = byRef.get(label);
    if (value === undefined)
      throw new Error("resolved input order is incomplete");
    return value;
  });
};

const fraudProofIdentity = ({
  proof,
  fraudProofPolicyId,
  fraudProofAddress,
  targetHeaderHash,
}: {
  proof: FraudProofRawL1TransactionV1["resolvedReferenceInputs"][number];
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  targetHeaderHash: string;
}): SDK.CorrectionIdentity => {
  const output = CML.TransactionOutput.from_cbor_hex(proof.outputCbor);
  const core = coreToTxOutput(output);
  const matching = Object.entries(core.assets).filter(([unit, quantity]) => {
    const assetName = unit.slice(fraudProofPolicyId.length);
    return (
      unit.startsWith(fraudProofPolicyId) &&
      quantity === 1n &&
      /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash
    );
  });
  if (
    core.address !== fraudProofAddress ||
    matching.length !== 1 ||
    Object.entries(core.assets).filter(
      ([unit, quantity]) =>
        unit.startsWith(fraudProofPolicyId) && quantity !== 0n,
    ).length !== 1 ||
    output.datum()?.as_datum() === undefined
  ) {
    throw new Error(
      "fraud correction reference is not the exact permanent proof",
    );
  }
  return {
    FraudProof: {
      fraud_proof_asset_name: matching[0]![0].slice(fraudProofPolicyId.length),
    },
  };
};

/** Pure address/policy identity test seam; it grants no observation authority. */
export const unsafeDeriveFraudProofCorrectionIdentityForTest =
  fraudProofIdentity;

const correctionLockWitness = ({
  raw,
  body,
  mintPolicies,
  redeemers,
  stateQueuePolicyId,
  correctionLockAddress,
  hubOraclePolicyId,
  fraudProofPolicyId,
  fraudProofAddress,
}: {
  raw: FraudProofRawL1TransactionV1;
  body: CML.TransactionBody;
  mintPolicies: readonly string[];
  redeemers: readonly SDK.StateQueueTransitionRedeemerV1[];
  stateQueuePolicyId: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
}): SDK.StateQueueCorrectionLockWitnessV1 => {
  const spentRefs = outputReferences(body.inputs());
  const referenceRefs = outputReferences(body.reference_inputs());
  const spentResolved = orderedResolved(spentRefs, raw.resolvedInputs);
  const referenceResolved = orderedResolved(
    referenceRefs,
    raw.resolvedReferenceInputs,
  );
  const locksIn = spentResolved.flatMap((input) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const locksReferenced = referenceResolved.flatMap((input) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const locksOut = decodeLockOutputs({
    body,
    transactionHash: raw.txHash,
    correctionLockAddress,
    hubOraclePolicyId,
  });
  const policyIndex = mintPolicies.indexOf(stateQueuePolicyId);
  if (policyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 0
    ) {
      throw new Error("non-mint queue update touched CorrectionLock");
    }
    return { kind: "none" };
  }
  const mintRedeemer = redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (mintRedeemer.length !== 1)
    throw new Error("state-queue mint redeemer is not unique");
  const decoded = Data.from(
    mintRedeemer[0]!.cborHex,
    SDK.StateQueueRedeemer,
  ) as SDK.StateQueueRedeemer;
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 1 ||
      locksOut[0]!.datum !== "Idle"
    ) {
      throw new Error("state-queue Init has invalid CorrectionLock topology");
    }
    return {
      kind: "genesis",
      producedOutRef: locksOut[0]!.outRef,
      nextDatum: "Idle",
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 0
    ) {
      throw new Error("state-queue Deinit has invalid CorrectionLock topology");
    }
    return {
      kind: "deinit",
      consumedOutRef: locksIn[0]!.outRef,
      previousDatum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("CommitBlockHeader" in decoded || "MergeToConfirmedStateV1" in decoded)
  ) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 1 ||
      locksReferenced[0]!.datum !== "Idle" ||
      locksOut.length !== 0
    ) {
      throw new Error("append/merge has invalid CorrectionLock topology");
    }
    return {
      kind: "idle_reference",
      referenceOutRef: locksReferenced[0]!.outRef,
      datum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 1
    ) {
      throw new Error("correction has invalid CorrectionLock topology");
    }
    const timeout = "RemoveUnattestedBlockAfterTimeout" in decoded;
    const targetHeaderHash = timeout
      ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
      : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const identity: SDK.CorrectionIdentity = timeout
      ? "AttestationTimeout"
      : fraudProofIdentity({
          proof:
            referenceResolved[
              Number(
                decoded.RemoveFraudulentBlockHeader.fraud_proof_ref_input_index,
              )
            ] ??
            (() => {
              throw new Error("fraud proof reference index is out of bounds");
            })(),
          fraudProofPolicyId,
          fraudProofAddress,
          targetHeaderHash,
        });
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: locksOut[0]!.outRef,
      targetHeaderHash,
      correctionIdentity: identity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: locksOut[0]!.datum,
    };
  }
  throw new Error(
    "state-queue mint redeemer has no admitted CorrectionLock topology",
  );
};

const sameQueue = (
  left: readonly QueueNode[],
  right: readonly QueueNode[],
): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );

const sameLockDatum = (
  left: SDK.CorrectionLockDatum,
  right: SDK.CorrectionLockDatum,
): boolean =>
  Data.to(left, SDK.CorrectionLockDatum) ===
  Data.to(right, SDK.CorrectionLockDatum);

const advanceCurrentLock = ({
  current,
  witness,
  transactionHash,
  point,
  chainPointId,
  finalityDepth,
}: {
  current: WatcherProductionCorrectionLockObservationV1 | null;
  witness: SDK.StateQueueCorrectionLockWitnessV1;
  transactionHash: string;
  point: Readonly<{ blockHash: string; blockNo: string; slot: string }>;
  chainPointId: string;
  finalityDepth: string;
}): WatcherProductionCorrectionLockObservationV1 | null => {
  if (witness.kind === "none") return current;
  if (witness.kind === "genesis") {
    if (current !== null)
      throw new Error("CorrectionLock genesis duplicated the singleton");
    return Object.freeze({
      outRef: witness.producedOutRef,
      datum: witness.nextDatum,
      observedTransactionHash: transactionHash,
      observedBlockHash: point.blockHash,
      observedSlot: point.slot,
      observedBlockNo: point.blockNo,
      observedChainPointId: chainPointId,
      finalityDepth,
    });
  }
  if (witness.kind === "idle_reference") {
    if (
      current === null ||
      current.outRef !== witness.referenceOutRef ||
      !sameLockDatum(current.datum, witness.datum)
    ) {
      throw new Error(
        "CorrectionLock reference differs from the authenticated cursor",
      );
    }
    return current;
  }
  if (
    current === null ||
    current.outRef !== witness.consumedOutRef ||
    !sameLockDatum(current.datum, witness.previousDatum)
  ) {
    throw new Error(
      "CorrectionLock spend differs from the authenticated cursor",
    );
  }
  if (witness.kind === "deinit") return null;
  return Object.freeze({
    outRef: witness.continuedOutRef,
    datum: witness.nextDatum,
    observedTransactionHash: transactionHash,
    observedBlockHash: point.blockHash,
    observedSlot: point.slot,
    observedBlockNo: point.blockNo,
    observedChainPointId: chainPointId,
    finalityDepth,
  });
};

const deriveObservation = ({
  nativeBlock,
  localObservation,
  authority,
  sourceId,
  previous,
  rawTransactions,
}: {
  nativeBlock: WatcherNativeBlockAdmissionV1;
  localObservation: WatcherLocalKupmiosNativeObservationV1;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthorityV1>;
  sourceId: string;
  previous: WatcherProductionStateQueueObservationV1 | null;
  rawTransactions: readonly FraudProofRawL1TransactionV1[];
}): WatcherProductionStateQueueObservationV1 => {
  if (
    localObservation.block.chainPoint.blockHash !== nativeBlock.blockHash ||
    localObservation.block.chainPoint.slot !== nativeBlock.slot ||
    localObservation.block.chainPoint.blockNo !== nativeBlock.blockNo ||
    BigInt(localObservation.block.chainPoint.depth) <
      BigInt(RELEASE_FINALITY_DEPTH) ||
    rawTransactions.length > nativeBlock.transactionIds.length
  ) {
    throw new Error(
      "state-queue source chain point/finality differs from native admission",
    );
  }
  const authenticatedChainPointId = computeFraudProofRawL1PointIdV1({
    blockHash: nativeBlock.blockHash,
    blockNo: nativeBlock.blockNo,
    slot: nativeBlock.slot,
  });
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const hubOraclePolicyId = authority.protocolScriptHashes.hubOracleMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const fraudProofPolicyId = authority.protocolScriptHashes.fraudProofMint;
  const fraudProofAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.fraudProofSpend),
  );
  let queue = previous?.finalizedQueue ?? Object.freeze([] as QueueNode[]);
  let finalizedHeaders = previous?.finalizedHeaders ?? Object.freeze([]);
  let finalizedCorrectionLock = previous?.finalizedCorrectionLock ?? null;
  const checkpoints: SDK.StateQueueAuthenticatedReplayCheckpointV1[] = [];
  const seenTransactionIndexes = new Set<number>();
  for (const raw of rawTransactions) {
    const transactionIndex = nativeBlock.transactionIds.indexOf(raw.txHash);
    if (
      transactionIndex < 0 ||
      seenTransactionIndexes.has(transactionIndex) ||
      raw.inclusionPoint.blockHash !== nativeBlock.blockHash ||
      raw.inclusionPoint.slot !== nativeBlock.slot ||
      raw.inclusionPoint.blockNo !== nativeBlock.blockNo ||
      raw.confirmationDepth < RELEASE_FINALITY_DEPTH
    ) {
      throw new Error(
        "resolved transaction was substituted across the native chain point",
      );
    }
    seenTransactionIndexes.add(transactionIndex);
    const normalized = localObservation.block.transactions[transactionIndex];
    if (
      normalized === undefined ||
      normalized.txHash !== raw.txHash ||
      normalized.body.bytesHex !== raw.bodyCbor ||
      normalized.witnessSet.bytesHex !== raw.witnessSetCbor
    ) {
      throw new Error(
        "resolved transaction bytes differ from the admitted watcher block",
      );
    }
    const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
    const policies = mintPolicyIds(body);
    const outputs = body.outputs();
    let outputTouchesQueue = false;
    for (let index = 0; index < outputs.len(); index += 1) {
      outputTouchesQueue ||= outputHasPolicy(
        outputs.get(index),
        stateQueuePolicyId,
      );
    }
    const touchesQueue =
      policies.includes(stateQueuePolicyId) ||
      outputTouchesQueue ||
      raw.resolvedInputs.some(({ outputCbor }) =>
        rawOutputHasPolicy(outputCbor, stateQueuePolicyId),
      );
    if (!touchesQueue) {
      const consumesLock = raw.resolvedInputs.some(
        ({ outRef, outputCbor }) =>
          lockOutput({
            output: CML.TransactionOutput.from_cbor_hex(outputCbor),
            outRef,
            correctionLockAddress,
            hubOraclePolicyId,
          }) !== null,
      );
      const producesLock =
        decodeLockOutputs({
          body,
          transactionHash: raw.txHash,
          correctionLockAddress,
          hubOraclePolicyId,
        }).length > 0;
      if (consumesLock || producesLock) {
        throw new Error(
          "CorrectionLock changed without an authenticated state-queue transition",
        );
      }
      continue;
    }
    const spentInputOutRefs = outputReferences(body.inputs());
    const referenceInputOutRefs = outputReferences(body.reference_inputs());
    const queueOutputs = decodedQueueOutputs({
      body,
      transactionHash: raw.txHash,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    const nextQueue =
      queue.length === 0 && queueOutputs.length === 1
        ? Object.freeze([queueOutputs[0]!.node])
        : reconstructQueue({
            previousQueue: queue,
            transactionHash: raw.txHash,
            spentInputOutRefs,
            resolvedInputs: raw.resolvedInputs,
            outputs: queueOutputs,
            stateQueueAddress,
            stateQueuePolicyId,
          });
    const redeemers = normalized.redeemers.map((redeemer) => ({
      purpose: redeemer.purpose,
      index: redeemer.index,
      cborHex: redeemer.bytes.bytesHex,
    }));
    const lockWitness = correctionLockWitness({
      raw,
      body,
      mintPolicies: policies,
      redeemers,
      stateQueuePolicyId,
      correctionLockAddress,
      hubOraclePolicyId,
      fraudProofPolicyId,
      fraudProofAddress,
    });
    const checkpoint = SDK.deriveStateQueueAuthenticatedReplayCheckpointV1({
      deploymentIdentityDigest: authority.deploymentFingerprint,
      stateQueuePolicyId,
      transactionHash: raw.txHash,
      blockHash: nativeBlock.blockHash,
      slot: nativeBlock.slot,
      blockNo: nativeBlock.blockNo,
      transactionIndex: transactionIndex.toString(),
      chainPointId: authenticatedChainPointId,
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
      mintPolicyIds: policies,
      redeemers,
      spentInputOutRefs,
      referenceInputOutRefs,
      correctionLockWitness: lockWitness,
      previousQueue: queue,
      nextQueue,
    });
    if (checkpoint === null) {
      throw new Error(
        "state-queue transaction failed authenticated checkpoint derivation",
      );
    }
    checkpoints.push(checkpoint);
    queue = nextQueue;
    finalizedCorrectionLock = advanceCurrentLock({
      current: finalizedCorrectionLock,
      witness: lockWitness,
      transactionHash: raw.txHash,
      point: nativeBlock,
      chainPointId: authenticatedChainPointId,
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    });
    const byHeaderHash = new Map(
      finalizedHeaders.map((header) => [header.headerHash, header]),
    );
    for (const output of queueOutputs) {
      if (output.header === null) continue;
      byHeaderHash.set(
        output.header.headerHash,
        Object.freeze({
          ...output.header,
          queueOutRef: output.node.outRef,
          nextHeaderHash: output.nextHeaderHash,
          observedTransactionHash: raw.txHash,
          observedBlockHash: nativeBlock.blockHash,
          observedSlot: nativeBlock.slot,
          observedBlockNo: nativeBlock.blockNo,
          observedChainPointId: authenticatedChainPointId,
          finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
        }),
      );
    }
    finalizedHeaders = Object.freeze(
      nextQueue.flatMap(({ headerHash }) => {
        if (headerHash === null) return [];
        const header = byHeaderHash.get(headerHash);
        if (header === undefined) {
          throw new Error(
            "state-queue cursor omitted authenticated HeaderV1 bytes",
          );
        }
        return [header];
      }),
    );
  }
  if (
    previous !== null &&
    checkpoints.length > 0 &&
    !sameQueue(checkpoints[0]!.previousQueue, previous.finalizedQueue)
  ) {
    throw new Error(
      "state-queue checkpoint does not extend its admitted predecessor",
    );
  }
  const nativePoint = Object.freeze({
    blockHash: nativeBlock.blockHash,
    parentBlockHash:
      nativeBlock.prevHash.length === 0 ? null : nativeBlock.prevHash,
    slot: nativeBlock.slot,
    blockNo: nativeBlock.blockNo,
    chainPointId: authenticatedChainPointId,
    finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
  });
  const canonical = {
    schemaVersion: WATCHER_PRODUCTION_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: authority.deploymentFingerprint,
    protocolScriptAuthorityDigest: authority.authorityDigest,
    stateQueuePolicyId,
    hubOraclePolicyId,
    nativePoint,
    sourceId,
    previousObservationDigest: previous?.observationDigest ?? null,
    checkpoints: Object.freeze(checkpoints),
    finalizedQueue: Object.freeze([...queue]),
    finalizedHeaders,
    finalizedCorrectionLock,
    correctionLockWitnesses: Object.freeze(
      checkpoints.map(({ correctionLockWitness: witness }) => witness),
    ),
  };
  return Object.freeze({
    ...canonical,
    observationDigest: watcherSha256CanonicalJsonV1(canonical),
  });
};

const normalizedEndpoint = (value: string): string => {
  const parsed = new URL(value);
  if (parsed.protocol === "ws:") parsed.protocol = "http:";
  if (parsed.protocol === "wss:") parsed.protocol = "https:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

export const createWatcherProductionStateQueueObservationSourceV1 = ({
  deploymentIdentity,
  rawSource,
}: {
  deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  rawSource: LocalKupmiosFraudProofRawSourceV1;
}): WatcherProductionStateQueueObservationSourceV1 => {
  assertVerifiedWatcherDeploymentIdentityV1(deploymentIdentity);
  const authority =
    watcherDeploymentProtocolScriptAuthorityV1(deploymentIdentity);
  assertWatcherDeploymentProtocolScriptAuthorityV1(authority);
  const sourceDetails = localKupmiosHttpOgmiosRawSourceDetailsV1(rawSource);
  if (
    sourceDetails === null ||
    sourceDetails.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
    sourceDetails.releaseIdentityDigest !==
      deploymentIdentity.releaseEvidenceDigest
  ) {
    throw new Error(
      "raw state-queue source is not bound to the verified deployment",
    );
  }
  const readers: PersistedRestoreReaders = {
    readBlock: (point) =>
      readAdmittedLocalKupmiosRawBlockAtPointV1({
        source: rawSource,
        point,
      }),
    readTransaction: (txHash, point) =>
      readAdmittedLocalKupmiosRawTransactionV1({
        source: rawSource,
        txHash,
        expectedInclusionPoint: point,
        minimumConfirmationDepth: RELEASE_FINALITY_DEPTH,
      }),
    readAddress: (address, point) =>
      readAdmittedLocalKupmiosAddressUtxosAtPointV1({
        source: rawSource,
        address,
        point,
      }),
    readUnitHistory: (unit, point) =>
      readAdmittedLocalKupmiosUnitHistoryAtPointV1({
        source: rawSource,
        unit,
        point,
      }),
  };
  const source = Object.freeze({
    observe: async ({ nativeBlock, localObservation, previous }) => {
      if (!admittedSources.has(source)) {
        throw new Error("state-queue observation source is not admitted");
      }
      assertWatcherLocalKupmiosNativeObservationV1(
        localObservation,
        nativeBlock,
      );
      if (previous !== null) {
        assertWatcherProductionStateQueueObservationV1(previous);
        if (
          previous.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
          previous.protocolScriptAuthorityDigest !==
            authority.authorityDigest ||
          BigInt(previous.nativePoint.blockNo) >= BigInt(nativeBlock.blockNo)
        ) {
          throw new Error(
            "state-queue observation predecessor is foreign or non-monotone",
          );
        }
      }
      const transportDetails = localObservation.transportAttestations
        .map(watcherL1TransportAttestationDetailsV1)
        .filter((value) => value !== null);
      const kupo = transportDetails.find(
        ({ provider }) =>
          provider.source.sourceMode === "local_node" &&
          provider.source.surface === "kupo",
      );
      const ogmios = transportDetails.find(
        ({ provider }) =>
          provider.source.sourceMode === "local_node" &&
          provider.source.surface === "ogmios",
      );
      if (
        kupo === undefined ||
        ogmios === undefined ||
        normalizedEndpoint(kupo.transportEndpoint) !==
          sourceDetails.kupoHttpUrl ||
        normalizedEndpoint(ogmios.transportEndpoint) !== sourceDetails.ogmiosUrl
      ) {
        throw new Error(
          "resolved state-queue source differs from admitted watcher transports",
        );
      }
      const point = Object.freeze({
        blockHash: nativeBlock.blockHash,
        blockNo: nativeBlock.blockNo,
        slot: nativeBlock.slot,
        pointId: computeFraudProofRawL1PointIdV1({
          blockHash: nativeBlock.blockHash,
          blockNo: nativeBlock.blockNo,
          slot: nativeBlock.slot,
        }),
      });
      const rawBlock = await readAdmittedLocalKupmiosRawBlockAtPointV1({
        source: rawSource,
        point,
      });
      if (
        rawBlock.parentBlockHash !==
          (nativeBlock.prevHash.length === 0 ? null : nativeBlock.prevHash) ||
        rawBlock.transactions.length !== nativeBlock.transactionIds.length ||
        rawBlock.transactions.some(
          (transaction, index) =>
            transaction.txHash !== nativeBlock.transactionIds[index] ||
            transaction.transactionCbor !== nativeBlock.transactionCbors[index],
        )
      ) {
        throw new Error("raw state-queue block differs from native admission");
      }
      const candidates = candidateRawBlockTransactions({
        rawBlock,
        queue: previous?.finalizedQueue ?? [],
        currentLock: previous?.finalizedCorrectionLock ?? null,
        stateQueuePolicyId: authority.protocolScriptHashes.stateQueueMint,
        hubOraclePolicyId: authority.protocolScriptHashes.hubOracleMint,
      });
      const rawTransactions = await Promise.all(
        candidates.map(({ txHash }) =>
          readAdmittedLocalKupmiosRawTransactionV1({
            source: rawSource,
            txHash,
            expectedInclusionPoint: point,
            minimumConfirmationDepth: RELEASE_FINALITY_DEPTH,
          }),
        ),
      );
      const result = deriveObservation({
        nativeBlock,
        localObservation,
        authority,
        sourceId: sourceDetails.sourceId,
        previous,
        rawTransactions,
      });
      if (result.checkpoints.length === 0) return previous;
      return admitObservation(result);
    },
    bootstrap: async () => {
      if (!admittedSources.has(source)) {
        throw new Error("state-queue observation source is not admitted");
      }
      const admittedBoundary = await readAdmittedLocalKupmiosBoundaryV1({
        source: rawSource,
      });
      const intersection = Object.freeze({
        blockHash: admittedBoundary.kupoCheckpoint.blockHash,
        blockNo: admittedBoundary.kupoCheckpoint.blockNo,
        slot: admittedBoundary.kupoCheckpoint.slot,
      });
      const previous = admitObservation(
        await snapshotObservationAtBoundary({
          intersection,
          authority,
          sourceId: sourceDetails.sourceId,
          readers,
        }),
      );
      return Object.freeze({
        previous,
        discardedObservationCount: 0,
        replayIntersection: Object.freeze({
          blockHash: intersection.blockHash,
          blockNo: intersection.blockNo,
          slot: intersection.slot,
          chainPointId: admittedBoundary.kupoCheckpoint.pointId,
        }),
        catchupBoundary: Object.freeze({
          ...intersection,
          chainPointId: admittedBoundary.kupoCheckpoint.pointId,
          finalityDepth: admittedBoundary.confirmationDepth.toString(),
          ogmiosTipBlockNo: admittedBoundary.ogmiosTip.blockNo,
        }),
      });
    },
    restore: async ({ persistedObservations }) => {
      if (!admittedSources.has(source)) {
        throw new Error("state-queue observation source is not admitted");
      }
      const boundary = await readAdmittedLocalKupmiosBoundaryV1({
        source: rawSource,
      });
      const intersection = Object.freeze({
        blockHash: boundary.kupoCheckpoint.blockHash,
        blockNo: boundary.kupoCheckpoint.blockNo,
        slot: boundary.kupoCheckpoint.slot,
      });
      const restored = await restoreLongestPersistedObservationChain({
        persistedObservations,
        intersection,
        ogmiosTipBlockNo: boundary.ogmiosTip.blockNo,
        authority,
        sourceId: sourceDetails.sourceId,
        maximumObservations: sourceDetails.automaticRecoveryMaxDepth,
        readers,
      });
      return Object.freeze({
        previous: admitObservation(restored.previous),
        discardedObservationCount: restored.discardedObservationCount,
        replayIntersection: restored.replayIntersection,
        catchupBoundary: restored.catchupBoundary,
      });
    },
    resolveRetainedHeader: async ({ headerHash }) => {
      if (!admittedSources.has(source)) {
        throw new Error("state-queue observation source is not admitted");
      }
      const header = await resolveRetainedHeaderAtBoundary({
        headerHash,
        authority,
        readers: {
          readBoundary: () =>
            readAdmittedLocalKupmiosBoundaryV1({ source: rawSource }),
          readHistory: (unit, point) =>
            readAdmittedLocalKupmiosUnitHistoryAtPointV1({
              source: rawSource,
              unit,
              point,
            }),
          readTransaction: (txHash, point) =>
            readAdmittedLocalKupmiosRawTransactionV1({
              source: rawSource,
              txHash,
              expectedInclusionPoint: point,
              minimumConfirmationDepth: RELEASE_FINALITY_DEPTH,
            }),
        },
      });
      admittedHeaders.add(header);
      return header;
    },
  } satisfies WatcherProductionStateQueueObservationSourceV1);
  admittedSources.add(source);
  return source;
};

/** Pure semantic test seam. It never admits the returned structural value. */
export const unsafeDeriveWatcherProductionStateQueueObservationForTest =
  deriveObservation;

const rawRedeemers = (
  witnessSetCbor: string,
): readonly SDK.StateQueueTransitionRedeemerV1[] => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const flat = witnessSet.redeemers()?.to_flat_format();
  if (flat === undefined) return Object.freeze([]);
  const result: SDK.StateQueueTransitionRedeemerV1[] = [];
  for (let index = 0; index < flat.len(); index += 1) {
    const redeemer = flat.get(index);
    const purpose = (() => {
      switch (redeemer.tag()) {
        case CML.RedeemerTag.Spend:
          return "spend";
        case CML.RedeemerTag.Mint:
          return "mint";
        case CML.RedeemerTag.Cert:
          return "certificate";
        case CML.RedeemerTag.Reward:
          return "withdrawal";
        case CML.RedeemerTag.Voting:
          return "vote";
        case CML.RedeemerTag.Proposing:
          return "propose";
        default:
          throw new Error(
            "persisted queue transaction has unknown redeemer tag",
          );
      }
    })();
    result.push(
      Object.freeze({
        purpose,
        index: redeemer.index().toString(),
        cborHex: redeemer.data().to_canonical_cbor_hex(),
      }),
    );
  }
  return Object.freeze(result);
};

type PersistedRestoreReaders = Readonly<{
  readBlock(
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<LocalKupmiosRawBlockAtPointV1>;
  readTransaction(
    txHash: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<FraudProofRawL1TransactionV1>;
  readAddress(
    address: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<readonly FraudProofRawL1UtxoV1[]>;
  readUnitHistory?(
    unit: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): ReturnType<typeof readAdmittedLocalKupmiosUnitHistoryAtPointV1>;
}>;

const snapshotObservationAtBoundary = async ({
  intersection,
  authority,
  sourceId,
  readers,
}: {
  intersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthorityV1>;
  sourceId: string;
  readers: PersistedRestoreReaders;
}): Promise<WatcherProductionStateQueueObservationV1> => {
  const intersectionPoint = Object.freeze({
    ...intersection,
    pointId: computeFraudProofRawL1PointIdV1(intersection),
  });
  const rawBlock = await readers.readBlock(intersectionPoint);
  if (
    rawBlock.sourceId !== sourceId ||
    rawBlock.point.pointId !== intersectionPoint.pointId
  ) {
    throw new Error("state-queue bootstrap boundary is not canonical");
  }
  if (readers.readUnitHistory === undefined) {
    throw new Error("state-queue bootstrap requires exact unit history");
  }
  const provenanceForOutRef = async ({
    unit,
    outRef,
    expectedOutput,
  }: {
    unit: string;
    outRef: string;
    expectedOutput: CML.TransactionOutput;
  }): Promise<FraudProofRawL1TransactionV1> => {
    const [txHash, outputIndexText] = outRef.split("#") as [string, string];
    const history = await readers.readUnitHistory!(unit, intersectionPoint);
    const creation = history.transactions.find(
      (entry) => entry.txHash === txHash,
    );
    if (creation === undefined) {
      throw new Error(
        "state-queue bootstrap output is absent from unit history",
      );
    }
    const transaction = await readers.readTransaction(
      txHash,
      creation.inclusionPoint,
    );
    const outputIndex = Number(outputIndexText);
    const outputs = CML.TransactionBody.from_cbor_hex(
      transaction.bodyCbor,
    ).outputs();
    if (
      !Number.isSafeInteger(outputIndex) ||
      outputIndex < 0 ||
      outputIndex >= outputs.len() ||
      outputs.get(outputIndex).to_canonical_cbor_hex() !==
        expectedOutput.to_canonical_cbor_hex()
    ) {
      throw new Error(
        "state-queue bootstrap output differs from its creation transaction",
      );
    }
    return transaction;
  };
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const hubOraclePolicyId = authority.protocolScriptHashes.hubOracleMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const [queueUtxos, lockUtxos] = await Promise.all([
    readers.readAddress(stateQueueAddress, intersectionPoint),
    readers.readAddress(correctionLockAddress, intersectionPoint),
  ]);
  const queueOutputs = queueUtxos.flatMap((utxo) => {
    const decoded = queueOutput({
      output: CML.TransactionOutput.from_cbor_hex(utxo.outputCbor),
      outRef: utxo.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const byHeaderHash = new Map(
    queueOutputs.map((output) => [output.node.headerHash, output]),
  );
  const orderedQueue: QueueOutput[] = [];
  let identity: string | null = null;
  while (true) {
    const output = byHeaderHash.get(identity);
    if (output === undefined) break;
    orderedQueue.push(output);
    byHeaderHash.delete(identity);
    if (output.nextHeaderHash === null) break;
    identity = output.nextHeaderHash;
  }
  if (orderedQueue.length === 0 || byHeaderHash.size !== 0) {
    throw new Error(
      "state-queue bootstrap snapshot is not one exact linked queue",
    );
  }
  const liveLock = lockUtxos.flatMap((utxo) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(utxo.outputCbor),
      outRef: utxo.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  if (liveLock.length !== 1) {
    throw new Error(
      "state-queue bootstrap snapshot has no exact CorrectionLock",
    );
  }
  const finalizedHeaders = Object.freeze(
    (
      await Promise.all(
        orderedQueue.map(async (output) => {
          if (output.header === null) return null;
          const creation = await provenanceForOutRef({
            unit: `${stateQueuePolicyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${output.header.headerHash}`,
            outRef: output.node.outRef,
            expectedOutput: CML.TransactionOutput.from_cbor_hex(
              queueUtxos.find(({ outRef }) => outRef === output.node.outRef)!
                .outputCbor,
            ),
          });
          return Object.freeze({
            ...output.header,
            queueOutRef: output.node.outRef,
            nextHeaderHash: output.nextHeaderHash,
            observedTransactionHash: creation.txHash,
            observedBlockHash: creation.inclusionPoint.blockHash,
            observedSlot: creation.inclusionPoint.slot,
            observedBlockNo: creation.inclusionPoint.blockNo,
            observedChainPointId: creation.inclusionPoint.pointId,
            finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
          });
        }),
      )
    ).filter(
      (header): header is WatcherProductionStateQueueHeaderObservationV1 =>
        header !== null,
    ),
  );
  const liveLockOutput = CML.TransactionOutput.from_cbor_hex(
    lockUtxos.find(({ outRef }) => outRef === liveLock[0]!.outRef)!.outputCbor,
  );
  const lockCreation = await provenanceForOutRef({
    unit: SDK.correctionLockUnit(hubOraclePolicyId),
    outRef: liveLock[0]!.outRef,
    expectedOutput: liveLockOutput,
  });
  const nativePoint = Object.freeze({
    blockHash: intersection.blockHash,
    parentBlockHash: rawBlock.parentBlockHash,
    slot: intersection.slot,
    blockNo: intersection.blockNo,
    chainPointId: intersectionPoint.pointId,
    finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
  });
  const canonical = {
    schemaVersion: WATCHER_PRODUCTION_STATE_QUEUE_OBSERVATION_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: authority.deploymentFingerprint,
    protocolScriptAuthorityDigest: authority.authorityDigest,
    stateQueuePolicyId,
    hubOraclePolicyId,
    nativePoint,
    sourceId,
    previousObservationDigest: null,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze(orderedQueue.map(({ node }) => node)),
    finalizedHeaders,
    finalizedCorrectionLock: Object.freeze({
      outRef: liveLock[0]!.outRef,
      datum: liveLock[0]!.datum,
      observedTransactionHash: lockCreation.txHash,
      observedBlockHash: lockCreation.inclusionPoint.blockHash,
      observedSlot: lockCreation.inclusionPoint.slot,
      observedBlockNo: lockCreation.inclusionPoint.blockNo,
      observedChainPointId: lockCreation.inclusionPoint.pointId,
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    }),
    correctionLockWitnesses: Object.freeze([]),
  };
  return Object.freeze({
    ...canonical,
    observationDigest: watcherSha256CanonicalJsonV1(canonical),
  });
};

const resolveRetainedHeaderAtBoundary = async ({
  headerHash,
  authority,
  readers,
}: {
  headerHash: string;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthorityV1>;
  readers: Readonly<{
    readBoundary(): ReturnType<typeof readAdmittedLocalKupmiosBoundaryV1>;
    readHistory(
      unit: string,
      point: Parameters<
        typeof readAdmittedLocalKupmiosUnitHistoryAtPointV1
      >[0]["point"],
    ): ReturnType<typeof readAdmittedLocalKupmiosUnitHistoryAtPointV1>;
    readTransaction(
      txHash: string,
      point: Parameters<
        typeof readAdmittedLocalKupmiosRawTransactionV1
      >[0]["expectedInclusionPoint"],
    ): Promise<FraudProofRawL1TransactionV1>;
  }>;
}): Promise<WatcherProductionStateQueueHeaderObservationV1> => {
  if (!HEX_28.test(headerHash)) {
    throw new Error("retained HeaderV1 lookup requires a 28-byte header hash");
  }
  const boundary = await readers.readBoundary();
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const unit = `${stateQueuePolicyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;
  const history = await readers.readHistory(unit, boundary.kupoCheckpoint);
  const candidates: WatcherProductionStateQueueHeaderObservationV1[] = [];
  for (const entry of history.transactions) {
    const raw = await readers.readTransaction(
      entry.txHash,
      entry.inclusionPoint,
    );
    const outputs = CML.TransactionBody.from_cbor_hex(raw.bodyCbor).outputs();
    for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
      const output = outputs.get(outputIndex);
      if (!outputHasUnit(output, unit)) continue;
      const decoded = queueOutput({
        output,
        outRef: `${raw.txHash}#${outputIndex.toString()}`,
        stateQueueAddress,
        stateQueuePolicyId,
      });
      if (
        decoded?.header === null ||
        decoded?.node.headerHash !== headerHash ||
        decoded.header.headerHash !== headerHash
      ) {
        throw new Error(
          "retained HeaderV1 unit history contains a substituted queue output",
        );
      }
      candidates.push(
        Object.freeze({
          ...decoded.header,
          queueOutRef: decoded.node.outRef,
          nextHeaderHash: decoded.nextHeaderHash,
          observedTransactionHash: raw.txHash,
          observedBlockHash: raw.inclusionPoint.blockHash,
          observedSlot: raw.inclusionPoint.slot,
          observedBlockNo: raw.inclusionPoint.blockNo,
          observedChainPointId: raw.inclusionPoint.pointId,
          finalityDepth: raw.confirmationDepth.toString(),
        }),
      );
    }
  }
  if (candidates.length === 0) {
    throw new Error(
      "retained HeaderV1 has no authenticated state-queue output",
    );
  }
  const headerBytes = new Set(
    candidates.map(({ headerCborHex }) => headerCborHex),
  );
  if (headerBytes.size !== 1) {
    throw new Error(
      "retained HeaderV1 unit history changes immutable header bytes",
    );
  }
  const retained = candidates
    .filter(({ daAvailability }) => daAvailability !== "Unattested")
    .sort((left, right) => {
      const blockOrder =
        BigInt(left.observedBlockNo) - BigInt(right.observedBlockNo);
      if (blockOrder !== 0n) return blockOrder < 0n ? -1 : 1;
      const slotOrder = BigInt(left.observedSlot) - BigInt(right.observedSlot);
      if (slotOrder !== 0n) return slotOrder < 0n ? -1 : 1;
      return left.queueOutRef.localeCompare(right.queueOutRef);
    })
    .at(-1);
  if (retained === undefined) {
    throw new Error(
      "retained HeaderV1 lookup requires an authenticated public DA attachment",
    );
  }
  return retained;
};

const pointAtOrBefore = (
  left: Readonly<{ blockNo: string; slot: string }>,
  right: Readonly<{ blockNo: string; slot: string }>,
): boolean =>
  BigInt(left.blockNo) < BigInt(right.blockNo) ||
  (left.blockNo === right.blockNo && BigInt(left.slot) <= BigInt(right.slot));

const authenticatePersistedBootstrapTopology = async ({
  persisted,
  throughPoint,
  authority,
  readers,
}: {
  persisted: WatcherProductionStateQueueObservationV1;
  throughPoint: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    pointId: string;
  }>;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthorityV1>;
  readers: PersistedRestoreReaders;
}): Promise<void> => {
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const transactionCache = new Map<
    string,
    Promise<FraudProofRawL1TransactionV1>
  >();
  if (readers.readUnitHistory === undefined) {
    throw new Error(
      "persisted bootstrap re-admission requires exact unit history",
    );
  }
  const readUnitHistory = readers.readUnitHistory;
  const readHistoryTransaction = (
    txHash: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<FraudProofRawL1TransactionV1> => {
    const cached = transactionCache.get(txHash);
    if (cached !== undefined) return cached;
    const read = readers.readTransaction(txHash, point);
    transactionCache.set(txHash, read);
    return read;
  };
  const authenticateOutRef = async ({
    unit,
    outRef,
  }: {
    unit: string;
    outRef: string;
  }): Promise<
    Readonly<{
      output: CML.TransactionOutput;
      creation: FraudProofRawL1TransactionV1;
    }>
  > => {
    if (!OUT_REF.test(outRef)) {
      throw new Error("persisted bootstrap contains a non-canonical outref");
    }
    const [creationHash, outputIndexText] = outRef.split("#") as [
      string,
      string,
    ];
    const history = await readUnitHistory(unit, throughPoint);
    const creation = history.transactions.find(
      ({ txHash }) => txHash === creationHash,
    );
    if (creation === undefined) {
      throw new Error("persisted bootstrap outref is absent from unit history");
    }
    const creationTransaction = await readHistoryTransaction(
      creation.txHash,
      creation.inclusionPoint,
    );
    const outputIndex = Number(outputIndexText);
    const outputs = CML.TransactionBody.from_cbor_hex(
      creationTransaction.bodyCbor,
    ).outputs();
    if (!Number.isSafeInteger(outputIndex) || outputIndex >= outputs.len()) {
      throw new Error("persisted bootstrap outref output is absent");
    }
    for (const entry of history.transactions) {
      if (
        entry.txHash === creationHash ||
        !pointAtOrBefore(entry.inclusionPoint, persisted.nativePoint)
      ) {
        continue;
      }
      const transaction = await readHistoryTransaction(
        entry.txHash,
        entry.inclusionPoint,
      );
      if (transaction.resolvedInputs.some((input) => input.outRef === outRef)) {
        throw new Error(
          "persisted bootstrap outref was already spent at its claimed point",
        );
      }
    }
    return Object.freeze({
      output: outputs.get(outputIndex),
      creation: creationTransaction,
    });
  };

  const headers = new Map(
    persisted.finalizedHeaders.map((header) => [header.headerHash, header]),
  );
  const queueOutputs: QueueOutput[] = [];
  for (const node of persisted.finalizedQueue) {
    const assetName =
      node.headerHash === null
        ? SDK.STATE_QUEUE_ROOT_ASSET_NAME
        : `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${node.headerHash}`;
    const authenticated = await authenticateOutRef({
      unit: `${stateQueuePolicyId}${assetName}`,
      outRef: node.outRef,
    });
    const decoded = queueOutput({
      output: authenticated.output,
      outRef: node.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    if (decoded === null || decoded.node.headerHash !== node.headerHash) {
      throw new Error("persisted bootstrap queue output was substituted");
    }
    if (decoded.header !== null) {
      const header = headers.get(decoded.header.headerHash);
      if (
        header === undefined ||
        header.queueOutRef !== node.outRef ||
        header.nextHeaderHash !== decoded.nextHeaderHash ||
        header.headerCborHex !== decoded.header.headerCborHex ||
        header.stateQueueNodeCborHex !== decoded.header.stateQueueNodeCborHex ||
        header.linkedListDatumCborHex !==
          decoded.header.linkedListDatumCborHex ||
        !watcherSameCanonicalJsonV1(
          header.daAvailability,
          decoded.header.daAvailability,
        ) ||
        header.observedTransactionHash !== authenticated.creation.txHash ||
        header.observedBlockHash !==
          authenticated.creation.inclusionPoint.blockHash ||
        header.observedSlot !== authenticated.creation.inclusionPoint.slot ||
        header.observedBlockNo !==
          authenticated.creation.inclusionPoint.blockNo ||
        header.observedChainPointId !==
          authenticated.creation.inclusionPoint.pointId ||
        header.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
      ) {
        throw new Error("persisted bootstrap HeaderV1 bytes were substituted");
      }
      headers.delete(decoded.header.headerHash);
    }
    queueOutputs.push(decoded);
  }
  if (
    headers.size !== 0 ||
    queueOutputs.some(
      (output, index) =>
        output.nextHeaderHash !==
        (queueOutputs[index + 1]?.node.headerHash ?? null),
    )
  ) {
    throw new Error("persisted bootstrap queue topology was substituted");
  }
  const persistedLock = persisted.finalizedCorrectionLock;
  if (persistedLock === null) {
    throw new Error("persisted bootstrap omitted CorrectionLock");
  }
  const lockUnit = SDK.correctionLockUnit(
    authority.protocolScriptHashes.hubOracleMint,
  );
  const authenticatedLock = await authenticateOutRef({
    unit: lockUnit,
    outRef: persistedLock.outRef,
  });
  const lock = lockOutput({
    output: authenticatedLock.output,
    outRef: persistedLock.outRef,
    correctionLockAddress,
    hubOraclePolicyId: authority.protocolScriptHashes.hubOracleMint,
  });
  if (
    lock === null ||
    lock.outRef !== persistedLock.outRef ||
    !sameLockDatum(lock.datum, persistedLock.datum) ||
    persistedLock.observedTransactionHash !==
      authenticatedLock.creation.txHash ||
    persistedLock.observedBlockHash !==
      authenticatedLock.creation.inclusionPoint.blockHash ||
    persistedLock.observedSlot !==
      authenticatedLock.creation.inclusionPoint.slot ||
    persistedLock.observedBlockNo !==
      authenticatedLock.creation.inclusionPoint.blockNo ||
    persistedLock.observedChainPointId !==
      authenticatedLock.creation.inclusionPoint.pointId ||
    persistedLock.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
  ) {
    throw new Error("persisted bootstrap CorrectionLock was substituted");
  }
};

const restorePersistedObservationChain = async ({
  persistedObservations,
  intersection,
  ogmiosTipBlockNo,
  authority,
  sourceId,
  maximumObservations,
  readers,
}: {
  persistedObservations: readonly unknown[];
  intersection: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  ogmiosTipBlockNo: string;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthorityV1>;
  sourceId: string;
  maximumObservations: number;
  readers: PersistedRestoreReaders;
}): Promise<WatcherProductionStateQueueRecoveryV1> => {
  if (
    !HEX_32.test(intersection.blockHash) ||
    !NATURAL.test(intersection.blockNo) ||
    !NATURAL.test(intersection.slot) ||
    !NATURAL.test(ogmiosTipBlockNo) ||
    !Number.isSafeInteger(maximumObservations) ||
    maximumObservations <= 0 ||
    persistedObservations.length === 0 ||
    persistedObservations.length > maximumObservations
  ) {
    throw new Error("state-queue restore input exceeds its release bound");
  }
  const parsed = persistedObservations.map(parsePersistedObservation);
  if (parsed.some((observation) => observation === null)) {
    throw new Error("state-queue restore contains a non-canonical observation");
  }
  const chain = parsed as readonly WatcherProductionStateQueueObservationV1[];
  const atOrBefore = chain.filter(
    ({ nativePoint }) =>
      BigInt(nativePoint.blockNo) < BigInt(intersection.blockNo) ||
      (nativePoint.blockNo === intersection.blockNo &&
        BigInt(nativePoint.slot) <= BigInt(intersection.slot)),
  );
  if (atOrBefore.length === 0 || atOrBefore.length !== chain.length) {
    throw new Error("state-queue restore observations cross the intersection");
  }
  const latestCached = chain[chain.length - 1]!;
  const offlineBlockDistance =
    BigInt(ogmiosTipBlockNo) - BigInt(latestCached.nativePoint.blockNo);
  if (
    offlineBlockDistance < 0n ||
    offlineBlockDistance > BigInt(maximumObservations)
  ) {
    throw new Error(
      "state-queue restore catch-up block distance exceeds its release bound",
    );
  }
  const intersectionPoint = Object.freeze({
    ...intersection,
    pointId: computeFraudProofRawL1PointIdV1(intersection),
  });
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const hubOraclePolicyId = authority.protocolScriptHashes.hubOracleMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const fraudProofAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.fraudProofSpend),
  );
  let prior: WatcherProductionStateQueueObservationV1 | null = null;
  let replayedHeaders = Object.freeze(
    [] as WatcherProductionStateQueueHeaderObservationV1[],
  );
  let replayedLock: WatcherProductionCorrectionLockObservationV1 | null = null;
  let previousCheckpoint: SDK.StateQueueAuthenticatedReplayCheckpointV1 | null =
    null;
  for (const persisted of chain) {
    const isAuthenticatedBase = prior === null;
    if (
      persisted.deploymentIdentityDigest !== authority.deploymentFingerprint ||
      persisted.protocolScriptAuthorityDigest !== authority.authorityDigest ||
      persisted.stateQueuePolicyId !== stateQueuePolicyId ||
      persisted.hubOraclePolicyId !== hubOraclePolicyId ||
      persisted.sourceId !== sourceId ||
      (persisted.checkpoints.length === 0 &&
        (prior !== null || persisted.previousObservationDigest !== null)) ||
      (prior !== null &&
        (persisted.previousObservationDigest !== prior.observationDigest ||
          BigInt(persisted.nativePoint.blockNo) <=
            BigInt(prior.nativePoint.blockNo) ||
          BigInt(persisted.nativePoint.slot) <= BigInt(prior.nativePoint.slot)))
    ) {
      throw new Error(
        "state-queue restore chain differs from deployment/source authority",
      );
    }
    const point = Object.freeze({
      blockHash: persisted.nativePoint.blockHash,
      blockNo: persisted.nativePoint.blockNo,
      slot: persisted.nativePoint.slot,
      pointId: computeFraudProofRawL1PointIdV1({
        blockHash: persisted.nativePoint.blockHash,
        blockNo: persisted.nativePoint.blockNo,
        slot: persisted.nativePoint.slot,
      }),
    });
    const rawBlock = await readers.readBlock(point);
    if (
      rawBlock.sourceId !== sourceId ||
      rawBlock.point.pointId !== point.pointId ||
      persisted.nativePoint.chainPointId !== point.pointId ||
      persisted.nativePoint.parentBlockHash !== rawBlock.parentBlockHash ||
      persisted.nativePoint.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
    ) {
      throw new Error("state-queue restore block metadata was substituted");
    }
    if (isAuthenticatedBase) {
      await authenticatePersistedBootstrapTopology({
        persisted,
        throughPoint: point,
        authority,
        readers,
      });
      replayedHeaders = persisted.finalizedHeaders;
      replayedLock = persisted.finalizedCorrectionLock;
    }
    for (const checkpoint of persisted.checkpoints) {
      if (
        checkpoint.blockHash !== point.blockHash ||
        checkpoint.blockNo !== point.blockNo ||
        checkpoint.slot !== point.slot ||
        checkpoint.chainPointId !== persisted.nativePoint.chainPointId ||
        checkpoint.finalityDepth !== RELEASE_FINALITY_DEPTH.toString() ||
        (prior !== null &&
          checkpoint === persisted.checkpoints[0] &&
          !sameQueue(prior.finalizedQueue, checkpoint.previousQueue)) ||
        (previousCheckpoint !== null &&
          !sameQueue(previousCheckpoint.nextQueue, checkpoint.previousQueue))
      ) {
        throw new Error(
          "state-queue restore checkpoint chain is discontinuous",
        );
      }
      const blockTransaction =
        rawBlock.transactions[Number(checkpoint.transactionIndex)];
      if (blockTransaction?.txHash !== checkpoint.transactionHash) {
        throw new Error(
          "state-queue restore transaction index was substituted",
        );
      }
      const raw = await readers.readTransaction(
        checkpoint.transactionHash,
        point,
      );
      if (
        raw.inclusionPoint.pointId !== point.pointId ||
        raw.confirmationDepth < Number(checkpoint.finalityDepth)
      ) {
        throw new Error(
          "state-queue restore transaction/finality was substituted",
        );
      }
      const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
      const queueOutputs = decodedQueueOutputs({
        body,
        transactionHash: raw.txHash,
        stateQueueAddress,
        stateQueuePolicyId,
      });
      const nextQueue =
        checkpoint.previousQueue.length === 0 && queueOutputs.length === 1
          ? Object.freeze([queueOutputs[0]!.node])
          : reconstructQueue({
              previousQueue: checkpoint.previousQueue,
              transactionHash: raw.txHash,
              spentInputOutRefs: outputReferences(body.inputs()),
              resolvedInputs: raw.resolvedInputs,
              outputs: queueOutputs,
              stateQueueAddress,
              stateQueuePolicyId,
            });
      const redeemers = rawRedeemers(raw.witnessSetCbor);
      const policies = mintPolicyIds(body);
      const lockWitness = correctionLockWitness({
        raw,
        body,
        mintPolicies: policies,
        redeemers,
        stateQueuePolicyId,
        correctionLockAddress,
        hubOraclePolicyId,
        fraudProofPolicyId: authority.protocolScriptHashes.fraudProofMint,
        fraudProofAddress,
      });
      const rederived = SDK.deriveStateQueueAuthenticatedReplayCheckpointV1({
        deploymentIdentityDigest: authority.deploymentFingerprint,
        stateQueuePolicyId,
        transactionHash: raw.txHash,
        blockHash: point.blockHash,
        slot: point.slot,
        blockNo: point.blockNo,
        transactionIndex: checkpoint.transactionIndex,
        chainPointId: persisted.nativePoint.chainPointId,
        finalityDepth: checkpoint.finalityDepth,
        mintPolicyIds: policies,
        redeemers,
        spentInputOutRefs: outputReferences(body.inputs()),
        referenceInputOutRefs: outputReferences(body.reference_inputs()),
        correctionLockWitness: lockWitness,
        previousQueue: checkpoint.previousQueue,
        nextQueue,
      });
      if (
        rederived === null ||
        !watcherSameCanonicalJsonV1(rederived, checkpoint)
      ) {
        throw new Error(
          "state-queue persisted checkpoint differs from authenticated L1 replay",
        );
      }
      if (!isAuthenticatedBase) {
        replayedLock = advanceCurrentLock({
          current: replayedLock,
          witness: lockWitness,
          transactionHash: raw.txHash,
          point,
          chainPointId: point.pointId,
          finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
        });
        const headersByHash = new Map(
          replayedHeaders.map((header) => [header.headerHash, header]),
        );
        for (const output of queueOutputs) {
          if (output.header === null) continue;
          headersByHash.set(
            output.header.headerHash,
            Object.freeze({
              ...output.header,
              queueOutRef: output.node.outRef,
              nextHeaderHash: output.nextHeaderHash,
              observedTransactionHash: raw.txHash,
              observedBlockHash: point.blockHash,
              observedSlot: point.slot,
              observedBlockNo: point.blockNo,
              observedChainPointId: point.pointId,
              finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
            }),
          );
        }
        replayedHeaders = Object.freeze(
          nextQueue.flatMap(({ headerHash }) => {
            if (headerHash === null) return [];
            const header = headersByHash.get(headerHash);
            if (header === undefined) {
              throw new Error(
                "state-queue restore omitted authenticated HeaderV1 bytes",
              );
            }
            return [header];
          }),
        );
      }
      previousCheckpoint = rederived;
    }
    if (
      (persisted.checkpoints.length === 0 &&
        (persisted.finalizedQueue.length === 0 ||
          persisted.finalizedCorrectionLock === null)) ||
      (persisted.checkpoints.length > 0 &&
        (previousCheckpoint === null ||
          !sameQueue(
            previousCheckpoint.nextQueue,
            persisted.finalizedQueue,
          ))) ||
      !watcherSameCanonicalJsonV1(
        replayedHeaders,
        persisted.finalizedHeaders,
      ) ||
      !watcherSameCanonicalJsonV1(
        replayedLock,
        persisted.finalizedCorrectionLock,
      )
    ) {
      throw new Error(
        "state-queue persisted cursor differs from checkpoint replay",
      );
    }
    prior = persisted;
  }
  const latest = chain[chain.length - 1]!;
  const intersectionBlock = await readers.readBlock(intersectionPoint);
  if (
    intersectionBlock.sourceId !== sourceId ||
    intersectionBlock.point.pointId !== intersectionPoint.pointId
  ) {
    throw new Error("state-queue restore intersection is not canonical");
  }
  const replayIntersection = Object.freeze({
    blockHash: latest.nativePoint.blockHash,
    blockNo: latest.nativePoint.blockNo,
    slot: latest.nativePoint.slot,
    chainPointId: latest.nativePoint.chainPointId,
  });
  const catchupBoundary = Object.freeze({
    blockHash: intersection.blockHash,
    blockNo: intersection.blockNo,
    slot: intersection.slot,
    chainPointId: intersectionPoint.pointId,
    finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    ogmiosTipBlockNo,
  });
  return Object.freeze({
    previous: latest,
    discardedObservationCount: 0,
    replayIntersection,
    catchupBoundary,
  });
};

const restoreLongestPersistedObservationChain = async (
  input: Parameters<typeof restorePersistedObservationChain>[0],
): Promise<WatcherProductionStateQueueRecoveryV1> => {
  if (
    input.persistedObservations.length === 0 ||
    input.persistedObservations.length > input.maximumObservations
  ) {
    return await restorePersistedObservationChain(input);
  }
  try {
    return await restorePersistedObservationChain(input);
  } catch (fullError) {
    if (!(fullError instanceof LocalKupmiosExactPointNotCanonicalV1Error)) {
      throw fullError;
    }
    for (
      let retainedCount = input.persistedObservations.length - 1;
      retainedCount > 0;
      retainedCount -= 1
    ) {
      const candidate = parsePersistedObservation(
        input.persistedObservations[retainedCount - 1],
      );
      if (candidate === null) {
        throw new Error(
          "state-queue rollback prefix contains a non-canonical observation",
        );
      }
      const candidatePoint = Object.freeze({
        blockHash: candidate.nativePoint.blockHash,
        blockNo: candidate.nativePoint.blockNo,
        slot: candidate.nativePoint.slot,
        pointId: computeFraudProofRawL1PointIdV1(candidate.nativePoint),
      });
      try {
        await input.readers.readBlock(candidatePoint);
      } catch (error) {
        if (error instanceof LocalKupmiosExactPointNotCanonicalV1Error) {
          continue;
        }
        throw error;
      }
      const restored = await restorePersistedObservationChain({
        ...input,
        persistedObservations: input.persistedObservations.slice(
          0,
          retainedCount,
        ),
      });
      return Object.freeze({
        ...restored,
        discardedObservationCount:
          input.persistedObservations.length - retainedCount,
      });
    }
    throw fullError;
  }
};

/** Pure replay test seam; unlike the production source, it grants no authority. */
export const unsafeRestorePersistedWatcherProductionStateQueueObservationForTest =
  restorePersistedObservationChain;

/** Pure rollback-prefix selection test seam; it grants no source authority. */
export const unsafeRestoreLongestWatcherProductionStateQueuePrefixForTest =
  restoreLongestPersistedObservationChain;

/** Pure bootstrap test seam; it grants no source admission authority. */
export const unsafeSnapshotWatcherProductionStateQueueAtBoundaryForTest =
  snapshotObservationAtBoundary;

/** Pure retained-header test seam; it grants no source admission authority. */
export const unsafeResolveRetainedWatcherStateQueueHeaderForTest =
  resolveRetainedHeaderAtBoundary;
