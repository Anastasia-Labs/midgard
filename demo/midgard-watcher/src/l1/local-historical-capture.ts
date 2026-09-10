import { performance } from "node:perf_hooks";
import { isProxy } from "node:util/types";

import {
  admitFraudProofRawL1Point,
  computeFraudProofReleaseFinalityPolicyDigest,
  type FraudProofRawL1Point,
  LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE,
  localKupmiosHttpOgmiosRawSourceDetails,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosPredecessorPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig, type WatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { createWatcherLocalKupmiosRawSource } from "./local-kupmios-raw-source.js";
import {
  admitWatcherNativeRollForwardBlock,
  type WatcherNativeBlockAdmission,
} from "./native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  watcherNativeChainSyncAuthorityDetails,
} from "./native-chain-sync.js";

const captureBrand = Symbol("local-historical-capture");
const MAX_UINT64 = (1n << 64n) - 1n;

/** Capture-time corroboration only; this is not historical/W12 authority. */
export type WatcherLocalHistoricalCaptureReceipt = Readonly<{
  [captureBrand]: true;
}>;

type CaptureRead = Readonly<{
  network: "Preprod";
  deploymentIdentityDigest: string;
  blueprintHash: string;
  sourceId: string;
  sourceBinding: Readonly<{
    sourceMode: "local_node";
    network: "Preprod";
    authorityNodeId: string;
    genesisIdentitySha256: string;
    chainSyncSocketPath: string;
    queryServices: readonly Readonly<{
      kind: "kupo" | "ogmios";
      providerId: string;
      endpoint: string;
      admittedSourceUrl: string;
    }>[];
  }>;
  rawBlock: LocalKupmiosRawBlockAtPoint;
  creatingTransactionBodies: readonly string[];
  finalityConfig: WatcherConfig["l1"]["finality"];
  startedAtMonotonicMs: number;
  nativeAuthorityDigest: string;
  nativeStartupDigest: string;
  targetEventDigest: string;
  point: FraudProofRawL1Point;
  predecessorPoint: FraudProofRawL1Point;
  nativeBlock: WatcherNativeBlockAdmission;
  observedNativeTip: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
  }>;
  depthAtObservedTip: string;
  observedAt: string;
  expiresAt: string;
}>;

const captures = new WeakMap<
  WatcherLocalHistoricalCaptureReceipt,
  Readonly<{ read(): CaptureRead }>
>();

/** A read checks the owned snapshot's liveness; it acquires no new chain tip. */
export const readWatcherLocalHistoricalCapture = (
  receipt: WatcherLocalHistoricalCaptureReceipt,
): CaptureRead => {
  const owner = captures.get(receipt);
  if (owner === undefined)
    throw new Error("local historical capture receipt is absent or stale");
  return owner.read();
};

const exactData = (
  value: unknown,
  required: readonly string[],
  optional: readonly string[],
  label: string,
): void => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be exact plain data`);
  }
  const descriptors = Object.getOwnPropertyDescriptors(value);
  if (
    Reflect.ownKeys(value).some(
      (key) =>
        typeof key !== "string" ||
        (!required.includes(key) && !optional.includes(key)),
    ) ||
    required.some((key) => !Object.hasOwn(descriptors, key)) ||
    Object.values(descriptors).some(
      (descriptor) => !("value" in descriptor) || !descriptor.enumerable,
    )
  )
    throw new Error(`${label} has unknown, missing or non-data fields`);
};

const boundedInteger = (
  value: number,
  minimum: number,
  maximum: number,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || value < minimum || value > maximum)
    throw new Error(`${label} is outside its operational bound`);
  return value;
};

const admitPoint = (value: FraudProofRawL1Point): FraudProofRawL1Point => {
  exactData(
    value,
    ["blockHash", "blockNo", "slot", "pointId"],
    [],
    "capture point",
  );
  const point = admitFraudProofRawL1Point(value, "capture point");
  for (const value of [point.blockNo, point.slot]) {
    if (value.length > 20 || BigInt(value) > MAX_UINT64)
      throw new Error("capture point exceeds UInt64");
  }
  return Object.freeze(point);
};

const queryPoint = ({ blockHash, blockNo, slot }: FraudProofRawL1Point) =>
  Object.freeze({ blockHash, blockNo, slot });

const httpEndpoint = (endpoint: string): string => {
  const url = new URL(endpoint.trim());
  if (url.protocol === "ws:") url.protocol = "http:";
  if (url.protocol === "wss:") url.protocol = "https:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};

const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const assertBlockAgreement = (
  block: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
  predecessor: FraudProofRawL1Point,
): void => {
  if (
    block.blockHash !== raw.point.blockHash ||
    block.blockNo !== raw.point.blockNo ||
    block.slot !== raw.point.slot ||
    block.prevHash !== raw.parentBlockHash ||
    block.prevHash !== predecessor.blockHash ||
    BigInt(predecessor.blockNo) + 1n !== BigInt(block.blockNo) ||
    BigInt(predecessor.slot) >= BigInt(block.slot) ||
    raw.kupoCheckpoint.blockHash !== block.blockHash ||
    raw.kupoCheckpoint.slot.toString() !== block.slot ||
    !sameStrings(
      block.transactionIds,
      raw.transactions.map(({ txHash }) => txHash),
    ) ||
    !sameStrings(
      block.transactionCbors,
      raw.transactions.map(({ transactionCbor }) => transactionCbor),
    )
  )
    throw new Error(
      "historical capture native and Kupo/Ogmios blocks disagree",
    );
};

type NativeQuery = Awaited<ReturnType<typeof openWatcherNativeExactPointQuery>>;

/** Owns a fresh concrete source and exactly two sequential native queries. */
export const openWatcherLocalHistoricalCapture = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    nativeChainSyncBinaryPath: string;
    point: FraudProofRawL1Point;
    signal?: AbortSignal;
    limits?: Readonly<{ timeoutMs?: number; maxRawResponseBytes?: number }>;
  }>,
): Promise<
  Readonly<{
    receipt: WatcherLocalHistoricalCaptureReceipt;
    close(): Promise<void>;
  }>
> => {
  exactData(
    input,
    [
      "watcherConfig",
      "deploymentIdentity",
      "nativeChainSyncBinaryPath",
      "point",
    ],
    ["signal", "limits"],
    "historical capture input",
  );
  if (input.limits !== undefined)
    exactData(
      input.limits,
      [],
      ["timeoutMs", "maxRawResponseBytes"],
      "capture limits",
    );
  const timeoutMs = boundedInteger(
    input.limits?.timeoutMs ?? 30_000,
    100,
    120_000,
    "capture timeout",
  );
  const maxRawResponseBytes = boundedInteger(
    input.limits?.maxRawResponseBytes ?? 8 * 1024 * 1024,
    1,
    64 * 1024 * 1024,
    "capture raw response bytes",
  );
  const signal = input.signal;
  if (signal !== undefined) {
    try {
      Object.getOwnPropertyDescriptor(
        AbortSignal.prototype,
        "aborted",
      )!.get!.call(signal);
    } catch {
      throw new Error("capture signal must be a platform AbortSignal");
    }
    signal.throwIfAborted();
  }
  const point = admitPoint(input.point);
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  const deploymentIdentity = input.deploymentIdentity;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const binaryPath = input.nativeChainSyncBinaryPath;
  if (typeof binaryPath !== "string")
    throw new Error("capture binary path is invalid");
  const started = performance.now();
  const startedUtc = Date.now();
  const deadline = started + timeoutMs;
  const controller = new AbortController();
  let active = true;
  let pendingQuery: Promise<NativeQuery> | undefined;
  const ownedQueries: { query: NativeQuery; close(): Promise<void> }[] = [];
  let closePromise: Promise<void> | undefined;
  const close = (): Promise<void> => {
    if (closePromise !== undefined) return closePromise;
    active = false;
    controller.abort();
    clearTimeout(timer);
    signal?.removeEventListener("abort", onAbort);
    closePromise = (async () => {
      await pendingQuery?.catch(() => undefined);
      for (const owned of ownedQueries) await owned.close();
    })();
    return closePromise;
  };
  const onAbort = (): void => {
    void close().catch(() => undefined);
  };
  const timer = setTimeout(onAbort, Math.max(0, deadline - performance.now()));
  signal?.addEventListener("abort", onAbort, { once: true });
  if (signal?.aborted === true) onAbort();
  const assertLive = (): void => {
    if (!active || controller.signal.aborted || performance.now() >= deadline) {
      onAbort();
      throw new Error(
        "local historical capture is cancelled, expired or closed",
      );
    }
  };
  try {
    assertLive();
    const source = createWatcherLocalKupmiosRawSource({
      watcherConfig,
      deploymentIdentity,
      captureBounds: {
        signal: controller.signal,
        timeoutMs: Math.min(
          watcherConfig.l1.requestTimeoutMs,
          Math.floor(deadline - performance.now()),
        ),
        blockScanLimit: 1,
        maxResponseBytes: maxRawResponseBytes,
      },
    });
    const topology = watcherConfig.l1.source;
    if (topology.sourceMode !== "local_node")
      throw new Error("capture requires local-node topology");
    const details = localKupmiosHttpOgmiosRawSourceDetails(source);
    const kupo = topology.queryServices.find(({ kind }) => kind === "kupo")!;
    const ogmios = topology.queryServices.find(
      ({ kind }) => kind === "ogmios",
    )!;
    if (
      details === null ||
      details.sourceId !== source.sourceId ||
      details.sourceId !==
        `${LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE}:watcher-native-crosscheck/${deploymentIdentity.manifestId}/${topology.authorityNodeId}` ||
      details.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
      details.blueprintHash !== deploymentIdentity.blueprintHash ||
      details.kupoHttpUrl !== httpEndpoint(kupo.endpoint) ||
      details.ogmiosUrl !== httpEndpoint(ogmios.endpoint) ||
      details.confirmationDepth !== 30 ||
      details.automaticRecoveryMaxDepth !== 2160 ||
      details.finalityPolicyDigest !==
        computeFraudProofReleaseFinalityPolicyDigest({
          confirmationDepth: 30,
          automaticRecoveryMaxDepth: 2160,
          deepRollbackPolicy: "automated_rewind_replay_incident-v1",
        })
    )
      throw new Error("capture raw source differs from deployment/topology");
    const predecessor = await readAdmittedLocalKupmiosPredecessorPoint({
      source,
      point,
    });
    assertLive();
    const predecessorPoint = admitPoint(predecessor.predecessorPoint);
    if (
      predecessor.sourceId !== details.sourceId ||
      predecessor.point.pointId !== point.pointId
    )
      throw new Error("capture predecessor differs from source/target");
    const raw = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source,
      point,
    });
    assertLive();
    const assertRawIdentity = (value: LocalKupmiosRawBlockAtPoint): void => {
      if (
        value.sourceId !== details.sourceId ||
        value.point.pointId !== point.pointId
      )
        throw new Error("capture raw block differs from source/target");
    };
    assertRawIdentity(raw);
    const openQuery = async () => {
      assertLive();
      const remaining = Math.floor(deadline - performance.now());
      if (remaining < 100)
        throw new Error(
          "capture has less than 100ms remaining for native query",
        );
      pendingQuery = openWatcherNativeExactPointQuery({
        binaryPath,
        watcherConfig,
        predecessor: queryPoint(predecessorPoint),
        target: queryPoint(point),
        timeoutMs: remaining,
        signal: controller.signal,
      }).then((query) => {
        let closed: Promise<void> | undefined;
        ownedQueries.push({ query, close: () => (closed ??= query.close()) });
        return query;
      });
      const query = await pendingQuery;
      assertLive();
      return ownedQueries.find((owned) => owned.query === query)!;
    };
    const readQuery = (query: NativeQuery) => {
      assertLive();
      const observed = readWatcherNativeExactPointQuery(query.receipt);
      const native = watcherNativeChainSyncAuthorityDetails(observed.authority);
      if (
        native === null ||
        native.network !== watcherConfig.targetNetwork ||
        native.authorityNodeId !== topology.authorityNodeId ||
        native.genesisIdentitySha256 !==
          topology.chainSync.genesisIdentitySha256 ||
        native.socketPath !== topology.chainSync.socketPath ||
        native.startupDigest !== observed.startupDigest ||
        native.operation.kind !== "exact_point" ||
        native.operation.predecessorBlockNo !== predecessorPoint.blockNo ||
        native.operation.target.blockHash !== point.blockHash ||
        native.operation.target.blockNo !== point.blockNo ||
        native.operation.target.slot !== point.slot ||
        native.selectedIntersection.kind !== "point" ||
        native.selectedIntersection.blockHash !== predecessorPoint.blockHash ||
        native.selectedIntersection.slot !== predecessorPoint.slot
      )
        throw new Error(
          "capture native query differs from configured authority/points",
        );
      return observed;
    };
    const first = await openQuery();
    const firstRead = readQuery(first.query);
    const firstBlock = admitWatcherNativeRollForwardBlock(firstRead.event);
    assertBlockAgreement(firstBlock, raw, predecessorPoint);
    readQuery(first.query);
    const references = await readAdmittedLocalKupmiosReferenceBodiesAtPoint({
      source,
      point,
    });
    assertLive();
    assertRawIdentity(references.targetBlock);
    assertBlockAgreement(firstBlock, references.targetBlock, predecessorPoint);
    readQuery(first.query);
    const rechecked = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source,
      point,
    });
    assertLive();
    assertRawIdentity(rechecked);
    assertBlockAgreement(firstBlock, rechecked, predecessorPoint);
    readQuery(first.query);
    await first.close();
    assertLive();
    const second = await openQuery();
    const secondRead = readQuery(second.query);
    const nativeBlock = admitWatcherNativeRollForwardBlock(secondRead.event);
    assertBlockAgreement(nativeBlock, rechecked, predecessorPoint);
    if (
      nativeBlock.rawBlockCbor !== firstBlock.rawBlockCbor ||
      nativeBlock.rawHeaderCbor !== firstBlock.rawHeaderCbor
    )
      throw new Error("capture native queries disagree on exact block bytes");
    const value: CaptureRead = Object.freeze({
      network: "Preprod",
      deploymentIdentityDigest: details.deploymentIdentityDigest,
      blueprintHash: details.blueprintHash,
      sourceId: details.sourceId,
      sourceBinding: Object.freeze({
        sourceMode: "local_node",
        network: "Preprod",
        authorityNodeId: topology.authorityNodeId,
        genesisIdentitySha256: topology.chainSync.genesisIdentitySha256,
        chainSyncSocketPath: topology.chainSync.socketPath,
        queryServices: Object.freeze(
          [kupo, ogmios].map((service) =>
            Object.freeze({
              kind: service.kind as "kupo" | "ogmios",
              providerId: service.identity,
              endpoint: service.endpoint,
              admittedSourceUrl:
                service.kind === "kupo"
                  ? details.kupoHttpUrl
                  : details.ogmiosUrl,
            }),
          ),
        ),
      }),
      rawBlock: Object.freeze({
        ...rechecked,
        point: Object.freeze({ ...rechecked.point }),
        kupoCheckpoint: Object.freeze({ ...rechecked.kupoCheckpoint }),
        transactions: Object.freeze(
          rechecked.transactions.map((transaction) =>
            Object.freeze({ ...transaction }),
          ),
        ),
      }),
      creatingTransactionBodies: Object.freeze([
        ...references.creatingTransactionBodies,
      ]),
      finalityConfig: Object.freeze({
        depth: watcherConfig.l1.finality.depth,
        rollback: Object.freeze({ ...watcherConfig.l1.finality.rollback }),
      }),
      startedAtMonotonicMs: started,
      nativeAuthorityDigest: secondRead.authority.authorityDigest,
      nativeStartupDigest: secondRead.startupDigest,
      targetEventDigest: secondRead.eventDigest,
      point,
      predecessorPoint,
      nativeBlock,
      observedNativeTip: secondRead.observedTip,
      depthAtObservedTip: secondRead.depthAtObservedTip,
      observedAt: secondRead.observedAt,
      expiresAt: new Date(
        Math.min(startedUtc + timeoutMs, Date.parse(secondRead.expiresAt)),
      ).toISOString(),
    });
    const read = (): CaptureRead => {
      assertLive();
      assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
      // Source details attest identity only. Its private owner signal/deadline
      // above supplies liveness; an idle query does not monitor future rollbacks.
      if (localKupmiosHttpOgmiosRawSourceDetails(source) !== details)
        throw new Error("capture source identity is absent");
      const current = readQuery(second.query);
      if (current !== secondRead)
        throw new Error("capture native receipt changed");
      return value;
    };
    read();
    const receipt = Object.freeze({ [captureBrand]: true as const });
    captures.set(receipt, Object.freeze({ read }));
    return Object.freeze({ receipt, close });
  } catch (error) {
    await close();
    throw error;
  }
};
