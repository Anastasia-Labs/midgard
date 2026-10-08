import { performance } from "node:perf_hooks";

import {
  type FraudProofRawL1Point,
  LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE,
  LocalKupmiosCheckpointChangedError,
  localKupmiosHttpOgmiosRawSourceDetails,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosPredecessorPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityPolicy,
} from "../runtime/deployment-identity.js";
import {
  admitPoint,
  assertBlockAgreement,
  boundedInteger,
  captureBrand,
  type CaptureRead,
  captures,
  exactData,
  httpEndpoint,
  type NativeQuery,
  queryPoint,
  type WatcherLocalHistoricalCaptureReceipt,
} from "./local-historical-capture.capture-read.js";
import { createWatcherLocalKupmiosRawSource } from "./local-kupmios-raw-source.js";
import { admitWatcherNativeRollForwardBlock } from "./native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  watcherNativeChainSyncAuthorityDetails,
} from "./native-chain-sync.js";

/** Owns a fresh concrete source and exactly two sequential native queries. */
export const openWatcherLocalHistoricalCapture = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    l1NodeTransportBinaryPath: string;
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
      "l1NodeTransportBinaryPath",
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
  const binaryPath = input.l1NodeTransportBinaryPath;
  if (typeof binaryPath !== "string")
    throw new Error("capture binary path is invalid");
  const started = performance.now();
  const startedUtc = Date.now();
  const deadline = started + timeoutMs;
  // Failed attempts close every owned transport. A retry repins a fresh source
  // and repeats both native observations within the original acquisition budget.
  for (let attempt = 0; ; attempt += 1) {
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
    const timer = setTimeout(
      onAbort,
      Math.max(0, deadline - performance.now()),
    );
    signal?.addEventListener("abort", onAbort, { once: true });
    if (signal?.aborted === true) onAbort();
    const assertLive = (): void => {
      if (
        !active ||
        controller.signal.aborted ||
        performance.now() >= deadline
      ) {
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
      const releaseFinality =
        watcherDeploymentReleaseFinalityPolicy(deploymentIdentity);
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
        details.confirmationDepth !==
          releaseFinality.policy.confirmationDepth ||
        details.automaticRecoveryMaxDepth !== 2160 ||
        details.finalityPolicyDigest !== releaseFinality.policyDigest
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
        const native = watcherNativeChainSyncAuthorityDetails(
          observed.authority,
        );
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
          native.selectedIntersection.blockHash !==
            predecessorPoint.blockHash ||
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
      assertBlockAgreement(
        firstBlock,
        references.targetBlock,
        predecessorPoint,
      );
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
        network: watcherConfig.targetNetwork,
        deploymentIdentityDigest: details.deploymentIdentityDigest,
        blueprintHash: details.blueprintHash,
        sourceId: details.sourceId,
        sourceBinding: Object.freeze({
          sourceMode: "local_node",
          network: watcherConfig.targetNetwork,
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
      if (
        error instanceof LocalKupmiosCheckpointChangedError &&
        attempt < 2 &&
        !signal?.aborted &&
        performance.now() + 100 < deadline
      )
        continue;
      throw error;
    }
  }
};
