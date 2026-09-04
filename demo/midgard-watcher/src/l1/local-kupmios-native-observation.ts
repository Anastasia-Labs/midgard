import {
  computeFraudProofRawL1PointId,
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosHttpOgmiosRawSource,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig, type WatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherLocalNodeAuthorityTransport,
  establishWatcherLocalNodeQueryTransport,
  normalizeWatcherL1BlockFromTransactionCbors,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency } from "./multi-provider-consistency.js";
import type { WatcherNativeBlockAdmission } from "./native-block-admission.js";
import {
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
} from "./native-chain-sync.js";

export const WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-local-kupmios-native-observation-v1" as const;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherLocalKupmiosNativeObservation = Readonly<{
  schemaVersion: typeof WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION;
  block: WatcherNormalizedL1Block;
  ogmiosBlock: WatcherNormalizedL1Block;
  kupoCheckpoint: WatcherNormalizedL1Block;
  observations: readonly WatcherNormalizedL1Block[];
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
  consistency: ReturnType<typeof evaluateWatcherMultiProviderConsistency>;
}>;

export type WatcherLocalKupmiosNativeObservationRuntime = Readonly<{
  /** Opaque, deployment-bound source shared with downstream exact L1 readers. */
  rawSource: LocalKupmiosFraudProofRawSource;
  observe(input: {
    readonly block: WatcherNativeBlockAdmission;
    readonly depth: string;
  }): Promise<WatcherLocalKupmiosNativeObservation>;
  close(): void;
}>;

const expectedRawSourceId = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
  authorityNodeId: string,
): string =>
  [
    "watcher-native-crosscheck",
    deploymentIdentity.manifestId,
    authorityNodeId,
  ].join("/");

const nativeBindingByLocalObservation = new WeakMap<object, string>();

const nativeObservationBinding = (block: WatcherNativeBlockAdmission): string =>
  watcherSha256CanonicalJson({
    schemaVersion: block.schemaVersion,
    blockType: block.blockType,
    protocolMajor: block.protocolMajor,
    blockHash: block.blockHash,
    prevHash: block.prevHash,
    slot: block.slot,
    blockNo: block.blockNo,
    rawBlockCbor: block.rawBlockCbor,
    rawHeaderCbor: block.rawHeaderCbor,
    transactionIds: block.transactionIds,
    transactionCbors: block.transactionCbors,
  });

/**
 * Proves this exact observation was produced after native/Kupo/Ogmios byte and
 * point agreement for the supplied native block. Structural copies reject.
 */
export const assertWatcherLocalKupmiosNativeObservation = (
  observation: WatcherLocalKupmiosNativeObservation,
  nativeBlock: WatcherNativeBlockAdmission,
): void => {
  if (
    nativeBindingByLocalObservation.get(observation) !==
    nativeObservationBinding(nativeBlock)
  ) {
    throw new Error(
      "watcher local Kupo/Ogmios observation is not admitted for the native block",
    );
  }
};

const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

/**
 * This comparison runs only after the fault-proof package has re-admitted the
 * opaque local-Kupmios source result. Keeping it separate makes the exact
 * native-vs-Ogmios byte/order invariant directly testable without creating a
 * second source-authority implementation in the watcher.
 */
const assertNativeKupmiosAgreement = (
  native: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
): void => {
  const transactionIds = raw.transactions.map(({ txHash }) => txHash);
  const transactionCbors = raw.transactions.map(
    ({ transactionCbor }) => transactionCbor,
  );
  if (
    raw.point.blockHash !== native.blockHash ||
    raw.point.slot !== native.slot ||
    raw.point.blockNo !== native.blockNo ||
    raw.kupoCheckpoint.blockHash !== native.blockHash ||
    raw.kupoCheckpoint.slot.toString() !== native.slot ||
    !sameStrings(transactionIds, native.transactionIds) ||
    !sameStrings(transactionCbors, native.transactionCbors)
  ) {
    throw new Error(
      "local Kupo/Ogmios observation differs from the native chain-sync block",
    );
  }
};

/** Test-only direct exercise of the deterministic comparison above. */
export const unsafeAssertNativeKupmiosAgreementForTest = (
  native: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
): void => assertNativeKupmiosAgreement(native, raw);

const releaseFinalityFromDeployment = (
  identity: VerifiedWatcherDeploymentIdentity,
): VerifiedFraudProofReleaseFinalityPolicy => {
  const policy = Object.freeze({
    confirmationDepth: 30 as const,
    automaticRecoveryMaxDepth: 2160 as const,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1" as const,
  });
  return validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: identity.manifestId,
    releaseIdentityDigest: identity.releaseEvidenceDigest,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  });
};

const tcpTransport = (
  service: Extract<
    WatcherConfig["l1"]["source"],
    { readonly sourceMode: "local_node" }
  >["queryServices"][number],
  timeoutMs: number,
) =>
  Object.freeze({
    transportKind: "tcp" as const,
    providerId: service.identity,
    surface: service.kind,
    endpoint: service.endpoint,
    connectTimeoutMs: timeoutMs,
  });

/**
 * Constructs the deployment/config-bound raw Kupo/Ogmios authority before a
 * native chain-sync process is started. The runtime must subsequently bind
 * this exact source to the native authority before processing any event.
 */
export const createWatcherLocalKupmiosRawSource = (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  }>,
): LocalKupmiosFraudProofRawSource => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  if (
    watcherConfig.mode !== "acceptance" ||
    watcherConfig.targetNetwork !== "Preprod" ||
    input.deploymentIdentity.network !== watcherConfig.targetNetwork ||
    watcherConfig.l1.source.sourceMode !== "local_node" ||
    watcherConfig.l1.finality.depth !== 30 ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      "local Kupo/Ogmios raw source differs from the admitted release",
    );
  }
  const source = watcherConfig.l1.source;
  const kupo = source.queryServices.find(({ kind }) => kind === "kupo");
  const ogmios = source.queryServices.find(({ kind }) => kind === "ogmios");
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("local Kupo/Ogmios raw source omitted a required service");
  }
  if (source.queryServices.some(({ kind }) => kind === "db_sync")) {
    throw new Error(
      "configured db-sync requires a concrete authenticated watcher query adapter",
    );
  }
  return createLocalKupmiosHttpOgmiosRawSource({
    sourceId: expectedRawSourceId(
      input.deploymentIdentity,
      source.authorityNodeId,
    ),
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
    releaseFinality: releaseFinalityFromDeployment(input.deploymentIdentity),
    timeoutMs: watcherConfig.l1.requestTimeoutMs,
  });
};

/**
 * Constructs the live W01 observation authority from one native NtC session
 * and the exact local Kupo/Ogmios topology admitted by watcher config. Kupo
 * contributes only its authenticated point; Ogmios must reproduce the native
 * transaction vector byte-for-byte.
 */
export const createWatcherLocalKupmiosNativeObservationRuntime = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    nativeAuthority: WatcherNativeChainSyncAuthority;
    rawSource?: LocalKupmiosFraudProofRawSource;
  }>,
): Promise<WatcherLocalKupmiosNativeObservationRuntime> => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  const nativeDetails = watcherNativeChainSyncAuthorityDetails(
    input.nativeAuthority,
  );
  if (
    watcherConfig.mode !== "acceptance" ||
    watcherConfig.targetNetwork !== "Preprod" ||
    input.deploymentIdentity.network !== watcherConfig.targetNetwork ||
    watcherConfig.l1.source.sourceMode !== "local_node" ||
    nativeDetails === null ||
    nativeDetails.network !== watcherConfig.targetNetwork ||
    nativeDetails.authorityNodeId !== watcherConfig.l1.source.authorityNodeId ||
    nativeDetails.genesisIdentitySha256 !==
      watcherConfig.l1.source.chainSync.genesisIdentitySha256 ||
    nativeDetails.socketPath !== watcherConfig.l1.source.chainSync.socketPath
  ) {
    throw new Error(
      "local Kupo/Ogmios runtime differs from the admitted native authority",
    );
  }
  const source = watcherConfig.l1.source;
  if (source.sourceMode !== "local_node") {
    throw new Error(
      "local Kupo/Ogmios runtime source changed during admission",
    );
  }
  if (
    watcherConfig.l1.finality.depth !== 30 ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      "local Kupo/Ogmios runtime finality differs from the release policy",
    );
  }
  const kupo = source.queryServices.find(({ kind }) => kind === "kupo");
  const ogmios = source.queryServices.find(({ kind }) => kind === "ogmios");
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("local Kupo/Ogmios runtime omitted a required service");
  }
  if (source.queryServices.some(({ kind }) => kind === "db_sync")) {
    throw new Error(
      "configured db-sync requires a concrete authenticated watcher query adapter",
    );
  }

  const authorityContext = establishWatcherLocalNodeAuthorityTransport(
    input.nativeAuthority,
  );
  const queryContexts: WatcherL1TransportAttestationContext[] = [];
  try {
    const kupoContext = await establishWatcherLocalNodeQueryTransport(
      authorityContext,
      tcpTransport(kupo, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(kupoContext);
    const ogmiosContext = await establishWatcherLocalNodeQueryTransport(
      authorityContext,
      tcpTransport(ogmios, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(ogmiosContext);
    const rawSource =
      input.rawSource ??
      createWatcherLocalKupmiosRawSource({
        watcherConfig,
        deploymentIdentity: input.deploymentIdentity,
      });
    const rawSourceDetails = localKupmiosHttpOgmiosRawSourceDetails(rawSource);
    if (
      rawSourceDetails === null ||
      rawSourceDetails.sourceId !==
        expectedRawSourceId(input.deploymentIdentity, source.authorityNodeId) ||
      rawSourceDetails.deploymentIdentityDigest !==
        input.deploymentIdentity.manifestId ||
      rawSourceDetails.releaseIdentityDigest !==
        input.deploymentIdentity.releaseEvidenceDigest ||
      rawSourceDetails.kupoHttpUrl !== kupo.endpoint ||
      rawSourceDetails.ogmiosUrl !== ogmios.endpoint
    ) {
      throw new Error(
        "local Kupo/Ogmios raw source differs from the admitted native topology",
      );
    }
    let closed = false;
    return Object.freeze({
      rawSource,
      observe: async ({ block, depth }) => {
        if (closed) throw new Error("local Kupo/Ogmios runtime is closed");
        if (!NATURAL.test(depth) || BigInt(depth) > 2160n) {
          throw new Error("local Kupo/Ogmios observation depth is invalid");
        }
        const point = Object.freeze({
          blockHash: block.blockHash,
          blockNo: block.blockNo,
          slot: block.slot,
          pointId: computeFraudProofRawL1PointId({
            blockHash: block.blockHash,
            blockNo: block.blockNo,
            slot: block.slot,
          }),
        });
        const raw = await readAdmittedLocalKupmiosRawBlockAtPoint({
          source: rawSource,
          point,
        });
        assertNativeKupmiosAgreement(block, raw);
        const chainPoint = Object.freeze({
          blockHash: block.blockHash,
          parentBlockHash: block.prevHash.length === 0 ? null : block.prevHash,
          slot: block.slot,
          blockNo: block.blockNo,
          depth,
        });
        const blockObservation = normalizeWatcherL1BlockFromTransactionCbors(
          authorityContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: block.transactionCbors,
          },
        );
        const ogmiosBlock = normalizeWatcherL1BlockFromTransactionCbors(
          ogmiosContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: raw.transactions.map(
              ({ transactionCbor }) => transactionCbor,
            ),
          },
        );
        const kupoCheckpoint = normalizeWatcherL1BlockFromTransactionCbors(
          kupoContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: [],
          },
        );
        const configuredSource = Object.freeze({
          sourceMode: "local_node" as const,
          network: watcherConfig.targetNetwork,
          authorityNodeId: source.authorityNodeId,
          genesisIdentitySha256: source.chainSync.genesisIdentitySha256,
          chainSyncSocketPath: source.chainSync.socketPath,
          queryServices: Object.freeze(
            [kupo, ogmios]
              .map((service) =>
                Object.freeze({
                  kind: service.kind,
                  providerId: service.identity,
                  endpoint: service.endpoint,
                }),
              )
              .sort((left, right) =>
                left.providerId.localeCompare(right.providerId),
              ),
          ),
        });
        const consistency = evaluateWatcherMultiProviderConsistency(
          configuredSource,
          [blockObservation, kupoCheckpoint, ogmiosBlock],
          [authorityContext, kupoContext, ogmiosContext],
        );
        if (
          consistency.status !== "agreed" ||
          consistency.protocolDecision !== "allowed" ||
          consistency.agreement?.blockContentDigest !==
            blockObservation.blockContentDigest ||
          consistency.localQueryServiceBindings.some(
            ({ observationStatus }) => observationStatus !== "aligned",
          )
        ) {
          throw new Error(
            "local Kupo/Ogmios observations did not agree with native chain-sync",
          );
        }
        const observations = Object.freeze([
          blockObservation,
          kupoCheckpoint,
          ogmiosBlock,
        ]);
        const observation = Object.freeze({
          schemaVersion:
            WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION,
          block: blockObservation,
          ogmiosBlock,
          kupoCheckpoint,
          observations,
          transportAttestations: Object.freeze([
            authorityContext,
            kupoContext,
            ogmiosContext,
          ]),
          consistency,
        });
        nativeBindingByLocalObservation.set(
          observation,
          nativeObservationBinding(block),
        );
        return observation;
      },
      close: () => {
        if (closed) return;
        closed = true;
        for (const context of queryContexts) {
          closeWatcherL1TransportAttestationContext(context);
        }
        closeWatcherL1TransportAttestationContext(authorityContext);
      },
    });
  } catch (error) {
    for (const context of queryContexts) {
      closeWatcherL1TransportAttestationContext(context);
    }
    closeWatcherL1TransportAttestationContext(authorityContext);
    throw error;
  }
};
