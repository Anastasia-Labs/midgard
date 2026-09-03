import {
  computeFraudProofRawL1PointIdV1,
  computeFraudProofReleaseFinalityPolicyDigestV1,
  createLocalKupmiosHttpOgmiosRawSourceV1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type LocalKupmiosFraudProofRawSourceV1,
  localKupmiosHttpOgmiosRawSourceDetailsV1,
  type LocalKupmiosRawBlockAtPointV1,
  readAdmittedLocalKupmiosRawBlockAtPointV1,
  validateVerifiedFraudProofReleaseFinalityPolicyV1,
  type VerifiedFraudProofReleaseFinalityPolicyV1,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig, type WatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentityV1,
  type VerifiedWatcherDeploymentIdentityV1,
} from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJsonV1 } from "../storage/durable-store.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  establishWatcherLocalNodeAuthorityTransportV1,
  establishWatcherLocalNodeQueryTransportV1,
  normalizeWatcherL1BlockFromTransactionCborsV1,
  type WatcherL1TransportAttestationContextV1,
  type WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "./multi-provider-consistency.js";
import type { WatcherNativeBlockAdmissionV1 } from "./native-block-admission-v1.js";
import {
  watcherNativeChainSyncAuthorityDetailsV1,
  type WatcherNativeChainSyncAuthorityV1,
} from "./native-chain-sync-v1.js";

export const WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-local-kupmios-native-observation-v1" as const;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherLocalKupmiosNativeObservationV1 = Readonly<{
  schemaVersion: typeof WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_V1_SCHEMA_VERSION;
  block: WatcherNormalizedL1BlockV1;
  ogmiosBlock: WatcherNormalizedL1BlockV1;
  kupoCheckpoint: WatcherNormalizedL1BlockV1;
  observations: readonly WatcherNormalizedL1BlockV1[];
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  consistency: ReturnType<typeof evaluateWatcherMultiProviderConsistencyV1>;
}>;

export type WatcherLocalKupmiosNativeObservationRuntimeV1 = Readonly<{
  /** Opaque, deployment-bound source shared with downstream exact L1 readers. */
  rawSource: LocalKupmiosFraudProofRawSourceV1;
  observe(input: {
    readonly block: WatcherNativeBlockAdmissionV1;
    readonly depth: string;
  }): Promise<WatcherLocalKupmiosNativeObservationV1>;
  close(): void;
}>;

const expectedRawSourceId = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentityV1,
  authorityNodeId: string,
): string =>
  [
    "watcher-native-crosscheck",
    deploymentIdentity.manifestId,
    authorityNodeId,
  ].join("/");

const nativeBindingByLocalObservationV1 = new WeakMap<object, string>();

const nativeObservationBindingV1 = (
  block: WatcherNativeBlockAdmissionV1,
): string =>
  watcherSha256CanonicalJsonV1({
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
export const assertWatcherLocalKupmiosNativeObservationV1 = (
  observation: WatcherLocalKupmiosNativeObservationV1,
  nativeBlock: WatcherNativeBlockAdmissionV1,
): void => {
  if (
    nativeBindingByLocalObservationV1.get(observation) !==
    nativeObservationBindingV1(nativeBlock)
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
const assertNativeKupmiosAgreementV1 = (
  native: WatcherNativeBlockAdmissionV1,
  raw: LocalKupmiosRawBlockAtPointV1,
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
  native: WatcherNativeBlockAdmissionV1,
  raw: LocalKupmiosRawBlockAtPointV1,
): void => assertNativeKupmiosAgreementV1(native, raw);

const releaseFinalityFromDeploymentV1 = (
  identity: VerifiedWatcherDeploymentIdentityV1,
): VerifiedFraudProofReleaseFinalityPolicyV1 => {
  const policy = Object.freeze({
    confirmationDepth: 30 as const,
    automaticRecoveryMaxDepth: 2160 as const,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1" as const,
  });
  return validateVerifiedFraudProofReleaseFinalityPolicyV1({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: identity.manifestId,
    releaseIdentityDigest: identity.releaseEvidenceDigest,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(policy),
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
export const createWatcherLocalKupmiosRawSourceV1 = (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  }>,
): LocalKupmiosFraudProofRawSourceV1 => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentityV1(input.deploymentIdentity);
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
  return createLocalKupmiosHttpOgmiosRawSourceV1({
    sourceId: expectedRawSourceId(
      input.deploymentIdentity,
      source.authorityNodeId,
    ),
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
    releaseFinality: releaseFinalityFromDeploymentV1(input.deploymentIdentity),
    timeoutMs: watcherConfig.l1.requestTimeoutMs,
  });
};

/**
 * Constructs the live W01 observation authority from one native NtC session
 * and the exact local Kupo/Ogmios topology admitted by watcher config. Kupo
 * contributes only its authenticated point; Ogmios must reproduce the native
 * transaction vector byte-for-byte.
 */
export const createWatcherLocalKupmiosNativeObservationRuntimeV1 = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
    nativeAuthority: WatcherNativeChainSyncAuthorityV1;
    rawSource?: LocalKupmiosFraudProofRawSourceV1;
  }>,
): Promise<WatcherLocalKupmiosNativeObservationRuntimeV1> => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentityV1(input.deploymentIdentity);
  const nativeDetails = watcherNativeChainSyncAuthorityDetailsV1(
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

  const authorityContext = establishWatcherLocalNodeAuthorityTransportV1(
    input.nativeAuthority,
  );
  const queryContexts: WatcherL1TransportAttestationContextV1[] = [];
  try {
    const kupoContext = await establishWatcherLocalNodeQueryTransportV1(
      authorityContext,
      tcpTransport(kupo, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(kupoContext);
    const ogmiosContext = await establishWatcherLocalNodeQueryTransportV1(
      authorityContext,
      tcpTransport(ogmios, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(ogmiosContext);
    const rawSource =
      input.rawSource ??
      createWatcherLocalKupmiosRawSourceV1({
        watcherConfig,
        deploymentIdentity: input.deploymentIdentity,
      });
    const rawSourceDetails =
      localKupmiosHttpOgmiosRawSourceDetailsV1(rawSource);
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
          pointId: computeFraudProofRawL1PointIdV1({
            blockHash: block.blockHash,
            blockNo: block.blockNo,
            slot: block.slot,
          }),
        });
        const raw = await readAdmittedLocalKupmiosRawBlockAtPointV1({
          source: rawSource,
          point,
        });
        assertNativeKupmiosAgreementV1(block, raw);
        const chainPoint = Object.freeze({
          blockHash: block.blockHash,
          parentBlockHash: block.prevHash.length === 0 ? null : block.prevHash,
          slot: block.slot,
          blockNo: block.blockNo,
          depth,
        });
        const blockObservation = normalizeWatcherL1BlockFromTransactionCborsV1(
          authorityContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: block.transactionCbors,
          },
        );
        const ogmiosBlock = normalizeWatcherL1BlockFromTransactionCborsV1(
          ogmiosContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: raw.transactions.map(
              ({ transactionCbor }) => transactionCbor,
            ),
          },
        );
        const kupoCheckpoint = normalizeWatcherL1BlockFromTransactionCborsV1(
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
        const consistency = evaluateWatcherMultiProviderConsistencyV1(
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
            WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_V1_SCHEMA_VERSION,
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
        nativeBindingByLocalObservationV1.set(
          observation,
          nativeObservationBindingV1(block),
        );
        return observation;
      },
      close: () => {
        if (closed) return;
        closed = true;
        for (const context of queryContexts) {
          closeWatcherL1TransportAttestationContextV1(context);
        }
        closeWatcherL1TransportAttestationContextV1(authorityContext);
      },
    });
  } catch (error) {
    for (const context of queryContexts) {
      closeWatcherL1TransportAttestationContextV1(context);
    }
    closeWatcherL1TransportAttestationContextV1(authorityContext);
    throw error;
  }
};
