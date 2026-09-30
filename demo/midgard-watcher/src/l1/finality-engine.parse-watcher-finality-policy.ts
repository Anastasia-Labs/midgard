import {
  parseWatcherConfig,
  WATCHER_CARDANO_SECURITY_PARAMETER_K,
  type WatcherConfig,
} from "../runtime/config.js";
import { parseWatcherCustomNetwork } from "../runtime/custom-network.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  cloneExternalProviders,
  cloneLocalQueryServices,
  cloneMarker,
  isExactAbsoluteSocketPath,
  isPositiveUint64,
  isUint64,
  makePolicy,
} from "./finality-engine.clone-external-providers.js";
import {
  exactPlainRecord,
  isHex32,
  isNetwork,
  SOURCE_AUTHORITY_ID,
  WATCHER_FINALITY_BOUNDS,
  WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
  type WatcherFinalityBoundObservation,
  type WatcherFinalityPolicy,
} from "./finality-engine.watcher-finality-reason-codes.js";

export const parseWatcherFinalityPolicy = (
  value: unknown,
): WatcherFinalityPolicy | null => {
  try {
    const custom =
      typeof value === "object" &&
      value !== null &&
      Object.getOwnPropertyDescriptor(value, "network")?.value === "Custom";
    const policy = exactPlainRecord(value, [
      "schemaVersion",
      "network",
      ...(custom ? ["customNetwork"] : []),
      "sourceMode",
      "authorityNodeId",
      "authorityGenesisIdentitySha256",
      "authorityChainSyncSocketPath",
      "localQueryServices",
      "externalProviders",
      "confirmationDepth",
      "maximumPreFinalityRollbackDepth",
      "maximumPostFinalityRecoveryDepth",
      "beforeFinalityRollback",
      "afterFinalityRollback",
      "blueprintHash",
      "deploymentMarker",
      "policyDigest",
    ]);
    const marker =
      policy === null ? null : cloneMarker(policy.deploymentMarker);
    const externalProviders =
      policy === null || policy.externalProviders === null
        ? null
        : cloneExternalProviders(policy.externalProviders);
    const localQueryServices =
      policy === null
        ? null
        : cloneLocalQueryServices(policy.localQueryServices);
    if (
      policy === null ||
      policy.schemaVersion !== WATCHER_FINALITY_POLICY_SCHEMA_VERSION ||
      !isNetwork(policy.network) ||
      (custom && policy.sourceMode !== "local_node") ||
      !["local_node", "external_providers"].includes(
        policy.sourceMode as string,
      ) ||
      !(
        (policy.sourceMode === "local_node" &&
          typeof policy.authorityNodeId === "string" &&
          SOURCE_AUTHORITY_ID.test(policy.authorityNodeId) &&
          isHex32(policy.authorityGenesisIdentitySha256) &&
          isExactAbsoluteSocketPath(policy.authorityChainSyncSocketPath) &&
          localQueryServices !== null &&
          policy.externalProviders === null) ||
        (policy.sourceMode === "external_providers" &&
          policy.authorityNodeId === null &&
          policy.authorityGenesisIdentitySha256 === null &&
          policy.authorityChainSyncSocketPath === null &&
          localQueryServices?.length === 0 &&
          externalProviders !== null)
      ) ||
      !isPositiveUint64(policy.confirmationDepth) ||
      BigInt(policy.confirmationDepth) >
        WATCHER_FINALITY_BOUNDS.confirmationDepth ||
      !isPositiveUint64(policy.maximumPreFinalityRollbackDepth) ||
      BigInt(policy.maximumPreFinalityRollbackDepth) >
        BigInt(policy.confirmationDepth) ||
      !isPositiveUint64(policy.maximumPostFinalityRecoveryDepth) ||
      BigInt(policy.maximumPostFinalityRecoveryDepth) !==
        BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K) ||
      policy.beforeFinalityRollback !== "rewind" ||
      policy.afterFinalityRollback !== "quarantine" ||
      !isHex32(policy.blueprintHash) ||
      marker === null ||
      !isHex32(policy.policyDigest)
    ) {
      return null;
    }
    const canonical = makePolicy({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
      network: policy.network,
      ...(custom
        ? { customNetwork: parseWatcherCustomNetwork(policy.customNetwork) }
        : {}),
      sourceMode: policy.sourceMode as WatcherFinalityPolicy["sourceMode"],
      authorityNodeId: policy.authorityNodeId as string | null,
      authorityGenesisIdentitySha256: policy.authorityGenesisIdentitySha256 as
        | string
        | null,
      authorityChainSyncSocketPath: policy.authorityChainSyncSocketPath as
        | string
        | null,
      localQueryServices:
        localQueryServices as WatcherFinalityPolicy["localQueryServices"],
      externalProviders,
      confirmationDepth: policy.confirmationDepth,
      maximumPreFinalityRollbackDepth: policy.maximumPreFinalityRollbackDepth,
      maximumPostFinalityRecoveryDepth: policy.maximumPostFinalityRecoveryDepth,
      beforeFinalityRollback: "rewind",
      afterFinalityRollback: "quarantine",
      blueprintHash: policy.blueprintHash,
      deploymentMarker: marker,
    });
    return canonical.policyDigest === policy.policyDigest ? canonical : null;
  } catch {
    return null;
  }
};

const parseVerifiedDeploymentIdentity = (
  value: unknown,
): VerifiedWatcherDeploymentIdentity | null => {
  try {
    const identity = exactPlainRecord(value, [
      "manifestId",
      "network",
      "trustRootId",
      "fundingProfileBundleDigest",
      "blueprintHash",
      "ruleBundleCommitment",
      "programCommitments",
      "durableMarker",
    ]);
    if (
      identity === null ||
      !isHex32(identity.manifestId) ||
      !isNetwork(identity.network) ||
      !isHex32(identity.trustRootId) ||
      !isHex32(identity.fundingProfileBundleDigest) ||
      !isHex32(identity.blueprintHash) ||
      !isHex32(identity.ruleBundleCommitment)
    ) {
      return null;
    }
    const commitments =
      typeof identity.programCommitments === "object" &&
      identity.programCommitments !== null &&
      !Array.isArray(identity.programCommitments)
        ? (identity.programCommitments as Record<string, unknown>)
        : null;
    if (
      commitments === null ||
      Reflect.ownKeys(commitments).length !== Object.keys(commitments).length ||
      Object.values(commitments).some((commitment) => !isHex32(commitment))
    ) {
      return null;
    }
    const marker = cloneMarker(identity.durableMarker);
    if (marker === null || marker.manifestId !== identity.manifestId) {
      return null;
    }
    return value as VerifiedWatcherDeploymentIdentity;
  } catch {
    return null;
  }
};

/**
 * Binds W01's strict finality configuration to W02's verified release and
 * durable deployment identity. The returned policy is the only policy shape
 * accepted by the transition engine.
 */
export const makeWatcherFinalityPolicy = (
  configInput: unknown,
  deploymentIdentityInput: unknown,
): WatcherFinalityPolicy | null => {
  try {
    const config: WatcherConfig = parseWatcherConfig(configInput);
    const identity = parseVerifiedDeploymentIdentity(deploymentIdentityInput);
    if (identity === null || identity.network !== config.targetNetwork) {
      return null;
    }
    const source = config.l1.source;
    const sourceAuthority =
      source.sourceMode === "local_node"
        ? {
            authorityNodeId: source.authorityNodeId,
            authorityGenesisIdentitySha256:
              source.chainSync.genesisIdentitySha256,
            authorityChainSyncSocketPath: source.chainSync.socketPath,
            localQueryServices: Object.freeze(
              source.queryServices.map((service) =>
                Object.freeze({
                  kind: service.kind,
                  providerId: service.identity,
                  endpoint: service.endpoint,
                }),
              ),
            ),
            externalProviders: null,
          }
        : {
            authorityNodeId: null,
            authorityGenesisIdentitySha256: null,
            authorityChainSyncSocketPath: null,
            localQueryServices: Object.freeze([]),
            externalProviders: Object.freeze(
              source.providers.map((provider) =>
                Object.freeze({
                  providerId: provider.identity,
                  operatorIdentitySha256: provider.operatorIdentitySha256,
                  endpoint: provider.endpoint,
                  authenticationKind: "https_tls_identity_v1" as const,
                }),
              ),
            ),
          };
    return makePolicy({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
      network: config.targetNetwork,
      ...(config.customNetwork === undefined
        ? {}
        : { customNetwork: config.customNetwork }),
      sourceMode: source.sourceMode,
      ...sourceAuthority,
      confirmationDepth: config.l1.finality.depth.toString(),
      maximumPreFinalityRollbackDepth:
        config.l1.finality.rollback.maxDepth.toString(),
      maximumPostFinalityRecoveryDepth:
        config.l1.finality.rollback.postFinalityRecoveryMaxDepth.toString(),
      beforeFinalityRollback: config.l1.finality.rollback.beforeFinality,
      afterFinalityRollback: config.l1.finality.rollback.afterFinality,
      blueprintHash: identity.blueprintHash,
      deploymentMarker: identity.durableMarker,
    });
  } catch {
    return null;
  }
};

export const parseBoundObservation = (
  value: unknown,
): WatcherFinalityBoundObservation | null => {
  const bound = exactPlainRecord(value, [
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "blockContentDigest",
    "firstSeenConsistencyDigest",
    "lastSeenConsistencyDigest",
    "firstSeenDepth",
    "currentDepth",
    "visibilityCount",
  ]);
  if (
    bound === null ||
    !isHex32(bound.pointDigest) ||
    !isHex32(bound.blockHash) ||
    !isUint64(bound.slot) ||
    !isUint64(bound.blockNo) ||
    !isHex32(bound.blockContentDigest) ||
    !isHex32(bound.firstSeenConsistencyDigest) ||
    !isHex32(bound.lastSeenConsistencyDigest) ||
    !isUint64(bound.firstSeenDepth) ||
    !isUint64(bound.currentDepth) ||
    !isPositiveUint64(bound.visibilityCount)
  ) {
    return null;
  }
  return Object.freeze({
    pointDigest: bound.pointDigest,
    blockHash: bound.blockHash,
    slot: bound.slot,
    blockNo: bound.blockNo,
    blockContentDigest: bound.blockContentDigest,
    firstSeenConsistencyDigest: bound.firstSeenConsistencyDigest,
    lastSeenConsistencyDigest: bound.lastSeenConsistencyDigest,
    firstSeenDepth: bound.firstSeenDepth,
    currentDepth: bound.currentDepth,
    visibilityCount: bound.visibilityCount,
  });
};

export const INCIDENT_REASONS = ["post_finality_point_changed"] as const;
