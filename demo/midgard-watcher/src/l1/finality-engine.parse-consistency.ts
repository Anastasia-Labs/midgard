import {
  isExactAbsoluteSocketPath,
  isUint64,
  sha256Canonical,
} from "./finality-engine.clone-external-providers.js";
import { parseLocalQueryServiceBindings } from "./finality-engine.parse-local-query-service-bindings.js";
import {
  parseExternalProviderBindings,
  parseStringArray,
  sameStringArray,
} from "./finality-engine.parse-watcher-finality-state.js";
import {
  type Agreement,
  exactArray,
  exactPlainRecord,
  isHex32,
  isNetwork,
  type ParsedConsistency,
  SOURCE_AUTHORITY_ID,
  type WatcherFinalityBoundObservation,
} from "./finality-engine.watcher-finality-reason-codes.js";
import {
  WATCHER_MULTI_PROVIDER_ALERT_CODES,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
  WATCHER_MULTI_PROVIDER_REASON_CODES,
} from "./multi-provider-consistency.js";

export const parseConsistency = (value: unknown): ParsedConsistency | null => {
  try {
    const result = exactPlainRecord(value, [
      "schemaVersion",
      "status",
      "protocolDecision",
      "sourceMode",
      "configuredNetwork",
      "configuredSourceDigest",
      "authorityNodeId",
      "authorityGenesisIdentitySha256",
      "authorityChainSyncSocketPath",
      "chainAuthorityObservationDigest",
      "queryObservationCount",
      "observationCount",
      "independentProviderCount",
      "externalProviderBindings",
      "localQueryServiceBindings",
      "reasonCodes",
      "alertCodes",
      "observationEvidenceDigests",
      "rejectedObservationCount",
      "agreement",
      "consistencyDigest",
    ]);
    if (
      result === null ||
      result.schemaVersion !==
        WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION ||
      !["agreed", "pending", "quarantined"].includes(result.status as string) ||
      !["local_node", "external_providers"].includes(
        result.sourceMode as string,
      ) ||
      !(
        result.configuredNetwork === null || isNetwork(result.configuredNetwork)
      ) ||
      !isHex32(result.configuredSourceDigest) ||
      !Number.isSafeInteger(result.queryObservationCount) ||
      (result.queryObservationCount as number) < 0 ||
      !Number.isSafeInteger(result.observationCount) ||
      (result.observationCount as number) < 0 ||
      (result.observationCount as number) >
        WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations ||
      !Number.isSafeInteger(result.independentProviderCount) ||
      (result.independentProviderCount as number) < 0 ||
      (result.independentProviderCount as number) >
        (result.observationCount as number) ||
      !Number.isSafeInteger(result.rejectedObservationCount) ||
      (result.rejectedObservationCount as number) < 0 ||
      (result.rejectedObservationCount as number) >
        (result.observationCount as number) ||
      !isHex32(result.consistencyDigest)
    ) {
      return null;
    }
    const reasons = parseStringArray(
      result.reasonCodes,
      WATCHER_MULTI_PROVIDER_REASON_CODES,
    );
    const alerts = parseStringArray(
      result.alertCodes,
      WATCHER_MULTI_PROVIDER_ALERT_CODES,
    );
    const externalProviderBindings = parseExternalProviderBindings(
      result.externalProviderBindings,
    );
    const localQueryServiceBindings = parseLocalQueryServiceBindings(
      result.localQueryServiceBindings,
    );
    const evidence = parseStringArray(result.observationEvidenceDigests, []);
    const evidenceArray =
      evidence === null
        ? exactArray(result.observationEvidenceDigests)
        : evidence;
    if (
      reasons === null ||
      alerts === null ||
      externalProviderBindings === null ||
      localQueryServiceBindings === null ||
      evidenceArray === null ||
      evidenceArray.some((digest) => !isHex32(digest)) ||
      new Set(evidenceArray).size !== evidenceArray.length ||
      !sameStringArray(
        reasons,
        WATCHER_MULTI_PROVIDER_REASON_CODES.filter((code) =>
          reasons.includes(code),
        ),
      ) ||
      !sameStringArray(
        alerts,
        WATCHER_MULTI_PROVIDER_ALERT_CODES.filter((code) =>
          alerts.includes(code),
        ),
      ) ||
      !sameStringArray(
        evidenceArray as readonly string[],
        [...(evidenceArray as readonly string[])].sort(),
      )
    ) {
      return null;
    }
    const sourceMode = result.sourceMode as "local_node" | "external_providers";
    const sourceShapeValid =
      sourceMode === "local_node"
        ? typeof result.authorityNodeId === "string" &&
          SOURCE_AUTHORITY_ID.test(result.authorityNodeId) &&
          isHex32(result.authorityGenesisIdentitySha256) &&
          isExactAbsoluteSocketPath(result.authorityChainSyncSocketPath) &&
          ((result.chainAuthorityObservationDigest === null &&
            result.status !== "agreed") ||
            (isHex32(result.chainAuthorityObservationDigest) &&
              evidenceArray.includes(
                result.chainAuthorityObservationDigest,
              ))) &&
          result.queryObservationCount ===
            localQueryServiceBindings.filter(
              ({ observationDigest }) => observationDigest !== null,
            ).length &&
          externalProviderBindings.length === 0 &&
          localQueryServiceBindings.length <= 8
        : result.authorityNodeId === null &&
          result.authorityGenesisIdentitySha256 === null &&
          result.authorityChainSyncSocketPath === null &&
          result.chainAuthorityObservationDigest === null &&
          result.queryObservationCount === 0 &&
          localQueryServiceBindings.length === 0 &&
          externalProviderBindings.length <=
            (result.observationCount as number);
    if (!sourceShapeValid) {
      return null;
    }

    let agreementCanonical: Record<string, unknown> | null = null;
    let agreement: Agreement | null = null;
    if (result.agreement !== null) {
      const parsed = exactPlainRecord(result.agreement, [
        "pointDigest",
        "blockHash",
        "slot",
        "blockNo",
        "minimumDepth",
        "blockContentDigest",
      ]);
      if (
        parsed === null ||
        !isHex32(parsed.pointDigest) ||
        !isHex32(parsed.blockHash) ||
        !isUint64(parsed.slot) ||
        !isUint64(parsed.blockNo) ||
        !isUint64(parsed.minimumDepth) ||
        !isHex32(parsed.blockContentDigest) ||
        !isNetwork(result.configuredNetwork)
      ) {
        return null;
      }
      agreementCanonical = {
        pointDigest: parsed.pointDigest,
        blockHash: parsed.blockHash,
        slot: parsed.slot,
        blockNo: parsed.blockNo,
        minimumDepth: parsed.minimumDepth,
        blockContentDigest: parsed.blockContentDigest,
      };
      agreement = Object.freeze({
        configuredNetwork: result.configuredNetwork,
        ...agreementCanonical,
        consistencyDigest: result.consistencyDigest,
      }) as Agreement;
    }
    const canonicalWithoutDigest = {
      schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
      status: result.status,
      protocolDecision: result.protocolDecision,
      sourceMode,
      configuredNetwork: result.configuredNetwork,
      configuredSourceDigest: result.configuredSourceDigest,
      authorityNodeId: result.authorityNodeId,
      authorityGenesisIdentitySha256: result.authorityGenesisIdentitySha256,
      authorityChainSyncSocketPath: result.authorityChainSyncSocketPath,
      chainAuthorityObservationDigest: result.chainAuthorityObservationDigest,
      queryObservationCount: result.queryObservationCount,
      observationCount: result.observationCount,
      independentProviderCount: result.independentProviderCount,
      externalProviderBindings,
      localQueryServiceBindings,
      reasonCodes: reasons,
      alertCodes: alerts,
      observationEvidenceDigests: evidenceArray,
      rejectedObservationCount: result.rejectedObservationCount,
      agreement: agreementCanonical,
    };
    if (sha256Canonical(canonicalWithoutDigest) !== result.consistencyDigest) {
      return null;
    }
    if (result.status === "agreed") {
      const agreementShapeValid =
        sourceMode === "local_node"
          ? (result.observationCount as number) >= 1 &&
            result.independentProviderCount === 1 &&
            localQueryServiceBindings.every(
              ({ observationStatus }) => observationStatus === "aligned",
            ) &&
            sameStringArray(reasons, ["local_node_consistent"])
          : (result.observationCount as number) >= 2 &&
            (result.independentProviderCount as number) >= 2 &&
            externalProviderBindings.length ===
              (result.observationCount as number) &&
            sameStringArray(reasons, ["providers_consistent"]);
      if (
        result.protocolDecision !== "allowed" ||
        agreement === null ||
        !agreementShapeValid ||
        result.rejectedObservationCount !== 0 ||
        alerts.length !== 0 ||
        evidenceArray.length !== result.observationCount
      ) {
        return null;
      }
      return Object.freeze({
        kind: "agreed",
        sourceMode,
        configuredSourceDigest: result.configuredSourceDigest,
        authorityNodeId: result.authorityNodeId as string | null,
        authorityGenesisIdentitySha256:
          result.authorityGenesisIdentitySha256 as string | null,
        authorityChainSyncSocketPath: result.authorityChainSyncSocketPath as
          | string
          | null,
        externalProviderBindings,
        localQueryServiceBindings,
        agreement,
      });
    }
    if (
      result.protocolDecision !== "quarantined" ||
      result.agreement !== null
    ) {
      return null;
    }
    return Object.freeze({
      kind: result.status as "pending" | "quarantined",
      sourceMode,
      configuredSourceDigest: result.configuredSourceDigest,
      authorityNodeId: result.authorityNodeId as string | null,
      authorityGenesisIdentitySha256: result.authorityGenesisIdentitySha256 as
        | string
        | null,
      authorityChainSyncSocketPath: result.authorityChainSyncSocketPath as
        | string
        | null,
      externalProviderBindings,
      localQueryServiceBindings,
      consistencyDigest: result.consistencyDigest,
    });
  } catch {
    return null;
  }
};

export const makeBoundObservation = (
  agreement: Agreement,
  existing: WatcherFinalityBoundObservation | null,
): WatcherFinalityBoundObservation =>
  Object.freeze({
    pointDigest: agreement.pointDigest,
    blockHash: agreement.blockHash,
    slot: agreement.slot,
    blockNo: agreement.blockNo,
    blockContentDigest: agreement.blockContentDigest,
    firstSeenConsistencyDigest:
      existing?.firstSeenConsistencyDigest ?? agreement.consistencyDigest,
    lastSeenConsistencyDigest: agreement.consistencyDigest,
    firstSeenDepth: existing?.firstSeenDepth ?? agreement.minimumDepth,
    currentDepth: agreement.minimumDepth,
    visibilityCount:
      existing === null
        ? "1"
        : (BigInt(existing.visibilityCount) + 1n).toString(),
  });
