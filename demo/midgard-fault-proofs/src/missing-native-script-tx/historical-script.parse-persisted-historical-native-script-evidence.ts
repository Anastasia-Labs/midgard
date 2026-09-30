import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
} from "../workflow/raw-l1-snapshot.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../workflow/release-finality-policy.js";
import {
  admitCandidate,
  sealEvidence,
} from "./historical-script.admit-candidate.js";
import {
  canonicalString,
  digest,
  exact,
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  HISTORICAL_NATIVE_SCRIPT_SOURCE,
  type HistoricalNativeScriptEvidence,
  type HistoricalNativeScriptSource,
  type HistoricalNativeScriptSourceMode,
  samePoint,
  SCRIPT_HASH,
} from "./historical-script.admit-source-identities.js";

const admitEvidenceSources = ({
  value,
  sourceMode,
}: {
  readonly value: unknown;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
}): HistoricalNativeScriptEvidence["sources"] => {
  if (!Array.isArray(value)) {
    throw new Error(
      "historical native script evidence.sources must be an array",
    );
  }
  const requiredCount = sourceMode === "local_node" ? 1 : 2;
  const maximumCount = sourceMode === "local_node" ? 1 : 4;
  if (value.length < requiredCount || value.length > maximumCount) {
    throw new Error(
      sourceMode === "local_node"
        ? "historical native script evidence requires one local source"
        : "historical native script evidence requires two to four external sources",
    );
  }
  const sourceIds = new Set<string>();
  const operatorIdentities = new Set<string>();
  return Object.freeze(
    value.map((untrustedSource, index) => {
      const label = `historical native script evidence.sources[${index.toString()}]`;
      const source = exact(
        untrustedSource,
        ["sourceId", "operatorIdentitySha256"],
        label,
      );
      const sourceId = canonicalString(source.sourceId, `${label}.sourceId`);
      if (sourceId.length > 256 || sourceIds.has(sourceId)) {
        throw new Error(`${label}.sourceId is too long or duplicated`);
      }
      sourceIds.add(sourceId);
      if (sourceMode === "local_node") {
        if (source.operatorIdentitySha256 !== null) {
          throw new Error(`${label} has an external operator identity`);
        }
        return Object.freeze({ sourceId, operatorIdentitySha256: null });
      }
      const operatorIdentitySha256 = digest(
        source.operatorIdentitySha256,
        `${label}.operatorIdentitySha256`,
      );
      if (operatorIdentities.has(operatorIdentitySha256)) {
        throw new Error(
          "historical native script evidence external sources are not independent",
        );
      }
      operatorIdentities.add(operatorIdentitySha256);
      return Object.freeze({ sourceId, operatorIdentitySha256 });
    }),
  );
};

/** Strict structural parser used before live roster-backed reconfirmation. */
export const parsePersistedHistoricalNativeScriptEvidence = ({
  value,
  sourceMode,
  applicationOverlayDigest: expectedApplicationOverlayDigest,
  rosterDigest: expectedRosterDigest,
  expectedScriptHash,
  releaseFinality: untrustedReleaseFinality,
}: {
  readonly value: unknown;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly expectedScriptHash: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): HistoricalNativeScriptEvidence => {
  if (sourceMode !== "local_node" && sourceMode !== "external_providers") {
    throw new Error("historical native script evidence source mode is invalid");
  }
  if (!SCRIPT_HASH.test(expectedScriptHash)) {
    throw new Error(
      "historical native script hash must be 28-byte lowercase hex",
    );
  }
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "deploymentIdentityDigest",
      "blueprintHash",
      "finalityPolicyDigest",
      "expectedScriptHash",
      "scriptBytesHex",
      "publicationOutRef",
      "publicationOutputCbor",
      "publicationTransactionBodyCbor",
      "publicationTransactionIndex",
      "inclusionBlockTransactionIds",
      "inclusionPoint",
      "throughPoint",
      "confirmationDepth",
      "sourceMode",
      "applicationOverlayDigest",
      "rosterDigest",
      "sources",
      "evidenceDigest",
    ],
    "historical native script evidence",
  );
  if (
    parsed.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION ||
    parsed.sourceMode !== sourceMode
  ) {
    throw new Error(
      "historical native script evidence schema/source mode mismatch",
    );
  }
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    untrustedReleaseFinality,
  );
  const throughPoint = admitFraudProofRawL1Point(
    parsed.throughPoint,
    "historical native script persisted throughPoint",
  );
  const sources = admitEvidenceSources({ value: parsed.sources, sourceMode });
  const rosterDigest = digest(
    parsed.rosterDigest,
    "historical native script evidence.rosterDigest",
  );
  if (rosterDigest !== expectedRosterDigest) {
    throw new Error("historical native script evidence roster digest mismatch");
  }
  const applicationOverlayDigest = digest(
    parsed.applicationOverlayDigest,
    "historical native script evidence.applicationOverlayDigest",
  );
  if (applicationOverlayDigest !== expectedApplicationOverlayDigest) {
    throw new Error(
      "historical native script evidence application overlay digest mismatch",
    );
  }
  const primarySource = sources[0]!;
  const source: HistoricalNativeScriptSource = {
    sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE,
    sourceMode,
    sourceId: primarySource.sourceId,
    operatorIdentitySha256: primarySource.operatorIdentitySha256,
    resolveReferenceScriptPublication: async () => {
      throw new Error("persisted historical evidence cannot resolve history");
    },
    confirmCanonicalHistory: async () => {
      throw new Error("persisted historical evidence cannot confirm history");
    },
  };
  const candidate = admitCandidate({
    value: {
      schemaVersion: parsed.schemaVersion,
      deploymentIdentityDigest: parsed.deploymentIdentityDigest,
      blueprintHash: parsed.blueprintHash,
      finalityPolicyDigest: parsed.finalityPolicyDigest,
      expectedScriptHash: parsed.expectedScriptHash,
      sourceMode,
      sourceId: primarySource.sourceId,
      operatorIdentitySha256: primarySource.operatorIdentitySha256,
      scriptBytesHex: parsed.scriptBytesHex,
      publicationOutRef: parsed.publicationOutRef,
      publicationOutputCbor: parsed.publicationOutputCbor,
      publicationTransactionBodyCbor: parsed.publicationTransactionBodyCbor,
      publicationTransactionIndex: parsed.publicationTransactionIndex,
      inclusionBlockTransactionIds: parsed.inclusionBlockTransactionIds,
      inclusionPoint: parsed.inclusionPoint,
      throughPoint: parsed.throughPoint,
    },
    source,
    expectedScriptHash,
    throughPoint,
    releaseFinality,
  });
  if (
    !Number.isSafeInteger(parsed.confirmationDepth) ||
    parsed.confirmationDepth !== candidate.confirmationDepth
  ) {
    throw new Error(
      "historical native script evidence confirmation depth mismatch",
    );
  }
  const claimedDigest = digest(
    parsed.evidenceDigest,
    "historical native script evidence.evidenceDigest",
  );
  const evidence = sealEvidence({
    candidate,
    sourceMode,
    applicationOverlayDigest,
    rosterDigest,
    sources,
  });
  if (claimedDigest !== evidence.evidenceDigest) {
    throw new Error("historical native script evidence digest mismatch");
  }
  return evidence;
};

export const confirmSourceHistory = async ({
  source,
  inclusionPoint,
  throughPoint,
}: {
  readonly source: HistoricalNativeScriptSource;
  readonly inclusionPoint: FraudProofRawL1Point;
  readonly throughPoint: FraudProofRawL1Point;
}): Promise<void> => {
  const parsed = exact(
    await source.confirmCanonicalHistory({ inclusionPoint, throughPoint }),
    ["canonical", "inclusionPoint", "throughPoint"],
    `historical native script confirmation ${source.sourceId}`,
  );
  const confirmedInclusionPoint = admitFraudProofRawL1Point(
    parsed.inclusionPoint,
    `historical native script confirmation ${source.sourceId}.inclusionPoint`,
  );
  const confirmedThroughPoint = admitFraudProofRawL1Point(
    parsed.throughPoint,
    `historical native script confirmation ${source.sourceId}.throughPoint`,
  );
  if (
    parsed.canonical !== true ||
    !samePoint(confirmedInclusionPoint, inclusionPoint) ||
    !samePoint(confirmedThroughPoint, throughPoint)
  ) {
    throw new Error(
      `historical native script source ${source.sourceId} rolled back during resolution`,
    );
  }
};
