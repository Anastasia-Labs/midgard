import { createHash } from "node:crypto";

import { missingNativeScriptTxVersionedScriptHashV1 } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  type ProductionHistoricalNativeScriptProviderRosterV1,
  requireProductionHistoricalNativeScriptProviderRosterV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  admitFraudProofRawL1PointV1,
  type FraudProofRawL1PointV1,
} from "../workflow/raw-l1-snapshot-v1.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicyV1,
  type VerifiedFraudProofReleaseFinalityPolicyV1,
} from "../workflow/release-finality-policy-v1.js";

export const HISTORICAL_NATIVE_SCRIPT_SOURCE_V1 =
  "midgard-historical-native-script-source-v1" as const;
export const HISTORICAL_NATIVE_SCRIPT_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-historical-native-script-evidence-v1" as const;
export const HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER_V1 =
  "midgard-historical-native-script-source-roster-v1" as const;

export type HistoricalNativeScriptSourceModeV1 =
  | "local_node"
  | "external_providers";

/**
 * Application-owned authenticated L1 history port.
 *
 * A local implementation must cross-check Kupo history with raw Ogmios block
 * CBOR. An external implementation represents one independently authenticated
 * provider; the coordinator below requires exact quorum agreement. Returned
 * values remain untrusted and are decoded again in this package.
 */
export interface HistoricalNativeScriptSourceV1 {
  readonly sourceVersion: typeof HISTORICAL_NATIVE_SCRIPT_SOURCE_V1;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly sourceId: string;
  readonly operatorIdentitySha256: string | null;
  resolveReferenceScriptPublication(input: {
    readonly deploymentIdentityDigest: string;
    readonly releaseIdentityDigest: string;
    readonly finalityPolicyDigest: string;
    readonly expectedScriptHash: string;
    readonly throughPoint: FraudProofRawL1PointV1;
  }): Promise<unknown>;
  /** Reconfirms inclusion ancestry and the pinned boundary after every read. */
  confirmCanonicalHistory(input: {
    readonly inclusionPoint: FraudProofRawL1PointV1;
    readonly throughPoint: FraudProofRawL1PointV1;
  }): Promise<unknown>;
}

export type HistoricalNativeScriptSourceRosterV1 = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER_V1;
  sourceMode: HistoricalNativeScriptSourceModeV1;
  /** Digest of the single immutable application-installed history overlay. */
  applicationOverlayDigest: string;
  deploymentIdentityDigest: string;
  releaseIdentityDigest: string;
  finalityPolicyDigest: string;
  sources: readonly Readonly<{
    sourceId: string;
    operatorIdentitySha256: string | null;
  }>[];
  rosterDigest: string;
}>;

export type HistoricalNativeScriptEvidenceV1 = Readonly<{
  readonly schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_EVIDENCE_V1_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly releaseIdentityDigest: string;
  readonly finalityPolicyDigest: string;
  readonly expectedScriptHash: string;
  /** Canonical Cardano NativeScript CBOR, ready for step-05. */
  readonly scriptBytesHex: string;
  readonly publicationOutRef: string;
  readonly publicationOutputCbor: string;
  readonly publicationTransactionBodyCbor: string;
  readonly publicationTransactionIndex: number;
  /** Exact raw-block transaction order at `inclusionPoint`. */
  readonly inclusionBlockTransactionIds: readonly string[];
  readonly inclusionPoint: FraudProofRawL1PointV1;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly confirmationDepth: number;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: readonly Readonly<{
    readonly sourceId: string;
    readonly operatorIdentitySha256: string | null;
  }>[];
  /** Digest persisted in the prepared workflow artifact before submission. */
  readonly evidenceDigest: string;
}>;

const admittedHistoricalNativeScriptEvidenceV1 = new WeakSet<object>();
const admittedHistoricalNativeScriptRostersV1 = new WeakMap<
  object,
  readonly HistoricalNativeScriptSourceV1[]
>();
const admittedProductionHistoricalNativeScriptRostersV1 = new WeakSet<object>();

const DIGEST = /^[0-9a-f]{64}$/u;
const SCRIPT_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
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

const canonicalString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

const digest = (value: unknown, label: string): string => {
  const parsed = canonicalString(value, label);
  if (!DIGEST.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

const cbor = (value: unknown, label: string): string => {
  const parsed = canonicalString(value, label);
  if (!EVEN_HEX.test(parsed))
    throw new Error(`${label} must be lowercase CBOR`);
  return parsed;
};

const samePoint = (
  left: FraudProofRawL1PointV1,
  right: FraudProofRawL1PointV1,
): boolean =>
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockHash === right.blockHash &&
  left.pointId === right.pointId;

const admitSourceIdentities = ({
  sourceMode,
  sources,
}: {
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly sources: readonly HistoricalNativeScriptSourceV1[];
}): HistoricalNativeScriptSourceRosterV1["sources"] => {
  const requiredCount = sourceMode === "local_node" ? 1 : 2;
  const maximumCount = sourceMode === "local_node" ? 1 : 4;
  if (sources.length < requiredCount || sources.length > maximumCount) {
    throw new Error(
      sourceMode === "local_node"
        ? "local historical script roster requires one Kupo/Ogmios authority"
        : "external historical script roster requires two to four independent providers",
    );
  }
  const sourceIds = new Set<string>();
  const operatorIdentities = new Set<string>();
  return Object.freeze(
    sources.map((source) => {
      if (
        source.sourceVersion !== HISTORICAL_NATIVE_SCRIPT_SOURCE_V1 ||
        source.sourceMode !== sourceMode ||
        source.sourceId.trim() !== source.sourceId ||
        source.sourceId.length === 0 ||
        sourceIds.has(source.sourceId)
      ) {
        throw new Error(
          "historical native script source identity is invalid or duplicated",
        );
      }
      sourceIds.add(source.sourceId);
      if (sourceMode === "local_node") {
        if (source.operatorIdentitySha256 !== null) {
          throw new Error(
            "local historical script source has an external operator identity",
          );
        }
      } else {
        const operatorIdentity = digest(
          source.operatorIdentitySha256,
          `historical native script source ${source.sourceId}.operatorIdentitySha256`,
        );
        if (operatorIdentities.has(operatorIdentity)) {
          throw new Error(
            "external historical script providers are not independent",
          );
        }
        operatorIdentities.add(operatorIdentity);
      }
      return Object.freeze({
        sourceId: source.sourceId,
        operatorIdentitySha256: source.operatorIdentitySha256,
      });
    }),
  );
};

/**
 * Immutable application-installed authority roster. Production resolution and
 * restart admission accept only this module-minted object, never a structural
 * array of callbacks supplied alongside a workflow invocation.
 */
const createHistoricalNativeScriptSourceRosterV1 = ({
  sourceMode,
  sources,
  applicationOverlayDigest: untrustedApplicationOverlayDigest,
  releaseFinality: untrustedReleaseFinality,
}: {
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly sources: readonly HistoricalNativeScriptSourceV1[];
  readonly applicationOverlayDigest: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): HistoricalNativeScriptSourceRosterV1 => {
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicyV1(
    untrustedReleaseFinality,
  );
  const applicationOverlayDigest = digest(
    untrustedApplicationOverlayDigest,
    "historical native script application overlay digest",
  );
  const identities = admitSourceIdentities({ sourceMode, sources });
  const withoutDigest = Object.freeze({
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER_V1,
    sourceMode,
    applicationOverlayDigest,
    deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
    releaseIdentityDigest: releaseFinality.releaseIdentityDigest,
    finalityPolicyDigest: releaseFinality.policyDigest,
    sources: identities,
  });
  const roster = Object.freeze({
    ...withoutDigest,
    rosterDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedHistoricalNativeScriptRostersV1.set(
    roster,
    Object.freeze([...sources]),
  );
  return roster;
};

/** Explicit callback seam for resolver unit tests; production rejects it. */
export const unsafeCreateHistoricalNativeScriptSourceRosterForTestV1 =
  createHistoricalNativeScriptSourceRosterV1;

const postHistoricalNativeScriptJsonV1 = async ({
  authorityEndpoint,
  path,
  body,
  sourceId,
}: {
  readonly authorityEndpoint: string;
  readonly path: string;
  readonly body: unknown;
  readonly sourceId: string;
}): Promise<unknown> => {
  const response = await fetch(`${authorityEndpoint}${path}`, {
    method: "POST",
    headers: {
      accept: "application/json",
      "content-type": "application/json",
    },
    body: JSON.stringify(body),
    signal: AbortSignal.timeout(30_000),
  });
  if (!response.ok) {
    throw new Error(
      `historical native-script provider ${sourceId} returned HTTP ${response.status.toString()}`,
    );
  }
  return (await response.json()) as unknown;
};

/**
 * Concrete production quorum derived only from the admitted immutable history
 * overlay. No callback or source identity is accepted alongside a workflow.
 */
export const createProductionExternalHistoricalNativeScriptSourceRosterV1 = ({
  providerRoster: untrustedProviderRoster,
  releaseFinality,
}: {
  readonly providerRoster: ProductionHistoricalNativeScriptProviderRosterV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): HistoricalNativeScriptSourceRosterV1 => {
  const providerRoster =
    requireProductionHistoricalNativeScriptProviderRosterV1(
      untrustedProviderRoster,
    );
  const sources = providerRoster.providers.map(
    (provider): HistoricalNativeScriptSourceV1 =>
      Object.freeze({
        sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE_V1,
        sourceMode: "external_providers",
        sourceId: provider.sourceId,
        operatorIdentitySha256: provider.operatorIdentitySha256,
        resolveReferenceScriptPublication: async (
          request: Parameters<
            HistoricalNativeScriptSourceV1["resolveReferenceScriptPublication"]
          >[0],
        ) =>
          await postHistoricalNativeScriptJsonV1({
            authorityEndpoint: provider.authorityEndpoint,
            path: "/midgard/v1/native-script-publication",
            body: request,
            sourceId: provider.sourceId,
          }),
        confirmCanonicalHistory: async (
          request: Parameters<
            HistoricalNativeScriptSourceV1["confirmCanonicalHistory"]
          >[0],
        ) =>
          await postHistoricalNativeScriptJsonV1({
            authorityEndpoint: provider.authorityEndpoint,
            path: "/midgard/v1/native-script-publication/canonicality",
            body: request,
            sourceId: provider.sourceId,
          }),
      }),
  );
  const roster = createHistoricalNativeScriptSourceRosterV1({
    sourceMode: "external_providers",
    sources,
    applicationOverlayDigest: providerRoster.rosterDigest,
    releaseFinality,
  });
  admittedProductionHistoricalNativeScriptRostersV1.add(roster);
  return roster;
};

export const requireProductionHistoricalNativeScriptSourceRosterV1 = (
  roster: HistoricalNativeScriptSourceRosterV1,
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1,
): HistoricalNativeScriptSourceRosterV1 => {
  const verifiedFinality =
    validateVerifiedFraudProofReleaseFinalityPolicyV1(releaseFinality);
  if (!admittedProductionHistoricalNativeScriptRostersV1.has(roster)) {
    throw new Error(
      "historical native-script source roster is not a concrete production authority",
    );
  }
  requireSourceRoster({ roster, releaseFinality: verifiedFinality });
  return roster;
};

const requireSourceRoster = ({
  roster,
  releaseFinality,
}: {
  readonly roster: HistoricalNativeScriptSourceRosterV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): readonly HistoricalNativeScriptSourceV1[] => {
  const sources = admittedHistoricalNativeScriptRostersV1.get(roster);
  if (
    sources === undefined ||
    roster.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    roster.releaseIdentityDigest !== releaseFinality.releaseIdentityDigest ||
    roster.finalityPolicyDigest !== releaseFinality.policyDigest ||
    roster.rosterDigest !==
      createHash("sha256")
        .update(
          JSON.stringify({
            schemaVersion: roster.schemaVersion,
            sourceMode: roster.sourceMode,
            applicationOverlayDigest: roster.applicationOverlayDigest,
            deploymentIdentityDigest: roster.deploymentIdentityDigest,
            releaseIdentityDigest: roster.releaseIdentityDigest,
            finalityPolicyDigest: roster.finalityPolicyDigest,
            sources: roster.sources,
          }),
        )
        .digest("hex")
  ) {
    throw new Error(
      "historical native script source roster is not the installed release authority",
    );
  }
  return sources;
};

type AdmittedCandidateV1 = Omit<
  HistoricalNativeScriptEvidenceV1,
  | "schemaVersion"
  | "sourceMode"
  | "applicationOverlayDigest"
  | "rosterDigest"
  | "sources"
  | "evidenceDigest"
>;

const admitCandidate = ({
  value,
  source,
  expectedScriptHash,
  throughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly source: HistoricalNativeScriptSourceV1;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): AdmittedCandidateV1 => {
  const label = `historical native script source ${source.sourceId}`;
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "deploymentIdentityDigest",
      "releaseIdentityDigest",
      "finalityPolicyDigest",
      "expectedScriptHash",
      "sourceMode",
      "sourceId",
      "operatorIdentitySha256",
      "scriptBytesHex",
      "publicationOutRef",
      "publicationOutputCbor",
      "publicationTransactionBodyCbor",
      "publicationTransactionIndex",
      "inclusionBlockTransactionIds",
      "inclusionPoint",
      "throughPoint",
    ],
    label,
  );
  if (
    parsed.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_EVIDENCE_V1_SCHEMA_VERSION
  ) {
    throw new Error(`${label} has an unsupported schema`);
  }
  if (
    parsed.sourceMode !== source.sourceMode ||
    parsed.sourceId !== source.sourceId ||
    parsed.operatorIdentitySha256 !== source.operatorIdentitySha256
  ) {
    throw new Error(`${label} changed its authenticated provider identity`);
  }
  const deploymentIdentityDigest = digest(
    parsed.deploymentIdentityDigest,
    `${label}.deploymentIdentityDigest`,
  );
  const releaseIdentityDigest = digest(
    parsed.releaseIdentityDigest,
    `${label}.releaseIdentityDigest`,
  );
  const finalityPolicyDigest = digest(
    parsed.finalityPolicyDigest,
    `${label}.finalityPolicyDigest`,
  );
  if (
    deploymentIdentityDigest !== releaseFinality.deploymentIdentityDigest ||
    releaseIdentityDigest !== releaseFinality.releaseIdentityDigest ||
    finalityPolicyDigest !== releaseFinality.policyDigest ||
    parsed.expectedScriptHash !== expectedScriptHash
  ) {
    throw new Error(`${label} changed the release/script identity`);
  }
  const admittedThroughPoint = admitFraudProofRawL1PointV1(
    parsed.throughPoint,
    `${label}.throughPoint`,
  );
  if (!samePoint(admittedThroughPoint, throughPoint)) {
    throw new Error(`${label} changed the pinned historical boundary`);
  }
  const inclusionPoint = admitFraudProofRawL1PointV1(
    parsed.inclusionPoint,
    `${label}.inclusionPoint`,
  );
  const inclusionBlock = BigInt(inclusionPoint.blockNo);
  const throughBlock = BigInt(throughPoint.blockNo);
  if (
    inclusionBlock > throughBlock ||
    BigInt(inclusionPoint.slot) > BigInt(throughPoint.slot) ||
    (inclusionBlock === throughBlock &&
      !samePoint(inclusionPoint, throughPoint))
  ) {
    throw new Error(
      `${label} placed the script publication after the boundary`,
    );
  }
  const confirmationDepth = throughBlock - inclusionBlock + 1n;
  if (
    confirmationDepth < BigInt(releaseFinality.policy.confirmationDepth) ||
    confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(`${label} script publication is below release finality`);
  }
  const publicationOutRef = canonicalString(
    parsed.publicationOutRef,
    `${label}.publicationOutRef`,
  );
  const outRefMatch = OUT_REF.exec(publicationOutRef);
  if (outRefMatch === null) {
    throw new Error(`${label}.publicationOutRef is not canonical`);
  }
  const publicationOutputCbor = cbor(
    parsed.publicationOutputCbor,
    `${label}.publicationOutputCbor`,
  );
  const publicationTransactionBodyCbor = cbor(
    parsed.publicationTransactionBodyCbor,
    `${label}.publicationTransactionBodyCbor`,
  );
  if (
    !Number.isSafeInteger(parsed.publicationTransactionIndex) ||
    (parsed.publicationTransactionIndex as number) < 0
  ) {
    throw new Error(`${label}.publicationTransactionIndex is invalid`);
  }
  if (
    !Array.isArray(parsed.inclusionBlockTransactionIds) ||
    parsed.inclusionBlockTransactionIds.length === 0 ||
    parsed.inclusionBlockTransactionIds.length > 10_000
  ) {
    throw new Error(`${label}.inclusionBlockTransactionIds is not bounded`);
  }
  const inclusionBlockTransactionIds = parsed.inclusionBlockTransactionIds.map(
    (candidate, index) =>
      digest(
        candidate,
        `${label}.inclusionBlockTransactionIds[${index.toString()}]`,
      ),
  );
  let output: CML.TransactionOutput;
  let body: CML.TransactionBody;
  try {
    output = CML.TransactionOutput.from_cbor_hex(publicationOutputCbor);
    body = CML.TransactionBody.from_cbor_hex(publicationTransactionBodyCbor);
  } catch {
    throw new Error(`${label} contains invalid Cardano CBOR`);
  }
  if (
    output.to_canonical_cbor_hex() !== publicationOutputCbor ||
    body.to_canonical_cbor_hex() !== publicationTransactionBodyCbor ||
    CML.hash_transaction(body).to_hex() !== outRefMatch[1]
  ) {
    throw new Error(`${label} contains non-canonical or hash-mismatched CBOR`);
  }
  const outputIndex = Number(outRefMatch[2]);
  const publicationTransactionIndex =
    parsed.publicationTransactionIndex as number;
  if (
    publicationTransactionIndex >= inclusionBlockTransactionIds.length ||
    inclusionBlockTransactionIds[publicationTransactionIndex] !==
      outRefMatch[1] ||
    inclusionBlockTransactionIds.filter((txHash) => txHash === outRefMatch[1])
      .length !== 1
  ) {
    throw new Error(
      `${label} publication body is not uniquely placed in the raw block`,
    );
  }
  const outputs = body.outputs();
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex >= outputs.len() ||
    outputs.get(outputIndex).to_canonical_cbor_hex() !== publicationOutputCbor
  ) {
    throw new Error(
      `${label} publication outRef does not name the exact output`,
    );
  }
  const referenceScript = output.script_ref();
  const nativeScript = referenceScript?.as_native();
  if (referenceScript === undefined || nativeScript === undefined) {
    throw new Error(`${label} publication is not a native reference script`);
  }
  const scriptBytesHex = nativeScript.to_canonical_cbor_hex();
  if (
    parsed.scriptBytesHex !== scriptBytesHex ||
    missingNativeScriptTxVersionedScriptHashV1(
      Buffer.from(scriptBytesHex, "hex"),
    ) !== expectedScriptHash
  ) {
    throw new Error(`${label} native script preimage has a substituted hash`);
  }
  return {
    deploymentIdentityDigest,
    releaseIdentityDigest,
    finalityPolicyDigest,
    expectedScriptHash,
    scriptBytesHex,
    publicationOutRef,
    publicationOutputCbor,
    publicationTransactionBodyCbor,
    publicationTransactionIndex,
    inclusionBlockTransactionIds,
    inclusionPoint,
    throughPoint,
    confirmationDepth: Number(confirmationDepth),
  };
};

const candidateDigest = (candidate: AdmittedCandidateV1): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        deploymentIdentityDigest: candidate.deploymentIdentityDigest,
        releaseIdentityDigest: candidate.releaseIdentityDigest,
        finalityPolicyDigest: candidate.finalityPolicyDigest,
        expectedScriptHash: candidate.expectedScriptHash,
        scriptBytesHex: candidate.scriptBytesHex,
        publicationOutRef: candidate.publicationOutRef,
        publicationOutputCbor: candidate.publicationOutputCbor,
        publicationTransactionBodyCbor:
          candidate.publicationTransactionBodyCbor,
        publicationTransactionIndex: candidate.publicationTransactionIndex,
        inclusionBlockTransactionIds: candidate.inclusionBlockTransactionIds,
        inclusionPoint: candidate.inclusionPoint,
        throughPoint: candidate.throughPoint,
        confirmationDepth: candidate.confirmationDepth,
      }),
    )
    .digest("hex");

const evidenceWithoutDigest = ({
  candidate,
  sourceMode,
  applicationOverlayDigest,
  rosterDigest,
  sources,
}: {
  readonly candidate: AdmittedCandidateV1;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: HistoricalNativeScriptEvidenceV1["sources"];
}): Omit<HistoricalNativeScriptEvidenceV1, "evidenceDigest"> => ({
  schemaVersion: HISTORICAL_NATIVE_SCRIPT_EVIDENCE_V1_SCHEMA_VERSION,
  ...candidate,
  sourceMode,
  applicationOverlayDigest,
  rosterDigest,
  sources,
});

const computeEvidenceDigest = (
  value: Omit<HistoricalNativeScriptEvidenceV1, "evidenceDigest">,
): string => createHash("sha256").update(JSON.stringify(value)).digest("hex");

const freezeCandidate = (candidate: AdmittedCandidateV1): AdmittedCandidateV1 =>
  Object.freeze({
    ...candidate,
    inclusionBlockTransactionIds: Object.freeze([
      ...candidate.inclusionBlockTransactionIds,
    ]),
    inclusionPoint: Object.freeze({ ...candidate.inclusionPoint }),
    throughPoint: Object.freeze({ ...candidate.throughPoint }),
  });

const sealEvidence = ({
  candidate: unsealedCandidate,
  sourceMode,
  applicationOverlayDigest,
  rosterDigest,
  sources,
}: {
  readonly candidate: AdmittedCandidateV1;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: HistoricalNativeScriptEvidenceV1["sources"];
}): HistoricalNativeScriptEvidenceV1 => {
  const candidate = freezeCandidate(unsealedCandidate);
  const withoutDigest = evidenceWithoutDigest({
    candidate,
    sourceMode,
    applicationOverlayDigest,
    rosterDigest,
    sources,
  });
  const evidence = Object.freeze({
    ...withoutDigest,
    evidenceDigest: computeEvidenceDigest(withoutDigest),
  });
  admittedHistoricalNativeScriptEvidenceV1.add(evidence);
  return evidence;
};

const admitEvidenceSources = ({
  value,
  sourceMode,
}: {
  readonly value: unknown;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
}): HistoricalNativeScriptEvidenceV1["sources"] => {
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
const parsePersistedHistoricalNativeScriptEvidenceV1 = ({
  value,
  sourceMode,
  applicationOverlayDigest: expectedApplicationOverlayDigest,
  rosterDigest: expectedRosterDigest,
  expectedScriptHash,
  throughPoint: untrustedThroughPoint,
  releaseFinality: untrustedReleaseFinality,
}: {
  readonly value: unknown;
  readonly sourceMode: HistoricalNativeScriptSourceModeV1;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): HistoricalNativeScriptEvidenceV1 => {
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
      "releaseIdentityDigest",
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
    parsed.schemaVersion !==
      HISTORICAL_NATIVE_SCRIPT_EVIDENCE_V1_SCHEMA_VERSION ||
    parsed.sourceMode !== sourceMode
  ) {
    throw new Error(
      "historical native script evidence schema/source mode mismatch",
    );
  }
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicyV1(
    untrustedReleaseFinality,
  );
  const throughPoint = admitFraudProofRawL1PointV1(
    untrustedThroughPoint,
    "historical native script expected throughPoint",
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
  const source: HistoricalNativeScriptSourceV1 = {
    sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE_V1,
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
      releaseIdentityDigest: parsed.releaseIdentityDigest,
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

const confirmSourceHistory = async ({
  source,
  inclusionPoint,
  throughPoint,
}: {
  readonly source: HistoricalNativeScriptSourceV1;
  readonly inclusionPoint: FraudProofRawL1PointV1;
  readonly throughPoint: FraudProofRawL1PointV1;
}): Promise<void> => {
  const parsed = exact(
    await source.confirmCanonicalHistory({ inclusionPoint, throughPoint }),
    ["canonical", "inclusionPoint", "throughPoint"],
    `historical native script confirmation ${source.sourceId}`,
  );
  const confirmedInclusionPoint = admitFraudProofRawL1PointV1(
    parsed.inclusionPoint,
    `historical native script confirmation ${source.sourceId}.inclusionPoint`,
  );
  const confirmedThroughPoint = admitFraudProofRawL1PointV1(
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

/**
 * Resolves a native-script preimage only from authenticated public L1 history.
 * No caller-supplied bytes are accepted. Optional retained-DA bytes can only
 * corroborate the L1 publication and can never replace it.
 */
export const resolveHistoricalNativeScriptEvidenceV1 = async ({
  roster,
  expectedScriptHash,
  throughPoint: untrustedThroughPoint,
  releaseFinality: untrustedReleaseFinality,
  retainedDaCorroboratingScriptBytes,
}: {
  readonly roster: HistoricalNativeScriptSourceRosterV1;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly retainedDaCorroboratingScriptBytes?: Uint8Array;
}): Promise<HistoricalNativeScriptEvidenceV1> => {
  if (!SCRIPT_HASH.test(expectedScriptHash)) {
    throw new Error(
      "historical native script hash must be 28-byte lowercase hex",
    );
  }
  const throughPoint = admitFraudProofRawL1PointV1(
    untrustedThroughPoint,
    "historical native script throughPoint",
  );
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicyV1(
    untrustedReleaseFinality,
  );
  const sources = requireSourceRoster({ roster, releaseFinality });
  const sourceMode = roster.sourceMode;
  const candidates = await Promise.all(
    sources.map(async (source) => {
      const candidate = admitCandidate({
        value: await source.resolveReferenceScriptPublication({
          deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
          releaseIdentityDigest: releaseFinality.releaseIdentityDigest,
          finalityPolicyDigest: releaseFinality.policyDigest,
          expectedScriptHash,
          throughPoint,
        }),
        source,
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      });
      await confirmSourceHistory({
        source,
        inclusionPoint: candidate.inclusionPoint,
        throughPoint,
      });
      return candidate;
    }),
  );
  const first = candidates[0]!;
  const expectedCandidateDigest = candidateDigest(first);
  if (
    candidates.some(
      (candidate) => candidateDigest(candidate) !== expectedCandidateDigest,
    )
  ) {
    throw new Error(
      "historical native script providers disagree on exact L1 bytes",
    );
  }
  if (retainedDaCorroboratingScriptBytes !== undefined) {
    const corroboratingHex = Buffer.from(
      retainedDaCorroboratingScriptBytes,
    ).toString("hex");
    if (corroboratingHex !== first.scriptBytesHex) {
      throw new Error(
        "retained DA native-script corroboration differs from authenticated L1 history",
      );
    }
  }
  return sealEvidence({
    candidate: first,
    sourceMode,
    applicationOverlayDigest: roster.applicationOverlayDigest,
    rosterDigest: roster.rosterDigest,
    sources: Object.freeze(
      sources.map((source) =>
        Object.freeze({
          sourceId: source.sourceId,
          operatorIdentitySha256: source.operatorIdentitySha256,
        }),
      ),
    ),
  });
};

/**
 * Re-admits a journal-loaded record by resolving the installed immutable
 * roster again and reconfirming publication ancestry through the pinned L1
 * point. A valid unkeyed digest or structural source clone is insufficient.
 */
export const admitHistoricalNativeScriptEvidenceV1 = async ({
  value,
  roster,
  expectedScriptHash,
  throughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly roster: HistoricalNativeScriptSourceRosterV1;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): Promise<HistoricalNativeScriptEvidenceV1> => {
  const persisted = parsePersistedHistoricalNativeScriptEvidenceV1({
    value,
    sourceMode: roster.sourceMode,
    applicationOverlayDigest: roster.applicationOverlayDigest,
    rosterDigest: roster.rosterDigest,
    expectedScriptHash,
    throughPoint,
    releaseFinality,
  });
  const live = await resolveHistoricalNativeScriptEvidenceV1({
    roster,
    expectedScriptHash,
    throughPoint,
    releaseFinality,
    retainedDaCorroboratingScriptBytes: Buffer.from(
      persisted.scriptBytesHex,
      "hex",
    ),
  });
  if (
    persisted.evidenceDigest !== live.evidenceDigest ||
    JSON.stringify(persisted) !== JSON.stringify(live)
  ) {
    throw new Error(
      "historical native script evidence changed after live roster reconfirmation",
    );
  }
  return live;
};

export const historicalNativeScriptBytesV1 = (
  evidence: HistoricalNativeScriptEvidenceV1,
): Uint8Array => {
  if (!admittedHistoricalNativeScriptEvidenceV1.has(evidence)) {
    throw new Error(
      "historical native script evidence was not admitted by the authenticated resolver",
    );
  }
  return Buffer.from(evidence.scriptBytesHex, "hex");
};
