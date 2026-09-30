import { createHash } from "node:crypto";

import { type FraudProofRawL1Point } from "../workflow/raw-l1-snapshot.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../workflow/release-finality-policy.js";

export const HISTORICAL_NATIVE_SCRIPT_SOURCE =
  "midgard-historical-native-script-source-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION =
  "midgard-historical-native-script-evidence-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER =
  "midgard-historical-native-script-source-roster-v1" as const;

export type HistoricalNativeScriptSourceMode =
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
export interface HistoricalNativeScriptSource {
  readonly sourceVersion: typeof HISTORICAL_NATIVE_SCRIPT_SOURCE;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly sourceId: string;
  readonly operatorIdentitySha256: string | null;
  resolveReferenceScriptPublication(input: {
    readonly deploymentIdentityDigest: string;
    readonly blueprintHash: string;
    readonly finalityPolicyDigest: string;
    readonly expectedScriptHash: string;
    readonly throughPoint: FraudProofRawL1Point;
  }): Promise<unknown>;
  /** Reconfirms inclusion ancestry and the pinned boundary after every read. */
  confirmCanonicalHistory(input: {
    readonly inclusionPoint: FraudProofRawL1Point;
    readonly throughPoint: FraudProofRawL1Point;
  }): Promise<unknown>;
}

export type HistoricalNativeScriptSourceRoster = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER;
  sourceMode: HistoricalNativeScriptSourceMode;
  /** Digest of the single immutable application-installed history overlay. */
  applicationOverlayDigest: string;
  deploymentIdentityDigest: string;
  blueprintHash: string;
  finalityPolicyDigest: string;
  sources: readonly Readonly<{
    sourceId: string;
    operatorIdentitySha256: string | null;
  }>[];
  rosterDigest: string;
}>;

export type HistoricalNativeScriptEvidence = Readonly<{
  readonly schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly blueprintHash: string;
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
  readonly inclusionPoint: FraudProofRawL1Point;
  /** Original observation metadata when re-admitting persisted evidence. */
  readonly throughPoint: FraudProofRawL1Point;
  /** Depth at throughPoint, not a current finality or actuation assertion. */
  readonly confirmationDepth: number;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: readonly Readonly<{
    readonly sourceId: string;
    readonly operatorIdentitySha256: string | null;
  }>[];
  /** Digest persisted in the prepared workflow artifact before submission. */
  readonly evidenceDigest: string;
}>;

export const admittedHistoricalNativeScriptEvidence = new WeakSet<object>();

export const admittedHistoricalNativeScriptRosters = new WeakMap<
  object,
  readonly HistoricalNativeScriptSource[]
>();

export const admittedAuthenticatedHistoricalNativeScriptRosters =
  new WeakSet<object>();

const DIGEST = /^[0-9a-f]{64}$/u;

export const SCRIPT_HASH = /^[0-9a-f]{56}$/u;

export const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

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

export const exact = (
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

export const canonicalString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

export const digest = (value: unknown, label: string): string => {
  const parsed = canonicalString(value, label);
  if (!DIGEST.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

export const cbor = (value: unknown, label: string): string => {
  const parsed = canonicalString(value, label);
  if (!EVEN_HEX.test(parsed))
    throw new Error(`${label} must be lowercase CBOR`);
  return parsed;
};

export const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockHash === right.blockHash &&
  left.pointId === right.pointId;

const admitSourceIdentities = ({
  sourceMode,
  sources,
}: {
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly sources: readonly HistoricalNativeScriptSource[];
}): HistoricalNativeScriptSourceRoster["sources"] => {
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
        source.sourceVersion !== HISTORICAL_NATIVE_SCRIPT_SOURCE ||
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
export const createHistoricalNativeScriptSourceRoster = ({
  sourceMode,
  sources,
  applicationOverlayDigest: untrustedApplicationOverlayDigest,
  releaseFinality: untrustedReleaseFinality,
}: {
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly sources: readonly HistoricalNativeScriptSource[];
  readonly applicationOverlayDigest: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): HistoricalNativeScriptSourceRoster => {
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    untrustedReleaseFinality,
  );
  const applicationOverlayDigest = digest(
    untrustedApplicationOverlayDigest,
    "historical native script application overlay digest",
  );
  const identities = admitSourceIdentities({ sourceMode, sources });
  const withoutDigest = Object.freeze({
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE_ROSTER,
    sourceMode,
    applicationOverlayDigest,
    deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
    blueprintHash: releaseFinality.blueprintHash,
    finalityPolicyDigest: releaseFinality.policyDigest,
    sources: identities,
  });
  const roster = Object.freeze({
    ...withoutDigest,
    rosterDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedHistoricalNativeScriptRosters.set(
    roster,
    Object.freeze([...sources]),
  );
  return roster;
};

/** Explicit callback seam for resolver unit tests; production rejects it. */
export const unsafeCreateHistoricalNativeScriptSourceRosterForTest =
  createHistoricalNativeScriptSourceRoster;

export const postHistoricalNativeScriptJson = async ({
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
