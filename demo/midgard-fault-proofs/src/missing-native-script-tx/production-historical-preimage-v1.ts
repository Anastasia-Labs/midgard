import { createHash } from "node:crypto";

import {
  PRODUCTION_HISTORICAL_NATIVE_SCRIPT_PREIMAGE_V1,
  type ProductionHistoricalNativeScriptCorpusPreimageV1,
  type ProductionHistoricalNativeScriptCorpusV1,
  productionHistoricalNativeScriptPreimageFromCorpusV1,
  requireProductionHistoricalNativeScriptCorpusPreimageV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import {
  admitFraudProofRawL1PointV1,
  type FraudProofRawL1PointV1,
} from "../workflow/raw-l1-snapshot-v1.js";
import { type VerifiedFraudProofReleaseFinalityPolicyV1 } from "../workflow/release-finality-policy-v1.js";
import {
  admitHistoricalNativeScriptEvidenceV1,
  historicalNativeScriptBytesV1,
  type HistoricalNativeScriptEvidenceV1,
  type HistoricalNativeScriptSourceRosterV1,
} from "./historical-script-v1.js";

export type ProductionHistoricalNativeScriptPreimageV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_HISTORICAL_NATIVE_SCRIPT_PREIMAGE_V1;
  throughHeaderHash: string;
  scriptHash: string;
  scriptBytesHex: string;
  occurrences: ProductionHistoricalNativeScriptCorpusPreimageV1["occurrences"];
  providerRosterDigest: string;
  corpusDigest: string;
  checkpointDigest: string;
  preimageDigest: string;
  /** Full exact publication/block/provider evidence, not an opaque digest. */
  historicalL1Corroboration: HistoricalNativeScriptEvidenceV1;
  artifactDigest: string;
}>;

export type AdmittedProductionHistoricalNativeScriptPreimageV1 = Readonly<{
  artifact: ProductionHistoricalNativeScriptPreimageV1;
  scriptBytes: Uint8Array;
  corroboration: HistoricalNativeScriptEvidenceV1;
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
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
  const parsed = value as Readonly<Record<string, unknown>>;
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

const hex = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const sha256 = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

const frozenOccurrences = (
  preimage: ProductionHistoricalNativeScriptCorpusPreimageV1,
): ProductionHistoricalNativeScriptCorpusPreimageV1["occurrences"] =>
  Object.freeze(
    preimage.occurrences.map((occurrence) => Object.freeze({ ...occurrence })),
  );

const artifactWithoutDigest = ({
  preimage,
  corroboration,
}: {
  readonly preimage: ProductionHistoricalNativeScriptCorpusPreimageV1;
  readonly corroboration: HistoricalNativeScriptEvidenceV1;
}) =>
  Object.freeze({
    schemaVersion: PRODUCTION_HISTORICAL_NATIVE_SCRIPT_PREIMAGE_V1,
    throughHeaderHash: preimage.throughHeaderHash,
    scriptHash: preimage.scriptHash,
    scriptBytesHex: preimage.scriptBytesHex,
    occurrences: frozenOccurrences(preimage),
    providerRosterDigest: preimage.providerRosterDigest,
    corpusDigest: preimage.corpusDigest,
    checkpointDigest: preimage.checkpointDigest,
    preimageDigest: preimage.preimageDigest,
    historicalL1Corroboration: corroboration,
  });

const seal = ({
  preimage,
  corroboration,
}: {
  readonly preimage: ProductionHistoricalNativeScriptCorpusPreimageV1;
  readonly corroboration: HistoricalNativeScriptEvidenceV1;
}): ProductionHistoricalNativeScriptPreimageV1 => {
  const withoutDigest = artifactWithoutDigest({ preimage, corroboration });
  return Object.freeze({
    ...withoutDigest,
    artifactDigest: sha256(withoutDigest),
  });
};

const matchingPreimage = ({
  corpus,
  expectedScriptHash,
}: {
  readonly corpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly expectedScriptHash: string;
}): ProductionHistoricalNativeScriptCorpusPreimageV1 => {
  if (!HEX_28.test(expectedScriptHash)) {
    throw new Error("historical native-script preimage hash is invalid");
  }
  const preimage = productionHistoricalNativeScriptPreimageFromCorpusV1({
    corpus,
    scriptHash: expectedScriptHash,
  });
  if (preimage === null) {
    throw new Error(
      "authenticated complete history has no native-script preimage",
    );
  }
  return requireProductionHistoricalNativeScriptCorpusPreimageV1(preimage);
};

/**
 * Creates the persisted preimage record only when two independent authority
 * paths agree: the deployment-bound complete checkpoint corpus supplies the
 * exact preimage and occurrence, while admitted local-node/quorum L1 history
 * supplies the exact publication transaction and canonical block placement.
 */
export const prepareProductionHistoricalNativeScriptPreimageV1 = ({
  corpus,
  expectedHeaderHash,
  expectedScriptHash,
  corroboration,
}: {
  readonly corpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly expectedHeaderHash: string;
  readonly expectedScriptHash: string;
  readonly corroboration: HistoricalNativeScriptEvidenceV1;
}): ProductionHistoricalNativeScriptPreimageV1 => {
  if (corpus.throughHeaderHash !== expectedHeaderHash) {
    throw new Error(
      "authenticated complete history does not end at the challenged header",
    );
  }
  const preimage = matchingPreimage({ corpus, expectedScriptHash });
  const corroboratingBytes = historicalNativeScriptBytesV1(corroboration);
  if (
    corroboration.expectedScriptHash !== expectedScriptHash ||
    corroboration.applicationOverlayDigest !== preimage.providerRosterDigest ||
    Buffer.from(corroboratingBytes).toString("hex") !== preimage.scriptBytesHex
  ) {
    throw new Error(
      "authenticated L1 history disagrees with the complete native-script corpus",
    );
  }
  return seal({ preimage, corroboration });
};

/**
 * Re-admits a journal-loaded record against both current opaque authorities.
 * A structural clone of either authority is rejected by its owning module.
 */
export const admitProductionHistoricalNativeScriptPreimageV1 = async ({
  value,
  corpus,
  expectedHeaderHash,
  expectedScriptHash,
  roster,
  throughPoint: untrustedThroughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly corpus: ProductionHistoricalNativeScriptCorpusV1;
  readonly expectedHeaderHash: string;
  readonly expectedScriptHash: string;
  readonly roster: HistoricalNativeScriptSourceRosterV1;
  readonly throughPoint: FraudProofRawL1PointV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): Promise<AdmittedProductionHistoricalNativeScriptPreimageV1> => {
  const parsed = exactRecord(
    value,
    [
      "schemaVersion",
      "throughHeaderHash",
      "scriptHash",
      "scriptBytesHex",
      "occurrences",
      "providerRosterDigest",
      "corpusDigest",
      "checkpointDigest",
      "preimageDigest",
      "historicalL1Corroboration",
      "artifactDigest",
    ],
    "production historical native-script preimage",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_HISTORICAL_NATIVE_SCRIPT_PREIMAGE_V1
  ) {
    throw new Error(
      "production historical native-script preimage schema changed",
    );
  }
  const throughPoint = admitFraudProofRawL1PointV1(
    untrustedThroughPoint,
    "production historical native-script through point",
  );
  const corroboration = await admitHistoricalNativeScriptEvidenceV1({
    value: parsed.historicalL1Corroboration,
    roster,
    expectedScriptHash,
    throughPoint,
    releaseFinality,
  });
  const preimage = matchingPreimage({ corpus, expectedScriptHash });
  if (corpus.throughHeaderHash !== expectedHeaderHash) {
    throw new Error(
      "authenticated complete history does not end at the challenged header",
    );
  }
  const expected = seal({ preimage, corroboration });
  const claimedDigest = hex(
    parsed.artifactDigest,
    HEX_32,
    "production historical native-script preimage digest",
  );
  if (
    parsed.throughHeaderHash !== expected.throughHeaderHash ||
    parsed.scriptHash !== expected.scriptHash ||
    parsed.scriptBytesHex !== expected.scriptBytesHex ||
    parsed.providerRosterDigest !== expected.providerRosterDigest ||
    parsed.corpusDigest !== expected.corpusDigest ||
    parsed.checkpointDigest !== expected.checkpointDigest ||
    parsed.preimageDigest !== expected.preimageDigest ||
    JSON.stringify(parsed.occurrences) !==
      JSON.stringify(expected.occurrences) ||
    JSON.stringify(parsed.historicalL1Corroboration) !==
      JSON.stringify(expected.historicalL1Corroboration) ||
    claimedDigest !== expected.artifactDigest
  ) {
    throw new Error(
      "production historical native-script preimage changed its corpus/L1 authority binding",
    );
  }
  return Object.freeze({
    artifact: expected,
    scriptBytes: Buffer.from(expected.scriptBytesHex, "hex"),
    corroboration,
  });
};
