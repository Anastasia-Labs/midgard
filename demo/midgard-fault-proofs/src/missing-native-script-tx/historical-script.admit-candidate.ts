import { createHash } from "node:crypto";

import { missingNativeScriptTxVersionedScriptHash } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
} from "../workflow/raw-l1-snapshot.js";
import { type VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";
import {
  admittedHistoricalNativeScriptEvidence,
  canonicalString,
  cbor,
  digest,
  exact,
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  type HistoricalNativeScriptEvidence,
  type HistoricalNativeScriptSource,
  type HistoricalNativeScriptSourceMode,
  OUT_REF,
  samePoint,
} from "./historical-script.admit-source-identities.js";
import { type AdmittedCandidate } from "./historical-script.create-external-historical-native-script-source-roster.js";

export const admitCandidate = ({
  value,
  source,
  expectedScriptHash,
  throughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly source: HistoricalNativeScriptSource;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1Point;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): AdmittedCandidate => {
  const label = `historical native script source ${source.sourceId}`;
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "deploymentIdentityDigest",
      "blueprintHash",
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
    parsed.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION
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
  const blueprintHash = digest(parsed.blueprintHash, `${label}.blueprintHash`);
  const finalityPolicyDigest = digest(
    parsed.finalityPolicyDigest,
    `${label}.finalityPolicyDigest`,
  );
  if (
    deploymentIdentityDigest !== releaseFinality.deploymentIdentityDigest ||
    blueprintHash !== releaseFinality.blueprintHash ||
    finalityPolicyDigest !== releaseFinality.policyDigest ||
    parsed.expectedScriptHash !== expectedScriptHash
  ) {
    throw new Error(`${label} changed the release/script identity`);
  }
  const admittedThroughPoint = admitFraudProofRawL1Point(
    parsed.throughPoint,
    `${label}.throughPoint`,
  );
  if (!samePoint(admittedThroughPoint, throughPoint)) {
    throw new Error(`${label} changed the pinned historical boundary`);
  }
  const inclusionPoint = admitFraudProofRawL1Point(
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
    confirmationDepth < 1n ||
    confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(`${label} script publication lacks canonical inclusion`);
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
    missingNativeScriptTxVersionedScriptHash(
      Buffer.from(scriptBytesHex, "hex"),
    ) !== expectedScriptHash
  ) {
    throw new Error(`${label} native script preimage has a substituted hash`);
  }
  return {
    deploymentIdentityDigest,
    blueprintHash,
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

export const candidateDigest = (candidate: AdmittedCandidate): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        deploymentIdentityDigest: candidate.deploymentIdentityDigest,
        blueprintHash: candidate.blueprintHash,
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
  readonly candidate: AdmittedCandidate;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: HistoricalNativeScriptEvidence["sources"];
}): Omit<HistoricalNativeScriptEvidence, "evidenceDigest"> => ({
  schemaVersion: HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  ...candidate,
  sourceMode,
  applicationOverlayDigest,
  rosterDigest,
  sources,
});

const computeEvidenceDigest = (
  value: Omit<HistoricalNativeScriptEvidence, "evidenceDigest">,
): string => createHash("sha256").update(JSON.stringify(value)).digest("hex");

const freezeCandidate = (candidate: AdmittedCandidate): AdmittedCandidate =>
  Object.freeze({
    ...candidate,
    inclusionBlockTransactionIds: Object.freeze([
      ...candidate.inclusionBlockTransactionIds,
    ]),
    inclusionPoint: Object.freeze({ ...candidate.inclusionPoint }),
    throughPoint: Object.freeze({ ...candidate.throughPoint }),
  });

export const sealEvidence = ({
  candidate: unsealedCandidate,
  sourceMode,
  applicationOverlayDigest,
  rosterDigest,
  sources,
}: {
  readonly candidate: AdmittedCandidate;
  readonly sourceMode: HistoricalNativeScriptSourceMode;
  readonly applicationOverlayDigest: string;
  readonly rosterDigest: string;
  readonly sources: HistoricalNativeScriptEvidence["sources"];
}): HistoricalNativeScriptEvidence => {
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
  admittedHistoricalNativeScriptEvidence.add(evidence);
  return evidence;
};
