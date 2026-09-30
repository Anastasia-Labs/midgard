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
  candidateDigest,
  sealEvidence,
} from "./historical-script.admit-candidate.js";
import {
  admittedHistoricalNativeScriptEvidence,
  type HistoricalNativeScriptEvidence,
  type HistoricalNativeScriptSourceRoster,
  SCRIPT_HASH,
} from "./historical-script.admit-source-identities.js";
import { requireSourceRoster } from "./historical-script.create-external-historical-native-script-source-roster.js";
import {
  confirmSourceHistory,
  parsePersistedHistoricalNativeScriptEvidence,
} from "./historical-script.parse-persisted-historical-native-script-evidence.js";

/**
 * Resolves a native-script preimage only from authenticated public L1 history.
 * No caller-supplied bytes are accepted. Optional retained-DA bytes can only
 * corroborate the L1 publication and can never replace it.
 */
export const resolveHistoricalNativeScriptEvidence = async ({
  roster,
  expectedScriptHash,
  throughPoint: untrustedThroughPoint,
  releaseFinality: untrustedReleaseFinality,
  retainedDaCorroboratingScriptBytes,
}: {
  readonly roster: HistoricalNativeScriptSourceRoster;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1Point;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly retainedDaCorroboratingScriptBytes?: Uint8Array;
}): Promise<HistoricalNativeScriptEvidence> => {
  if (!SCRIPT_HASH.test(expectedScriptHash)) {
    throw new Error(
      "historical native script hash must be 28-byte lowercase hex",
    );
  }
  const throughPoint = admitFraudProofRawL1Point(
    untrustedThroughPoint,
    "historical native script throughPoint",
  );
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    untrustedReleaseFinality,
  );
  const sources = requireSourceRoster({ roster, releaseFinality });
  const sourceMode = roster.sourceMode;
  const candidates = await Promise.all(
    sources.map(async (source) => {
      const candidate = admitCandidate({
        value: await source.resolveReferenceScriptPublication({
          deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
          blueprintHash: releaseFinality.blueprintHash,
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
 * roster again and reconfirming publication ancestry through the current L1
 * point. Returns the original evidence for its immutable artifact digest:
 * its throughPoint and confirmationDepth describe the prepared observation,
 * not current depth or finality. A valid unkeyed digest alone is insufficient.
 */
export const admitHistoricalNativeScriptEvidence = async ({
  value,
  roster,
  expectedScriptHash,
  throughPoint,
  releaseFinality,
}: {
  readonly value: unknown;
  readonly roster: HistoricalNativeScriptSourceRoster;
  readonly expectedScriptHash: string;
  readonly throughPoint: FraudProofRawL1Point;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): Promise<HistoricalNativeScriptEvidence> => {
  const persisted = parsePersistedHistoricalNativeScriptEvidence({
    value,
    sourceMode: roster.sourceMode,
    applicationOverlayDigest: roster.applicationOverlayDigest,
    rosterDigest: roster.rosterDigest,
    expectedScriptHash,
    releaseFinality,
  });
  const live = await resolveHistoricalNativeScriptEvidence({
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
    JSON.stringify({
      ...persisted,
      // Observation metadata may advance or roll back; every publication and
      // provider field must still match the independently resolved evidence.
      throughPoint: live.throughPoint,
      confirmationDepth: live.confirmationDepth,
      evidenceDigest: live.evidenceDigest,
    }) !== JSON.stringify(live)
  ) {
    throw new Error(
      "historical native script evidence changed after live roster reconfirmation",
    );
  }
  return persisted;
};

export const historicalNativeScriptBytes = (
  evidence: HistoricalNativeScriptEvidence,
): Uint8Array => {
  if (!admittedHistoricalNativeScriptEvidence.has(evidence)) {
    throw new Error(
      "historical native script evidence was not admitted by the authenticated resolver",
    );
  }
  return Buffer.from(evidence.scriptBytesHex, "hex");
};
