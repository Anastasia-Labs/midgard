import type { AvailabilityResponseAdmissionDecision } from "@al-ft/midgard-core";

import { committeePromiseProfileRequired } from "./availability/promise-profile-selection.js";
import type {
  CommitteeServiceDeps,
  SignedHeaderResult,
} from "./committee-service.ingest-da-conflict-evidence.js";
import type { VerifiedDaPayload } from "./da/payload.js";
import type { DaSignatureRecord, StateQueueHeaderRecord } from "./domain.js";
import { deriveExpectedDaAvailabilityCommitment } from "./peer/signatures.js";
import { signDaAttestation } from "./signer.js";

/** The only fresh-signing path, after verification and exact commitment derivation. */
export const signVerifiedCommitteePayload = async (
  deps: CommitteeServiceDeps,
  record: StateQueueHeaderRecord,
  verified: VerifiedDaPayload,
  observe: (decision: AvailabilityResponseAdmissionDecision) => void,
): Promise<SignedHeaderResult | undefined> => {
  if (
    deps.signer === undefined ||
    deps.signerValidation === undefined ||
    deps.config.signerIndex === undefined
  )
    throw new Error("DA signer is not configured");
  const retirementGuard = deps.store.captureRetirementGuard();
  const expected = deriveExpectedDaAvailabilityCommitment({
    authority: {
      deploymentIdentity: deps.config.hubOraclePolicyId,
      responseGeometry: deps.config.availabilityChallenge.responseGeometry,
    },
    headerHash: record.headerHash,
    payloadCborHex: verified.storedPayloadCbor.toString("hex"),
  });
  const missing: AvailabilityResponseAdmissionDecision = {
    status: "incomplete_evidence",
    deploymentId: deps.config.deploymentFingerprint,
    candidateHeaderHash: record.headerHash,
    candidateCommitmentDigest: expected.commitmentDigest,
    reason: "finite_runtime_policy_unavailable",
  };
  const required =
    deps.promiseAdmission !== undefined ||
    (await committeePromiseProfileRequired(deps.config, deps.store));
  const result = await deps.promiseAdmission?.check({
    record,
    commitment: expected.commitment,
    commitmentDigest: expected.commitmentDigest,
    verifiedPayload: {
      payloadHash: verified.payloadSha256,
      validation: verified.validation,
    },
  });
  if (
    required &&
    (result?.decision.status !== "admitted" ||
      result.assertCurrent === undefined)
  ) {
    observe(result?.decision ?? missing);
    return undefined;
  }
  try {
    await result?.assertCurrent?.();
  } catch {
    observe({
      ...(result?.decision ?? missing),
      status: "incomplete_evidence",
      reason: "canonical_admission_boundary_changed",
    });
    return undefined;
  }
  deps.store.assertRetirementGuard(retirementGuard, record);
  // No await between the generation fence and the synchronous signature.
  const signatureWitness = signDaAttestation({
    signer: deps.signer,
    signerIndex: deps.config.signerIndex,
    availabilityCommitment: expected.commitment,
  });
  if (result !== undefined) observe(result.decision);
  const signature: DaSignatureRecord = {
    deploymentFingerprint: deps.config.deploymentFingerprint,
    headerHash: record.headerHash,
    signerIndex: deps.config.signerIndex,
    signatureWitness,
    availabilityCommitmentCbor: expected.commitmentCbor,
    availabilityCommitmentDigest: expected.commitmentDigest,
    payloadHash: verified.payloadSha256,
    committeeSignersHash: deps.signerValidation.committeeSignersHash,
    signedAt: new Date().toISOString(),
    broadcastStatus: "local",
    source: "local",
    verifiedAt: new Date().toISOString(),
    l1ChainPoint: record.observedChainPoint,
    validation: verified.validation,
  };
  return { signature };
};
