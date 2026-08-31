import {
  admitAuthenticatedStateQueueHeaderObservationV1,
  admitEvidenceProvenanceV1,
  assertSecurityGradeEvidenceV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  CanonicalEvidenceRejectionV1,
} from "@al-ft/midgard-sdk";

import {
  daHashPreimageBlockEvidenceFromVerifiedPayloadV1,
  prepareDaHashPreimageFromCommittedLeavesV1,
  type PreparedDaHashPreimageOutput,
} from "../prepare-da-hash-preimage.js";
import { TransitionTraceChallengerError } from "../transition-trace/errors.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
} from "./canonical-block-evidence-v1.js";
import {
  canonicalDecodabilityRawBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalDecodabilityRawBlockEvidenceV1,
} from "./canonical-decodability-raw-evidence-v1.js";

export const PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1 =
  "midgard-production-fraud-proof-evidence-route-v1" as const;

export type ProductionFraudProofEvidenceV1 =
  | Readonly<{
      schemaVersion: typeof PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1;
      kind: "canonical_block";
      evidence: CanonicalBlockEvidenceV1;
    }>
  | Readonly<{
      schemaVersion: typeof PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1;
      kind: "da_hash_preimage";
      evidence: Awaited<
        ReturnType<typeof daHashPreimageBlockEvidenceFromVerifiedPayloadV1>
      >;
      plan: PreparedDaHashPreimageOutput;
    }>
  | Readonly<{
      schemaVersion: typeof PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1;
      kind: "canonical_decodability";
      evidence: CanonicalDecodabilityRawBlockEvidenceV1;
    }>;

const isAuthenticatedSourceLeafDefect = (
  cause: unknown,
): cause is TransitionTraceChallengerError =>
  cause instanceof TransitionTraceChallengerError &&
  cause.code === "authenticatedSourceLeafDefect";

const isAuthenticatedCommittedFieldDefect = (
  cause: unknown,
): cause is TransitionTraceChallengerError =>
  cause instanceof TransitionTraceChallengerError &&
  cause.code === "authenticatedCommittedFieldDefect";

/**
 * Exact production evidence branches for Q44 source-leaf faults and Q17
 * committed-field envelope faults.
 *
 * One public retained-DA payload is fetched and admitted once. Canonical
 * reconstruction is always attempted first. Only its dedicated typed error,
 * emitted after the raw transactions root/count and header are authenticated
 * against L1, may enter a raw-fault preparation route. Transport, provenance,
 * finality, envelope, header, root, and count errors propagate unchanged.
 */
export const fetchProductionFraudProofEvidenceV1 = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
}): Promise<ProductionFraudProofEvidenceV1> => {
  if (sources.length === 0) {
    throw new CanonicalEvidenceRejectionV1(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservationV1({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admittedObservation.headerHash,
    sources,
    ...(retries === undefined ? {} : { retries }),
  });
  const daProvenance = assertSecurityGradeEvidenceV1(
    admitEvidenceProvenanceV1({ provenance: fetched.provenance }),
  );
  let defectRoute: "da_hash_preimage" | "canonical_decodability";
  try {
    return Object.freeze({
      schemaVersion: PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1,
      kind: "canonical_block",
      evidence: await canonicalBlockEvidenceFromVerifiedPayloadV1({
        observation: admittedObservation,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
        daProvenance,
        ...(minimumConfirmationDepth === undefined
          ? {}
          : { minimumConfirmationDepth }),
      }),
    });
  } catch (cause) {
    if (isAuthenticatedSourceLeafDefect(cause)) {
      defectRoute = "da_hash_preimage";
    } else if (isAuthenticatedCommittedFieldDefect(cause)) {
      defectRoute = "canonical_decodability";
    } else {
      throw cause;
    }
  }

  if (defectRoute === "canonical_decodability") {
    return Object.freeze({
      schemaVersion: PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1,
      kind: "canonical_decodability",
      evidence:
        await canonicalDecodabilityRawBlockEvidenceFromVerifiedPayloadV1({
          observation: admittedObservation,
          payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
          daProvenance,
          ...(minimumConfirmationDepth === undefined
            ? {}
            : { minimumConfirmationDepth }),
        }),
    });
  }

  // This second derivation is intentionally independent of the reconstruction
  // exception. It reopens the counted raw transactions root and emits an MPF
  // membership proof for the first total Q44 violation. A forged exception or
  // a root/key/value substitution therefore cannot manufacture a plan.
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const plan = await prepareDaHashPreimageFromCommittedLeavesV1({
    headerHash: raw.headerHash,
    committedTransactionsRoot: raw.committedTransactionsRoot,
    l2TransactionCount: raw.l2TransactionCount,
    entries: raw.entries,
  });
  return Object.freeze({
    schemaVersion: PRODUCTION_FRAUD_PROOF_EVIDENCE_ROUTE_V1,
    kind: "da_hash_preimage",
    evidence: raw,
    plan,
  });
};
