import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  admitEvidenceProvenance,
  assertSecurityGradeEvidence,
  type AuthenticatedStateQueueHeaderObservation,
  CanonicalEvidenceRejection,
} from "@al-ft/midgard-sdk";

import {
  fieldPreimageLengthEvidenceFromVerifiedPayload,
  type RoutedFieldPreimageLengthEvidence,
} from "../field-preimage-length-mismatch/evidence.js";
import {
  detectMintDeclaredAssetLimitAcceptedRawReplay,
  type MintDeclaredAssetLimitRawBlockEvidence,
  mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayload,
  type MintDeclaredAssetLimitReplayDetection,
} from "../mint-declared-asset-limit/replay.js";
import {
  detectObserversForbiddenAcceptedRawReplay,
  type ObserversForbiddenRawBlockEvidence,
  observersForbiddenRawBlockEvidenceFromVerifiedPayload,
  type ObserversForbiddenReplayDetection,
} from "../observers-forbidden-on-untagged-network/replay.js";
import {
  daHashPreimageBlockEvidenceFromVerifiedPayload,
  prepareDaHashPreimageFromCommittedLeaves,
  type PreparedDaHashPreimageOutput,
} from "../prepare-da-hash-preimage.js";
import { TransitionTraceChallengerError } from "../transition-trace/errors.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "./canonical-block-evidence.js";
import {
  type CanonicalDecodabilityRawBlockEvidence,
  canonicalDecodabilityRawBlockEvidenceFromVerifiedPayload,
} from "./canonical-decodability-raw-evidence.js";

export const FRAUD_PROOF_EVIDENCE_ROUTE =
  "midgard-production-fraud-proof-evidence-route-v1" as const;

export type FraudProofEvidence =
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "canonical_block";
      evidence: CanonicalBlockEvidence;
    }>
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "da_hash_preimage";
      evidence: Awaited<
        ReturnType<typeof daHashPreimageBlockEvidenceFromVerifiedPayload>
      >;
      plan: PreparedDaHashPreimageOutput;
    }>
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "canonical_decodability";
      evidence: CanonicalDecodabilityRawBlockEvidence;
    }>
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "field_preimage_length_mismatch";
      evidence: RoutedFieldPreimageLengthEvidence;
    }>
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "mint_declared_asset_limit";
      evidence: MintDeclaredAssetLimitRawBlockEvidence;
      selected: MintDeclaredAssetLimitReplayDetection;
    }>
  | Readonly<{
      schemaVersion: typeof FRAUD_PROOF_EVIDENCE_ROUTE;
      kind: "observers_forbidden_on_untagged_network";
      evidence: ObserversForbiddenRawBlockEvidence;
      selected: ObserversForbiddenReplayDetection;
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

const isAuthenticatedFieldPreimageLengthDefect = (
  cause: unknown,
): cause is TransitionTraceChallengerError =>
  cause instanceof TransitionTraceChallengerError &&
  cause.code === "malformedPayload" &&
  cause.message.startsWith("Failed to authenticate transactions[");

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
export const fetchFraudProofEvidence = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
}): Promise<FraudProofEvidence> => {
  if (sources.length === 0) {
    throw new CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservation({
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
  const daProvenance = assertSecurityGradeEvidence(
    admitEvidenceProvenance({ provenance: fetched.provenance }),
  );
  const acceptedMintLimitRoute = async (): Promise<
    | Extract<
        FraudProofEvidence,
        { readonly kind: "mint_declared_asset_limit" }
      >
    | undefined
  > => {
    const evidence =
      await mintDeclaredAssetLimitRawBlockEvidenceFromVerifiedPayload({
        observation: admittedObservation,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
        daProvenance,
      });
    const detections = detectMintDeclaredAssetLimitAcceptedRawReplay(evidence);
    if (detections.length === 0) return undefined;
    const selected = [...detections].sort((left, right) =>
      left.position < right.position
        ? -1
        : left.position > right.position
          ? 1
          : left.detectionId < right.detectionId
            ? -1
            : left.detectionId > right.detectionId
              ? 1
              : 0,
    )[0]!;
    return Object.freeze({
      schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
      kind: "mint_declared_asset_limit",
      evidence,
      selected,
    });
  };
  const acceptedObserversRoute = async (): Promise<
    | Extract<
        FraudProofEvidence,
        { readonly kind: "observers_forbidden_on_untagged_network" }
      >
    | undefined
  > => {
    const evidence =
      await observersForbiddenRawBlockEvidenceFromVerifiedPayload({
        observation: admittedObservation,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
        daProvenance,
        ...(minimumConfirmationDepth === undefined
          ? {}
          : { minimumConfirmationDepth }),
      });
    const selected = [
      ...detectObserversForbiddenAcceptedRawReplay(evidence),
    ].sort((left, right) =>
      left.position < right.position
        ? -1
        : left.position > right.position
          ? 1
          : left.detectionId.localeCompare(right.detectionId),
    )[0];
    if (selected === undefined) return undefined;
    return Object.freeze({
      schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
      kind: "observers_forbidden_on_untagged_network",
      evidence,
      selected,
    });
  };
  let acceptedMintFailure: unknown;
  try {
    const acceptedMint = await acceptedMintLimitRoute();
    if (acceptedMint !== undefined) return acceptedMint;
  } catch (cause) {
    acceptedMintFailure = cause;
  }
  let acceptedObserversFailure: unknown;
  try {
    const acceptedObservers = await acceptedObserversRoute();
    if (acceptedObservers !== undefined) return acceptedObservers;
  } catch (cause) {
    acceptedObserversFailure = cause;
  }
  let defectRoute:
    | "da_hash_preimage"
    | "canonical_decodability"
    | "field_preimage_length_mismatch";
  try {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: admittedObservation,
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      daProvenance,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
    if (acceptedMintFailure !== undefined) {
      throw new Error(
        `Accepted mint replay failed: ${formatUnknownError(acceptedMintFailure)}`,
      );
    }
    if (acceptedObserversFailure !== undefined) {
      throw new Error(
        `Accepted observer replay failed: ${formatUnknownError(acceptedObserversFailure)}`,
      );
    }
    return Object.freeze({
      schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
      kind: "canonical_block",
      evidence,
    });
  } catch (cause) {
    if (isAuthenticatedSourceLeafDefect(cause)) {
      defectRoute = "da_hash_preimage";
    } else if (isAuthenticatedCommittedFieldDefect(cause)) {
      defectRoute = "canonical_decodability";
    } else if (isAuthenticatedFieldPreimageLengthDefect(cause)) {
      defectRoute = "field_preimage_length_mismatch";
    } else {
      throw cause;
    }
  }

  if (defectRoute === "canonical_decodability") {
    return Object.freeze({
      schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
      kind: "canonical_decodability",
      evidence: await canonicalDecodabilityRawBlockEvidenceFromVerifiedPayload({
        observation: admittedObservation,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
        daProvenance,
        ...(minimumConfirmationDepth === undefined
          ? {}
          : { minimumConfirmationDepth }),
      }),
    });
  }

  if (defectRoute === "field_preimage_length_mismatch") {
    return Object.freeze({
      schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
      kind: "field_preimage_length_mismatch",
      evidence: await fieldPreimageLengthEvidenceFromVerifiedPayload({
        observation: admittedObservation,
        payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      }),
    });
  }

  // This second derivation is intentionally independent of the reconstruction
  // exception. It reopens the counted raw transactions root and emits an MPF
  // membership proof for the first total Q44 violation. A forged exception or
  // a root/key/value substitution therefore cannot manufacture a plan.
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const plan = await prepareDaHashPreimageFromCommittedLeaves({
    headerHash: raw.headerHash,
    committedTransactionsRoot: raw.committedTransactionsRoot,
    l2TransactionCount: raw.l2TransactionCount,
    entries: raw.entries,
  });
  return Object.freeze({
    schemaVersion: FRAUD_PROOF_EVIDENCE_ROUTE,
    kind: "da_hash_preimage",
    evidence: raw,
    plan,
  });
};
