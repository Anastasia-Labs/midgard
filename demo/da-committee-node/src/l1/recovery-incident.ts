import type { L1SourceState, StoreData } from "../store.committee-store.js";
import { parseL1SourceState } from "../store.parse-l1-source-state.js";
import { retirementStoreDigest } from "../store/retirement-transition.js";
import {
  type L1RecoveryProofInput,
  proveUnsignedL1Recovery,
} from "./recovery-native-proof.js";

export const RECOVERABLE_L1_REASON =
  "l1_source_finalized_decision_missing_canonical_chain_point";
export type L1RecoverySnapshot = Readonly<{ data: StoreData; digest: string }>;
export type L1RecoveryCertificate = Readonly<{
  readonly recoveryCertificate: unique symbol;
}>;
type VerifiedRecovery = Readonly<{
  snapshot: L1RecoverySnapshot;
  source: L1SourceState;
  assertCurrent: () => Promise<void>;
  assertScopeCurrent: () => void;
}>;
const certificates = new WeakMap<L1RecoveryCertificate, VerifiedRecovery>();

export const l1RecoverySnapshot = (data: StoreData): L1RecoverySnapshot => ({
  data,
  digest: retirementStoreDigest(data),
});

/** This initial recovery lane cannot reconcile any prior external effect. */
export const assertUnsignedL1Incident = (
  snapshot: L1RecoverySnapshot,
): void => {
  const { data } = snapshot;
  const source = data.chainCursor;
  if (source?.status !== "quarantined")
    throw new Error("source_not_quarantined");
  if (source.quarantineReason !== RECOVERABLE_L1_REASON)
    throw new Error("unsupported_quarantine_reason");
  if (!data.deployment) throw new Error("deployment_binding_missing");
  if (data.retirementFloor?.breach)
    throw new Error("retirement_floor_breached");
  if (data.retirementFloor?.point || data.retirementFloor?.checkpoint)
    throw new Error("protected_floor_proof_unavailable");
  if (!source.stateQueueReplayAnchor)
    throw new Error("durable_replay_anchor_missing");
  if (source.observations.some((o) => o.hasPersistedDecision))
    throw new Error("prior_persisted_decision_requires_reconciliation");
  for (const [family, code] of [
    ["daSignatures", "prior_da_signatures_requires_reconciliation"],
    ["l1Submissions", "prior_l1_submissions_requires_reconciliation"],
    ["peerBroadcasts", "prior_peer_broadcasts_requires_reconciliation"],
    ["decisionOutbox", "prior_decision_outbox_requires_reconciliation"],
    [
      "daAttestationCandidates",
      "prior_da_attestation_candidates_requires_reconciliation",
    ],
    [
      "daConflictEvidence",
      "prior_da_conflict_evidence_requires_reconciliation",
    ],
    [
      "promiseCapacityEvidence",
      "prior_promise_capacity_evidence_requires_reconciliation",
    ],
  ] as const)
    if (Object.keys(data[family]).length) throw new Error(code);
  if (
    Object.values(data.stateQueueHeaders).some((h) => h.status === "conflicted")
  )
    throw new Error("conflicted_header_requires_reconciliation");
  if (
    Object.values(data.daPayloads).some(
      (p) => p.validationStatus === "conflicted",
    )
  )
    throw new Error("conflicted_payload_cause_unknown");
};

/** Called only by the native verifier after its complete proof and final fence. */
const verifiedL1RecoveryCertificate = (
  recovery: VerifiedRecovery,
): L1RecoveryCertificate => {
  assertUnsignedL1Incident(recovery.snapshot);
  const prior = recovery.snapshot.data.chainCursor!;
  const source = parseL1SourceState(recovery.source);
  const expected = parseL1SourceState({
    ...prior,
    status: "healthy",
    observedAt: source.observedAt,
    quarantineReason: undefined,
    quarantinedAt: undefined,
  });
  if (
    retirementStoreDigest({
      ...recovery.snapshot.data,
      chainCursor: source,
    }) !==
    retirementStoreDigest({ ...recovery.snapshot.data, chainCursor: expected })
  )
    throw new Error("recovery_changed_incident_observations_or_binding");
  const token = Object.freeze({}) as L1RecoveryCertificate;
  certificates.set(token, { ...recovery, source });
  return token;
};

export const verifyL1Recovery = async (
  args: L1RecoveryProofInput,
): Promise<L1RecoveryCertificate> => {
  const evidence = await proveUnsignedL1Recovery(args);
  const prior = args.snapshot.data.chainCursor!;
  const source = parseL1SourceState({
    ...prior,
    status: "healthy",
    observedAt: new Date().toISOString(),
    quarantineReason: undefined,
    quarantinedAt: undefined,
  });
  return verifiedL1RecoveryCertificate({
    snapshot: args.snapshot,
    source,
    ...evidence,
  });
};

export const readL1RecoveryCertificate = (
  token: L1RecoveryCertificate,
): VerifiedRecovery => {
  const verified = certificates.get(token);
  if (!verified) throw new Error("unverified_or_consumed_recovery_certificate");
  return verified;
};
export const consumeL1RecoveryCertificate = (
  token: L1RecoveryCertificate,
): void => {
  certificates.delete(token);
};
export const applyVerifiedL1Recovery = (
  data: StoreData,
  token: L1RecoveryCertificate,
): StoreData => {
  const verified = readL1RecoveryCertificate(token);
  if (retirementStoreDigest(data) !== verified.snapshot.digest)
    throw new Error("recovery_incident_changed");
  assertUnsignedL1Incident(l1RecoverySnapshot(data));
  verified.assertScopeCurrent();
  return { ...data, chainCursor: verified.source };
};
