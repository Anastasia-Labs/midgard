import { blake2b } from "@noble/hashes/blake2.js";

import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  L1SubmissionRecord,
} from "../domain.js";
import type { DaAttestationChainReader } from "../l1/da-attestation-reader.js";
import type { InFlightSubmissionStatus } from "../l1/submitter.js";
import { planDaAttestationLifecycle } from "./planner.js";
import type { DaBondPoolCheck } from "./pool-monitor.js";

export type DaAttestationContext = Pick<
  DaSignatureRecord,
  | "deploymentFingerprint"
  | "headerHash"
  | "payloadHash"
  | "availabilityCommitmentCbor"
  | "availabilityCommitmentDigest"
  | "committeeSignersHash"
  | "l1ChainPoint"
  | "validation"
>;

export type ReconcileAttestationArgs = {
  readonly context: DaAttestationContext;
  readonly witnessHexes: readonly string[];
  readonly requireThresholdWitnesses?: boolean;
  readonly signerIndex?: number;
  readonly submitterId?: string;
};

export type RequiredReconcileArgs = {
  readonly context: DaAttestationContext;
  readonly witnessHexes: readonly string[];
  readonly requireThresholdWitnesses: boolean;
  readonly signerIndex?: number;
  readonly submitterId?: string;
};

export type AttestationSubmissionResult =
  | { readonly status: "submitted"; readonly txHash: string }
  | { readonly status: "already_attested" };

export interface OnChainAttestationSubmitter {
  initAttestation(
    record: Pick<
      DaAttestationContext,
      | "headerHash"
      | "availabilityCommitmentCbor"
      | "availabilityCommitmentDigest"
    >,
  ): Promise<AttestationSubmissionResult>;
  addSignatures(args: {
    readonly record: DaAttestationContext;
    readonly candidate: DaAttestationCandidateRecord;
    readonly packedWitnessesHex: string;
    readonly signerIndexes: readonly number[];
  }): Promise<AttestationSubmissionResult>;
  applyAttestation(args: {
    readonly record: DaAttestationContext;
    readonly candidate: DaAttestationCandidateRecord;
  }): Promise<AttestationSubmissionResult>;
  /** Reads and classifies the pooled DA bond, when the submitter can. */
  checkDaBondPool?(): Promise<DaBondPoolCheck>;
  /**
   * Resolves a submission whose outcome is unknown by the follower's view of
   * its transaction id (`inFlightSubmissionStatus`). Without it the
   * coordinator cannot resolve one and never builds its replacement.
   */
  submissionStatus?(txHash: string): Promise<InFlightSubmissionStatus>;
}

export type OnChainLifecycleCoordinatorDeps = {
  readonly chainReader: Pick<
    DaAttestationChainReader,
    "fetchDaAttestationCandidates"
  >;
  readonly submitter: OnChainAttestationSubmitter;
  readonly threshold: number;
  readonly recordCandidate?: (
    record: DaAttestationCandidateRecord,
  ) => Promise<void>;
  readonly recordSubmission?: (record: L1SubmissionRecord) => Promise<void>;
  readonly peerWitnessesFor?: (
    headerHash: string,
  ) => Promise<readonly string[]>;
  readonly peerSignaturesFor?: (
    headerHash: string,
  ) => Promise<readonly DaSignatureRecord[]>;
  readonly visibilityRetryCount?: number;
  readonly visibilityRetryDelayMs?: number;
  readonly raceRecoveryRetryCount?: number;
  readonly raceRecoveryRetryDelayMs?: number;
  readonly submitterSignerIndexes?: readonly number[];
  readonly l1SubmitterId?: string;
  readonly l1SubmitterIds?: readonly string[];
  readonly l1LeaderFailoverMs?: number;
  /**
   * Where the single-key attest-loop notice goes. Defaults to `console.warn`.
   *
   * Injected rather than imported so the notice is measurable, and so this
   * package — which otherwise logs nothing at all — does not acquire a logging
   * dependency for one line.
   */
  readonly log?: (message: string) => void;
  /**
   * Minimum gap between single-key notices. Defaults to ten minutes.
   *
   * The rate limit is the point: the ruling accepted the single-key attest
   * loop *with a rate-limited explanatory log*, and this coordinator reconciles
   * once per published signature and per header, so an unlimited notice would
   * emit thousands of identical lines and train an operator to filter it.
   */
  readonly singleKeyNoticeIntervalMs?: number;
};

/** Ten minutes. */
export const DEFAULT_SINGLE_KEY_NOTICE_INTERVAL_MS = 600_000;

/**
 * The single-key attest-loop notice, emitted at most once per interval.
 *
 * `threshold === 1` is exactly the single-key configuration and not a proxy
 * for it: the governor floors `da_threshold` at `ceil(2*committee_len/3)`,
 * which is `>= 2` for every committee of two or more, so a threshold of one is
 * representable only over a committee of one.
 */
export const SINGLE_KEY_ATTEST_NOTICE =
  "Single-key DA attest loop: da_threshold is 1, so this committee attests every block's data availability with one key — " +
  "no independent corroboration, and no liveness redundancy if that key is lost. " +
  "F04 §4 (amended 2026-08-11) permits it; two-key committees are the standing configuration. " +
  "This notice is rate-limited.";

export const requireCandidate = (
  candidates: readonly DaAttestationCandidateRecord[],
  outRef: string,
): DaAttestationCandidateRecord => {
  const candidate = candidates.find((entry) => entry.outRef === outRef);
  if (candidate === undefined) {
    throw new Error(`selected DA attestation candidate disappeared: ${outRef}`);
  }
  return candidate;
};

export const sleep = (delayMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, delayMs));

export type CoordinatorPlan = ReturnType<typeof planDaAttestationLifecycle>;

export const usablePeerSignature = (
  peerRecord: DaSignatureRecord,
  context: DaAttestationContext,
): boolean =>
  peerRecord.deploymentFingerprint === context.deploymentFingerprint &&
  peerRecord.headerHash === context.headerHash &&
  peerRecord.payloadHash === context.payloadHash &&
  peerRecord.availabilityCommitmentCbor ===
    context.availabilityCommitmentCbor &&
  peerRecord.availabilityCommitmentDigest ===
    context.availabilityCommitmentDigest &&
  peerRecord.committeeSignersHash === context.committeeSignersHash &&
  peerRecord.validation.headerHash === context.headerHash &&
  peerRecord.validation.rootsMatch;

export const contextFromSignatureRecord = (
  record: DaSignatureRecord,
): DaAttestationContext => ({
  deploymentFingerprint: record.deploymentFingerprint,
  headerHash: record.headerHash,
  payloadHash: record.payloadHash,
  availabilityCommitmentCbor: record.availabilityCommitmentCbor,
  availabilityCommitmentDigest: record.availabilityCommitmentDigest,
  committeeSignersHash: record.committeeSignersHash,
  l1ChainPoint: record.l1ChainPoint,
  validation: record.validation,
});

export const rankSubmitter = ({
  deploymentFingerprint,
  headerHash,
  actionKind,
  submitterId,
  eligibleSubmitterIds,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly actionKind: string;
  readonly submitterId: string;
  readonly eligibleSubmitterIds: readonly string[];
}): number => {
  const ranked = [...eligibleSubmitterIds]
    .map((id) => ({
      id,
      digest: Buffer.from(
        blake2b(
          Buffer.from(
            `${deploymentFingerprint}:${headerHash}:${actionKind}:${id}`,
            "utf8",
          ),
          { dkLen: 32 },
        ),
      ).toString("hex"),
    }))
    .sort(
      (left, right) =>
        left.digest.localeCompare(right.digest) ||
        left.id.localeCompare(right.id),
    );
  return ranked.findIndex((entry) => entry.id === submitterId);
};

export const errorMessageWithCause = (error: unknown): string => {
  if (error instanceof Error) {
    const cause =
      error.cause === undefined
        ? ""
        : `\n${errorMessageWithCause(error.cause)}`;
    return `${error.message}${cause}`;
  }
  return String(error);
};

export const sameCoordinatorAction = (
  left: CoordinatorPlan,
  right: CoordinatorPlan,
): boolean => {
  if (left.kind !== right.kind) {
    return false;
  }
  switch (left.kind) {
    case "init":
      return true;
    case "add_signatures":
      return (
        right.kind === "add_signatures" &&
        left.candidateOutRef === right.candidateOutRef &&
        left.packedWitnessesHex === right.packedWitnessesHex
      );
    case "apply":
      return (
        right.kind === "apply" && left.candidateOutRef === right.candidateOutRef
      );
    case "wait":
      return right.kind === "wait" && left.reason === right.reason;
  }
};
