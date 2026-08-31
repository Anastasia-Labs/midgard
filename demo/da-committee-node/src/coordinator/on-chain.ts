import { blake2b } from "@noble/hashes/blake2.js";

import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  L1SubmissionRecord,
} from "../domain.js";
import type { DaAttestationChainReader } from "../l1/da-attestation-reader.js";
import type { AttestationCoordinator } from "./coordinator.js";
import { planDaAttestationLifecycle } from "./planner.js";
import { parseSignatureWitness } from "./witnesses.js";

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

type RequiredReconcileArgs = {
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
const DEFAULT_SINGLE_KEY_NOTICE_INTERVAL_MS = 600_000;

/**
 * The single-key attest-loop notice, emitted at most once per interval.
 *
 * `threshold === 1` is exactly the single-key configuration and not a proxy
 * for it: the governor floors `da_threshold` at `ceil(2*committee_len/3)`,
 * which is `>= 2` for every committee of two or more, so a threshold of one is
 * representable only over a committee of one.
 */
const SINGLE_KEY_ATTEST_NOTICE =
  "Single-key DA attest loop: da_threshold is 1, so this committee attests every block's data availability with one key — " +
  "no independent corroboration, and no liveness redundancy if that key is lost. " +
  "F04 §4 (amended 2026-08-11) permits it; two-key committees are the standing configuration. " +
  "This notice is rate-limited.";

export class OnChainLifecycleCoordinator implements AttestationCoordinator {
  readonly retryPublishedSignatures = true;

  private readonly deps: OnChainLifecycleCoordinatorDeps;
  private readonly headerLocks = new Map<string, Promise<void>>();
  private readonly lastErrors = new Map<string, string>();
  private lastSingleKeyNoticeAt: number | undefined;

  constructor(deps: OnChainLifecycleCoordinatorDeps) {
    this.deps = deps;
  }

  /**
   * Emits the single-key attest-loop notice, at most once per configured
   * interval, for as long as the configuration stays single-key.
   *
   * Deliberately not once-per-process, but note what that does and does not
   * buy: `deps.threshold` is fixed at construction, so a rotation is noticed
   * only when the coordinator is rebuilt around the new params, not mid-run.
   * The re-emission is for the operator rather than for the rotation — a
   * long-lived single-key node keeps saying so, instead of burying the fact in
   * one line at startup that whoever reads the log later never sees.
   */
  private noticeSingleKeyAttestLoop(): void {
    if (this.deps.threshold !== 1) {
      return;
    }
    const intervalMs =
      this.deps.singleKeyNoticeIntervalMs ??
      DEFAULT_SINGLE_KEY_NOTICE_INTERVAL_MS;
    const now = Date.now();
    if (
      this.lastSingleKeyNoticeAt !== undefined &&
      now - this.lastSingleKeyNoticeAt < intervalMs
    ) {
      return;
    }
    this.lastSingleKeyNoticeAt = now;
    (this.deps.log ?? ((message: string) => console.warn(message)))(
      SINGLE_KEY_ATTEST_NOTICE,
    );
  }

  async publishSignature(
    record: DaSignatureRecord,
  ): Promise<"posted" | "post_failed"> {
    return this.reconcileAttestation({
      context: contextFromSignatureRecord(record),
      witnessHexes: [record.signatureWitness],
      signerIndex: record.signerIndex,
    });
  }

  async reconcileAttestation({
    context,
    witnessHexes,
    requireThresholdWitnesses = false,
    signerIndex,
    submitterId = this.deps.l1SubmitterId,
  }: ReconcileAttestationArgs): Promise<"posted" | "post_failed"> {
    this.noticeSingleKeyAttestLoop();
    try {
      await this.withHeaderLock(context.headerHash, () =>
        this.reconcile({
          context,
          witnessHexes,
          requireThresholdWitnesses,
          signerIndex,
          submitterId,
        }),
      );
      this.lastErrors.delete(context.headerHash);
      return "posted";
    } catch (error) {
      this.lastErrors.set(context.headerHash, errorMessageWithCause(error));
      return "post_failed";
    }
  }

  lastPublishError(
    record: Pick<DaSignatureRecord, "headerHash">,
  ): string | undefined {
    return this.lastErrors.get(record.headerHash);
  }

  private async reconcile(args: RequiredReconcileArgs): Promise<void> {
    const retryCount = this.deps.raceRecoveryRetryCount ?? 2;
    const retryDelayMs =
      this.deps.raceRecoveryRetryDelayMs ??
      this.deps.visibilityRetryDelayMs ??
      2_000;
    for (let attempt = 0; attempt <= retryCount; attempt += 1) {
      try {
        await this.reconcileOnce(args);
        return;
      } catch (error) {
        if (!isRecoverableL1Race(error) || attempt === retryCount) {
          throw error;
        }
        await sleep(retryDelayMs);
      }
    }
  }

  private async reconcileOnce(args: RequiredReconcileArgs): Promise<void> {
    const { context } = args;
    let candidates = await this.fetchCandidates(context.headerHash);
    let action = await this.plan(args, candidates);
    action = await this.waitForLeadership(args, action);

    if (action.kind === "init") {
      const result = await this.deps.submitter.initAttestation(context);
      if (result.status === "already_attested") {
        return;
      }
      await this.recordSubmission(context, "init", result.txHash, [
        context.validation.stateQueueOutRef,
      ]);
      candidates = await this.fetchCandidatesUntilVisible(context.headerHash);
      action = await this.plan(args, candidates);
      action = await this.waitForLeadership(args, action);
      if (action.kind === "init") {
        throw new Error(
          `DA attestation init for ${context.headerHash} was not visible after confirmation`,
        );
      }
    }

    if (action.kind === "add_signatures") {
      const candidate = requireCandidate(candidates, action.candidateOutRef);
      const result = await this.deps.submitter.addSignatures({
        record: context,
        candidate,
        packedWitnessesHex: action.packedWitnessesHex,
        signerIndexes: action.signerIndexes,
      });
      if (result.status === "already_attested") {
        return;
      }
      await this.recordSubmission(context, "add_signatures", result.txHash, [
        candidate.outRef,
      ]);
      const updated = await this.fetchCandidatesUntilPlanChanges(args, action);
      candidates = updated.candidates;
      action = updated.action;
      action = await this.waitForLeadership(args, action);
      if (
        action.kind === "add_signatures" &&
        action.candidateOutRef === candidate.outRef
      ) {
        throw new Error(
          `DA attestation add-signatures for ${context.headerHash} was not visible after confirmation`,
        );
      }
    }

    if (action.kind === "apply") {
      const candidate = requireCandidate(candidates, action.candidateOutRef);
      const result = await this.deps.submitter.applyAttestation({
        record: context,
        candidate,
      });
      if (result.status === "already_attested") {
        return;
      }
      await this.recordSubmission(context, "apply", result.txHash, [
        candidate.outRef,
        context.validation.stateQueueOutRef,
      ]);
    }
  }

  private async fetchCandidates(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const candidates =
      await this.deps.chainReader.fetchDaAttestationCandidates(headerHash);
    if (this.deps.recordCandidate !== undefined) {
      await Promise.all(
        candidates.map((candidate) => this.deps.recordCandidate!(candidate)),
      );
    }
    return candidates;
  }

  private async fetchCandidatesUntilVisible(
    headerHash: string,
  ): Promise<readonly DaAttestationCandidateRecord[]> {
    const retryCount = this.deps.visibilityRetryCount ?? 12;
    const retryDelayMs = this.deps.visibilityRetryDelayMs ?? 2_000;
    for (let attempt = 0; attempt <= retryCount; attempt += 1) {
      const candidates = await this.fetchCandidates(headerHash);
      if (candidates.length > 0 || attempt === retryCount) {
        return candidates;
      }
      await sleep(retryDelayMs);
    }
    return [];
  }

  private async fetchCandidatesUntilPlanChanges(
    args: RequiredReconcileArgs,
    previousAction: CoordinatorPlan,
  ): Promise<{
    readonly candidates: readonly DaAttestationCandidateRecord[];
    readonly action: CoordinatorPlan;
  }> {
    const retryCount = this.deps.visibilityRetryCount ?? 12;
    const retryDelayMs = this.deps.visibilityRetryDelayMs ?? 2_000;
    for (let attempt = 0; attempt <= retryCount; attempt += 1) {
      const candidates = await this.fetchCandidates(args.context.headerHash);
      const action = await this.plan(args, candidates);
      if (
        !sameCoordinatorAction(action, previousAction) ||
        attempt === retryCount
      ) {
        return { candidates, action };
      }
      await sleep(retryDelayMs);
    }
    const candidates = await this.fetchCandidates(args.context.headerHash);
    return { candidates, action: await this.plan(args, candidates) };
  }

  private async plan(
    args: RequiredReconcileArgs,
    candidates: readonly DaAttestationCandidateRecord[],
  ): Promise<CoordinatorPlan> {
    return planDaAttestationLifecycle({
      headerHash: args.context.headerHash,
      threshold: this.deps.threshold,
      committeeSignersHash: args.context.committeeSignersHash,
      candidates,
      witnessHexes: await this.witnesses(args),
      requireThresholdWitnesses: args.requireThresholdWitnesses,
    });
  }

  private async waitForLeadership(
    args: RequiredReconcileArgs,
    action: CoordinatorPlan,
  ): Promise<CoordinatorPlan> {
    if (action.kind === "wait") {
      return action;
    }
    if (
      args.signerIndex !== undefined &&
      this.deps.submitterSignerIndexes !== undefined &&
      !this.deps.submitterSignerIndexes.includes(args.signerIndex)
    ) {
      return {
        kind: "wait",
        headerHash: args.context.headerHash,
        reason: "local signer is not configured as an L1 submitter",
      };
    }
    const failoverMs = this.deps.l1LeaderFailoverMs ?? 0;
    const rankedIds = this.leadershipIds(args);
    if (rankedIds?.allowed === false) {
      return {
        kind: "wait",
        headerHash: args.context.headerHash,
        reason: "local submitter is not configured as an L1 submitter",
      };
    }
    if (
      failoverMs <= 0 ||
      rankedIds === undefined ||
      rankedIds.eligible.length <= 1
    ) {
      return action;
    }
    const rank = rankSubmitter({
      deploymentFingerprint: args.context.deploymentFingerprint,
      headerHash: args.context.headerHash,
      actionKind: action.kind,
      submitterId: rankedIds.local,
      eligibleSubmitterIds: rankedIds.eligible,
    });
    if (rank <= 0) {
      return action;
    }
    await sleep(rank * failoverMs);
    const candidates = await this.fetchCandidates(args.context.headerHash);
    return this.plan(args, candidates);
  }

  private leadershipIds(args: RequiredReconcileArgs):
    | {
        readonly allowed: true;
        readonly local: string;
        readonly eligible: readonly string[];
      }
    | { readonly allowed: false }
    | undefined {
    const submitterIds = this.deps.l1SubmitterIds ?? [];
    if (args.submitterId !== undefined && submitterIds.length > 0) {
      return submitterIds.includes(args.submitterId)
        ? { allowed: true, local: args.submitterId, eligible: submitterIds }
        : { allowed: false };
    }
    if (
      args.signerIndex !== undefined &&
      this.deps.submitterSignerIndexes !== undefined
    ) {
      return {
        allowed: true,
        local: args.signerIndex.toString(),
        eligible: this.deps.submitterSignerIndexes.map((index) =>
          index.toString(),
        ),
      };
    }
    return undefined;
  }

  private async witnesses(
    args: RequiredReconcileArgs,
  ): Promise<readonly string[]> {
    const selected = new Map<number, string>();
    for (const witnessHex of args.witnessHexes) {
      this.addWitness(selected, witnessHex);
    }
    for (const witnessHex of await this.peerWitnessesFromRecords(
      args.context,
    )) {
      this.addWitness(selected, witnessHex);
    }
    if (this.deps.peerWitnessesFor !== undefined) {
      for (const witnessHex of await this.deps.peerWitnessesFor(
        args.context.headerHash,
      )) {
        this.addWitness(selected, witnessHex);
      }
    }
    return [...selected.values()];
  }

  private async peerWitnessesFromRecords(
    context: DaAttestationContext,
  ): Promise<readonly string[]> {
    if (this.deps.peerSignaturesFor === undefined) {
      return [];
    }
    return (await this.deps.peerSignaturesFor(context.headerHash))
      .filter((peerRecord) => usablePeerSignature(peerRecord, context))
      .map((peerRecord) => peerRecord.signatureWitness);
  }

  private addWitness(selected: Map<number, string>, witnessHex: string): void {
    const witness = parseSignatureWitness(witnessHex);
    if (!selected.has(witness.signerIndex)) {
      selected.set(witness.signerIndex, witness.witnessHex);
    }
  }

  private async recordSubmission(
    context: DaAttestationContext,
    txKind: L1SubmissionRecord["txKind"],
    txHash: string,
    inputsUsed: readonly string[],
  ): Promise<void> {
    if (this.deps.recordSubmission === undefined) {
      return;
    }
    const now = new Date().toISOString();
    await this.deps.recordSubmission({
      deploymentFingerprint: context.deploymentFingerprint,
      headerHash: context.headerHash,
      txKind,
      txHash,
      inputsUsed,
      submittedAt: now,
      confirmedAt: now,
      resultStatus: "confirmed",
    });
  }

  private async withHeaderLock<T>(
    headerHash: string,
    action: () => Promise<T>,
  ): Promise<T> {
    const previous = this.headerLocks.get(headerHash) ?? Promise.resolve();
    let releaseCurrent!: () => void;
    const current = new Promise<void>((resolve) => {
      releaseCurrent = resolve;
    });
    const tail = previous.catch(() => undefined).then(() => current);
    this.headerLocks.set(headerHash, tail);
    await previous.catch(() => undefined);
    try {
      return await action();
    } finally {
      releaseCurrent();
      if (this.headerLocks.get(headerHash) === tail) {
        this.headerLocks.delete(headerHash);
      }
    }
  }
}

const requireCandidate = (
  candidates: readonly DaAttestationCandidateRecord[],
  outRef: string,
): DaAttestationCandidateRecord => {
  const candidate = candidates.find((entry) => entry.outRef === outRef);
  if (candidate === undefined) {
    throw new Error(`selected DA attestation candidate disappeared: ${outRef}`);
  }
  return candidate;
};

const sleep = (delayMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, delayMs));

type CoordinatorPlan = ReturnType<typeof planDaAttestationLifecycle>;

const usablePeerSignature = (
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

const contextFromSignatureRecord = (
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

const rankSubmitter = ({
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

const recoverableL1RacePatterns = [
  /selected DA attestation candidate disappeared/i,
  /expected exactly one DA attestation UTxO .* found 0/i,
  /state queue header .* was not found/i,
  /input.*not.*found/i,
  /utxo.*not.*found/i,
  /\bspent\b/i,
];

const isRecoverableL1Race = (error: unknown): boolean => {
  const message = errorMessageWithCause(error);
  return recoverableL1RacePatterns.some((pattern) => pattern.test(message));
};

const errorMessageWithCause = (error: unknown): string => {
  if (error instanceof Error) {
    const cause =
      error.cause === undefined
        ? ""
        : `\n${errorMessageWithCause(error.cause)}`;
    return `${error.message}${cause}`;
  }
  return String(error);
};

const sameCoordinatorAction = (
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
