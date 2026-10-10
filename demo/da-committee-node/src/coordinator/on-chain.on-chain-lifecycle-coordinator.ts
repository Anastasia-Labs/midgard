import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  L1SubmissionRecord,
} from "../domain.js";
import type { AttestationCoordinator } from "./coordinator.js";
import {
  contextFromSignatureRecord,
  type CoordinatorPlan,
  type DaAttestationContext,
  DEFAULT_SINGLE_KEY_NOTICE_INTERVAL_MS,
  errorMessageWithCause,
  type OnChainLifecycleCoordinatorDeps,
  rankSubmitter,
  type ReconcileAttestationArgs,
  requireCandidate,
  type RequiredReconcileArgs,
  sameCoordinatorAction,
  SINGLE_KEY_ATTEST_NOTICE,
  sleep,
  usablePeerSignature,
} from "./on-chain.on-chain-lifecycle-coordinator-deps.js";
import { UnresolvedSubmissions } from "./on-chain.unresolved-submissions.js";
import { planDaAttestationLifecycle } from "./planner.js";
import { DaBondPoolApplyBackoffError } from "./pool-backoff.js";
import type { DaBondPoolCheck } from "./pool-monitor.js";
import { parseSignatureWitness } from "./witnesses.js";

export class OnChainLifecycleCoordinator implements AttestationCoordinator {
  readonly retryPublishedSignatures = true;

  private readonly deps: OnChainLifecycleCoordinatorDeps;
  private readonly headerLocks = new Map<string, Promise<void>>();
  private readonly lastErrors = new Map<string, string>();
  private readonly unresolved = new UnresolvedSubmissions();
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

  /**
   * Reads the pooled DA bond through the submitter, which reports the check
   * or the read failure to its record hooks. Resolves `undefined` for a
   * submitter that cannot read the pool.
   */
  async checkDaBondPool(): Promise<DaBondPoolCheck | undefined> {
    return this.deps.submitter.checkDaBondPool?.();
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
        // A pooled-bond backoff is classified before any race pattern: the
        // pool will not change within a retry delay, so it is reported as not
        // posted at once and the next reconcile tries again.
        if (
          error instanceof DaBondPoolApplyBackoffError ||
          !isRecoverableL1Race(error) ||
          attempt === retryCount
        ) {
          throw error;
        }
        await sleep(retryDelayMs);
      }
    }
  }

  private async reconcileOnce(args: RequiredReconcileArgs): Promise<void> {
    const { context } = args;
    const planned = await this.unresolved.plan(context.headerHash, {
      submitter: this.deps.submitter,
      plan: async () => {
        const candidates = await this.fetchCandidates(context.headerHash);
        return { candidates, action: await this.plan(args, candidates) };
      },
      planOnceShown: (landed) =>
        this.fetchCandidatesUntilPlanChanges(args, landed),
      record: (landed) =>
        this.recordSubmission(
          context,
          landed.txKind,
          landed.txHash,
          landed.inputsUsed,
        ),
    });
    if (planned === undefined) {
      return;
    }
    let candidates = planned.candidates;
    let action = await this.waitForLeadership(args, planned.action);

    if (action.kind === "init") {
      const result = await this.unresolved.submitting(
        context.headerHash,
        {
          txKind: "init",
          action,
          inputsUsed: [context.validation.stateQueueOutRef],
        },
        () => this.deps.submitter.initAttestation(context),
      );
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
      const signing = action;
      const result = await this.unresolved.submitting(
        context.headerHash,
        { txKind: "add_signatures", action, inputsUsed: [candidate.outRef] },
        () =>
          this.deps.submitter.addSignatures({
            record: context,
            candidate,
            packedWitnessesHex: signing.packedWitnessesHex,
            signerIndexes: signing.signerIndexes,
          }),
      );
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
      const result = await this.unresolved.submitting(
        context.headerHash,
        {
          txKind: "apply",
          action,
          inputsUsed: [candidate.outRef, context.validation.stateQueueOutRef],
        },
        () =>
          this.deps.submitter.applyAttestation({
            record: context,
            candidate,
          }),
      );
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

const recoverableL1RacePatterns = [
  /selected DA attestation candidate disappeared/i,
  /expected exactly one DA attestation UTxO .* found 0/i,
  /state queue header .* was not found/i,
  /input.*not.*found/i,
  /utxo.*not.*found/i,
  /\bspent\b/i,
];

export const isRecoverableL1Race = (error: unknown): boolean => {
  const message = errorMessageWithCause(error);
  return recoverableL1RacePatterns.some((pattern) => pattern.test(message));
};
