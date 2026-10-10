import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { retainedDaPayloadCommitmentVerifier } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Lucid,
  paymentCredentialOf,
  type Provider,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "../runtime/process-config.js";
import {
  bindWatcherL1AvailabilityPayloadSource,
  createWatcherRetainedDaRuntime,
} from "../storage/retained-da-runtime.js";
import {
  orderWatcherAvailabilityActions,
  selectWatcherAvailabilityActions,
  type WatcherAvailabilityAction,
  watcherAvailabilityOpenDeadlineMissed,
  watcherAvailabilityQueueStatus,
} from "./action.js";
import { createWatcherAvailabilityDeployment } from "./deployment.js";
import type { WatcherAvailabilityL1 } from "./follower-reads.js";
import { createWatcherAvailabilityObservation } from "./observation.js";
import {
  deriveWatcherDaBondPoolObservation,
  type WatcherDaBondPoolObservation,
} from "./pool-observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "./published-payload.js";
import { watcherAvailabilityAttemptOperation } from "./runtime.build-attempt.js";
import { buildWatcherAvailabilityOperation } from "./runtime.build-operation.js";
import { watcherAvailabilityExecutionLimits } from "./runtime.execution-limits.js";
import {
  createWatcherAvailabilityReadAttempt,
  watcherAvailabilityAuthenticatedOpenDeadline,
} from "./runtime.read-attempt.js";
import {
  createWatcherAvailabilityReconcileRetry,
  watcherAvailabilityStatusDetail,
} from "./runtime.reconcile-retry.js";
import { watcherAvailabilityRecoveryContext } from "./runtime.recovery-limits.js";
import {
  availabilityUndecidable,
  DA_CHALLENGE_WINDOW_MS,
  dropReleasedHeaders,
  releaseWatcherAvailabilityWorkflows,
  required,
  type WatcherAvailabilityRuntime,
  type WatcherAvailabilityStatus,
  type WatcherAvailabilityStatusTransition,
  type WatcherAvailabilityWorkflowRefusal,
  watcherAvailabilityWorkflowRefusal,
  type WatcherReleasedHeadersReader,
} from "./runtime.release-watcher-availability-workflows.js";
import {
  buildAdmittedWatcherAvailabilityOperation,
  watcherAvailabilityValidity,
} from "./runtime.select-watcher-availability-funding.js";

/** Concrete independent actor: signed release, public DA, exact L1 intake and durable executor. */
export const createWatcherAvailabilityRuntime = async (input: {
  config: WatcherProcessConfig;
  identity: VerifiedWatcherDeploymentIdentity;
  /** The watcher's chain follower: raw reads, and the provider every build uses. */
  l1: WatcherAvailabilityL1 & Readonly<{ provider: Provider }>;
  /** The verified deployment's release depth. */
  confirmationDepth: number;
  /** Read-only payload reconstruction may follow reversible fault-proof inclusion (the tip view). */
  faultProofObservation?: Readonly<{
    currentObservation(): WatcherAuthenticatedStateQueueObservation | null;
  }>;
  mergedHeaders?: WatcherReleasedHeadersReader;
  proverWalletAddress: string;
  onStatusTransition?: (event: WatcherAvailabilityStatusTransition) => void;
  /**
   * Called with the pool readout after each successful pool read, once per
   * reconciliation. An error it throws propagates from `reconcile` and never
   * becomes an availability failure. Required, so a composition root cannot
   * drop the pool readout silently; production passes
   * `watcherDaBondPoolReporter`.
   */
  onDaBondPool: (pool: WatcherDaBondPoolObservation) => void;
  /**
   * Called with the cause after each failed pool read, once per
   * reconciliation, in place of `onDaBondPool`. Required for the same reason;
   * production passes `watcherDaBondPoolReadFailureReporter`.
   */
  onDaBondPoolReadFailure: (error: string) => void;
}): Promise<WatcherAvailabilityRuntime> => {
  const lucid = await Lucid(input.l1.provider, input.identity.network, {
    evaluator: createScalusEvaluator(),
    slotConfig: input.config.watcherConfig.customNetwork?.slotConfig,
  });
  const secret = await loadWatcherSecretText(
    input.config.availability.keySource,
  );
  if (secret.startsWith("ed25519_sk"))
    lucid.selectWallet.fromPrivateKey(secret);
  else lucid.selectWallet.fromSeed(secret, { addressType: "Enterprise" });
  const walletAddress = await lucid.wallet().address();
  if (
    paymentCredentialOf(walletAddress).hash ===
    paymentCredentialOf(input.proverWalletAddress).hash
  ) {
    throw new Error(
      "Availability and fault-proof actors must use independent payment keys",
    );
  }
  const actor = paymentCredentialOf(walletAddress).hash;
  const deployment = await createWatcherAvailabilityDeployment(
    lucid,
    input.identity,
  );
  const intake = createWatcherAvailabilityObservation({
    identity: input.identity,
    l1: input.l1,
    confirmationDepth: input.confirmationDepth,
    deployment,
  });
  const publicDa = await createWatcherRetainedDaRuntime({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity: input.identity,
  });
  let journal: ReturnType<typeof openAvailabilityOperationJournal>;
  try {
    journal = openAvailabilityOperationJournal(
      input.config.availability.journalPath,
    );
  } catch (error) {
    await publicDa.close();
    throw error;
  }
  const activeScopes = new Set<SDK.DaAvailabilityReadScope>();
  const closeScopes = () => {
    for (const scope of activeScopes) scope.close();
    activeScopes.clear();
  };
  let generation = 0;
  let closed = false;
  let current: WatcherAuthenticatedStateQueueObservation | null = null;
  const l1PayloadSource = createWatcherL1AvailabilityPayloadSource({
    identity: input.identity,
    deployment,
    l1: input.l1,
    minimumConfirmationDepth:
      input.faultProofObservation === undefined ? input.confirmationDepth : 1,
    lucid,
    currentObservation:
      input.faultProofObservation?.currentObservation ?? (() => current),
  });
  const l1PayloadBinding = bindWatcherL1AvailabilityPayloadSource({
    deploymentIdentity: input.identity,
    source: l1PayloadSource,
  });
  let pending = new Set<string>();
  let report: WatcherAvailabilityStatus = {
    phase: "waiting",
    pendingHeaders: [],
  };
  let lastPool: WatcherDaBondPoolObservation | undefined;
  let lastPoolReadFailure: string | undefined;
  let serial: Promise<void> = Promise.resolve();
  let lastBlockedStatus: string | undefined;
  const retry = createWatcherAvailabilityReconcileRetry();
  const reportTransition = (
    observation: WatcherAuthenticatedStateQueueObservation,
    startedAt: number,
  ): void => {
    if (input.onStatusTransition === undefined) return;
    // A new finalized point alone must not repeat the same blocked diagnostic.
    const blockedStatus =
      report.phase === "blocked"
        ? JSON.stringify({
            ...report,
            pendingHeaders: [...report.pendingHeaders].sort(),
          })
        : undefined;
    if (blockedStatus === lastBlockedStatus) return;
    lastBlockedStatus = blockedStatus;
    input.onStatusTransition(
      Object.freeze({
        status: Object.freeze({
          ...report,
          pendingHeaders: Object.freeze([...report.pendingHeaders]),
          ...(report.detail === undefined
            ? {}
            : { detail: watcherAvailabilityStatusDetail(report.detail) }),
        }),
        observationDigest: observation.observationDigest,
        nativePoint: Object.freeze({ ...observation.nativePoint }),
        elapsedMs: Math.max(0, performance.now() - startedAt),
        observedAt: new Date().toISOString(),
      }),
    );
  };
  const assertCurrent = (epoch: number): void => {
    if (closed || epoch !== generation || current === null)
      throw new Error("Availability actuation generation was revoked");
  };
  const commitmentOf = async (
    observation: WatcherAuthenticatedStateQueueObservation,
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
    reader = intake,
  ): Promise<SDK.DaAvailabilityCommitment | undefined> => {
    const status = watcherAvailabilityQueueStatus(snapshot);
    if (
      status === undefined ||
      status === "Unattested" ||
      "Published" in status
    )
      return undefined;
    // The snapshot authenticated the record against the Challenged status.
    if ("Challenged" in status) return snapshot.recordDatum?.commitment;
    // E1: an Attested node carries only the hash; the preimage is the DAAT
    // datum its Apply spent, recovered and checked against that hash.
    return await reader.attestedCommitment(
      observation,
      snapshot.headerHash,
      status.Attested.commitment_hash,
    );
  };
  const publicPayloadAvailable = async (
    headerHash: string,
    commitment: SDK.DaAvailabilityCommitment,
  ): Promise<boolean> => {
    const verifyPayload = retainedDaPayloadCommitmentVerifier(commitment);
    for (const source of publicDa.sources) {
      const result = await source.fetchPayloadByHeaderHash(headerHash, {
        verifyPayload,
      });
      if (result.ok && (await verifyPayload(result.payloadEnvelopeCbor)).ok)
        return true;
    }
    return false;
  };
  const build = (
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
    action: WatcherAvailabilityAction,
    observation: WatcherAuthenticatedStateQueueObservation,
    commitment: SDK.DaAvailabilityCommitment | undefined,
    buildLucid = lucid,
    scope?: SDK.DaAvailabilityReadScope,
  ) =>
    buildWatcherAvailabilityOperation(
      snapshot,
      action,
      observation,
      commitment,
      buildLucid,
      {
        deployment,
        walletAddress,
        actor,
        config: input.config,
        journal,
        proverWalletAddress: input.proverWalletAddress,
      },
      scope,
    );
  const reconcile = (
    observation: WatcherAuthenticatedStateQueueObservation,
    actuate: boolean,
  ): Promise<void> => {
    const epoch = generation;
    const ticket = retry.request();
    const work = serial.then(async () => {
      if (closed || epoch !== generation) return;
      assertWatcherStateQueueObservation(observation);
      const startedAt = performance.now();
      current = observation;
      pending = new Set(
        observation.finalizedHeaders
          .filter((header) => !availabilityUndecidable(header))
          .map(({ headerHash }) => headerHash),
      );
      report = { phase: "waiting", pendingHeaders: [...pending] };
      const ownedScopes: SDK.DaAvailabilityReadScope[] = [];
      const trackScope = (scope: SDK.DaAvailabilityReadScope) => {
        if (!activeScopes.has(scope)) {
          activeScopes.add(scope);
          ownedScopes.push(scope);
        }
        return scope;
      };
      const attempt = (scope: SDK.DaAvailabilityReadScope) =>
        createWatcherAvailabilityReadAttempt({
          config: input.config,
          identity: input.identity,
          l1: input.l1,
          confirmationDepth: input.confirmationDepth,
          deployment,
          observation,
          baseLucid: lucid,
          scope: trackScope(scope),
          assertCurrent: () => assertCurrent(epoch),
          selectWallet: (attemptLucid) => {
            if (secret.startsWith("ed25519_sk"))
              attemptLucid.selectWallet.fromPrivateKey(secret);
            else
              attemptLucid.selectWallet.fromSeed(secret, {
                addressType: "Enterprise",
              });
          },
        });
      const readScope = (deadlineEpochMs?: number) => {
        const scope = SDK.createDaAvailabilityReadScope({
          deadlineEpochMs,
          attemptTimeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
        });
        return trackScope(scope);
      };
      const opens = new Map<string, ReturnType<typeof attempt>>();
      let poolRead: WatcherDaBondPoolObservation | undefined;
      let poolReadFailure: string | undefined;
      try {
        for (const header of observation.finalizedHeaders) {
          if (
            typeof header.daAvailability === "object" &&
            "Attested" in header.daAvailability
          ) {
            const deadline =
              watcherAvailabilityAuthenticatedOpenDeadline(header);
            if (deadline > Date.now())
              opens.set(header.headerHash, attempt(readScope(deadline)));
          }
        }
        await dropReleasedHeaders(pending, observation, input.mergedHeaders);
        report = { phase: "waiting", pendingHeaders: [...pending] };
        assertCurrent(epoch);
        // E5: the pool is read on every reconciliation, pending headers or
        // not, and only reported: neither its state nor a failed read changes
        // the phase or holds back an Open, Settle, Close, Timeout or prune.
        try {
          const read = deriveWatcherDaBondPoolObservation({
            pool: await (() => {
              const reader = attempt(readScope());
              return reader.read(() => reader.intake.pool(observation));
            })(),
            policyId: deployment.contracts.daBondPool.policyId,
            parameters: deployment.parameters,
            nowMs: BigInt(Date.now()),
          });
          assertCurrent(epoch);
          lastPool = poolRead = read;
          lastPoolReadFailure = undefined;
        } catch (cause) {
          // A revoked generation still aborts the whole reconciliation.
          assertCurrent(epoch);
          lastPoolReadFailure = poolReadFailure =
            cause instanceof Error ? cause.message : String(cause);
        }
        const context = watcherAvailabilityRecoveryContext({
          lucid,
          parameters: deployment.parameters,
          observation,
          attempt: (scope) => attempt(scope ?? readScope()),
          assertCurrent: () => assertCurrent(epoch),
        })(() => ({
          deploymentIdentity: input.identity.manifestId,
          actor,
          journal,
          stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
          minimumConfirmationDepth: intake.confirmationDepth,
          observationTimeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
          readBoundary: async (scope) => {
            if (scope !== undefined) trackScope(scope);
            assertCurrent(epoch);
            scope?.assertCurrent();
            return { blockNo: Number(observation.nativePoint.blockNo) };
          },
          assertActuationCurrent: (scope) => {
            assertCurrent(epoch);
            scope?.assertCurrent();
          },
          submit: (cbor) =>
            required(lucid.config().provider, "local provider").submitTx(cbor),
        }));
        const recovered = await SDK.reconcileDaAvailabilityOperations(context);
        // P20: after our own steps reconcile and before any admission, rows
        // whose challenge someone else's terminal step ended are released,
        // in every deployment of this actor.
        const workflowRelease = await releaseWatcherAvailabilityWorkflows(
          journal,
          actor,
          (openIntent, headerHash) => {
            const reader = attempt(readScope());
            return reader.read(() =>
              reader.intake.workflowRelease(
                observation,
                openIntent,
                headerHash,
              ),
            );
          },
          () => assertCurrent(epoch),
        );
        const unresolved = recovered.find(
          ({ status }) =>
            status !== "confirmed" &&
            status !== "expired" &&
            status !== "included",
        );
        // A held intent fails readiness with its reason until fresh evidence
        // clears it; it never latches the journal or other intents.
        const unresolvedReport = unresolved && {
          phase:
            unresolved.status === "conflict" || unresolved.status === "held"
              ? ("blocked" as const)
              : ("waiting" as const),
          pendingHeaders: [...pending],
          txHash: unresolved.txHash,
          ...(unresolved.detail === undefined
            ? {}
            : { detail: unresolved.detail }),
        };
        if (unresolvedReport !== undefined) report = unresolvedReport;
        const now = BigInt(Date.now());
        const openWindow = {
          inclusiveValidityUpper: now,
          daChallengeWindowMs: DA_CHALLENGE_WINDOW_MS,
        };
        const commitments = new Map<string, SDK.DaAvailabilityCommitment>();
        const candidates: {
          snapshot: SDK.DaAvailabilityChallengeSnapshot;
          publiclyAvailable: boolean;
        }[] = [];
        const missedOpenDeadlines: string[] = [];
        let deferredOpenRead:
          | SDK.DaAvailabilityReadScopeExpiredError
          | undefined;
        const headers = observation.finalizedHeaders.filter(({ headerHash }) =>
          pending.has(headerHash),
        );
        const snapshotReads = await Promise.allSettled(
          headers.map(async (header) => {
            const open = opens.get(header.headerHash);
            const attested =
              typeof header.daAvailability === "object" &&
              "Attested" in header.daAvailability;
            if (attested && open === undefined) return undefined;
            const reader = open ?? attempt(readScope());
            return {
              reader,
              snapshot: await reader.read(() =>
                reader.intake.snapshot(observation, header.headerHash),
              ),
            };
          }),
        );
        for (let index = 0; index < headers.length; index += 1) {
          const header = headers[index]!;
          const open = opens.get(header.headerHash);
          const attested =
            typeof header.daAvailability === "object" &&
            "Attested" in header.daAvailability;
          if (attested && open === undefined) {
            missedOpenDeadlines.push(header.headerHash);
            continue;
          }
          try {
            const read = snapshotReads[index]!;
            if (read.status === "rejected") throw read.reason;
            if (read.value === undefined) continue;
            const { snapshot, reader } = read.value;
            const commitment = await reader.read(() =>
              commitmentOf(observation, snapshot, reader.intake),
            );
            if (commitment !== undefined)
              commitments.set(snapshot.headerHash, commitment);
            const available =
              attested &&
              commitment !== undefined &&
              (await reader.publicRead(() =>
                publicPayloadAvailable(snapshot.headerHash, commitment),
              ));
            if (attested && available) pending.delete(snapshot.headerHash);
            if (
              watcherAvailabilityOpenDeadlineMissed(
                snapshot,
                available,
                openWindow,
              )
            )
              missedOpenDeadlines.push(snapshot.headerHash);
            candidates.push({ snapshot, publiclyAvailable: available });
          } catch (cause) {
            assertCurrent(epoch);
            if (
              open === undefined ||
              !(cause instanceof SDK.DaAvailabilityReadScopeExpiredError)
            )
              throw cause;
            if (Date.now() >= (open.scope.deadlineEpochMs ?? Infinity))
              missedOpenDeadlines.push(header.headerHash);
            else deferredOpenRead = cause;
          }
        }
        // E3: every header is selected independently; no live challenge
        // suppresses another header's Open, including a withheld descendant of
        // a Challenged header (if that ancestor settles, the descendant would
        // otherwise merge unchallenged). Opens go first, earliest deadline
        // first, since a missed Open deadline cannot be recovered.
        const ordered = orderWatcherAvailabilityActions(
          selectWatcherAvailabilityActions(
            candidates,
            watcherAvailabilityValidity().validFrom,
            openWindow,
          ),
        );
        // E5: missed-deadline, refused-Open, deferred-Timeout,
        // refused-workflow and workflow-release alerts are reported, never
        // blocking.
        let alerts: Pick<
          WatcherAvailabilityStatus,
          | "missedOpenDeadlines"
          | "openRefused"
          | "timeoutsDeferred"
          | "workflowRefused"
          | "workflowReleased"
          | "workflowReleaseDeferred"
        > = {
          ...(workflowRelease.released.length === 0
            ? {}
            : {
                workflowReleased: Object.freeze([...workflowRelease.released]),
              }),
          ...(workflowRelease.deferred.length === 0
            ? {}
            : {
                workflowReleaseDeferred: Object.freeze([
                  ...workflowRelease.deferred,
                ]),
              }),
          ...(missedOpenDeadlines.length === 0
            ? {}
            : { missedOpenDeadlines: Object.freeze(missedOpenDeadlines) }),
        };
        assertCurrent(epoch);
        report =
          unresolvedReport === undefined
            ? { phase: "ready", pendingHeaders: [...pending], ...alerts }
            : { ...unresolvedReport, ...alerts };
        if (!actuate || unresolved !== undefined) return;
        // The journal keeps one workflow per header and admits every header of
        // this deployment. A step it refuses, or an Open the wallet cannot
        // fund, must not starve a live challenge's own settle, close or
        // Timeout, so take the first step that is admitted and builds.
        const workflowRefused: WatcherAvailabilityWorkflowRefusal[] = [];
        const buildAttempts = new Map<
          WatcherAvailabilityAction,
          ReturnType<typeof attempt>
        >();
        const { selected, openRefused, timeoutsDeferred } =
          await buildAdmittedWatcherAvailabilityOperation(
            ordered,
            (headerHash, action) => {
              const detail = watcherAvailabilityWorkflowRefusal(journal, {
                actor,
                deploymentIdentity: input.identity.manifestId,
                headerHash,
                action,
                nowMs: Date.now(),
              });
              if (detail === undefined) return true;
              workflowRefused.push({ headerHash, action, detail });
              return false;
            },
            async (step) => {
              // Completion starts its own monotonic scope before wallet or
              // provider work; only Open inherits an authenticated cutoff.
              const builder =
                step.action.action === "open"
                  ? required(
                      opens.get(step.snapshot.headerHash),
                      "Open attempt",
                    )
                  : attempt(readScope());
              buildAttempts.set(step.action, builder);
              const attemptLucid = await builder.lucid();
              return await builder.read(() =>
                build(
                  step.snapshot,
                  step.action,
                  observation,
                  commitments.get(step.snapshot.headerHash),
                  attemptLucid,
                  builder.scope,
                ),
              );
            },
            (headerHash) => {
              const cutoff = opens.get(headerHash)?.scope.deadlineEpochMs;
              if (cutoff !== undefined && Date.now() >= cutoff)
                missedOpenDeadlines.push(headerHash);
              else
                deferredOpenRead = new SDK.DaAvailabilityReadScopeExpiredError(
                  cutoff,
                );
            },
          );
        alerts = {
          ...alerts,
          ...(missedOpenDeadlines.length === 0
            ? {}
            : { missedOpenDeadlines: Object.freeze([...missedOpenDeadlines]) }),
          ...(openRefused.length === 0
            ? {}
            : { openRefused: Object.freeze(openRefused) }),
          ...(timeoutsDeferred.length === 0
            ? {}
            : { timeoutsDeferred: Object.freeze(timeoutsDeferred) }),
          ...(workflowRefused.length === 0
            ? {}
            : { workflowRefused: Object.freeze(workflowRefused) }),
        };
        report = { ...report, ...alerts };
        if (selected === undefined) {
          if (deferredOpenRead !== undefined)
            report = {
              ...retry.failed(deferredOpenRead, [...pending]),
              ...alerts,
            };
          return;
        }
        const { operation, step: selectedStep } = selected;
        const buildAttempt = required(
          buildAttempts.get(selectedStep.action),
          "selected availability attempt",
        );
        const attemptLucid = await buildAttempt.lucid();
        const execution = watcherAvailabilityExecutionLimits(
          context,
          deployment.parameters,
          () => assertCurrent(epoch),
        );
        execution.capture(attemptLucid, buildAttempt.scope);
        const result = await SDK.runDaAvailabilityOperation(execution.context, {
          headerHash: selectedStep.snapshot.headerHash,
          ...watcherAvailabilityAttemptOperation({
            attempt: buildAttempt,
            operation,
            execution,
            assertCurrent: () => assertCurrent(epoch),
            reselect: () =>
              build(
                selectedStep.snapshot,
                selectedStep.action,
                observation,
                commitments.get(selectedStep.snapshot.headerHash),
                attemptLucid,
                buildAttempt.scope,
              ),
          }),
        });
        report = {
          phase:
            result.status === "conflict" || result.status === "held"
              ? "blocked"
              : "waiting",
          pendingHeaders: [...pending],
          action: operation.action,
          txHash: result.txHash,
          ...(result.detail === undefined ? {} : { detail: result.detail }),
          ...alerts,
        };
      } catch (cause) {
        if (epoch !== generation || closed) return;
        report = retry.failed(cause, [...pending]);
      } finally {
        for (const scope of ownedScopes) {
          scope.close();
          activeScopes.delete(scope);
        }
        // Diagnostics describe the completed reconciliation, not its temporary
        // waiting state. A revoked observation cannot emit a recovery signal.
        if (epoch === generation && !closed) {
          try {
            if (poolRead !== undefined) input.onDaBondPool(poolRead);
            else if (poolReadFailure !== undefined)
              input.onDaBondPoolReadFailure(poolReadFailure);
          } finally {
            retry.settle(ticket, () => reconcile(observation, actuate));
            reportTransition(observation, startedAt);
          }
        }
      }
    });
    serial = work.catch(() => undefined);
    return work;
  };
  return {
    reconcile,
    pendingAvailabilityHeaders: async (observation, merged) => {
      assertWatcherStateQueueObservation(observation);
      const epoch = generation;
      const assertCurrentClassification = () => {
        const inclusion = input.faultProofObservation?.currentObservation();
        if (
          closed ||
          epoch !== generation ||
          current === null ||
          observation.deploymentIdentityDigest !== input.identity.manifestId ||
          (current.observationDigest !== observation.observationDigest &&
            inclusion?.observationDigest !== observation.observationDigest)
        )
          throw new Error(
            "Availability pending state requires a current authenticated observation",
          );
      };
      assertCurrentClassification();
      // Classification follows the inclusion overlay; this read never changes
      // the finalized observation or authorizes an availability transaction.
      const result = new Set<string>();
      for (const {
        headerHash,
        daAvailability,
      } of observation.finalizedHeaders) {
        if (
          daAvailability === "Unattested" ||
          "Published" in daAvailability ||
          merged?.has(headerHash) === true
        )
          continue;
        if ("Challenged" in daAvailability || pending.has(headerHash)) {
          result.add(headerHash);
          continue;
        }
        if (
          current?.finalizedHeaders.some(
            (header) => header.headerHash === headerHash,
          )
        )
          continue;
        // A newly included attestation can precede public DA propagation, so
        // classification waits until some source serves a copy. A bad copy
        // decides nothing: the classifier's own fetch verifies what it uses.
        let unavailable = true;
        for (const source of publicDa.sources) {
          const payload = await source.fetchPayloadByHeaderHash(headerHash);
          if (payload.ok) {
            unavailable = false;
            break;
          }
        }
        if (unavailable) result.add(headerHash);
      }
      assertCurrentClassification();
      return result;
    },
    invalidateForRollback: () => {
      generation += 1;
      closeScopes();
      retry.cancel();
      // Even a rollback through the finalized observation only rewinds it:
      // the next observation re-derives every intent, and reconciliation
      // rebroadcasts the same bytes of any confirmation the fork dropped.
      current = null;
      pending = new Set();
      report = { phase: "waiting", pendingHeaders: [] };
    },
    invalidateForShutdown: () => {
      generation += 1;
      closeScopes();
      closed = true;
      retry.cancel();
    },
    status: () => ({
      ...report,
      ...(lastPool === undefined ? {} : { pool: lastPool }),
      ...(lastPoolReadFailure === undefined
        ? {}
        : { poolReadFailure: lastPoolReadFailure }),
    }),
    close: async () => {
      generation += 1;
      closeScopes();
      closed = true;
      retry.cancel();
      await serial;
      l1PayloadBinding.close();
      await publicDa.close();
      journal.close();
      report = { phase: "closed", pendingHeaders: [] };
    },
  };
};
