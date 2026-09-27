import { readFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";

import {
  createFileTimeoutCorrectionJournalStore,
  createLocalKupmiosTimeoutCorrectionRecovery,
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  submitUnattestedTimeoutCorrection,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionJournalStore,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Cause, Effect, type Either, Ref, Runtime, Schedule } from "effect";

import { ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD } from "../commands/readiness.js";
import {
  DaPayloadTerminalOutcomesDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import {
  attestationTimeoutJournalPathOverride,
  contractDeploymentInfoPathOverride,
} from "../environment.js";
import {
  type AttestationTimeoutObservation,
  observeAttestationTimeoutQueue,
  timeoutCorrectionJournalNeedsRecovery,
} from "../services/attestation-timeout-observation.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import {
  type AttestationTimeoutCorrectionHealth,
  publishMempoolLedgerDelta,
} from "../services/globals.js";
import {
  authorizeStateQueueCorrectionReinclusion,
  ContractDeploymentIdentity,
  createDatabaseStateQueueCorrectionObserverStore,
  Database,
  Globals,
  Lucid,
  makeLocalKupmiosStateQueueCorrectionSource,
  MidgardContracts,
  NodeConfig,
  reconcileStateQueueCorrectionObserver,
  refuseRewoundStateQueueCorrectionRollback,
  reincludeFinalizedStateQueueCorrectionTransition,
  restoreRetractedStateQueueCorrectionTransition,
  type StateQueueCorrectionObserverResult,
  type StateQueueCorrectionObserverSource,
  StateQueueCorrectionRewindIntegrityError,
} from "../services/index.js";

export const ATTESTATION_TIMEOUT_ALERT_LEAD_MS =
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

/**
 * How long readiness lets the correction go without progress, and the state
 * queue go unread, before it reports the node unready. `tickIntervalMs` is the
 * fiber's schedule interval.
 *
 * Stall: between two progress marks a healthy step waits on at most one
 * removal transaction. Its validity range starts no later than it is built
 * and spans STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS, so within that window it
 * either lands or can never land. The bound takes the profile's
 * MAX_VALIDITY_RANGE_LENGTH_MS (the protocol's cap on any validity range, 8
 * minutes in every shipped profile) where it is the longer, and its surplus
 * over the removal window covers building, submitting and confirmation
 * polling. A further failure-threshold of tick intervals covers the wait for
 * the tick that starts the step and provider indexing lag.
 *
 * Queue unknown: a commit's header end time equals its transaction's validity
 * upper bound (commit_bound_header_time_is_valid), which is at or after the
 * moment it lands, so a header the last read did not see comes due no sooner
 * than DA_ATTESTATION_TIMEOUT_MS after that read. Shorter outages are L1 blips
 * that hide nothing due. The bound is never below the stall bound, because a
 * step waiting on a removal records no queue read while it waits.
 */
export const attestationTimeoutCorrectionReadinessBounds = (
  tickIntervalMs: number,
  profile: {
    readonly maxValidityRangeMs: bigint;
    readonly daAttestationTimeoutMs: bigint;
  } = {
    maxValidityRangeMs: SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
    daAttestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
  },
): { readonly stallBoundMs: number; readonly queueUnknownBoundMs: number } => {
  const removalWaitMs =
    profile.maxValidityRangeMs > STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS
      ? profile.maxValidityRangeMs
      : STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS;
  const stallBoundMs =
    Number(removalWaitMs) +
    ATTESTATION_TIMEOUT_CORRECTION_FAILURE_THRESHOLD * tickIntervalMs;
  return {
    stallBoundMs,
    queueUnknownBoundMs: Math.max(
      Number(profile.daAttestationTimeoutMs),
      stallBoundMs,
    ),
  };
};

/** Classifies the state queue once for the tick and records the result for
 * readiness. A classification failure is returned rather than raised, so the
 * tick raises it where it uses the classification and recording never moves
 * that failure ahead of the tick's earlier work. */
export const observeAndRecordAttestationTimeoutQueue = (
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  queue: readonly SDK.StateQueueUTxO[],
  nowMs: number,
): Effect.Effect<
  Either.Either<AttestationTimeoutObservation, SDK.DataCoercionError>
> =>
  observeAttestationTimeoutQueue(
    queue,
    BigInt(nowMs),
    ATTESTATION_TIMEOUT_ALERT_LEAD_MS,
  ).pipe(
    Effect.tap((observation) =>
      Ref.update(health, (current) => ({
        ...current,
        lastQueueReadAtMs: nowMs,
        oldestUnattestedHeader:
          "headerHash" in observation
            ? {
                headerHash: observation.headerHash,
                deadlineMs: Number(observation.deadlineMs),
              }
            : null,
      })),
    ),
    Effect.either,
  );

/** Credits a saved correction journal as progress when it starts a correction
 * or confirms a removal not yet credited. Re-saving an unchanged journal, or
 * resubmitting a removal that never lands, is not progress. */
export const recordTimeoutCorrectionJournalProgress = (
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  journal: TimeoutCorrectionJournal,
  nowMs: number,
): Effect.Effect<void> =>
  Ref.update(health, (current) => {
    const confirmedRemovals = journal.steps.filter(
      (step) => step.status === "confirmed",
    ).length;
    const credited = current.correctionProgress;
    return credited !== null &&
      credited.targetHeaderHash === journal.targetHeaderHash &&
      credited.confirmedRemovals >= confirmedRemovals
      ? current
      : {
          ...current,
          lastProgressAtMs: nowMs,
          correctionProgress: {
            targetHeaderHash: journal.targetHeaderHash,
            confirmedRemovals,
          },
        };
  });

/** The journal store the correction writes through, crediting each save that
 * moves the correction forward, so readiness sees a step that is pruning
 * several descendants as progressing rather than stalled. */
export const withTimeoutCorrectionProgress = (
  store: TimeoutCorrectionJournalStore,
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
): TimeoutCorrectionJournalStore => ({
  ...store,
  save: async (journal) => {
    await store.save(journal);
    Effect.runSync(
      recordTimeoutCorrectionJournalProgress(health, journal, Date.now()),
    );
  },
});

/**
 * Admits authenticated state-queue corrections into the durable observer and
 * applies their local consequences. Reinclusion (a removed block's payloads
 * return to the pending set) and its post-finality rollback inverse rewrite
 * history-owned event rows, so each runs as its own registered Ready history
 * producer, exactly like every other node writer; nothing else holds one here.
 * A closed gate or lagging follower refuses the whole reconciliation before
 * the observer persists, so the next tick replays the same transition from the
 * chain. Both mutations are idempotent, so a crash after either commits is also
 * resumed by replay.
 *
 * Both also change which deposit outputs are spendable (a deposit's L2 output
 * is spendable only while it is assigned to a header), so the producer that
 * committed the change publishes a full validation-cache reload before it
 * releases its registration, the way every ledger mutator publishes its delta.
 *
 * Under Architecture G a removed block this node committed has already moved
 * the native ledger root, so its reinclusion is a rewind, not a forward
 * write: the fiber only admits the correction (the observer persists it) and
 * the history owner's recovery rewinds the native root and reincludes the
 * payloads (see state-queue-correction-rewind). The admission needs no
 * producer, so a gate the owner closed for an earlier removal of the same
 * suffix never blocks admitting the later one.
 */
export const reconcileStateQueueCorrections = ({
  source,
  deploymentIdentityDigest,
  stateQueuePolicyId,
  requiredFinalityDepth,
  deploymentManifest,
  ledgerDeltaLogMax,
  rewindThroughHistoryOwner,
}: {
  readonly source: StateQueueCorrectionObserverSource;
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly requiredFinalityDepth: bigint;
  readonly deploymentManifest: unknown;
  readonly ledgerDeltaLogMax: number;
  /** Architecture G: admit only; the history owner rewinds and reincludes. */
  readonly rewindThroughHistoryOwner: boolean;
}): Effect.Effect<
  StateQueueCorrectionObserverResult,
  unknown,
  Database | Globals
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const globals = yield* Globals;
    const run = Runtime.runPromise(yield* Effect.runtime<Database | Globals>());
    const authority = {
      expectedDeploymentIdentityDigest: deploymentIdentityDigest,
      requiredFinalityDepth,
    };
    const reloadLedgerCacheIf = (changed: boolean) =>
      changed
        ? publishMempoolLedgerDelta(
            globals,
            { full: true, upserts: [], deletes: [] },
            ledgerDeltaLogMax,
          )
        : Effect.void;
    return yield* Effect.tryPromise({
      try: () =>
        reconcileStateQueueCorrectionObserver({
          deploymentIdentityDigest,
          stateQueuePolicyId,
          requiredFinalityDepth,
          source,
          store: createDatabaseStateQueueCorrectionObserverStore({
            sql,
            deploymentManifest,
          }),
          reinclude: async (transition) => {
            if (rewindThroughHistoryOwner) {
              // Refuse an unauthorized transition before the observer admits it.
              authorizeStateQueueCorrectionReinclusion(transition, authority);
              return;
            }
            await run(
              Effect.suspend(() =>
                reincludeFinalizedStateQueueCorrectionTransition(
                  transition,
                  authority,
                ),
              ).pipe(
                Effect.tap((results) =>
                  reloadLedgerCacheIf(
                    results.some(
                      (result) =>
                        result.reopenedEvents > 0 ||
                        result.restoredMempoolTransactions > 0 ||
                        result.restoredProcessedTransactions > 0,
                    ),
                  ),
                ),
                runHistoryProducer,
              ),
            );
          },
          // Refused before the terminal outcome is revoked, so the DA
          // 'removed' authority of a rewound block survives the refusal.
          assertRollbackPermitted: rewindThroughHistoryOwner
            ? async (transition) => {
                await run(
                  refuseRewoundStateQueueCorrectionRollback(
                    transition,
                    authority,
                  ),
                );
              }
            : undefined,
          restoreAfterRollback: async (transition) => {
            if (rewindThroughHistoryOwner) {
              // The native rewind has no inverse: a rolled-back removal whose
              // rewind ran is an explicit integrity failure, and one whose
              // rewind never ran left nothing to restore.
              await run(
                refuseRewoundStateQueueCorrectionRollback(
                  transition,
                  authority,
                ),
              );
              return;
            }
            await run(
              Effect.suspend(() =>
                restoreRetractedStateQueueCorrectionTransition(
                  transition,
                  authority,
                ),
              ).pipe(
                Effect.tap((results) =>
                  reloadLedgerCacheIf(
                    results.some((result) => result.restoredCanonicalBlock),
                  ),
                ),
                runHistoryProducer,
              ),
            );
          },
          revokeTerminal: async (transition) => {
            await run(
              DaPayloadTerminalOutcomesDB.revokeAuthenticatedTransition(
                transition,
                deploymentManifest,
              ),
            );
          },
        }),
      catch: (cause) => cause,
    });
  });

export const attestationTimeoutCorrectionAction = (): Effect.Effect<
  void,
  unknown,
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | Globals
  | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const fetchConfig = {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    };
    const queue = yield* SDK.fetchSortedStateQueueUTxOsProgram(
      lucid.api,
      fetchConfig,
    );
    // Recorded before anything below can fail, so readiness knows whether a
    // failing step is leaving a timed-out header uncorrected. A classification
    // failure is raised below, where the tick uses it.
    const observed = yield* observeAndRecordAttestationTimeoutQueue(
      globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
      queue,
      Date.now(),
    );
    if (deploymentIdentity.manifestId === undefined) {
      return yield* Effect.fail(
        new Error(
          "State-queue correction observation requires a finalized deployment manifest identity.",
        ),
      );
    }
    if (deploymentIdentity.manifest === undefined) {
      return yield* Effect.fail(
        new Error(
          "State-queue terminal retention requires the exact authenticated deployment manifest.",
        ),
      );
    }
    const manifestFinalityDepth =
      deploymentIdentity.l1Finality?.confirmationDepth;
    if (
      manifestFinalityDepth === undefined ||
      manifestFinalityDepth !== nodeConfig.STATE_QUEUE_CORRECTION_FINALITY_DEPTH
    ) {
      return yield* Effect.fail(
        new Error(
          `State-queue correction finality configuration must match the manifest-verified release depth (manifest=${manifestFinalityDepth?.toString() ?? "missing"},node=${nodeConfig.STATE_QUEUE_CORRECTION_FINALITY_DEPTH.toString()}).`,
        ),
      );
    }
    const queueNodes = async (
      sorted: readonly SDK.StateQueueUTxO[],
    ): Promise<readonly SDK.StateQueueTransitionNode[]> => {
      if (
        sorted.some(
          ({ utxo }) => utxo.address !== fetchConfig.stateQueueAddress,
        )
      ) {
        throw new Error(
          "Authenticated state-queue traversal returned a foreign-address output.",
        );
      }
      return await Promise.all(
        sorted.map(async (utxo, index) => ({
          headerHash:
            index === 0
              ? null
              : await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(utxo)),
          outRef: `${utxo.utxo.txHash}#${utxo.utxo.outputIndex.toString()}`,
        })),
      );
    };
    const source = makeLocalKupmiosStateQueueCorrectionSource({
      deploymentIdentityDigest: deploymentIdentity.manifestId,
      stateQueuePolicyId: contracts.stateQueue.policyId,
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
      kupoUrl: nodeConfig.L1_KUPO_KEY,
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      readQueue: async () =>
        await queueNodes(
          await Effect.runPromise(
            SDK.fetchSortedStateQueueUTxOsProgram(lucid.api, fetchConfig),
          ),
        ),
    });
    const observerResult = yield* reconcileStateQueueCorrections({
      source,
      deploymentIdentityDigest: deploymentIdentity.manifestId,
      stateQueuePolicyId: contracts.stateQueue.policyId,
      requiredFinalityDepth: BigInt(manifestFinalityDepth),
      deploymentManifest: deploymentIdentity.manifest,
      ledgerDeltaLogMax: nodeConfig.VALIDATION_LEDGER_DELTA_LOG_MAX,
      rewindThroughHistoryOwner: nodeConfig.MPF_ENGINE === "architecture_g",
    });
    if (
      observerResult.admittedTransactionHashes.length > 0 ||
      observerResult.retractedTransactionHashes.length > 0
    ) {
      yield* Effect.logInfo(
        `State-queue correction observer reconciled admitted=${observerResult.admittedTransactionHashes.join(",") || "none"},retracted=${observerResult.retractedTransactionHashes.join(",") || "none"},post_finality_incidents=${observerResult.postFinalityRollbackTransactionHashes.join(",") || "none"}.`,
      );
    }
    const journalPath =
      attestationTimeoutJournalPathOverride() ??
      resolve(
        dirname(nodeConfig.LEDGER_MPF_DB_PATH),
        "attestation-timeout-correction-v1.json",
      );
    const journalStore = withTimeoutCorrectionProgress(
      createFileTimeoutCorrectionJournalStore(journalPath),
      globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
    );
    const retainedJournal = yield* Effect.tryPromise({
      try: () => journalStore.load(),
      catch: (cause) => cause,
    });
    const resumeRetainedJournal = timeoutCorrectionJournalNeedsRecovery(
      retainedJournal,
      queue,
    );
    const observation = yield* observed;
    const lock = yield* SDK.fetchCorrectionLockUTxOProgram(lucid.api, {
      correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const resumeLockedTimeout =
      lock.datum !== "Idle" &&
      lock.datum.Locked.correction_identity === "AttestationTimeout";
    if (
      !resumeLockedTimeout &&
      !resumeRetainedJournal &&
      (observation.status === "queue-empty" ||
        observation.status === "queue-attested" ||
        observation.status === "waiting")
    ) {
      return;
    }
    if (
      !resumeLockedTimeout &&
      !resumeRetainedJournal &&
      observation.status === "near-timeout"
    ) {
      yield* Effect.logWarning(
        `Pending state-queue block is nearing its DA-attestation timeout (header=${observation.headerHash},deadline_ms=${observation.deadlineMs.toString()},remaining_ms=${observation.remainingMs.toString()}).`,
      );
      return;
    }

    const deploymentInfoPath = contractDeploymentInfoPathOverride();
    if (deploymentInfoPath === undefined) {
      return yield* Effect.fail(
        new Error(
          "Timed-out unattested state-queue block requires MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH for authenticated reference-script identities.",
        ),
      );
    }
    const deploymentInfo = yield* Effect.tryPromise({
      try: async () =>
        JSON.parse(await readFile(deploymentInfoPath, "utf8")) as unknown,
      catch: (cause) =>
        new Error(
          `Failed to read timeout-correction deployment manifest at ${deploymentInfoPath}`,
          { cause },
        ),
    });
    const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      TIMEOUT_CORRECTION_LEASE_HOLDER,
      () =>
        Effect.gen(function* () {
          yield* lucid.switchToOperatorsMainWallet;
          const paymentCredential = getAddressDetails(
            lucid.operatorMainAddress,
          ).paymentCredential;
          if (paymentCredential?.type !== "Key") {
            return yield* Effect.fail(
              new Error(
                "Operator main wallet must use a payment key credential.",
              ),
            );
          }
          const result = yield* Effect.tryPromise({
            try: () =>
              submitUnattestedTimeoutCorrection({
                lucid: lucid.api,
                deploymentInfo,
                network: nodeConfig.NETWORK,
                signer: {
                  source: "operator-node-main-wallet",
                  address: lucid.operatorMainAddress,
                  paymentKeyHash: paymentCredential.hash,
                  selectWallet: () => undefined,
                },
                journalStore,
                awaitConfirmation: true,
                recovery: createLocalKupmiosTimeoutCorrectionRecovery({
                  deploymentManifest: deploymentIdentity.manifest,
                  kupoUrl: nodeConfig.L1_KUPO_KEY,
                  ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
                  network: nodeConfig.NETWORK,
                }),
              }),
            catch: (cause) => cause,
          });
          // Transaction confirmation is not correction provenance or release
          // finality. Payload recovery is driven separately by the node's
          // authenticated, rollback-aware transition observer through
          // reincludeFinalizedStateQueueCorrectionTransition.
          yield* Effect.logInfo(
            `Attestation-timeout correction result status=${result.status},target=${result.targetHeaderHash ?? "none"},transactions=${result.submittedTxHashes.join(",")}.`,
          );
        }),
    );
    if (leaseResult._tag === "Busy") {
      yield* Effect.logInfo(
        `Skipping attestation-timeout correction because state-queue mutation lease is busy (holder=${leaseResult.activeLease?.holder ?? "unknown"}).`,
      );
    }
  });

/** The rewind integrity failure a cause carries, however deeply it was
 * wrapped on its way out of the observer's promise callbacks. */
export const findStateQueueCorrectionRewindIntegrityError = (
  cause: Cause.Cause<unknown>,
): StateQueueCorrectionRewindIntegrityError | undefined => {
  const seen = new Set<unknown>();
  const search = (
    value: unknown,
  ): StateQueueCorrectionRewindIntegrityError | undefined => {
    if (value === null || typeof value !== "object" || seen.has(value))
      return undefined;
    seen.add(value);
    if (value instanceof StateQueueCorrectionRewindIntegrityError) return value;
    if (Cause.isCause(value)) return searchCause(value);
    if (Runtime.isFiberFailure(value))
      return searchCause(value[Runtime.FiberFailureCauseId]);
    return value instanceof Error ? search(value.cause) : undefined;
  };
  const searchCause = (
    inner: Cause.Cause<unknown>,
  ): StateQueueCorrectionRewindIntegrityError | undefined => {
    for (const value of [...Cause.failures(inner), ...Cause.defects(inner)]) {
      const found = search(value);
      if (found !== undefined) return found;
    }
    return undefined;
  };
  return searchCause(cause);
};

/** One scheduled correction step. A transient failure is logged and retried
 * on the next tick; a rewind integrity failure is not transient (the node
 * cannot re-apply a rewound block), so it fails the fiber and stops the node.
 * Every outcome is recorded in `health`, which readiness reads. */
export const attestationTimeoutCorrectionStep = <R>(
  action: Effect.Effect<void, unknown, R>,
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
): Effect.Effect<void, StateQueueCorrectionRewindIntegrityError, R> =>
  action.pipe(
    Effect.zipRight(
      Ref.update(health, (current) => ({
        ...current,
        lastProgressAtMs: Date.now(),
        consecutiveFailures: 0,
      })),
    ),
    Effect.catchAllCause((cause) => {
      const recordFailure = Ref.update(health, (current) => ({
        ...current,
        lastFailureAtMs: Date.now(),
        lastError: String(Cause.squash(cause)),
        consecutiveFailures: current.consecutiveFailures + 1,
      }));
      const integrity = findStateQueueCorrectionRewindIntegrityError(cause);
      return recordFailure.pipe(
        Effect.zipRight(
          integrity === undefined
            ? Effect.logWarning(cause)
            : Effect.logError(integrity.message).pipe(
                Effect.zipRight(Effect.fail(integrity)),
              ),
        ),
      );
    }),
  );

/** Operator-owned correction scheduler. Watcher processes remain observe-only. */
export const attestationTimeoutCorrectionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  StateQueueCorrectionRewindIntegrityError,
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | Globals
  | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Effect.logInfo("Attestation-timeout correction fiber started.");
    yield* Effect.repeat(
      attestationTimeoutCorrectionStep(
        attestationTimeoutCorrectionAction().pipe(
          Effect.withSpan("attestation-timeout-correction-fiber"),
        ),
        globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
      ),
      schedule,
    );
  });
