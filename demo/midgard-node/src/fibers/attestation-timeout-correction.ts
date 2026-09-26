import { readFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";

import {
  createFileTimeoutCorrectionJournalStore,
  createLocalKupmiosTimeoutCorrectionRecovery,
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  submitUnattestedTimeoutCorrection,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Effect, Runtime, Schedule } from "effect";

import {
  DaPayloadTerminalOutcomesDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import {
  attestationTimeoutJournalPathOverride,
  contractDeploymentInfoPathOverride,
} from "../environment.js";
import {
  observeAttestationTimeoutQueue,
  timeoutCorrectionJournalNeedsRecovery,
} from "../services/attestation-timeout-observation.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import { publishMempoolLedgerDelta } from "../services/globals.js";
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
} from "../services/index.js";

export const ATTESTATION_TIMEOUT_ALERT_LEAD_MS =
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

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
    const journalStore = createFileTimeoutCorrectionJournalStore(journalPath);
    const retainedJournal = yield* Effect.tryPromise({
      try: () => journalStore.load(),
      catch: (cause) => cause,
    });
    const resumeRetainedJournal = timeoutCorrectionJournalNeedsRecovery(
      retainedJournal,
      queue,
    );
    const observation = yield* observeAttestationTimeoutQueue(
      queue,
      BigInt(Date.now()),
      ATTESTATION_TIMEOUT_ALERT_LEAD_MS,
    );
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

/** Operator-owned correction scheduler. Watcher processes remain observe-only. */
export const attestationTimeoutCorrectionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | Lucid
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | Globals
  | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Attestation-timeout correction fiber started.");
    yield* Effect.repeat(
      attestationTimeoutCorrectionAction().pipe(
        Effect.withSpan("attestation-timeout-correction-fiber"),
        Effect.catchAllCause(Effect.logWarning),
      ),
      schedule,
    );
  });
