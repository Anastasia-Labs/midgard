import { readFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";

import {
  createFileTimeoutCorrectionJournalStore,
  createLocalKupmiosTimeoutCorrectionRecovery,
  submitUnattestedTimeoutCorrection,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Cause, Effect, Ref, Runtime } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import {
  attestationTimeoutJournalPathOverride,
  contractDeploymentInfoPathOverride,
} from "../environment.js";
import { l1NowUnixTimeMs } from "../l1-heads.js";
import { timeoutCorrectionJournalNeedsRecovery } from "../services/attestation-timeout-observation.js";
import { type AttestationTimeoutCorrectionHealth } from "../services/globals.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  makeLocalKupmiosStateQueueCorrectionSource,
  MidgardContracts,
  NodeConfig,
  StateQueueCorrectionRewindIntegrityError,
} from "../services/index.js";
import { landedStateQueueUTxOs } from "../services/landed-state-queue.js";
import {
  clearLivenessIncident,
  HaltSource,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  observeAndRecordAttestationTimeoutQueue,
  reconcileStateQueueCorrections,
  TIMEOUT_CORRECTION_LEASE_HOLDER,
  withTimeoutCorrectionProgress,
} from "./attestation-timeout-correction.reconcile-state-queue-corrections.js";

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
    const readQueue = landedStateQueueUTxOs(
      contracts.stateQueue,
      "attestation-timeout correction",
    );
    const queue = yield* readQueue;
    const runtime = yield* Effect.runtime<Database>();
    // The timeout is an L1 deadline, judged at the L1 `slotNow`; an unknown
    // slot fails the tick and the fiber retries.
    const l1NowMs = yield* l1NowUnixTimeMs(lucid.api);
    // Recorded before anything below can fail, so readiness knows whether a
    // failing step is leaving a timed-out header uncorrected. A classification
    // failure is raised below, where the tick uses it.
    const observed = yield* observeAndRecordAttestationTimeoutQueue(
      globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
      queue,
      { l1NowMs, readAtMs: Date.now() },
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
        await queueNodes(await Runtime.runPromise(runtime)(readQueue)),
    });
    const observerResult = yield* reconcileStateQueueCorrections({
      source,
      deploymentIdentityDigest: deploymentIdentity.manifestId,
      stateQueuePolicyId: contracts.stateQueue.policyId,
      requiredFinalityDepth: BigInt(manifestFinalityDepth),
      deploymentManifest: deploymentIdentity.manifest,
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
          // authenticated, rollback-aware transition observer and the history
          // owner's correction rewind.
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

/** The readiness reason a rewind integrity failure raises. */
export const STATE_QUEUE_CORRECTION_REWIND_CONFLICT =
  "state_queue_correction_rewind_conflict";

/** One scheduled correction step. A transient failure is logged and retried
 * on the next tick. A rewind integrity failure (the node cannot re-apply a
 * rewound block) is not transient: it raises
 * `state_queue_correction_rewind_conflict`, which holds the commit, merge and
 * settlement fibers, and the step keeps refusing its own effects (the
 * observer reconciliation that raises it runs before any submission) while
 * every later tick re-derives the removal against L1. The first step that
 * completes, so whose re-derivation agrees, clears it. Never fails. Every
 * outcome is recorded in `health`, which readiness reads. */
export const attestationTimeoutCorrectionStep = <R>(
  action: Effect.Effect<void, unknown, R>,
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void, never, R> =>
  action.pipe(
    Effect.zipRight(
      Ref.update(health, (current) => ({
        ...current,
        lastProgressAtMs: Date.now(),
        consecutiveFailures: 0,
      })),
    ),
    Effect.zipRight(
      clearLivenessIncident(globals, HaltSource.stateQueueCorrectionRewind),
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
            : raiseLivenessIncident(
                globals,
                HaltSource.stateQueueCorrectionRewind,
                STATE_QUEUE_CORRECTION_REWIND_CONFLICT,
                `${integrity.message} Block commitment, merge and settlement are held, and the correction re-derives the removal against L1 on every tick until it agrees.`,
              ),
        ),
      );
    }),
  );
