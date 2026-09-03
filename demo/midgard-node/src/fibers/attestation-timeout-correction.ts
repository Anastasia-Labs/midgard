import { readFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";

import {
  createFileTimeoutCorrectionJournalStore,
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  submitUnattestedTimeoutCorrection,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  DaPayloadTerminalOutcomesDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import {
  ContractDeploymentIdentity,
  createDatabaseStateQueueCorrectionObserverStore,
  Database,
  Lucid,
  makeLocalKupmiosStateQueueCorrectionSource,
  MidgardContracts,
  NodeConfig,
  reconcileStateQueueCorrectionObserver,
  reincludeFinalizedStateQueueCorrectionTransition,
  restoreRetractedStateQueueCorrectionTransition,
} from "../services/index.js";

export const ATTESTATION_TIMEOUT_ALERT_LEAD_MS =
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
const TIMEOUT_CORRECTION_LEASE_HOLDER = "attestation_timeout_removal";

export type AttestationTimeoutObservation =
  | { readonly status: "queue-empty" | "head-attested" }
  | {
      readonly status: "waiting" | "near-timeout" | "timed-out";
      readonly headerHash: string;
      readonly deadlineMs: bigint;
      readonly remainingMs: bigint;
    };

/** Read-only head classification; the watcher never owns correction writes. */
export const observeAttestationTimeoutHead = (
  queue: readonly SDK.StateQueueUTxO[],
  nowMs: bigint,
): Effect.Effect<AttestationTimeoutObservation, SDK.DataCoercionError> =>
  Effect.gen(function* () {
    const head = queue[1];
    if (head === undefined) {
      return { status: "queue-empty" } as const;
    }
    const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(head.datum);
    if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
      return { status: "head-attested" } as const;
    }
    const headerHash = yield* SDK.headerHashFromStateQueueUTxO(head);
    const deadlineMs = node.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    const remainingMs = deadlineMs - nowMs;
    return {
      status:
        remainingMs <= 0n
          ? "timed-out"
          : remainingMs <= ATTESTATION_TIMEOUT_ALERT_LEAD_MS
            ? "near-timeout"
            : "waiting",
      headerHash,
      deadlineMs,
      remainingMs,
    } as const;
  });

export const attestationTimeoutCorrectionAction = (): Effect.Effect<
  void,
  unknown,
  Lucid | MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const nodeConfig = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const sql = yield* SqlClient.SqlClient;
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
    const observerResult = yield* Effect.tryPromise({
      try: () =>
        reconcileStateQueueCorrectionObserver({
          deploymentIdentityDigest: deploymentIdentity.manifestId!,
          stateQueuePolicyId: contracts.stateQueue.policyId,
          requiredFinalityDepth: BigInt(manifestFinalityDepth),
          source,
          store: createDatabaseStateQueueCorrectionObserverStore({
            sql,
            deploymentManifest: deploymentIdentity.manifest,
          }),
          reinclude: async (transition) => {
            await Effect.runPromise(
              reincludeFinalizedStateQueueCorrectionTransition(transition, {
                expectedDeploymentIdentityDigest:
                  deploymentIdentity.manifestId!,
                requiredFinalityDepth: BigInt(manifestFinalityDepth),
              }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
            );
          },
          restoreAfterRollback: async (transition) => {
            await Effect.runPromise(
              restoreRetractedStateQueueCorrectionTransition(transition, {
                expectedDeploymentIdentityDigest:
                  deploymentIdentity.manifestId!,
                requiredFinalityDepth: BigInt(manifestFinalityDepth),
              }).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
            );
          },
          revokeTerminal: async (transition) => {
            await Effect.runPromise(
              DaPayloadTerminalOutcomesDB.revokeAuthenticatedTransition(
                transition,
                deploymentIdentity.manifest,
              ).pipe(Effect.provideService(SqlClient.SqlClient, sql)),
            );
          },
        }),
      catch: (cause) => cause,
    });
    if (
      observerResult.admittedTransactionHashes.length > 0 ||
      observerResult.retractedTransactionHashes.length > 0
    ) {
      yield* Effect.logInfo(
        `State-queue correction observer reconciled admitted=${observerResult.admittedTransactionHashes.join(",") || "none"},retracted=${observerResult.retractedTransactionHashes.join(",") || "none"},post_finality_incidents=${observerResult.postFinalityRollbackTransactionHashes.join(",") || "none"}.`,
      );
    }
    const observation = yield* observeAttestationTimeoutHead(
      queue,
      BigInt(Date.now()),
    );
    if (
      observation.status === "queue-empty" ||
      observation.status === "head-attested" ||
      observation.status === "waiting"
    ) {
      return;
    }
    if (observation.status === "near-timeout") {
      yield* Effect.logWarning(
        `State-queue head is nearing its DA-attestation timeout (header=${observation.headerHash},deadline_ms=${observation.deadlineMs.toString()},remaining_ms=${observation.remainingMs.toString()}).`,
      );
      return;
    }

    const deploymentInfoPath =
      process.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH?.trim();
    if (deploymentInfoPath === undefined || deploymentInfoPath.length === 0) {
      return yield* Effect.fail(
        new Error(
          "Timed-out unattested state-queue head requires MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH for authenticated reference-script identities.",
        ),
      );
    }
    const journalPath =
      process.env.MIDGARD_ATTESTATION_TIMEOUT_JOURNAL_PATH?.trim() ||
      resolve(
        dirname(nodeConfig.LEDGER_MPF_DB_PATH),
        "attestation-timeout-correction-v1.json",
      );
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
                journalStore:
                  createFileTimeoutCorrectionJournalStore(journalPath),
                awaitConfirmation: true,
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
  Lucid | MidgardContracts | ContractDeploymentIdentity | Database | NodeConfig
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
