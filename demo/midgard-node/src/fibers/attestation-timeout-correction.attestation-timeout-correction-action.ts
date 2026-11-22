import { readFile } from "node:fs/promises";

import { proofMaterialsAddress } from "@al-ft/midgard-fault-proofs/funding/proof-materials-address";
import { submitUnattestedTimeoutCorrection } from "@al-ft/midgard-fault-proofs/remove-unattested-block";
import * as SDK from "@al-ft/midgard-sdk/core";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { Cause, Effect, Ref, Runtime } from "effect";

import { StateQueueMutationLeasesDB } from "../database/index.js";
import { contractDeploymentInfoPathOverride } from "../environment.js";
import { l1NowUnixTimeMs } from "../l1-heads.js";
import { Globals } from "../services/globals.globals.js";
import { type AttestationTimeoutCorrectionHealth } from "../services/globals.js";
import { Database, NodeConfig } from "../services/index.js";
import { type IntentJournal } from "../services/intent-journal.js";
import { landedStateQueueUTxOs } from "../services/landed-state-queue.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import {
  readSelectedWalletViewInputs,
  signOverWalletView,
} from "../transactions/utils.wallet-view.js";
import {
  observeAndRecordAttestationTimeoutQueue,
  TIMEOUT_CORRECTION_LEASE_HOLDER,
} from "./attestation-timeout-correction.reconcile-state-queue-corrections.js";

export const attestationTimeoutCorrectionAction = (): Effect.Effect<
  void,
  unknown,
  Lucid | MidgardContracts | Database | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const queue = yield* landedStateQueueUTxOs(
      contracts.stateQueue,
      "attestation-timeout correction",
    );
    const runtime = yield* Effect.runtime<Database | IntentJournal>();
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
      (observation.status === "queue-empty" ||
        observation.status === "queue-attested" ||
        observation.status === "waiting")
    ) {
      return;
    }
    if (!resumeLockedTimeout && observation.status === "near-timeout") {
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
                  // A timeout correction publishes no proof material.
                  proofMaterialsAddress: proofMaterialsAddress(
                    nodeConfig.NETWORK,
                    paymentCredential.hash,
                  ),
                  paymentKeyHash: paymentCredential.hash,
                  selectWallet: () => undefined,
                },
                // One step per tick: the next tick, on the next L1 head,
                // reads the queue and the lock and builds the step after it.
                awaitConfirmation: false,
                // Funded from the operator wallet's view (§8.5) and signed
                // over it, like every other node transaction.
                wallet: {
                  utxos: () =>
                    Runtime.runPromise(runtime)(
                      readSelectedWalletViewInputs(
                        lucid.api,
                        "an attestation-timeout correction",
                      ),
                    ),
                  sign: (unsigned) =>
                    Runtime.runPromise(runtime)(
                      signOverWalletView(lucid.api, unsigned).pipe(
                        Effect.flatMap((signing) =>
                          Effect.tryPromise(() => signing.complete()),
                        ),
                      ),
                    ),
                },
              }),
            catch: (cause) => cause,
          });
          // The removal's effects follow from the follower's facts once it
          // lands: the queue-terminal projection records it, and the
          // landed-block rebase disposes of the removed blocks' journals.
          yield* Effect.logInfo(
            `Attestation-timeout correction result status=${result.status},target=${result.targetHeaderHash ?? "none"},transaction=${result.pendingTxHash ?? "none"}.`,
          );
        }),
    );
    if (leaseResult._tag === "Busy") {
      yield* Effect.logInfo(
        `Skipping attestation-timeout correction because state-queue mutation lease is busy (holder=${leaseResult.activeLease?.holder ?? "unknown"}).`,
      );
    }
  });

/** One scheduled correction step. A failure is logged and retried on the
 * next tick. Never fails. Every outcome is recorded in `health`, which
 * readiness reads. */
export const attestationTimeoutCorrectionStep = <R>(
  action: Effect.Effect<void, unknown, R>,
  health: Ref.Ref<AttestationTimeoutCorrectionHealth>,
): Effect.Effect<void, never, R> =>
  action.pipe(
    Effect.zipRight(
      Ref.update(health, (current) => ({
        ...current,
        lastProgressAtMs: Date.now(),
        consecutiveFailures: 0,
      })),
    ),
    Effect.catchAllCause((cause) =>
      Ref.update(health, (current) => ({
        ...current,
        lastFailureAtMs: Date.now(),
        lastError: String(Cause.squash(cause)),
        consecutiveFailures: current.consecutiveFailures + 1,
      })).pipe(Effect.zipRight(Effect.logWarning(cause))),
    ),
  );
