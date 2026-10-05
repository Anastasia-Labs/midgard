import {
  DA_ATTESTATION_TIMEOUT_MS,
  fetchCorrectionLockUTxOProgram,
  fetchSortedStateQueueUTxOsProgram,
  getStateQueueNodeFromStateQueueDatum,
  HUB_ORACLE_ASSET_NAME,
  incompletePruneUnattestedBlockDescendantTxProgram,
  incompleteRemoveLastUnattestedBlockTxProgram,
  NO_DA_ATTESTATION,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  headerHashOf,
  type TimeoutCorrectionJournalStep,
  transactionInputOutRefs,
} from "./remove-unattested-block.parse-timeout-correction-journal.js";
import {
  hasCompetingCorrection,
  pendingTimeoutCorrection,
  planNextTimeoutCorrection,
  reconcileCompletedTimeoutCorrectionJournal,
  releaseTimeoutCorrectionLeaseBeforeYield,
  reopenRolledBackTimeoutCorrectionSteps,
  requireDeploymentScript,
  selectTimeoutCorrectionTarget,
  type SubmitUnattestedTimeoutCorrectionResult,
} from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import {
  recoverTimeoutCorrectionAttempt,
  resolveTimeoutCorrectionValidityRange,
  type SubmitUnattestedTimeoutCorrectionParams,
} from "./remove-unattested-block.recover-timeout-correction-attempt.js";
import {
  adoptLandedTimeoutCorrectionAttempts,
  assertTimeoutCorrectionExclusion,
  timeoutCorrectionExclusionInputs,
} from "./remove-unattested-block.supersede-timeout-correction-attempts.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  outRefLabel,
  requireDeploymentReferenceScript,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
} from "./runtime.js";
import { selectFeeInput } from "./step-support.js";
import { inspectSignedWorkflowTransaction } from "./workflow/signed-transaction-reconciliation.js";

export const submitUnattestedTimeoutCorrection = async ({
  lucid,
  deploymentInfo: rawDeploymentInfo,
  network,
  signer,
  journalStore,
  awaitConfirmation = true,
  nowMs = Date.now,
  stateQueueMutationLeaseCoordinator,
  recovery,
  attemptReadSchedule,
}: SubmitUnattestedTimeoutCorrectionParams): Promise<SubmitUnattestedTimeoutCorrectionResult> => {
  signer.selectWallet(lucid);
  const deploymentInfo = parseContractDeploymentInfo(rawDeploymentInfo);
  const stateQueuePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueUnattestedTimeoutWithdrawalScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueUnattestedTimeoutWithdraw",
  );
  const correctionLockSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "correctionLockSpend",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "hubOracleMint",
  );
  const stateQueueAddress = validatorToAddress(
    network,
    stateQueueSpendingScript,
  );
  const stateQueueConfig = { stateQueueAddress, stateQueuePolicyId };
  const [
    correctionLockSpendRef,
    stateQueueSpendRef,
    stateQueueMintRef,
    stateQueueUnattestedTimeoutWithdrawRef,
  ] = await Promise.all([
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "correctionLockSpend",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueSpend",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueMint",
    }),
    requireDeploymentReferenceScript({
      lucid,
      deploymentInfo,
      name: "stateQueueUnattestedTimeoutWithdraw",
    }),
  ]);
  const referenceScripts = {
    correctionLockSpend: correctionLockSpendRef,
    stateQueueSpend: stateQueueSpendRef,
    stateQueueMint: stateQueueMintRef,
  };
  const hubOracleRefInput = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(hubOraclePolicyId),
    ),
    unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "hub oracle",
  });
  const loadCorrectionLock = () =>
    Effect.runPromise(
      fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: validatorToAddress(
          network,
          correctionLockSpendingScript,
        ),
        hubOraclePolicyId,
      }),
    );
  const loadQueue = () =>
    Effect.runPromise(
      fetchSortedStateQueueUTxOsProgram(lucid, stateQueueConfig),
    );

  let queue = await loadQueue();
  const initialLock = await loadCorrectionLock();
  let journal = await journalStore.load();
  if (hasCompetingCorrection(initialLock.datum, journal?.targetHeaderHash)) {
    // Another timeout actor may have stopped after acquiring the on-chain lock.
    // Retire or confirm our old attempts before adopting that authenticated target.
    if (
      initialLock.datum === "Idle" ||
      initialLock.datum.Locked.correction_identity !== "AttestationTimeout" ||
      journal === undefined
    )
      return pendingTimeoutCorrection(journal);
    journal = reopenRolledBackTimeoutCorrectionSteps(journal, queue);
    await journalStore.save(journal);
    while (
      journal.steps.some(
        (step) => step.status === "prepared" || step.status === "submitted",
      )
    ) {
      const reconciled = await recoverTimeoutCorrectionAttempt({
        journal,
        queue,
        transactionStatus: "unknown",
        recovery,
        allowRebroadcast: false,
        authorizeResubmission: async () => {
          throw new Error("A competing correction owns the lock.");
        },
      });
      if (reconciled.disposition === "pending")
        return pendingTimeoutCorrection(journal);
      journal = reconciled.journal;
      await journalStore.save(journal);
      queue = await loadQueue();
    }
    if (journalStore.archive === undefined)
      return pendingTimeoutCorrection(journal);
    await journalStore.archive(journal);
    journal = undefined;
  }
  if (journal?.completed === true) {
    journal = reconcileCompletedTimeoutCorrectionJournal(journal, queue);
    if (journal?.completed === true) {
      if (initialLock.datum !== "Idle")
        throw new Error(
          "Completed timeout journal conflicts with an active correction lock.",
        );
      return {
        status: "complete",
        targetHeaderHash: journal.targetHeaderHash,
        deadlineMs: journal.targetDeadlineMs,
        pendingTxHash: null,
        submittedTxHashes: journal.steps.map((step) => step.txHash),
        removedHeaderHashes: journal.steps
          .filter((step) => step.status === "confirmed")
          .map((step) => step.removedHeaderHash),
      };
    }
    if (journal !== undefined) await journalStore.save(journal);
  }
  if (journal === undefined) {
    const selected = await selectTimeoutCorrectionTarget(
      queue,
      BigInt(nowMs()),
      initialLock.datum,
    );
    if (selected === undefined)
      return {
        status: "empty",
        targetHeaderHash: null,
        deadlineMs: null,
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    if (BigInt(nowMs()) < selected.deadline)
      return {
        status: "not-ready",
        targetHeaderHash: headerHashOf(selected.target),
        deadlineMs: selected.deadline.toString(),
        pendingTxHash: null,
        submittedTxHashes: [],
        removedHeaderHashes: [],
      };
    journal = {
      version: 1,
      targetHeaderHash: headerHashOf(selected.target),
      targetDeadlineMs: selected.deadline.toString(),
      steps: [],
      completed: false,
    };
    await journalStore.save(journal);
  }

  const lease = await stateQueueMutationLeaseCoordinator?.acquire();
  let leaseReleased = false;
  try {
    while (true) {
      queue = await loadQueue();
      const reopened = await adoptLandedTimeoutCorrectionAttempts({
        journal: reopenRolledBackTimeoutCorrectionSteps(journal, queue),
        queue,
        recovery,
        nowMs: nowMs(),
        ...(attemptReadSchedule === undefined
          ? {}
          : { schedule: attemptReadSchedule }),
      });
      if (reopened !== journal) {
        journal = reopened;
        await journalStore.save(journal);
      }
      if (
        hasCompetingCorrection(
          (await loadCorrectionLock()).datum,
          journal.targetHeaderHash,
        )
      ) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return pendingTimeoutCorrection(journal);
      }
      const lastStep = journal.steps.find(
        (step) => step.status === "prepared" || step.status === "submitted",
      );
      if (lastStep !== undefined) {
        const txStatus = await lucid
          .transactionStatus(lastStep.txHash)
          .catch(() => ({ status: "not_found" as const }));
        const activeTargetHeaderHash = journal.targetHeaderHash;
        const reconciliation = await recoverTimeoutCorrectionAttempt({
          journal,
          queue,
          transactionStatus: txStatus.status,
          recovery,
          authorizeResubmission: async (signed) => {
            const retained = await journalStore.load();
            const pending = retained?.steps.find(
              (step) =>
                step.status === "prepared" || step.status === "submitted",
            );
            if (
              retained?.targetHeaderHash !== activeTargetHeaderHash ||
              pending?.txHash !== signed.transactionHash ||
              pending.signedCbor !== signed.signedTransactionCborHex
            )
              throw new Error(
                "Timeout rebroadcast no longer matches the retained active attempt.",
              );
            const currentQueue = await loadQueue();
            const currentPlan = planNextTimeoutCorrection(
              currentQueue,
              activeTargetHeaderHash,
            );
            const lock = await loadCorrectionLock();
            if (
              currentPlan === undefined ||
              currentPlan.removed.datum.key === "Empty" ||
              headerHashOf(currentPlan.removed) !== pending.removedHeaderHash ||
              (lock.datum !== "Idle" &&
                (lock.datum.Locked.correction_identity !==
                  "AttestationTimeout" ||
                  lock.datum.Locked.target_header_hash !==
                    activeTargetHeaderHash))
            )
              throw new Error(
                "Timeout rebroadcast requires the same live target, descendant and correction owner.",
              );
          },
        });
        journal = reconciliation.journal;
        if (reconciliation.disposition === "pending") {
          if (!awaitConfirmation) {
            leaseReleased =
              await releaseTimeoutCorrectionLeaseBeforeYield(lease);
            return {
              status: "pending",
              targetHeaderHash: journal.targetHeaderHash,
              deadlineMs: journal.targetDeadlineMs,
              pendingTxHash: lastStep.txHash,
              submittedTxHashes: journal.steps.map((step) => step.txHash),
              removedHeaderHashes: journal.steps
                .filter((step) => step.status === "confirmed")
                .map((step) => step.removedHeaderHash),
            };
          }
          await new Promise((resolve) =>
            setTimeout(resolve, DEFAULT_CONFIRMATION_POLL_MS),
          );
          continue;
        }
        await journalStore.save(journal);
        // A rollback can reopen several prior signed attempts. Reconcile every
        // one before creating any replacement transaction.
        if (
          journal.steps.some(
            (step) => step.status === "prepared" || step.status === "submitted",
          )
        )
          continue;
        queue = await loadQueue();
      }

      const plan = planNextTimeoutCorrection(queue, journal.targetHeaderHash);
      if (plan === undefined) {
        const correctionLock = await loadCorrectionLock();
        if (correctionLock.datum !== "Idle") {
          leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
          return pendingTimeoutCorrection(journal);
        }
        journal = { ...journal, completed: true };
        await journalStore.save(journal);
        await lease?.release();
        leaseReleased = true;
        return {
          status: "complete",
          targetHeaderHash: journal.targetHeaderHash,
          deadlineMs: journal.targetDeadlineMs,
          pendingTxHash: null,
          submittedTxHashes: journal.steps.map((step) => step.txHash),
          removedHeaderHashes: journal.steps
            .filter((step) => step.status === "confirmed")
            .map((step) => step.removedHeaderHash),
        };
      }

      const deadline = BigInt(journal.targetDeadlineMs);
      const { validFrom, validTo } = resolveTimeoutCorrectionValidityRange(
        lucid,
        deadline,
        BigInt(nowMs()),
      );
      const targetNode = await Effect.runPromise(
        getStateQueueNodeFromStateQueueDatum(plan.target.datum),
      );
      if (
        targetNode.da_attestation !== NO_DA_ATTESTATION ||
        targetNode.header.endTime + DA_ATTESTATION_TIMEOUT_MS !== deadline
      )
        throw new Error(
          "Timeout target attestation or immutable deadline changed before signing.",
        );
      lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
      const walletUtxos = await lucid.wallet().getUtxos();
      const feeInput = selectFeeInput(walletUtxos);
      const correctionLockInput = await loadCorrectionLock();
      if (
        hasCompetingCorrection(
          correctionLockInput.datum,
          journal.targetHeaderHash,
        )
      ) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return pendingTimeoutCorrection(journal);
      }
      // Mutually exclusive with every abandoned attempt: a shared node or
      // lock input, else one of its wallet inputs, which coin selection tops
      // up from the rest of the wallet.
      const exclusionInputs = timeoutCorrectionExclusionInputs({
        journal,
        protocolInputOutRefs: [
          ...plan.inputOutRefs,
          outRefLabel(correctionLockInput.utxo),
        ],
        walletUtxos,
      });
      const common = {
        timedOutBlockUTxO: plan.target,
        additionalInputs: [
          ...exclusionInputs,
          ...(exclusionInputs.some(
            (utxo) => outRefLabel(utxo) === outRefLabel(feeInput),
          )
            ? []
            : [feeInput]),
        ],
        hubOracleRefInput,
        correctionLockInput,
        correctionLockSpendingScript,
        validFrom,
        validTo,
        stateQueueSpendingScript,
        stateQueueMintingScript,
        referenceScripts,
        yieldWitness: {
          referenceInput: stateQueueUnattestedTimeoutWithdrawRef,
          script: stateQueueUnattestedTimeoutWithdrawalScript,
        },
      } as const;
      const tx =
        plan.kind === "prune-descendant"
          ? incompletePruneUnattestedBlockDescendantTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                predecessorRefInput: plan.predecessor,
                removedDescendantUTxO: plan.removed,
              },
            )
          : incompleteRemoveLastUnattestedBlockTxProgram(
              lucid,
              stateQueueConfig,
              {
                ...common,
                predecessorUTxO: plan.predecessor,
              },
            );
      const unsigned = await tx
        .addSignerKey(signer.paymentKeyHash)
        .complete({ localUPLCEval: true });
      const signed = await unsigned.sign.withWallet().complete();
      const txHash = signed.toHash();
      const signedCbor = signed.toCBOR();
      const inspected = inspectSignedWorkflowTransaction({
        transactionHash: txHash,
        signedTransactionCborHex: signedCbor,
      });
      if (
        inspected.validFromSlot === undefined ||
        inspected.expiresAtSlot === undefined
      )
        throw new Error("Timeout correction requires bounded signed validity.");
      assertTimeoutCorrectionExclusion({
        journal,
        inputOutRefs: transactionInputOutRefs(inspected.body.inputs()),
        walletUtxos,
      });
      const step: TimeoutCorrectionJournalStep = {
        kind: plan.kind,
        removedHeaderHash: headerHashOf(plan.removed),
        inputOutRefs: transactionInputOutRefs(inspected.body.inputs()),
        txHash,
        signedCbor,
        validFromSlot: inspected.validFromSlot.toString(),
        validToSlot: inspected.expiresAtSlot.toString(),
        status: "prepared",
      };
      const sameTxIndex = journal.steps.findIndex(
        (entry) => entry.txHash === txHash,
      );
      if (
        sameTxIndex >= 0 &&
        !["superseded", "abandoned", "retired"].includes(
          journal.steps[sameTxIndex]!.status,
        )
      ) {
        throw new Error(
          `Timeout-correction transaction hash ${txHash} conflicts with non-superseded journal state.`,
        );
      }
      journal = {
        ...journal,
        steps:
          sameTxIndex < 0
            ? [...journal.steps, step]
            : [
                ...journal.steps.filter((_, index) => index !== sameTxIndex),
                step,
              ],
      };
      await journalStore.save(journal);
      let submittedTxHash: string;
      try {
        submittedTxHash = await signed.submit();
      } catch {
        // Submission may have reached the node. The next iteration observes
        // these exact retained bytes before authorizing any replacement.
        if (awaitConfirmation) continue;
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return {
          status: "pending",
          targetHeaderHash: journal.targetHeaderHash,
          deadlineMs: journal.targetDeadlineMs,
          pendingTxHash: txHash,
          submittedTxHashes: journal.steps.map((entry) => entry.txHash),
          removedHeaderHashes: journal.steps
            .filter((entry) => entry.status === "confirmed")
            .map((entry) => entry.removedHeaderHash),
        };
      }
      if (submittedTxHash !== txHash) {
        throw new Error(
          `Provider returned transaction hash ${submittedTxHash}, expected ${txHash}.`,
        );
      }
      const submittedStepIndex: number = journal.steps.length - 1;
      journal = {
        ...journal,
        steps: journal.steps.map(
          (entry, index): TimeoutCorrectionJournalStep =>
            index === submittedStepIndex
              ? { ...entry, status: "submitted" }
              : entry,
        ),
      };
      await journalStore.save(journal);
      await lease?.renew();
      if (!awaitConfirmation) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return {
          status: "pending",
          targetHeaderHash: journal.targetHeaderHash,
          deadlineMs: journal.targetDeadlineMs,
          pendingTxHash: txHash,
          submittedTxHashes: journal.steps.map((entry) => entry.txHash),
          removedHeaderHashes: journal.steps
            .filter((entry) => entry.status === "confirmed")
            .map((entry) => entry.removedHeaderHash),
        };
      }
      // Canonical signed-byte recovery and queue observation confirm on the next iteration.
    }
  } catch (error) {
    if (lease !== undefined && !leaseReleased) {
      await lease.fail(error instanceof Error ? error.message : String(error));
    }
    throw error;
  }
};
