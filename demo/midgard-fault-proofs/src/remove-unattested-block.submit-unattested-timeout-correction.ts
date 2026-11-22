import { isSpentInputSubmitRejection } from "@al-ft/midgard-core/ogmios-json-rpc-error";
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
  resolveTimeoutCorrectionValidityRange,
  type SubmitUnattestedTimeoutCorrectionParams,
  TimeoutCorrectionAttemptInFlightError,
  timeoutCorrectionWallet,
} from "./remove-unattested-block.timeout-correction-params.js";
import {
  hasCompetingCorrection,
  headerHashOf,
  planNextTimeoutCorrection,
  releaseTimeoutCorrectionLeaseBeforeYield,
  requireDeploymentScript,
  selectTimeoutCorrectionTarget,
  type SubmitUnattestedTimeoutCorrectionResult,
} from "./remove-unattested-block.timeout-correction-plan.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  requireDeploymentReferenceScript,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
} from "./runtime.js";
import { selectFeeInput } from "./step-support.js";

export const submitUnattestedTimeoutCorrection = async ({
  lucid,
  deploymentInfo: rawDeploymentInfo,
  network,
  signer,
  awaitConfirmation = true,
  nowMs = Date.now,
  stateQueueMutationLeaseCoordinator,
  wallet,
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
  const lock = await loadCorrectionLock();
  // A lock of another correction kind holds the queue until it is released.
  if (hasCompetingCorrection(lock.datum))
    return {
      status: "pending",
      targetHeaderHash: null,
      deadlineMs: null,
      pendingTxHash: null,
    };
  const selected = await selectTimeoutCorrectionTarget(
    await loadQueue(),
    BigInt(nowMs()),
    lock.datum,
  );
  if (selected === undefined)
    return {
      status: "empty",
      targetHeaderHash: null,
      deadlineMs: null,
      pendingTxHash: null,
    };
  const targetHeaderHash = headerHashOf(selected.target);
  const deadline = selected.deadline;
  const result = (
    status: SubmitUnattestedTimeoutCorrectionResult["status"],
    pendingTxHash: string | null = null,
  ): SubmitUnattestedTimeoutCorrectionResult => ({
    status,
    targetHeaderHash,
    deadlineMs: deadline.toString(),
    pendingTxHash,
  });
  if (BigInt(nowMs()) < deadline) return result("not-ready");

  const lease = await stateQueueMutationLeaseCoordinator?.acquire();
  let leaseReleased = false;
  try {
    // Each pass reads the queue and the lock and builds the step they call
    // for: the target's descendant to prune, or the target itself.
    while (true) {
      const plan = planNextTimeoutCorrection(
        await loadQueue(),
        targetHeaderHash,
      );
      const correctionLockInput = await loadCorrectionLock();
      if (hasCompetingCorrection(correctionLockInput.datum, targetHeaderHash)) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return result("pending");
      }
      if (plan === undefined) {
        if (correctionLockInput.datum !== "Idle") {
          leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
          return result("pending");
        }
        await lease?.release();
        leaseReleased = true;
        return result("complete");
      }

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
        targetNode.block_time + DA_ATTESTATION_TIMEOUT_MS !== deadline
      )
        throw new Error(
          "Timeout target attestation or immutable deadline changed before signing.",
        );
      const funding = timeoutCorrectionWallet(lucid, wallet);
      const walletUtxos = await funding.utxos();
      const common = {
        timedOutBlockUTxO: plan.target,
        additionalInputs: [selectFeeInput(walletUtxos)],
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
        .complete({ localUPLCEval: true, presetWalletInputs: walletUtxos });
      const signed = await funding.sign(unsigned);
      const txHash = signed.toHash();
      let submittedTxHash: string;
      try {
        submittedTxHash = await signed.submit();
      } catch (error) {
        if (!isSpentInputSubmitRejection(error)) throw error;
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        throw new TimeoutCorrectionAttemptInFlightError(txHash, {
          cause: error,
        });
      }
      if (submittedTxHash !== txHash) {
        throw new Error(
          `Provider returned transaction hash ${submittedTxHash}, expected ${txHash}.`,
        );
      }
      if (!awaitConfirmation) {
        leaseReleased = await releaseTimeoutCorrectionLeaseBeforeYield(lease);
        return result("pending", txHash);
      }
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
      await lease?.renew();
    }
  } catch (error) {
    if (lease !== undefined && !leaseReleased) {
      await lease.fail(error instanceof Error ? error.message : String(error));
    }
    throw error;
  }
};
