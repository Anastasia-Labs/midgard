import { join } from "node:path";

import {
  type FraudProofWorkflowJournalEntry,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  Data,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import {
  verifyJourneyComputationThreadAbsent,
  verifyJourneyWorkflowTransactions,
} from "./correction.verify-journey-computation-thread-absent.js";
import {
  JOURNEY_WORKFLOW_STALL_ALLOWANCE_MS,
  journeyIntentRecovery,
  journeyWorkflowProgressCount,
  journeyWorkflowUpdates,
  readJourneyWorkflowEntries,
  transactionOutputs,
  verifyJourneyCorrectedScheduler,
  verifyJourneyCorrectedTail,
} from "./correction.verify-journey-corrected-scheduler.js";
import type { loadJourneyContext } from "./live-context.js";
import type { startJourneyNativeRecorder } from "./native-recorder.js";

/** Shared acceptance assertions for every installed automatic workflow. */
export const verifyJourneyCorrection = async ({
  context,
  native,
  workflowJournalDirectory,
  directory,
  category,
  headerHash,
  predecessorHeaderHash,
  workflowBaseline,
  actionDepth,
  correctionTimeoutMs = 1_800_000,
  progressAllowanceMs,
  reconciliationAllowanceMs,
  stallAllowanceMs = JOURNEY_WORKFLOW_STALL_ALLOWANCE_MS,
  operatorVkey,
  requireLive,
  poll,
  stage,
}: {
  context: Awaited<ReturnType<typeof loadJourneyContext>>;
  native: Awaited<ReturnType<typeof startJourneyNativeRecorder>>;
  workflowJournalDirectory: string;
  directory: string;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
  predecessorHeaderHash: string;
  workflowBaseline: readonly FraudProofWorkflowJournalEntry[];
  /** Depth the watcher acts at; the completed terminal must be this deep. */
  actionDepth: number;
  /** Hard cap on the whole correction: the family's audited or generic plan. */
  correctionTimeoutMs?: number;
  /**
   * Fails the correction as soon as the workflow journal makes no durable
   * progress for this long; one transaction's build, submit, and confirmation
   * allowance. A multi-step family legitimately runs past any fixed wall
   * clock, so progress, not elapsed time, is the health signal.
   * A retained workflow first gets the automatic-decision allowance (15
   * minutes) to re-observe its target before this transaction clock applies.
   */
  progressAllowanceMs?: number;
  /** Existing release-depth window for resolving an outstanding signed intent. */
  reconciliationAllowanceMs?: number;
  stallAllowanceMs?: number;
  operatorVkey: string;
  requireLive(): void;
  poll<T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs?: number,
  ): Promise<T>;
  stage<T>(name: string, action: () => Promise<T>): Promise<T>;
}) => {
  const { deployment, provider, accounts } = context;
  let reported = workflowBaseline.length - 1;
  let reportedStall: string | undefined;
  let progressCount = journeyWorkflowProgressCount(workflowBaseline);
  let progressAt = Date.now();
  let awaitingResumeProgress = workflowBaseline.length > 0;
  const completion = await poll(
    "confirmed proof and correction",
    async () => {
      requireLive();
      const records = await readJourneyWorkflowEntries({
        workflowJournalDirectory,
        category,
        headerHash,
      });
      const recovery = journeyIntentRecovery(records);
      const progress = recovery.progressCount;
      if (progress !== progressCount) {
        progressCount = progress;
        progressAt = Date.now();
        awaitingResumeProgress = false;
      }
      const normalAllowanceMs = awaitingResumeProgress
        ? 900_000
        : progressAllowanceMs;
      const pendingIntent = [...recovery.latest.values()].some(
        ({ outcome }) => outcome === "pending",
      );
      const allowanceMs =
        pendingIntent && reconciliationAllowanceMs !== undefined
          ? Math.max(normalAllowanceMs ?? 0, reconciliationAllowanceMs)
          : normalAllowanceMs;
      if (allowanceMs !== undefined && Date.now() - progressAt > allowanceMs) {
        throw new Error(
          `Workflow made no durable progress for ${allowanceMs.toString()} ms (${progressCount.toString()} progress records)`,
        );
      }
      for (const { sequence, event } of journeyWorkflowUpdates(
        records,
        workflowBaseline,
        reported,
        { now: Date.now(), allowanceMs: stallAllowanceMs },
      )) {
        if (event.kind === "submitted" || event.kind === "confirmed")
          console.info(`Live proof: ${event.kind} ${event.actionId}`);
        if (event.kind === "stalled" && event.reason !== reportedStall) {
          reportedStall = event.reason;
          console.info(`Live proof: stalled, retrying: ${event.reason}`);
        }
        reported = sequence;
      }
      const terminal = [...records]
        .reverse()
        .find(
          ({ event }) =>
            event.kind === "terminal_included" || event.kind === "completed",
        )?.event;
      if (
        terminal?.kind !== "terminal_included" &&
        terminal?.kind !== "completed"
      )
        return undefined;
      await verifyJourneyWorkflowTransactions(records, (txHash) =>
        native.transaction(txHash),
      );
      expect(terminal.terminal).toMatchObject({
        category,
        headerHash: headerHash,
        correction: { fraudulentHeaderAbsent: true },
        proofToken: { retainedAtFinalState: true },
      });
      // Completion is inclusion at the action depth. Release finality is the
      // separate anchor the finalized evidence stamp waits for.
      expect(
        terminal.terminal.observedAt.confirmationDepth,
      ).toBeGreaterThanOrEqual(actionDepth);
      await writeJourneyArtifact(
        join(directory, "completed-workflow.json"),
        records,
      );
      return terminal.terminal;
    },
    correctionTimeoutMs,
  );
  const { contracts } = deployment;
  const headerUnit = (hash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
    );
  await stage("independent corrected-state observations", async () => {
    const economics = deployment.manifest.economics;
    const readOutput = async (outRef: string | null) => {
      if (outRef === null)
        throw new Error("Expected an actual economic output reference");
      const [txHash, index] = outRef.split("#");
      const observed = await native.transaction(txHash!);
      const tx = CML.Transaction.from_cbor_hex(observed.cbor);
      expect(tx.is_valid()).toBe(true);
      const output = tx.body().outputs().get(Number(index));
      return { tx, output };
    };
    const bond = await readOutput(completion.economics.operatorBondInputOutRef);
    const reward = await readOutput(
      completion.economics.proverRewardOutputOutRef,
    );
    expect(bond.output.amount().coin()).toBe(
      BigInt(economics.requiredBondLovelace),
    );
    expect(reward.output.amount().coin()).toBe(
      BigInt(economics.fraudProverRewardLovelace),
    );
    expect(reward.tx.body().fee()).toBe(
      BigInt(economics.slashingPenaltyLovelace),
    );
    const proverAddress = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: accounts.publisher.seedPhrase,
    }).address;
    expect(reward.output.address().to_bech32()).toBe(proverAddress);
    const inputs = reward.tx.body().inputs();
    expect(
      Array.from({ length: inputs.len() }, (_, index) => {
        const value = inputs.get(index);
        return `${value.transaction_id().to_hex()}#${value.index()}`;
      }),
    ).toContain(completion.economics.operatorBondInputOutRef);
    expect(completion.economics).toMatchObject({
      operatorBondInputLovelace: economics.requiredBondLovelace.toString(),
      slashedLovelace: economics.slashingPenaltyLovelace.toString(),
      proverRewardLovelace: economics.fraudProverRewardLovelace.toString(),
    });
    await verifyJourneyComputationThreadAbsent({
      kupoUrl: context.kupoUrl,
      computationThreadPolicyId: contracts.computationThread.policyId,
      category,
      headerHash,
    });
    expect(
      await provider.getUtxosWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        headerUnit(headerHash),
      ),
    ).toHaveLength(0);
    // Correction is a historical chain fact. A resumed runner can already have
    // committed a healthy successor, which legitimately changes both the tail
    // link and scheduler. Authenticate the outputs at removal instead of
    // mistaking that later progress for an incomplete correction.
    const removed = await readOutput(
      completion.correction.removedStateQueueOutRef,
    );
    const removedOutputs = transactionOutputs(removed.tx);
    const removedOutput = removedOutputs.find(
      ({ outputIndex }) =>
        outputIndex ===
        Number(completion.correction.removedStateQueueOutRef.split("#")[1]),
    );
    expect(removedOutput?.address).toBe(
      contracts.stateQueue.spendingScriptAddress,
    );
    expect(removedOutput?.assets[headerUnit(headerHash)]).toBe(1n);
    const removal = CML.Transaction.from_cbor_hex(
      (await native.transaction(completion.correction.removalTxHash)).cbor,
    );
    expect(removal.is_valid()).toBe(true);
    const removalInputs = removal.body().inputs();
    expect(
      Array.from({ length: removalInputs.len() }, (_, index) => {
        const input = removalInputs.get(index);
        return `${input.transaction_id().to_hex()}#${input.index()}`;
      }),
    ).toContain(completion.correction.removedStateQueueOutRef);
    const proofReferences = removal.body().reference_inputs();
    expect(proofReferences).toBeDefined();
    expect(
      Array.from({ length: proofReferences!.len() }, (_, index) => {
        const input = proofReferences!.get(index);
        return `${input.transaction_id().to_hex()}#${input.index()}`;
      }),
    ).toContain(completion.proofToken.outRef);
    const outputsAfterRemoval = transactionOutputs(removal);
    expect(
      outputsAfterRemoval.every(
        ({ assets }) => assets[headerUnit(headerHash)] === undefined,
      ),
    ).toBe(true);
    await verifyJourneyCorrectedScheduler(removal, {
      removedOperator: operatorVkey,
      activeOperators: contracts.activeOperators,
      scheduler: contracts.scheduler,
      slotToUnixTime: (slot) => deployment.operatorLucid.slotToUnixTime(slot),
      resolveOutput: async (outRef) => {
        const [txHash, index] = outRef.split("#");
        const resolved = await readOutput(outRef);
        expect(CML.hash_transaction(resolved.tx.body()).to_hex()).toBe(txHash);
        return coreToUtxo(
          CML.TransactionUnspentOutput.new(
            CML.TransactionInput.new(
              CML.TransactionHash.from_hex(txHash!),
              BigInt(index!),
            ),
            resolved.output,
          ),
        );
      },
    });
    const predecessors = await provider.getUtxosWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      headerUnit(predecessorHeaderHash),
    );
    expect(predecessors).toHaveLength(1);
    await verifyJourneyCorrectedTail(removal, {
      address: contracts.stateQueue.spendingScriptAddress,
      unit: headerUnit(predecessorHeaderHash),
      headerHash: predecessorHeaderHash,
    });
    const expectedProofUnit = toUnit(
      contracts.fraudProof.policyId,
      SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + headerHash,
    );
    expect(completion.proofToken.unit).toBe(expectedProofUnit);
    const proofs = await provider.getUtxosWithUnit(
      contracts.fraudProof.spendingScriptAddress,
      expectedProofUnit,
    );
    expect(proofs).toHaveLength(1);
    expect(proofs[0]!.assets[expectedProofUnit]).toBe(1n);
    expect(Data.from(proofs[0]!.datum!, SDK.FraudProofTokenDatum)).toEqual({
      fraud_prover: paymentCredentialOf(proverAddress).hash,
    });
    expect(`${proofs[0]!.txHash}#${proofs[0]!.outputIndex}`).toBe(
      completion.proofToken.outRef,
    );
    // The authenticated removal above proves removal from the active list.
    // A later legitimate registration can recreate this operator's current node.
    const locks = await provider.getUtxosWithUnit(
      contracts.correctionLock.spendingScriptAddress,
      toUnit(contracts.hubOracle.policyId, SDK.CORRECTION_LOCK_ASSET_NAME),
    );
    expect(locks).toHaveLength(1);
    expect(Data.from(locks[0]!.datum!, SDK.CorrectionLockDatum)).toBe("Idle");
  });
  return completion;
};
