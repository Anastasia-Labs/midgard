import { WORKFLOW_CONTINUATION_QUIET_BOUND_MS } from "@al-ft/midgard-fault-proofs/workflow/pending-continuation";
import { isSafe } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import {
  CML,
  coreToUtxo,
  Data,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type {
  JourneyChainCorrection,
  JourneyCorrectionChain,
} from "./correction.chain-correction.js";
import { verifyJourneyComputationThreadAbsent } from "./correction.verify-journey-computation-thread-absent.js";
import {
  transactionOutputs,
  verifyJourneyCorrectedScheduler,
  verifyJourneyCorrectedTail,
} from "./correction.verify-journey-corrected-scheduler.js";
import {
  journeyStartDeadlinePassed,
  journeyTargetHolds,
  type JourneyWatcherStartStatus,
} from "./correction.watcher-start-deadline.js";
import type { loadJourneyContext } from "./live-context.js";
import { onQueueOrMerged } from "./state-queue-root-anchor.js";

type JourneyContext = Awaited<ReturnType<typeof loadJourneyContext>>;

/**
 * What the correction checks read of a deployment: a devnet journey's own
 * context, or an attached stack's.
 */
type JourneyCorrectionContext = Readonly<{
  deployment: Pick<
    JourneyContext["deployment"],
    "manifest" | "contracts" | "operatorLucid"
  >;
  provider: Pick<JourneyContext["provider"], "getUtxosWithUnit">;
  kupoUrl: string;
}>;

/** Where the journey reads the exact bytes of a confirmed transaction. */
export type JourneyTransactions = {
  transaction(txHash: string): Promise<{ readonly cbor: string }>;
};

/** One output of a valid confirmed transaction, by `txHash#index`. */
const readJourneyOutput = async (
  transactions: JourneyTransactions,
  outRef: string,
) => {
  const [txHash, index] = outRef.split("#");
  const observed = await transactions.transaction(txHash!);
  const tx = CML.Transaction.from_cbor_hex(observed.cbor);
  expect(tx.is_valid()).toBe(true);
  const output = tx.body().outputs().get(Number(index));
  return { tx, output };
};

/**
 * The conviction's exact economics: slashing consumed the operator's whole
 * bond, burned the penalty as the fee, and paid the reward to `proverAddress`.
 */
export const verifyJourneyProverReward = async ({
  economics,
  completion,
  proverAddress,
  transactions,
}: {
  /** The deployment manifest's conviction economics. */
  economics: Readonly<
    Record<
      | "requiredBondLovelace"
      | "fraudProverRewardLovelace"
      | "slashingPenaltyLovelace",
      number | bigint
    >
  >;
  completion: Pick<JourneyChainCorrection, "economics">;
  proverAddress: string;
  transactions: JourneyTransactions;
}) => {
  const bond = await readJourneyOutput(
    transactions,
    completion.economics.operatorBondInputOutRef,
  );
  const reward = await readJourneyOutput(
    transactions,
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
  expect(reward.output.address().to_bech32()).toBe(proverAddress);
  const inputs = reward.tx.body().inputs();
  expect(
    Array.from({ length: inputs.len() }, (_, index) => {
      const value = inputs.get(index);
      return `${value.transaction_id().to_hex()}#${value.index()}`;
    }),
  ).toContain(completion.economics.operatorBondInputOutRef);
};

/**
 * Shared acceptance assertions for every installed automatic workflow. The
 * correction is read from the chain: the watcher keeps no record of what it
 * sent. Progress is the transactions of
 * ours that land; the watcher's `/v1/status` holds say when it stopped.
 */
export const verifyJourneyCorrection = async ({
  context,
  transactions,
  chain,
  watcherStatus,
  category,
  headerHash,
  predecessorHeaderHash,
  actionDepth,
  operatorVkey,
  proverAddress,
  requireLive,
  poll,
  stage,
}: {
  context: JourneyCorrectionContext;
  /** Where the confirmed bytes of the workflow's transactions are read. */
  transactions: JourneyTransactions;
  /** The family's landed transactions and correction, read from chain. */
  chain: JourneyCorrectionChain;
  /**
   * The watcher's `/v1/status`, read on each poll. Before anything of ours
   * lands, the correction fails at once when the watcher can no longer start
   * this proof. At any time it fails at once on a hold by which the watcher
   * gave up on it, and on a pausing hold that outlasts the watcher's quiet
   * bound with nothing of ours landing.
   */
  watcherStatus?: () => Promise<JourneyWatcherStartStatus>;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
  predecessorHeaderHash: string;
  /** Depth the watcher acts at; the removal must be this deep. */
  actionDepth: number;
  operatorVkey: string;
  /**
   * The watcher's prover wallet, which the reward must pay: on a devnet the
   * prover the journey launched, on a stack the watcher's configured prover,
   * whose address the journey knows and whose key it never holds.
   */
  proverAddress: string;
  requireLive(): void;
  poll<T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs?: number,
  ): Promise<T>;
  stage<T>(name: string, action: () => Promise<T>): Promise<T>;
}): Promise<
  Readonly<{
    completion: JourneyChainCorrection;
    /** Whether the chain already held the correction at the first read. */
    alreadyCorrected: boolean;
  }>
> => {
  const { deployment, provider } = context;
  const target = { category, headerHash };
  const reported = new Set<string>();
  let polls = 0;
  let paused: Readonly<{ reasons: string; since: number }> | undefined;
  const corrected = await poll(
    "confirmed proof and correction",
    async () => {
      requireLive();
      polls += 1;
      const landed = await chain.landed();
      let landedNew = false;
      for (const txHash of landed)
        if (!reported.has(txHash)) {
          reported.add(txHash);
          landedNew = true;
          console.info(`Live proof: landed ${txHash}`);
        }
      if (watcherStatus !== undefined) {
        const status = await watcherStatus();
        if (landed.size === 0) {
          const passed = journeyStartDeadlinePassed(status, target);
          if (passed !== undefined) throw new Error(passed);
        }
        const holds = journeyTargetHolds(status, target);
        if (holds.fatal.length > 0)
          throw new Error(
            `The watcher gave up on ${category}/${headerHash}: ${holds.fatal.join("; ")}`,
          );
        const reasons = holds.pausing.join(",");
        // A landing of ours, or another set of holds, restarts the pause's
        // clock at this read; no hold clears it.
        if (reasons === "") paused = undefined;
        else if (landedNew || paused?.reasons !== reasons)
          paused = { reasons, since: Date.now() };
        else if (
          Date.now() - paused.since >
          WORKFLOW_CONTINUATION_QUIET_BOUND_MS
        )
          throw new Error(
            `The watcher held ${category}/${headerHash} under ${reasons} for over ${WORKFLOW_CONTINUATION_QUIET_BOUND_MS.toString()} ms with nothing of ours landing`,
          );
      }
      const completion = await chain.correction();
      if (completion === undefined) return undefined;
      // Completion is inclusion at the action depth. Release finality is the
      // separate anchor the finalized evidence stamp waits for.
      if (
        !isSafe(completion.observedAt.confirmationDepth, {
          confirmationDepth: actionDepth,
        })
      )
        return undefined;
      return { completion, alreadyCorrected: polls === 1 };
    },
    // Only the watcher's own verdicts end a correction; the family's own
    // time limit does not run while it waits (`journeyFamilyDeadline`).
    Number.POSITIVE_INFINITY,
  );
  const { completion } = corrected;
  const { contracts } = deployment;
  const headerUnit = (hash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash,
    );
  await stage("independent corrected-state observations", async () => {
    const readOutput = (outRef: string) =>
      readJourneyOutput(transactions, outRef);
    await verifyJourneyProverReward({
      economics: deployment.manifest.economics,
      completion,
      proverAddress,
      transactions,
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
      (await transactions.transaction(completion.correction.removalTxHash))
        .cbor,
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
    // The predecessor is live, or our node merged it after it matured.
    expect(
      await onQueueOrMerged({
        provider,
        kupoUrl: context.kupoUrl,
        stateQueue: contracts.stateQueue,
        headerHash: predecessorHeaderHash,
        readConfirmedTransaction: (txHash) => transactions.transaction(txHash),
      }),
    ).toBe(true);
    await verifyJourneyCorrectedTail(removal, {
      address: contracts.stateQueue.spendingScriptAddress,
      policyId: contracts.stateQueue.policyId,
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
    expect(
      Data.from(proofs[0]!.datum!, SDKFraudProof.FraudProofTokenDatum),
    ).toEqual({
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
  return corrected;
};
