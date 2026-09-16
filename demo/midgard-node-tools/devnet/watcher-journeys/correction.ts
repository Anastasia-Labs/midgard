import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  resolveProverSigner,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS,
  WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS,
} from "midgard-watcher";
import { expect } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { loadJourneyContext } from "./live-context.js";
import type { startJourneyNativeRecorder } from "./native-recorder.js";

const transactionOutputs = (transaction: CML.Transaction) => {
  const txHash = CML.hash_transaction(transaction.body());
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(txHash, BigInt(index)),
        outputs.get(index),
      ),
    ),
  );
};

/** Read correction's authenticated tail at its immutable transaction output. */
export const verifyJourneyCorrectedTail = async (
  transaction: CML.Transaction,
  expected: { address: string; unit: string; headerHash: string },
) => {
  expect(transaction.is_valid()).toBe(true);
  const matches = transactionOutputs(transaction).filter(
    ({ address, assets }) =>
      address === expected.address && assets[expected.unit] === 1n,
  );
  expect(matches).toHaveLength(1);
  const node = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(matches[0]!),
  );
  expect(node.key).toEqual({ Key: { key: expected.headerHash } });
  expect(node.next).toBe("Empty");
  return matches[0]!;
};

/** Derive scheduler continuation from the removal's authenticated list inputs.
 * The root/tail topology determines whether removal appoints a surviving actor;
 * wall time and a later live scheduler state are not evidence for this result. */
export const verifyJourneyCorrectedScheduler = async (
  transaction: CML.Transaction,
  expected: {
    removedOperator: string;
    activeOperators: { spendingScriptAddress: string; policyId: string };
    scheduler: { spendingScriptAddress: string; policyId: string };
    slotToUnixTime(slot: number): number;
    resolveOutput(outRef: string): Promise<UTxO>;
  },
) => {
  expect(transaction.is_valid()).toBe(true);
  const resolve = async (inputs: CML.TransactionInputList | undefined) => {
    if (inputs === undefined) return [];
    return await Promise.all(
      Array.from({ length: inputs.len() }, async (_, index) => {
        const input = inputs.get(index);
        const outRef = `${input.transaction_id().to_hex()}#${input.index()}`;
        const output = await expected.resolveOutput(outRef);
        expect(`${output.txHash}#${output.outputIndex}`).toBe(outRef);
        return output;
      }),
    );
  };
  const inputs = await resolve(transaction.body().inputs());
  const references = await resolve(transaction.body().reference_inputs());
  const outputs = transactionOutputs(transaction);
  const activeUnit = (key: SDK.LinkedListNodeView["key"]) =>
    toUnit(
      expected.activeOperators.policyId,
      key === "Empty"
        ? SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME
        : SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + key.Key.key,
    );
  const activeNodes = async (values: readonly UTxO[]) =>
    await Promise.all(
      values
        .filter(
          (output) =>
            output.address === expected.activeOperators.spendingScriptAddress &&
            Object.keys(output.assets).some((unit) =>
              unit.startsWith(expected.activeOperators.policyId),
            ),
        )
        .map(async (output) => {
          const node = await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(output),
          );
          expect(output.assets[activeUnit(node.key)]).toBe(1n);
          return { output, node };
        }),
    );
  const activeInputs = await activeNodes(inputs);
  const removed = activeInputs.filter(
    ({ node }) =>
      node.key !== "Empty" && node.key.Key.key === expected.removedOperator,
  );
  expect(removed).toHaveLength(1);
  const removedNode = removed[0]!.node;
  const anchors = activeInputs.filter(
    ({ node }) =>
      node.next !== "Empty" && node.next.Key.key === expected.removedOperator,
  );
  expect(anchors).toHaveLength(1);
  const anchor = anchors[0]!;
  const continued = (await activeNodes(outputs)).filter(
    ({ node }) => activeUnit(node.key) === activeUnit(anchor.node.key),
  );
  expect(continued).toHaveLength(1);
  expect(continued[0]!.node).toEqual({
    ...anchor.node,
    next: removedNode.next,
  });
  expect(continued[0]!.output.assets).toEqual(anchor.output.assets);
  expect(
    outputs.every(
      (output) => output.assets[activeUnit(removedNode.key)] === undefined,
    ),
  ).toBe(true);

  const schedulerUnit = toUnit(
    expected.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const schedulers = (values: readonly UTxO[]) =>
    values.filter(
      (output) =>
        output.address === expected.scheduler.spendingScriptAddress &&
        output.assets[schedulerUnit] === 1n,
    );
  const prior = [...schedulers(inputs), ...schedulers(references)];
  expect(prior).toHaveLength(1);
  const priorDatum = Data.from(prior[0]!.datum!, SDK.SchedulerDatum);
  expect(priorDatum).not.toBe("NoActiveOperators");
  if (priorDatum === "NoActiveOperators")
    throw new Error("Removal requires an appointed scheduler");
  if (priorDatum.ActiveOperator.operator !== expected.removedOperator) {
    expect(schedulers(inputs)).toHaveLength(0);
    expect(schedulers(outputs)).toHaveLength(0);
    return prior[0]!;
  }
  expect(schedulers(inputs)).toHaveLength(1);
  const next = schedulers(outputs);
  expect(next).toHaveLength(1);
  expect(next[0]!.assets).toEqual(prior[0]!.assets);
  let nextOperator: string | null;
  if (anchor.node.key !== "Empty") nextOperator = anchor.node.key.Key.key;
  else if (removedNode.next === "Empty") nextOperator = null;
  else {
    const tails = (await activeNodes(references)).filter(
      ({ node }) => node.key !== "Empty" && node.next === "Empty",
    );
    expect(tails).toHaveLength(1);
    const key = tails[0]!.node.key;
    if (key === "Empty") throw new Error("Active tail has no operator key");
    nextOperator = key.Key.key;
  }
  const nextDatum = Data.from(next[0]!.datum!, SDK.SchedulerDatum);
  if (nextOperator === null) expect(nextDatum).toBe("NoActiveOperators");
  else {
    expect(nextOperator).not.toBe(expected.removedOperator);
    const upperSlot = transaction.body().ttl();
    expect(upperSlot).toBeDefined();
    if (upperSlot === undefined || upperSlot > BigInt(Number.MAX_SAFE_INTEGER))
      throw new Error("Removal has no exact ledger validity upper bound");
    expect(nextDatum).toEqual({
      ActiveOperator: {
        operator: nextOperator,
        start_time: BigInt(expected.slotToUnixTime(Number(upperSlot))) - 1n,
      },
    });
  }
  return next[0]!;
};

export const readJourneyWorkflowEntries = async ({
  workflowJournalDirectory,
  category,
  headerHash,
}: {
  workflowJournalDirectory: string;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
}): Promise<readonly FraudProofWorkflowJournalEntry[]> => {
  const workflowDirectory = join(
    workflowJournalDirectory,
    `fault-proofs/${category}`,
    headerHash,
  );
  const workflow = new DirectoryFraudProofWorkflowJournalStore(
    workflowDirectory,
  );

  if (!existsSync(workflowDirectory)) return [];
  const ids = (
    await readdir(workflowDirectory, { withFileTypes: true })
  ).filter(
    (entry) => entry.isDirectory() && /^[0-9a-f]{64}$/u.test(entry.name),
  );
  expect(ids.length).toBeLessThanOrEqual(1);
  return ids.length === 0 ? [] : await workflow.load(ids[0]!.name);
};

/**
 * How long a workflow may keep journaling `stalled` before the attempt fails.
 *
 * The orchestrator journals a stall as a diagnostic and retries every pass.
 * The watcher observes the state queue at its action depth while preflight
 * builds against the live chain, so a header the queue re-created in that
 * window (a DA attestation moves the header output) stalls preflight until
 * the re-creating block reaches that depth; the next pass then binds the live
 * output.
 * The watcher itself resumes such a preflight stall every
 * `WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS` for up to its retry budget, so the
 * journey allows the whole budget plus one delay before calling it a failure.
 */
export const JOURNEY_WORKFLOW_STALL_ALLOWANCE_MS =
  WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS +
  WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS;

const journeyIntentRecovery = (
  records: readonly FraudProofWorkflowJournalEntry[],
) => {
  const latest = new Map<
    string,
    { txHash: string; outcome: "pending" | "confirmed" | "not_found" }
  >();
  const abandoned = new Set<string>();
  let progressCount = 0;
  for (const { event } of records) {
    if (event.kind !== "reconciled") progressCount += 1;
    if (event.kind === "submission_intent") {
      latest.set(event.actionId, { txHash: event.txHash, outcome: "pending" });
    } else if (event.kind === "reconciled" || event.kind === "confirmed") {
      const intent = latest.get(event.actionId);
      if (intent === undefined || intent.txHash !== event.txHash) continue;
      // Reconciled inclusion precedes funding acknowledgment and the actual
      // confirmed event; keep its recovery allowance until that handoff ends.
      if (event.kind === "confirmed") intent.outcome = "confirmed";
      else if (event.outcome !== "confirmed") intent.outcome = event.outcome;
      if (event.kind === "reconciled" && event.outcome === "not_found") {
        const key = `${event.actionId}:${event.txHash}`;
        if (!abandoned.has(key)) {
          abandoned.add(key);
          progressCount += 1;
        }
      }
    }
  }
  return { latest, progressCount };
};

/** Pending observations do not advance the clock; exact abandonment does once. */
export const journeyWorkflowProgressCount = (
  records: readonly FraudProofWorkflowJournalEntry[],
) => journeyIntentRecovery(records).progressCount;

/**
 * A retained failure is history. A new stall fails the current attempt unless
 * the workflow is still retrying inside the allowance or already moved past
 * it; without an allowance every new stall fails immediately.
 */
export const journeyWorkflowUpdates = (
  records: readonly FraudProofWorkflowJournalEntry[],
  baseline: readonly FraudProofWorkflowJournalEntry[],
  reported = baseline.length - 1,
  stall?: { readonly now: number; readonly allowanceMs: number },
) => {
  expect(records.slice(0, baseline.length)).toEqual(baseline);
  const updates = records.filter(({ sequence }) => sequence > reported);
  const failure = updates.find(({ event }) => event.kind === "stalled")?.event;
  if (failure?.kind !== "stalled") return updates;
  let trailingStart = records.length;
  while (
    trailingStart > 0 &&
    records[trailingStart - 1]!.event.kind === "stalled"
  )
    trailingStart -= 1;
  const trailing = records.slice(trailingStart);
  if (stall === undefined)
    throw new Error(`Workflow stalled: ${failure.reason}`);
  if (trailing.length === 0) return updates;
  const since = Date.parse(trailing[0]!.recordedAt);
  const latest = trailing[trailing.length - 1]!.event;
  if (
    !Number.isFinite(since) ||
    stall.now - since > stall.allowanceMs ||
    latest.kind !== "stalled"
  )
    throw new Error(
      `Workflow stalled: ${latest.kind === "stalled" ? latest.reason : failure.reason}`,
    );
  return updates;
};

/**
 * Compare the exact transactions the workflow confirmed against the last
 * intent journaled per action, excluding exact intents durably abandoned even
 * when a replacement binds a different action identity.
 */
export const journeyLatestIntentsByAction = (
  records: readonly FraudProofWorkflowJournalEntry[],
) => {
  return [...journeyIntentRecovery(records).latest.values()]
    .filter(({ outcome }) => outcome !== "not_found")
    .map(({ txHash }) => txHash);
};

/** Recovering an included attempt need not have emitted a confirmed event. */
export const verifyJourneyWorkflowTransactions = async (
  records: readonly FraudProofWorkflowJournalEntry[],
  authenticate: (txHash: string) => Promise<unknown>,
) => {
  const intents = journeyLatestIntentsByAction(records);
  const submitted = new Set(
    records.flatMap(({ event }) =>
      event.kind === "submission_intent" ? [event.txHash] : [],
    ),
  );
  for (const { event } of records)
    if (event.kind === "confirmed")
      expect(submitted.has(event.txHash)).toBe(true);
  for (const txHash of intents) await authenticate(txHash);
};

/** A correction retires its own computation thread while other proofs may continue. */
export const verifyJourneyComputationThreadAbsent = async ({
  kupoUrl,
  computationThreadPolicyId,
  category,
  headerHash,
}: {
  kupoUrl: string;
  computationThreadPolicyId: string;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
}) => {
  const assetName =
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + headerHash;
  const threadResponse = await fetch(
    `${kupoUrl}/matches/${computationThreadPolicyId}.${assetName}?unspent`,
  );
  expect(threadResponse.ok).toBe(true);
  expect(await threadResponse.json()).toEqual([]);
};

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

/** Final evidence comes only from the production verifier's completed event. */
export const journeyAnchoredEvidence = (
  records: readonly FraudProofWorkflowJournalEntry[],
): FraudProofWorkflowTerminal | undefined => {
  const completed = records.filter(({ event }) => event.kind === "completed");
  expect(completed.length).toBeLessThanOrEqual(1);
  const event = completed[0]?.event;
  return event?.kind === "completed" ? event.terminal : undefined;
};

export interface JourneyPendingEvidenceStamp {
  readonly category: SDK.FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly deploymentFingerprint: string;
  readonly completedAtConfirmationDepth: number;
  readonly terminalObservedAt: string;
  readonly successorTxHash: string;
  readonly releaseFinalityPolicyDigest: string;
  readonly finalityDepth: number;
}

/**
 * Sweep durable pending stamps while the shared watcher reconciles terminals.
 * Earlier families need no live harness process of their own. A restart reads
 * these same requests; already stamped families and older run evidence are left
 * intact. The last selected family polls this sweep until nothing remains.
 */
export const finalizePendingJourneyEvidence = async ({
  journeysDirectory,
  workflowJournalDirectory,
  deploymentFingerprint,
  releaseFinalityPolicyDigest,
  finalityDepth,
  nativeEvidencePath,
  authenticate,
}: {
  journeysDirectory: string;
  workflowJournalDirectory: string;
  deploymentFingerprint: string;
  releaseFinalityPolicyDigest: string;
  finalityDepth: number;
  nativeEvidencePath: string;
  authenticate(
    request: JourneyPendingEvidenceStamp,
    terminal: FraudProofWorkflowTerminal,
  ): Promise<boolean>;
}): Promise<number> => {
  let pending = 0;
  for (const directoryEntry of await readdir(journeysDirectory, {
    withFileTypes: true,
  })) {
    if (!directoryEntry.isDirectory()) continue;
    const directory = join(journeysDirectory, directoryEntry.name);
    const requestPath = join(directory, "pending-evidence-stamp.json");
    if (
      !existsSync(requestPath) ||
      existsSync(join(directory, "finalized-evidence-stamp.json"))
    )
      continue;
    const request =
      await readJourneyArtifact<JourneyPendingEvidenceStamp>(requestPath);
    expect(request.deploymentFingerprint).toBe(deploymentFingerprint);
    expect(request.releaseFinalityPolicyDigest).toBe(
      releaseFinalityPolicyDigest,
    );
    expect(request.finalityDepth).toBe(finalityDepth);
    const records = await readJourneyWorkflowEntries({
      workflowJournalDirectory,
      category: request.category,
      headerHash: request.headerHash,
    });
    const terminal = journeyAnchoredEvidence(records);
    if (terminal === undefined) {
      pending += 1;
      continue;
    }
    expect(terminal.category).toBe(request.category);
    expect(terminal.headerHash).toBe(request.headerHash);
    expect(terminal.observedAt.confirmationDepth).toBeGreaterThanOrEqual(
      finalityDepth,
    );
    if (!(await authenticate(request, terminal))) {
      pending += 1;
      continue;
    }
    await writeJourneyArtifact(
      join(directory, "finalized-workflow.json"),
      records,
    );
    // The terminal may have been rebuilt or included at a new point after a
    // rollback. The completed event independently reauthenticates its effects.
    await writeJourneyArtifact(
      join(directory, "finalized-evidence-stamp.json"),
      {
        ...request,
        anchoredAt: new Date().toISOString(),
        nativeEvidencePath,
        terminal,
      },
    );
  }
  return pending;
};
