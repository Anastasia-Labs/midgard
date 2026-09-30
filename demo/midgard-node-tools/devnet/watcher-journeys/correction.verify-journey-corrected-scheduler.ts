import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToUtxo,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  WATCHER_PREFLIGHT_STALL_RETRY_BUDGET_MS,
  WATCHER_PREFLIGHT_STALL_RETRY_DELAY_MS,
} from "midgard-watcher";
import { expect } from "vitest";

export const transactionOutputs = (transaction: CML.Transaction) => {
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

export const journeyIntentRecovery = (
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
