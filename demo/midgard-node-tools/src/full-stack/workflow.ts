import { randomUUID } from "node:crypto";
import { join } from "node:path";

import {
  createDurableJson,
  parseJournal,
  readJsonIfPresent,
  type StackJournal,
  type StepRecord,
  writeDurableJson,
} from "./journal.js";

export type Recovery =
  | { status: "complete"; data: unknown }
  | { status: "retry" | "pending" };
export type StackStep = {
  id: string;
  /** Reads authoritative state. A retry must be safe for the same durable intent. */
  reconcile: (record: StepRecord | undefined) => Promise<Recovery>;
  execute: (record: StepRecord | undefined) => Promise<unknown>;
};
export type WorkflowContext = {
  directory: string;
  intentDigest: string;
  /** Runs once this controller's intent owns the journal, before any step. */
  onJournalAccepted?: () => Promise<void>;
  onProgress?: (id: string, status: string) => void;
};

/** Creating the journal is exclusive, so two controllers never share one run directory. */
async function openJournal(
  path: string,
  intentDigest: string,
): Promise<StackJournal> {
  const saved = await readJsonIfPresent(path);
  if (saved !== undefined) return parseJournal(saved, intentDigest);
  const journal: StackJournal = {
    schemaVersion: "midgard-full-stack-v1",
    runId: randomUUID(),
    intentDigest,
    steps: {},
  };
  try {
    await createDurableJson(path, journal);
    return journal;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
    return parseJournal(await readJsonIfPresent(path), intentDigest);
  }
}

export async function runStackWorkflow(
  context: WorkflowContext,
  steps: readonly StackStep[],
) {
  if (new Set(steps.map((step) => step.id)).size !== steps.length)
    throw new Error("Duplicate workflow step");
  const path = join(context.directory, "stack-journal.json");
  const journal = await openJournal(path, context.intentDigest);
  await context.onJournalAccepted?.();
  for (const step of steps) {
    const prior = journal.steps[step.id];
    const recovery = await step.reconcile(prior);
    if (recovery.status === "pending") {
      throw new Error(
        `${step.id}: previous submission remains ambiguous; no transaction was repeated`,
      );
    }
    if (recovery.status === "complete") {
      journal.steps[step.id] = {
        status: "complete",
        attempts: prior?.attempts ?? 1,
        data: recovery.data ?? null,
      };
      await writeDurableJson(path, journal);
      context.onProgress?.(step.id, "confirmed");
      continue;
    }
    journal.steps[step.id] = {
      status: "running",
      attempts: (prior?.attempts ?? 0) + 1,
      // Never carry earlier data: non-null data on a running record means this attempt's execute returned.
      data: null,
    };
    await writeDurableJson(path, journal);
    context.onProgress?.(step.id, "running");
    const data = await step.execute(prior);
    // Persist submitted evidence before checking confirmation. Resume verifies it.
    const submitted: StepRecord = {
      ...journal.steps[step.id]!,
      data: data ?? null,
    };
    journal.steps[step.id] = submitted;
    await writeDurableJson(path, journal);
    const confirmed = await step.reconcile(submitted);
    if (confirmed.status !== "complete")
      throw new Error(`${step.id}: completion has not been confirmed`);
    journal.steps[step.id] = {
      ...submitted,
      status: "complete",
      data: confirmed.data ?? null,
    };
    await writeDurableJson(path, journal);
    context.onProgress?.(step.id, "confirmed");
  }
  return journal;
}
