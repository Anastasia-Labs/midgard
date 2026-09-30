import { randomUUID } from "node:crypto";
import { join } from "node:path";

import {
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
  onProgress?: (id: string, status: string) => void;
};

export async function runStackWorkflow(
  context: WorkflowContext,
  steps: readonly StackStep[],
) {
  const path = join(context.directory, "stack-journal.json");
  const saved = await readJsonIfPresent(path);
  const journal: StackJournal =
    saved === undefined
      ? {
          schemaVersion: "midgard-full-stack-v1",
          runId: randomUUID(),
          intentDigest: context.intentDigest,
          steps: {},
        }
      : parseJournal(saved, context.intentDigest);
  if (new Set(steps.map((step) => step.id)).size !== steps.length)
    throw new Error("Duplicate workflow step");
  await writeDurableJson(path, journal);
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
      data: prior?.data ?? null,
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
