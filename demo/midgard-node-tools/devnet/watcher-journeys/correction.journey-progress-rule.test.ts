import { createHash } from "node:crypto";

import {
  type FraudProofWorkflowJournalEntry,
  WORKFLOW_CONTINUATION_QUIET_BOUND_MS,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import {
  entry,
  pollCorrectionProgress,
} from "./correction.journey-progress-clock.test-support.js";
import { journeyTimingForCategory } from "./journey-timing.js";

/**
 * The watcher's own continuation rule, at Preprod's cadence and at the fast
 * devnet's: a workflow progresses while its journal reaches new milestones
 * (`journaledProgress`), however many transactions that takes, and fails
 * once it reaches none for the watcher's quiet bound. Every journal is
 * spaced by the family's own transaction allowance at that cadence, and the
 * gate is the one the runner builds.
 */
describe.each([0.05, 0.25])(
  "the watcher's progress rule at active slot coefficient %s",
  (activeSlotsCoeff) => {
    const timing = journeyTimingForCategory("executionNativeScriptInvalid", {
      slotLengthSeconds: 1,
      activeSlotsCoeff,
      confirmationDepth: 30,
      actionDepth: 1,
    });
    const stepMs = timing.transactionAllowanceMs;
    const quietBoundMs = WORKFLOW_CONTINUATION_QUIET_BOUND_MS;
    const gate = {
      reconciliationAllowanceMs: timing.allowances.finalizedEvidenceStampMs,
      resumeAllowanceMs: timing.allowances.automaticDecisionMs,
    };
    const startAt = 1_800_000_000_000;
    /** A journal under construction, each record stamped when it lands. */
    const journal = () => {
      const records: FraudProofWorkflowJournalEntry[] = [];
      const add = (
        at: number,
        event: FraudProofWorkflowJournalEntry["event"],
      ) =>
        records.push(entry(records.length, event, new Date(at).toISOString()));
      return {
        add,
        /** One action's intent, submission and confirmation. */
        step: (at: number, actionId: string) => {
          const txHash = createHash("sha256").update(actionId).digest("hex");
          add(at, {
            kind: "submission_intent",
            actionId,
            actionInput: {},
            attempt: 1,
            txHash,
          });
          add(at, { kind: "submitted", actionId, attempt: 1, txHash });
          add(at, { kind: "confirmed", actionId, txHash });
        },
        snapshot: () => [...records],
      };
    };

    it("accepts a family that confirms more steps than the old 32-transaction bound", async () => {
      const steps = journal();
      steps.add(startAt, { kind: "started" });
      const observations = Array.from({ length: 40 }, (_, index) => {
        const at = startAt + (index + 1) * stepMs;
        steps.step(at, `step-${index.toString()}`);
        return { at, records: steps.snapshot() };
      });
      await expect(
        pollCorrectionProgress({ retained: [], observations, startAt, gate }),
      ).rejects.toThrow("at observation 40 of 40: observations ended");
    });

    it.each([
      ["", 4],
      // The orchestrator journals a diagnostic between the action's entries
      // each time; the watcher writes it again because it is no longer the
      // journal's last entry, and only the first one is news.
      ["reconciliation failed for step-1: provider unavailable", 5],
    ])(
      "fails a loop that resubmits and abandons one action, naming it, just past the quiet bound (diagnostic %j)",
      async (diagnostic, milestones) => {
        const loop = journal();
        loop.add(startAt, { kind: "started" });
        loop.step(startAt + stepMs, "step-0");
        const actionId = "step-1";
        const firstAttemptAt = startAt + 2 * stepMs;
        const observations: {
          at: number;
          records: FraudProofWorkflowJournalEntry[];
        }[] = [];
        for (
          let attempt = 1;
          (attempt - 1) * stepMs < quietBoundMs;
          attempt += 1
        ) {
          const at = firstAttemptAt + (attempt - 1) * stepMs;
          const txHash = createHash("sha256")
            .update(`${actionId}:${attempt.toString()}`)
            .digest("hex");
          loop.add(at, {
            kind: "submission_intent",
            actionId,
            actionInput: {},
            attempt,
            txHash,
          });
          loop.add(at, { kind: "submitted", actionId, attempt, txHash });
          if (diagnostic !== "")
            loop.add(at, { kind: "stalled", reason: diagnostic });
          loop.add(at, {
            kind: "reconciled",
            actionId,
            outcome: "not_found",
            txHash,
          });
          loop.add(at, {
            kind: "signed_attempt_retired",
            txHash,
            retirement: {} as never,
          });
          observations.push({ at, records: loop.snapshot() });
        }
        observations.push(
          { at: firstAttemptAt + quietBoundMs, records: loop.snapshot() },
          { at: firstAttemptAt + quietBoundMs + 1, records: loop.snapshot() },
        );
        const count = observations.length.toString();
        await expect(
          pollCorrectionProgress({ retained: [], observations, startAt, gate }),
        ).rejects.toThrow(
          `at observation ${count} of ${count}: Workflow made no journaled progress for ${quietBoundMs.toString()} ms (${milestones.toString()} milestones, last action ${actionId})`,
        );
      },
    );

    // A preflight stall journals its reason once: the watcher retries it every
    // minute without writing the same reason again while it is still the
    // journal's last entry (`journalRepeatsStall`).
    const preflightStall = (resumeAfterMs: number) => {
      const stalls = journal();
      stalls.add(startAt, { kind: "started" });
      stalls.step(startAt + stepMs, "step-0");
      const observations: {
        at: number;
        records: FraudProofWorkflowJournalEntry[];
      }[] = [{ at: startAt + stepMs, records: stalls.snapshot() }];
      const stallAt = startAt + stepMs + 10 * 60_000;
      stalls.add(stallAt, {
        kind: "stalled",
        reason: "preflight: header moved",
      });
      for (let at = stallAt; at - stallAt < resumeAfterMs; at += 60_000)
        observations.push({ at, records: stalls.snapshot() });
      return { stalls, stallAt, observations };
    };

    it("counts a preflight stall as progress, so a workflow that resumes within the quiet bound of it passes", async () => {
      const { stalls, stallAt, observations } = preflightStall(
        quietBoundMs - 60_000,
      );
      const resumedAt = stallAt + quietBoundMs - 60_000;
      // The silence between the two confirmations is longer than the quiet
      // bound; the stall between them is what keeps the workflow going.
      expect(resumedAt - (startAt + stepMs)).toBeGreaterThan(quietBoundMs);
      stalls.step(resumedAt, "step-1");
      observations.push({ at: resumedAt, records: stalls.snapshot() });
      const count = observations.length.toString();
      await expect(
        pollCorrectionProgress({ retained: [], observations, startAt, gate }),
      ).rejects.toThrow(
        `at observation ${count} of ${count}: observations ended`,
      );
    });

    it("fails a preflight stall that never resumes once the quiet bound passes", async () => {
      const { stallAt, observations } = preflightStall(quietBoundMs + 120_000);
      const failing = (
        observations.findIndex(({ at }) => at - stallAt > quietBoundMs) + 1
      ).toString();
      await expect(
        pollCorrectionProgress({ retained: [], observations, startAt, gate }),
      ).rejects.toThrow(
        `at observation ${failing} of ${observations.length.toString()}: Workflow made no journaled progress for ${quietBoundMs.toString()} ms (4 milestones, last action step-0)`,
      );
    });

    it("fails a workflow whose journal stays silent past the quiet bound", async () => {
      const silent = journal();
      silent.add(startAt, { kind: "started" });
      silent.step(startAt + stepMs, "step-0");
      const confirmedAt = startAt + stepMs;
      await expect(
        pollCorrectionProgress({
          retained: [],
          observations: [
            { at: confirmedAt, records: silent.snapshot() },
            { at: confirmedAt + quietBoundMs, records: silent.snapshot() },
            { at: confirmedAt + quietBoundMs + 1, records: silent.snapshot() },
          ],
          startAt,
          gate,
        }),
      ).rejects.toThrow(
        `at observation 3 of 3: Workflow made no journaled progress for ${quietBoundMs.toString()} ms (3 milestones, last action step-0)`,
      );
    });
  },
);
