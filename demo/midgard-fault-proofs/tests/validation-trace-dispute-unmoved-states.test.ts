/** A validation-trace case that waits without a move of ours hands its turn
 * back under a named state, and the operator is alarmed whenever that wait
 * can run down a deadline of ours: a thread of ours whose trace we do not
 * hold cannot move while our clock or the case's expiry runs on it, and a
 * thread the case counts but discovery did not find may be on our turn. A
 * case with no open thread and no valid claim runs no clock of ours, so its
 * wait is named but no alarm. */
import { describe, expect, it } from "vitest";

import {
  decideValidationTraceThreads,
  VALIDATION_TRACE_CASE_CONTESTED,
  VALIDATION_TRACE_MATERIAL_HOLD,
  VALIDATION_TRACE_THREAD_COUNT_HOLD,
  type ValidationTraceResolvingThread,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";

const NOW = 1_000_000;

/** A thread of ours in its one-step resolution, past the boundary. */
const resolving: ValidationTraceResolvingThread = {
  utxo: {},
  outRef: `${"6".repeat(64)}#0`,
  unit: "6".repeat(60),
  defenderVkh: "d".repeat(56),
  holder: "core",
} as never;

const runState = (
  resolvingThreads: readonly ValidationTraceResolvingThread[],
  caseDatum: object,
) =>
  validationTraceRunState(
    decideValidationTraceThreads({
      threads: [],
      resolving: resolvingThreads,
      caseUnit: "c".repeat(64),
      caseOutRef: `${"c".repeat(64)}#0`,
      caseDatum: {
        open_threads: BigInt(resolvingThreads.length),
        defender_won: false,
        neither_claim_valid: false,
        started_at: BigInt(NOW),
        ...caseDatum,
      } as never,
      now: NOW,
      material: undefined,
    }) as FraudProofWorkflowRunResult,
  );

describe("validation-trace dispute: a case waiting without a move of ours", () => {
  it.each([
    {
      name: VALIDATION_TRACE_MATERIAL_HOLD,
      alarm: true,
      resolvingThreads: [resolving],
      caseDatum: {},
    },
    {
      name: VALIDATION_TRACE_THREAD_COUNT_HOLD,
      alarm: true,
      resolvingThreads: [],
      caseDatum: { open_threads: 1n },
    },
    {
      name: VALIDATION_TRACE_CASE_CONTESTED,
      alarm: false,
      resolvingThreads: [],
      caseDatum: { neither_claim_valid: true },
    },
  ])(
    "hands its turn back under $name (alarm: $alarm)",
    ({ name, alarm, resolvingThreads, caseDatum }) => {
      expect(runState(resolvingThreads, caseDatum)).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: { name, alarm },
      });
    },
  );
});
