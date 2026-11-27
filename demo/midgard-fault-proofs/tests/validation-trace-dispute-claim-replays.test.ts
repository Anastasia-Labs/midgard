/** How often a validation-trace claim replays its disputed transaction, over
 * a trace that runs three scripts. Opening the claim replays it once and
 * keeps its recorded script selections and its script evaluations: reading
 * the selections replays nothing more, and each batch of unread steps
 * replays once more on the kept evaluations, never running the evaluator
 * again. The evaluator is counted at the pinned UPLC evaluator, and a trace
 * replay at the validation context it derives once. */
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  buildValidationTraceClaimMaterial,
  type ValidationTraceClaimMaterial,
} from "../src/validation-trace-dispute/claim-material.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./support/validation-trace-claim-material.js";

const runs = vi.hoisted(() => ({ replays: 0, evaluations: 0 }));

vi.mock("@lucid-evolution/uplc", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@lucid-evolution/uplc")>();
  return {
    ...actual,
    eval_program_raw: (
      ...args: Parameters<typeof actual.eval_program_raw>
    ): ReturnType<typeof actual.eval_program_raw> => {
      runs.evaluations += 1;
      return actual.eval_program_raw(...args);
    },
  };
});

vi.mock("@al-ft/midgard-core", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-core")>();
  return {
    ...actual,
    deriveMidgardValidationContextCborV1: (
      ...args: Parameters<typeof actual.deriveMidgardValidationContextCborV1>
    ): ReturnType<typeof actual.deriveMidgardValidationContextCborV1> => {
      runs.replays += 1;
      return actual.deriveMidgardValidationContextCborV1(...args);
    },
  };
});

describe("validation-trace claim material: replays per claim", () => {
  let material: ValidationTraceClaimMaterial;
  let opened: typeof runs;

  beforeAll(async () => {
    const fixture = await plutusExecutionBlockFixture("failing", {
      passingSpends: 2,
    });
    const { event, ledger } = await eventAndLedger(fixture);
    runs.replays = 0;
    runs.evaluations = 0;
    material = await buildValidationTraceClaimMaterial({
      evidence: fixture.evidence,
      event,
      ledger,
      l1SlotConfig,
    });
    opened = { ...runs };
  }, 600_000);

  it("opens with one replay that runs each of the three scripts once", () => {
    expect(opened).toStrictEqual({ replays: 1, evaluations: 3 });
  });

  it("reads the selections from the opening replay and each batch of unread steps from one replay on its evaluations", async () => {
    const last = Number(material.stepCount) - 1;
    const after: (typeof runs)[] = [];
    const record = () => after.push({ ...runs });
    expect(material.selections).toHaveLength(3);
    record();
    await material.steps(0);
    record();
    // Step 0 is read; steps 1 and `last` are read by one more replay.
    await material.stepsAt([0, 1, last]);
    record();
    await material.steps(last);
    record();
    expect(after).toStrictEqual([
      { replays: 1, evaluations: 3 },
      { replays: 2, evaluations: 3 },
      { replays: 3, evaluations: 3 },
      { replays: 3, evaluations: 3 },
    ]);
  }, 600_000);
});
