/** The claim material's reads over a trace that runs three scripts (two
 * passing spends before the failing one): its recorded script selections
 * are the proof build's, in trace order, and one batched read of several
 * steps yields exactly the windows each step's own read does. */
import {
  buildDeterministicValidationMachineTrace,
  type DeterministicValidationMachineTrace,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { recordedSelectionOf } from "../src/validation-trace-dispute/selected-scripts.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import {
  claimMaterialOf,
  rejectedReplayInputOf,
} from "./support/validation-trace-claim-material.js";

describe("validation-trace claim material: reads over a three-script trace", () => {
  let fixture: Awaited<ReturnType<typeof plutusExecutionBlockFixture>>;
  let whole: DeterministicValidationMachineTrace;

  beforeAll(async () => {
    fixture = await plutusExecutionBlockFixture("failing", {
      passingSpends: 2,
    });
    whole = await Effect.runPromise(
      buildDeterministicValidationMachineTrace(
        await rejectedReplayInputOf(fixture),
      ),
    );
  }, 600_000);

  it("reads every recorded selection in the proof build's trace order", async () => {
    const selections = whole.witnesses.flatMap((witness) => {
      const selection = recordedSelectionOf(witness);
      return selection === undefined ? [] : [selection];
    });
    expect(selections).toHaveLength(3);
    const material = await claimMaterialOf(fixture);
    expect(await material.selections()).toStrictEqual(selections);
  }, 600_000);

  it("batches several steps' reads into exactly the windows each step's own read yields", async () => {
    const stepCount = whole.states.length - 1;
    const firstCek = whole.witnesses.findIndex(
      (witness) => witness.auxiliary?.kind === "cekCoreStep",
    );
    expect(firstCek).toBeGreaterThan(0);
    // Adjacent, repeated and last steps share or end at a window's edge.
    const indices = [
      stepCount - 1,
      firstCek,
      firstCek + 1,
      0,
      Math.floor(stepCount / 2),
      firstCek,
    ];
    const batched = await claimMaterialOf(fixture);
    const single = await claimMaterialOf(fixture);
    const windows = await batched.stepsAt(indices);
    const own = [];
    for (const index of indices) own.push(await single.steps(index));
    expect(windows).toStrictEqual(own);
    for (const [at, index] of indices.entries()) {
      expect(Object.keys(windows[at]!.states)).toStrictEqual([
        String(index),
        String(index + 1),
      ]);
      expect(await batched.steps(index)).toStrictEqual(own[at]);
    }
  }, 600_000);
});
