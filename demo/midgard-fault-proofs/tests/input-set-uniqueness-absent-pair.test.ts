/**
 * A forced `DuplicateInput` rejection whose accused pair cannot name two
 * distinct inputs is wrongful: a coordinate outside the spend or reference
 * field it names (or naming another field), or both coordinates on the same
 * input, names no duplicate. Step 03 seeds the scan with the authenticated
 * counts and step 04 finalizes on the cursor-0 state, without scanning. An
 * honest rejection whose pair is in range and distinct is refused at cursor 0
 * in either orientation, and the route convicts over the largest reference
 * field.
 */
import { describe, expect, it } from "vitest";

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  type AccusedInputPair,
  exerciseForcedLifecycle,
} from "./input-set-uniqueness-wrongful-rejection-lifecycle.exercise-forced-lifecycle.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";

const pair = (
  first: readonly [bigint, bigint],
  second: readonly [bigint, bigint],
): AccusedInputPair => ({
  first_field_index: first[0],
  first_item_index: first[1],
  second_field_index: second[0],
  second_item_index: second[1],
});

describe("input-set-uniqueness forced rejection of a pair naming no duplicate", () => {
  it.each([
    {
      pairName: "a field outside the input fields",
      accused: pair([2n, 0n], [1n, 0n]),
    },
    { pairName: "a negative item", accused: pair([0n, -1n], [1n, 0n]) },
    { pairName: "an item past its field", accused: pair([0n, 0n], [1n, 400n]) },
    { pairName: "one input twice", accused: pair([1n, 3n], [1n, 3n]) },
  ])(
    "convicts at cursor 0 a pair naming $pairName",
    async ({ accused }) => {
      await exerciseForcedLifecycle({
        referenceCount: 400,
        complete: true,
        accused,
        finalizeAtSeed: true,
      });
    },
    600_000,
  );

  it("refuses on chain a cursor-0 conviction of an honest pair in reversed orientation", async () => {
    // The first reference input repeats the spend input, so the operator's
    // rejection is right; the pair names it reference-first.
    await exerciseForcedLifecycle({
      referenceCount: 400,
      complete: true,
      accused: pair([1n, 0n], [0n, 0n]),
      overlap: true,
      finalizeAtSeed: true,
      refuseFinalize: (finalize) =>
        expectOnchainRefusal(finalize, {
          refusedBy: "fraud_proofs/input_set_uniqueness/step_04",
          check: /^Validator returned false$/u,
        }),
    });
  }, 600_000);

  it("convicts at cursor 0 a 64-bit pair over the 819-input reference field", async () => {
    const measurements = await exerciseForcedLifecycle({
      referenceCount: 819,
      complete: true,
      accused: pair([2n ** 63n, 2n ** 63n], [1n, 2n ** 63n]),
      finalizeAtSeed: true,
    });
    // init, step 01, step 03 (seed), step 04 (finalize), removal.
    expect(measurements).toHaveLength(5);
    for (const { executionMemory, executionSteps } of measurements) {
      expect(executionMemory).toBeLessThanOrEqual(VAN_ROSSEM_MAX_MEMORY_UNITS);
      expect(executionSteps).toBeLessThanOrEqual(VAN_ROSSEM_MAX_CPU_UNITS);
    }
    const [, bind, seed, finalize] = measurements;
    console.info(
      `max-shape pair naming no duplicate (819 reference inputs, coordinates 2^63): step-01 mem=${bind!.executionMemory.toString()} cpu=${bind!.executionSteps.toString()}; step-03 mem=${seed!.executionMemory.toString()} cpu=${seed!.executionSteps.toString()}; step-04 mem=${finalize!.executionMemory.toString()} cpu=${finalize!.executionSteps.toString()}`,
    );
  }, 900_000);
});
