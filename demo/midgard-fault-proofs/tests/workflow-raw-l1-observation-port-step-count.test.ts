import { FraudProofTokenDatum } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import { MAX_COMPUTATION_THREAD_STEPS } from "../src/workflow/computation-thread-steps.js";
import { createFraudProofFamilyRawL1ObservationPort } from "../src/workflow/family-l1-observation.js";
import type {
  FraudProofRawL1ComputationStepRole,
  FraudProofRawL1FamilyDefinition,
} from "../src/workflow/index.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../src/workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { fixture } from "./support/raw-l1-terminal-fixture.fixture.js";
import { scriptAddress } from "./support/raw-l1-terminal-fixture.output.js";

/** The port for a family whose computation thread has `stepCount` steps. */
const portFor = async (stepCount: number) => {
  const value = await fixture();
  const base = value.definition.computationThread.steps;
  const definition: FraudProofRawL1FamilyDefinition = {
    ...value.definition,
    computationThread: {
      ...value.definition.computationThread,
      steps: [
        ...base,
        ...Array.from({ length: stepCount - base.length }, (_, index) => ({
          role: `computation_thread_step_${(base.length + index + 1).toString().padStart(2, "0")}` as FraudProofRawL1ComputationStepRole,
          address: scriptAddress((0x80 + index).toString(16)),
          datumSchema: FraudProofTokenDatum,
        })),
      ],
    },
  };
  return () =>
    createFraudProofFamilyRawL1ObservationPort({
      authority: {
        authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
        capture: async () => value.snapshot,
      },
      releaseFinality: value.binding.releaseFinality,
      releaseEconomics: value.binding.releaseEconomics,
      definition: { ...definition, category: "doubleSpend" },
    });
};

describe("raw L1 observation port step count", () => {
  it("opens the port for every installed computation-thread length", async () => {
    for (const stepCount of [18, 21, MAX_COMPUTATION_THREAD_STEPS]) {
      expect((await portFor(stepCount))().category).toBe("doubleSpend");
    }
  });

  it("refuses a thread longer than any installed family", async () => {
    expect(await portFor(MAX_COMPUTATION_THREAD_STEPS + 1)).toThrow(
      /raw L1 observation authority is incomplete/u,
    );
  });
});
