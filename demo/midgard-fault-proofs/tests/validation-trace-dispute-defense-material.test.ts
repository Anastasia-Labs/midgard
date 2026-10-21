/** The defender's material for a case on its own committed Plutus event: its
 * state at the case's step count, proven under its own trace root at that
 * step count; and its own fault when the block accepted an event that the
 * replay rejects. */
import {
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  type MidgardValidationTraceDescriptor,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  buildValidationTraceDefenseMaterial,
  type ValidationTraceDefenseMaterial,
} from "../src/validation-trace-dispute/defense-material.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  plutusExecutionBlockFixture,
  type PlutusExecutionScenario,
} from "./helpers/plutus-execution-block-fixture.js";
import { l1SlotConfig } from "./support/validation-trace-claim-material.js";

const defenseMaterial = async (
  scenario: PlutusExecutionScenario,
  stepCount: number,
): Promise<ValidationTraceDefenseMaterial> => {
  const block = await plutusExecutionBlockFixture(scenario);
  return await buildValidationTraceDefenseMaterial({
    evidence: block.evidence,
    predecessor: completeCanonicalReplayPredecessorLedger(block),
    eventKey: block.eventKey,
    stepCount,
    l1SlotConfig,
  });
};

type Defense = Extract<ValidationTraceDefenseMaterial, { kind: "defend" }>;

const defend = async (stepCount: number): Promise<Defense> => {
  const material = await defenseMaterial("passing", stepCount);
  if (material.kind !== "defend")
    throw new Error(`expected defense material, got ${material.kind}`);
  return material;
};

const descriptorOf = (material: Defense): MidgardValidationTraceDescriptor => ({
  schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
  traceRoot: Buffer.from(material.root, "hex"),
  stepCount: material.stepCount,
  initialStateHash: material.initialStateHash,
  terminalStateHash: hashMidgardValidationMachineState(material.stateAtS),
  verdict: "accepted",
  rejectionCodeHash: material.stateAtS.rejectionCodeHash,
});

describe("validation-trace dispute defense material", () => {
  it("proves its state at every step count under its own root at that step count", async () => {
    const honestStepCount = (await defend(1)).honestStepCount;
    expect(honestStepCount).toBeGreaterThan(2);
    for (const stepCount of [
      1,
      honestStepCount - 1,
      honestStepCount,
      honestStepCount + 5,
    ]) {
      const material = await defend(stepCount);
      expect(material.stepCount).toBe(stepCount);
      expect(material.honestStepCount).toBe(honestStepCount);
      expect(material.proofAtS.stateIndex).toBe(stepCount);
      expect(material.proofAtS.stateHash).toStrictEqual(
        hashMidgardValidationMachineState(material.stateAtS),
      );
      if (stepCount < honestStepCount)
        expect(material.stateAtS.phase).not.toBe("terminal");
      else
        expect(material.stateAtS).toMatchObject({
          phase: "terminal",
          verdict: "accepted",
        });
      const descriptor = descriptorOf(material);
      const initial = material.proofAt(0);
      expect(initial.stateHash).toStrictEqual(material.initialStateHash);
      for (const proof of [material.proofAtS, initial])
        expect(verifyMidgardValidationTraceProof({ descriptor, proof })).toBe(
          true,
        );
      expect(
        verifyMidgardValidationTraceProof({
          descriptor,
          proof: { ...material.proofAtS, stateHash: Buffer.alloc(32, 0x5c) },
        }),
      ).toBe(false);
    }
  });

  it("starts every step count's trace from the same initial state", async () => {
    const [short, long] = await Promise.all([defend(1), defend(64)]);
    expect(short.initialStateHash).toStrictEqual(long.initialStateHash);
  });

  it("reports its own fault, and never defends, when its block accepted an event the replay rejects", async () => {
    const material = await defenseMaterial("failing", 1);
    expect(material.kind).toBe("operator_fault");
  });
});
