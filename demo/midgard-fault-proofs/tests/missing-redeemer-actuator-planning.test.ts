import {
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  type BoundMissingRedeemerActuatorConfig,
  createMissingRedeemerActuator,
} from "../src/missing-redeemer/actuator.js";
import type { MissingRedeemerArtifact } from "../src/missing-redeemer/replay.js";
import { createMissingRedeemerStagedPlanner } from "../src/missing-redeemer/staged-plan.js";
import { submitMissingRedeemerStep04 } from "../src/missing-redeemer/submit-field-scan.js";

vi.mock("../src/missing-redeemer/submit-field-scan.js", async (original) => ({
  ...(await original<
    typeof import("../src/missing-redeemer/submit-field-scan.js")
  >()),
  submitMissingRedeemerStep04: vi.fn(async () => {
    throw new Error("builder boundary");
  }),
}));
vi.mock("../src/missing-redeemer/submit-step-05.js", () => ({
  submitMissingRedeemerStep05: vi.fn(async () => {
    throw new Error("builder boundary");
  }),
}));
vi.mock("../src/missing-redeemer/submit-cancel.js", () => ({
  submitMissingRedeemerCancel: vi.fn(async () => {
    throw new Error("builder boundary");
  }),
}));

const headerHash = "ab".repeat(28);
const artifact = (
  transactionId = "12".repeat(32),
  index = 0,
): MissingRedeemerArtifact =>
  ({
    schemaVersion: "midgard-missing-redeemer-production-artifact-v1",
    headerHash,
    header: { validationTracesRoot: "cd".repeat(32), validationTraceCount: 1n },
    evidence: {
      subject: acceptedVerdictSubject(transactionId),
      purposeKind: 0,
      purposeIndex: 0,
      redeemerMissing: true,
      purpose: { sourceLeafHashHex: "ef".repeat(32), sourceLanguageTag: 3 },
      fieldPreimageHex: encodeMidgardFieldPreimage([
        encodeMidgardRedeemerWitnessItem({
          purpose: "Mint",
          index: BigInt(index),
          redeemerCbor: Buffer.from("00", "hex"),
          executionUnits: { memory: 1n, steps: 2n },
        }),
      ]).toString("hex"),
    },
    authentication: {
      validationTracesRoot: "cd".repeat(32),
      validationTraceCount: 1n,
      machineState: { transaction_id: transactionId },
      sourceLanguageTag: 3n,
      control: {
        discovery: {
          current_purpose_kind: 0n,
          current_purpose_index: 0n,
          matched_source_leaf: "ef".repeat(32),
        },
      },
    },
    acceptedInclusion: {},
  }) as unknown as MissingRedeemerArtifact;

describe("missing redeemer actuator planning", () => {
  it("shares exact-input work across captures while preserving artifact admission", async () => {
    const planStagedWalk = vi.fn(createMissingRedeemerStagedPlanner());
    const actuator = createMissingRedeemerActuator({
      binding: {
        definition: { headerHash },
        resolvedContracts: { category: { categoryId: "00000001" } },
      },
      references: { steps: [], witnesses: {} },
      planStagedWalk,
    } as unknown as BoundMissingRedeemerActuatorConfig);
    const capture = (value: MissingRedeemerArtifact) =>
      actuator.capture({
        action: { stage: "scan", threadOutRef: `${"11".repeat(32)}#0` },
        artifact: value,
      });
    const submit = vi.mocked(submitMissingRedeemerStep04);
    await expect(capture(artifact())).rejects.toThrow("builder boundary");
    const first = submit.mock.calls.at(-1)![0].staged;
    first.items[0]!.fill(0);
    await expect(capture(artifact())).rejects.toThrow("builder boundary");
    const second = submit.mock.calls.at(-1)![0].staged;
    expect(second.initialGrammar).toBe(first.initialGrammar);
    expect(second.items[0]).not.toEqual(first.items[0]);
    await expect(capture(artifact("34".repeat(32)))).rejects.toThrow(
      "builder boundary",
    );
    expect(submit.mock.calls.at(-1)![0].staged.initialGrammar.txId).toBe(
      "34".repeat(32),
    );
    await expect(capture(artifact("34".repeat(32), 1))).rejects.toThrow(
      "builder boundary",
    );
    expect(
      submit.mock.calls.at(-1)![0].staged.initialGrammar.fieldCommitment,
    ).not.toBe(second.initialGrammar.fieldCommitment);
    await expect(
      capture({ ...artifact(), headerHash: "ff".repeat(28) }),
    ).rejects.toThrow("bound header");
    await expect(
      capture({
        ...artifact(),
        schemaVersion: "changed",
      } as unknown as MissingRedeemerArtifact),
    ).rejects.toThrow("not admitted");
    expect(planStagedWalk).toHaveBeenCalledTimes(4);
    for (const stage of ["finalize", "cancel"] as const) {
      await expect(
        actuator.capture({
          action: { stage, threadOutRef: "unused#0", stepIndex: 0 },
          artifact: artifact(),
        }),
      ).rejects.toThrow("builder boundary");
    }
    expect(planStagedWalk).toHaveBeenCalledTimes(4);
  });
});
