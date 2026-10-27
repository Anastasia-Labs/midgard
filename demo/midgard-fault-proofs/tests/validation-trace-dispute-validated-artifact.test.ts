/** The validation-trace dispute's check of its saved artifact on a resumed
 * run, on the emulator: the artifact the block's claim prepared passes, and
 * passes again; once it has passed, the same artifact with one byte changed,
 * the same artifact under another replay context for the same block, under a
 * copy of its predecessor authority that was never admitted, and under a
 * classification that does not detect its event, are each refused, and the
 * prepared artifact under its own inputs still passes after them. */
import { beforeAll, describe, expect, it } from "vitest";

import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  detectOn,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/** `artifact` with the last hex digit of its challenger root changed. */
const oneByteOff = (artifact: JournalJsonObject): JournalJsonObject => {
  const root = artifact.challengerRoot as string;
  const last = root.at(-1) === "0" ? "1" : "0";
  return { ...artifact, challengerRoot: `${root.slice(0, -1)}${last}` };
};

describe("a saved validation-trace artifact checked again", () => {
  let scene: Scene;
  let adapter: FraudProofFamilyWorkflowAdapter;
  let inputs: Parameters<
    NonNullable<FraudProofFamilyWorkflowAdapter["validatePreparedArtifact"]>
  >[0];
  const validate = (input: typeof inputs) =>
    adapter.validatePreparedArtifact!(input);

  beforeAll(async () => {
    scene = await setUpScene();
    makeRunner(scene, {
      wrap: (base) => {
        adapter = base;
        return base;
      },
    });
    const { evidence, context } = scene.failing;
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: await detectOn(scene.failing),
      minimumConfirmationDepth: 1,
    });
    if (classification.decision !== "fault_detected")
      throw new Error("the failing block detects no fault");
    const prepared = { evidence, classification, replayContext: context };
    // The artifact as the journal saves it.
    const artifact = normalizeJournalJson(
      await adapter.prepare(prepared),
    ) as JournalJsonObject;
    inputs = { ...prepared, artifact };
  }, 600_000);

  it("passes under the inputs it was prepared from, again and again", async () => {
    await validate(inputs);
    await validate(inputs);
  });

  it("refuses the artifact with one byte changed, after the artifact has passed", async () => {
    await validate(inputs);
    await expect(
      validate({ ...inputs, artifact: oneByteOff(inputs.artifact) }),
    ).rejects.toThrow(
      "validation-trace artifact differs from this evidence's replay",
    );
    await validate(inputs);
  });

  it("refuses the artifact under another replay context for the same block, after it has passed", async () => {
    await validate(inputs);
    const { replayContext: _context, ...withoutContext } = inputs;
    await expect(validate(withoutContext)).rejects.toThrow(
      "supply the ledger after its predecessor",
    );
    await validate(inputs);
    await expect(
      validate({ ...inputs, replayContext: scene.passing.context }),
    ).rejects.toThrow(
      "complete replay predecessor was not admitted for this challenged header",
    );
    await validate(inputs);
  });

  it("refuses an unadmitted copy of the admitted predecessor authority, after the artifact has passed", async () => {
    await validate(inputs);
    const context = inputs.replayContext!;
    await expect(
      validate({
        ...inputs,
        replayContext: {
          ...context,
          predecessor: { ...context.predecessor! },
        },
      }),
    ).rejects.toThrow(
      "complete replay predecessor was not admitted for this challenged header",
    );
    await validate(inputs);
  });

  it("refuses the artifact under a classification that does not detect its event, after it has passed", async () => {
    await validate(inputs);
    await expect(
      validate({
        ...inputs,
        classification: { ...inputs.classification, detections: [] },
      }),
    ).rejects.toThrow(
      "validation-trace artifact differs from this evidence's replay",
    );
    await validate(inputs);
  });
});
