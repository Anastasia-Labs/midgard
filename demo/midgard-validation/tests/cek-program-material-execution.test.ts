import { readFileSync } from "node:fs";

import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
const largeIntegerProgram = JSON.parse(
  readFileSync(
    new URL("./fixtures/cek-program-large-integer.json", import.meta.url),
    "utf8",
  ),
) as { readonly envelopeCbor: string; readonly material: readonly string[] };

// Separate from the authoring tests: input is an externally supplied sidecar,
// and the authoring/execution test file is already at its module-size cap.
describe("core-admitted program material execution", () => {
  it("executes externally supplied chunked big-integer program material", () => {
    // Captured core-valid material for (lam ctx (addInteger 2^512 1)); this
    // bypasses our authoring builder, as a transaction sidecar can in Phase A.
    const envelope = decodeMidgardCekProgramEnvelope(
      Buffer.from(largeIntegerProgram.envelopeCbor, "hex"),
    );
    const material = largeIntegerProgram.material.map((entry) =>
      decodeMidgardCekProgramMaterialEntry(Buffer.from(entry, "hex")),
    );
    expect(
      verifyMidgardCekProgramMaterial(envelope, material).constants.length,
    ).toBe(2);
    const graph = buildMidgardCekExecutionGraph(
      envelope,
      material,
      Buffer.from("d87980", "hex"),
    );
    const execution = executeMidgardCekStructuralProgram({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      maxSteps: 64,
    });
    expect(execution.terminalState.mode).toBe("haltSuccess");
    expect(
      execution.steps.every((step) =>
        verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
      ),
    ).toBe(true);
  });
});
