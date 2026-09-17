import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { afterAll, describe, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { runInvalidSignatureWrongfulRejectionScenario } from "./support/invalid-signature-wrongful-emulator.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

const measuredFit = createMeasuredFitRecorder(
  "invalid-signature-wrongful-rejection",
  "lifecycle",
  "selected witness at counts 0, 139, 317 and maximum depth64 membership, exact and out-of-range forced coordinates",
);

describe("invalidSignature wrongful-rejection real lifecycle", () => {
  it.each([
    { decoyWitnessCount: 0, accused: "honest" as const, rejectedIndex: null },
    { decoyWitnessCount: 139, accused: "honest" as const, rejectedIndex: null },
    { decoyWitnessCount: 317, accused: "honest" as const, rejectedIndex: null },
    { decoyWitnessCount: 0, accused: "honest" as const, rejectedIndex: 1n },
    { decoyWitnessCount: 0, accused: "honest" as const, rejectedIndex: -1n },
    { decoyWitnessCount: 0, accused: "invalid" as const, rejectedIndex: null },
    {
      decoyWitnessCount: 317,
      accused: "honest" as const,
      rejectedIndex: null,
      deepMembership: true,
    },
  ])(
    "runs exact forced lifecycle $decoyWitnessCount/$accused/$rejectedIndex/$deepMembership",
    (scenario) =>
      runInvalidSignatureWrongfulRejectionScenario(scenario, measuredFit),
    600_000,
  );
});

afterAll(async () => {
  console.info(
    `[invalid-signature-blueprint] ${createHash("sha256")
      .update(await readFile(realBlueprintPath))
      .digest("hex")}`,
  );
});
