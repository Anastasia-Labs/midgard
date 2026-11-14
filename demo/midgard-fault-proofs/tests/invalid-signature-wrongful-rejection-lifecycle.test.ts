import { describe, it } from "vitest";

import { runInvalidSignatureWrongfulRejectionScenario } from "./support/invalid-signature-wrongful-emulator.js";

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
    (scenario) => runInvalidSignatureWrongfulRejectionScenario(scenario),
    600_000,
  );
});
