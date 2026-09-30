import "./input-set-uniqueness-wrongful-rejection-lifecycle.measured-fit.js";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/field-opening.js";
import "../src/input-set-uniqueness/index.js";
import "../src/remove-fraudulent-block.js";
import "../src/transition-trace/phas.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/measurement.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/removal-deployment.js";
import "./support/emulator/setup-tx.js";
import "./support/input-set-uniqueness-emulator.js";
import "./support/measured-fit-ledger.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./input-set-uniqueness-wrongful-rejection-lifecycle.measured-fit.js";
import "./input-set-uniqueness-wrongful-rejection-lifecycle.exercise-forced-lifecycle.js";

import { describe, it } from "vitest";

import { exerciseForcedLifecycle } from "./input-set-uniqueness-wrongful-rejection-lifecycle.exercise-forced-lifecycle.js";

describe("input-set-uniqueness wrongful-rejection lifecycle", () => {
  it("cancels at every forced boundary, restarts, resumes exactly, and rejects reference-script substitution", async () => {
    await exerciseForcedLifecycle({ referenceCount: 400, complete: false });
  }, 600_000);

  it("authenticates the maximum 819-reference field through every batch, permanent mint, and leased removal", async () => {
    await exerciseForcedLifecycle({ referenceCount: 819, complete: true });
  }, 600_000);
});
