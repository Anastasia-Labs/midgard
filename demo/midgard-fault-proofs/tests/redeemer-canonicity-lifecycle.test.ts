import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/redeemer-canonicity/submit-step-01.js";
import "../src/redeemer-canonicity/submit-step-02.js";
import "../src/redeemer-canonicity/submit-step-03.js";
import "../src/redeemer-canonicity/family.js";
import "../src/redeemer-canonicity/authenticated-workflow.js";
import "../src/redeemer-canonicity/schemas.js";
import "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import "../src/transition-trace/witnesses.build-source-membership-proof.js";
import "./support/committed-field-shape-emulator.committed-field-shape-scenario-material.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/harness.js";
import "./support/emulator/header-fixtures.js";
import "./support/emulator/measurement.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import "./support/emulator/setup-tx.js";
import "./support/submit-init/removal.js";
import "./support/submit-init/successor.js";
import "./support/submit-init/forced-transitions.js";
import "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import "./redeemer-canonicity-lifecycle.init-thread.js";
import "./redeemer-canonicity-lifecycle.redeemer-canonicity-accepted-lifecycle.js";
import "./redeemer-canonicity-lifecycle.redeemer-canonicity-forced-lifecycle.js";
import "./redeemer-canonicity-lifecycle.redeemer-canonicity-forced-accepted-lifecycle.js";

import { describe, it } from "vitest";

import { refuseFinalizeAgainstHonestCanonicalRedeemer } from "./redeemer-canonicity-lifecycle.redeemer-canonicity-forced-accepted-lifecycle.js";

describe("redeemer-canonicity forced wrongful acceptance", () => {
  it(
    "refuses to finalize a wrongful-acceptance thread against an honest canonical redeemer",
    refuseFinalizeAgainstHonestCanonicalRedeemer,
    300_000,
  );
});
