/** The validation-trace dispute runner at the phase-A script-preconditions
 * step after state 16 of our trace, driven through the phase resolver and
 * the phase-A script-preconditions finalize semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 16,
  phase: "phaseAScriptPreconditions",
  at: "phaseAScriptPreconditionsFinalizeSemantic",
});

describe("validation-trace dispute runner: driving a thread at the phase-A script-preconditions step after state 16", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the phase-A script-preconditions finalize semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the phase-A script-preconditions finalize semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
