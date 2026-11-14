/** The validation-trace dispute runner at a script-sources output proof step whose stage is the structure finish stage, after state 63 of our
 * trace, driven through the phase resolver and the script-sources output
 * proof step semantic resolver, beside the structure finish yield, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 63,
  phase: "scriptSources",
  semantic: 2,
  evidence: { auxiliary: 30, yieldList: ["ledgerOutputProofStructureFinish"] },
  at: "scriptSourcesOutputProofStepSemantic",
});

describe("validation-trace dispute runner: driving a thread at the script-sources output proof step whose output proof stage is the structure finish stage after state 63", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the script-sources output proof step semantic resolver, committing the transition beside its output proof evidence, beside the structure finish yield",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the script-sources output proof step semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses the step invoking a yield other than its stage's", async () => {
    const refused = await drive.otherYieldList(["ledgerOutputProofStructure"]);
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1",
      check: /^expect candidate_role == role_asset_name/u,
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
