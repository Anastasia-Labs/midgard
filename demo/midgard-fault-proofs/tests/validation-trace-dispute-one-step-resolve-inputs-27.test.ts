/** The validation-trace dispute runner at the resolve-inputs membership finalize that attaches the reference script
 * after state 27 of our trace, driven through the phase resolver and the
 * resolve-inputs membership finalize semantic resolver, beside the reference-script descriptor yield, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 27,
  phase: "resolveInputs",
  semantic: 4,
  evidence: {
    auxiliary: 31,
    yieldList: ["ledgerOutputDescriptorReferenceScript"],
  },
  at: "resolveInputsMembershipFinalizeSemantic",
});

describe("validation-trace dispute runner: driving a thread at the resolve-inputs membership finalize that attaches the reference script after state 27", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the resolve-inputs membership finalize semantic resolver, committing the transition beside its output proof evidence, beside the reference-script descriptor yield",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the resolve-inputs membership finalize semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses the step invoking a descriptor yield other than its fact group's", async () => {
    const refused = await drive.otherYieldList([
      "ledgerOutputDescriptorScanFacts",
    ]);
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1",
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
