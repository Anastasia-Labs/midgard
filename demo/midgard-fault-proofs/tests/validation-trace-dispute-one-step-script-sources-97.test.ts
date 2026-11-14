/** The validation-trace dispute runner at the script-sources stage-six begin-policy
 * step after state 97 of our trace of the failing transaction that also mints under a native script's policy, driven through the phase
 * resolver and the script-sources non-output semantic resolver, which
 * invokes the stage-six begin-policy yield beside the step, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 97,
  phase: "scriptSources",
  semantic: 0,
  evidence: {
    auxiliary: 1,
    role: 5,
    itemYield: { name: "scriptSourcesStageSixBeginPolicy" },
  },
  at: "scriptSourcesNonOutputSemantic",
  failingNativeMint: true,
});

describe("validation-trace dispute runner: driving a thread at the script-sources stage-six begin-policy step after state 97", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the script-sources non-output semantic resolver, committing the transition beside the step's auxiliary witness and its role, with the stage-six begin-policy yield",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the script-sources non-output semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses the step beside another role's yield, at the script-sources non-output semantic resolver's role check", async () => {
    const refused = await drive.otherYield({
      name: "scriptSourcesStageSixFinish",
    });
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1",
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
