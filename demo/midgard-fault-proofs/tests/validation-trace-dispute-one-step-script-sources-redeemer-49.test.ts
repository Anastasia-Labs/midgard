/** The validation-trace dispute runner at the script-sources redeemer
 * normalization step after state 49 of our trace, the redeemer item step
 * that opens the redeemer's header, driven through the phase resolver, the
 * redeemer normalization semantic resolver and the redeemer item chain to
 * the award
 * (support/validation-trace-dispute-one-step-script-sources-redeemer-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { scriptSourcesRedeemerDrive } from "./support/validation-trace-dispute-one-step-script-sources-redeemer-drive.js";

const drive = scriptSourcesRedeemerDrive({
  from: 49,
  executor: "redeemerItemOpenHeaderExecutor",
});

describe("validation-trace dispute runner: driving a thread at the script-sources redeemer normalization step after state 49", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the redeemer normalization semantic resolver, committing the transition beside the redeemer item",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the redeemer normalization semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the redeemer item chain to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
