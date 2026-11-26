/** The validation-trace dispute runner at the signatures step after state
 * 12 of our trace, which reads the failing transaction's one address witness: driven through the phase resolver and the
 * signatures address item semantic resolver, beside the address-witness field carried inline, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 12,
  phase: "signatures",
  semantic: 1,
  evidence: { fieldIndex: 7 },
  at: "signaturesAddressItemSemantic",
});

describe("validation-trace dispute runner: driving a thread at the signatures address item step after state 12", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the signatures address item semantic resolver, committing the transition beside the address witness and its field carried inline",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the signatures address item semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/signatures_address_item_semantic_v1",
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
