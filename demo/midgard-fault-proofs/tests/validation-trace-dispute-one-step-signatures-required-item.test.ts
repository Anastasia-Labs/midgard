/** The validation-trace dispute runner at the signatures step after state
 * 13 of our trace, whose failing transaction requires its owner's signature: the step reads that required signer: driven through the phase resolver and the
 * signatures required item semantic resolver, beside the required-signer field carried inline and the signer's membership among the transaction's signers, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 13,
  phase: "signatures",
  semantic: 2,
  evidence: { requiredSigner: true },
  failingSigners: { requiredOwner: 1 },
  at: "signaturesRequiredItemSemantic",
});

describe("validation-trace dispute runner: driving a thread at the signatures required item step after state 13", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the signatures required item semantic resolver, committing the transition beside the required signer, its field carried inline, and its membership proof",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the signatures required item semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/signatures_required_item_semantic_v1",
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
