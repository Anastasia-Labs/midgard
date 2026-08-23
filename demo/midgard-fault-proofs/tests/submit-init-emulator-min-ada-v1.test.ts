import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  validationResolverIndexV1,
  validationSemanticResolverGlobalIndexV1,
} from "../src/index.js";
import {
  buildAcceptedClaimOverMinAdaRejectingTransactionFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// R8 of decision 0005 (#618), with the #627 owner ruling deciding where the
// rate lives: the ValueAndMint output ladder now convicts an under-funded
// produced output with `E_MIN_ADA`. This is the emulator measurement of how
// far that conviction travels on L1 -- open, source, bisection,
// enter-resolution, prepare-resolution and `value_and_mint_v1`'s
// prepare-selected, which routes the disputed step to semantic slot 5,
// `value_and_mint_output_descriptor_semantic_v1`.
//
// MEASURED FRONTIER, NOT AN ASSUMPTION -- WHY THIS STOPS AT prepare-selected.
// The semantic-resolution transaction that follows does not fit the literal
// 16,384-byte L1 proof envelope: running this same journey through to the
// award measures a complete transaction of 21,576 bytes. The cause is not
// min-Ada and is not this fixture. The ValueAndMint semantic resolvers are
// attached INLINE, because
// `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
// (src/validation-dispute/submit.ts) publishes reference scripts for the CEK
// decomposition's oversized semantics only and has no ValueAndMint
// counterpart; and eight of the eleven ValueAndMint semantic bodies are over
// the envelope on their own at parent HEAD 55f59529, before any redeemer:
// replay_input 21,203, replay_asset 21,881, replay_finish 20,962,
// output_descriptor 20,958, output_asset 21,583, output_finish 20,740,
// mint_asset 18,458 and mint_finish 17,767 bytes. Only begin (11,473),
// replay_begin (11,013) and finalize (11,987) fit, which is exactly the set
// the existing journeys in submit-init-emulator-cek-value-and-mint-v1.test.ts
// exercise. The E_MIN_ADA wiring adds 78 bytes to the output-descriptor body
// (20,958 -> 21,036); it neither creates this gap nor materially widens it.
// Giving the ValueAndMint semantics the reference-script role the CEK
// semantics already have is an R3-class deployment change (new deployment-info
// roles, new publication steps, moving deployment identity) and is escalated
// rather than taken here.
describe("validation-dispute journey to the E_MIN_ADA output-descriptor conviction", () => {
  it("bisects an Accepted claim over a min-Ada rejection onto the output-descriptor semantic resolver", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildAcceptedClaimOverMinAdaRejectingTransactionFixture({
          operatorVkey,
          now,
        }),
      { stopAfter: "prepare-selected" },
    );
    const { fixture, lowIndex, highIndex } = result;

    // The disputed boundary is the rejecting terminal itself, which is the
    // only boundary at which `rejected_successor_is_exact` runs -- and here
    // its `pre` is the ValueAndMint output-descriptor instruction, so the
    // conjunct that decides the step is the new `reject_min_ada` one.
    expect(highIndex).toBe(lowIndex + 1);
    expect(highIndex).toBe(fixture.challengerTrace.states.length - 1);
    const preState = fixture.challengerTrace.states[lowIndex]!;
    const challengerSuccessor = fixture.challengerTrace.states[highIndex]!;
    expect(preState.phase).toBe("valueAndMint");
    expect(preState.verdict).toBe("pending");
    expect(challengerSuccessor.phase).toBe("terminal");
    expect(challengerSuccessor.verdict).toBe("rejected");
    expect(fixture.challengerTrace.rejectionCode).toBe(RejectCodes.MinAda);

    // The operator really did commit `Accepted`, which the forced-source
    // binding forces from `operator_validity: TxIsValid`.
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.evidence.moves.length).toBeGreaterThan(0);

    // prepare-selected completing on the emulator is the on-chain half of this
    // measurement: `value_and_mint_v1` accepted the disputed step and routed
    // it to slot 5, the output-descriptor semantic resolver -- not to a
    // neighbouring kind, and not to the retired direct resolver.
    const resolverIndex = validationResolverIndexV1("ValueAndMint");
    // Stage 3's descriptor step: `validationSemanticResolverIndexV1` maps the
    // `valueOutputDescriptor` witness kind to 5.
    const semanticResolverIndex = 5;
    expect(fixture.evidence.oneStepArgument.resolverIndex).toBe(resolverIndex);
    expect(fixture.evidence.oneStepArgument.semanticResolverIndex).toBe(
      semanticResolverIndex,
    );
    expect(
      Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics)[
        validationSemanticResolverGlobalIndexV1(
          resolverIndex,
          semanticResolverIndex,
        )
      ],
    ).toBe(
      "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend",
    );
  }, 900_000);
});
