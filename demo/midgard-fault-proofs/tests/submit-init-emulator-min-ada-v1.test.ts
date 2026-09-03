import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
  validationResolverIndex,
  validationSemanticResolverGlobalIndex,
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
// WHY THIS NOW REACHES SEMANTIC RESOLUTION (#634, re-authored 2026-08-23).
// This journey previously stopped at `prepare-selected`: the resolution
// transaction that follows measured 21,576 complete signed bytes against the
// literal 16,384-byte L1 proof envelope. The cause was never min-Ada and never
// this fixture -- the ValueAndMint semantic resolvers were attached INLINE,
// because `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
// (src/validation-dispute/submit.ts) published reference scripts for the CEK
// decomposition's oversized semantics only and had no ValueAndMint
// counterpart; and eight of the eleven ValueAndMint semantic bodies are over
// the envelope on their own, before any redeemer. Applied bodies measured in
// THIS tree (#634, 2026-08-23) by resolving each semantic through
// `resolveValidationTraceDisputeDeploymentContracts` -- the same route the
// resolution hash-checks -- against a blueprint built from this tree's
// `onchain/`: replay_input 21,367, replay_asset 22,046, replay_finish 21,138,
// output_descriptor 21,207, output_asset 21,823, output_finish 20,987,
// mint_asset 18,622 and mint_finish 17,931 bytes. Only begin (11,545),
// replay_begin (11,085) and finalize (12,059) fit -- exactly the set the
// existing journeys in submit-init-emulator-cek-value-and-mint-v1.test.ts
// exercise. The output-descriptor body is 21,207 bytes with the E_MIN_ADA
// wiring in place -- 4,823 bytes past the envelope on the body alone: the gap
// is structural to the ValueAndMint decomposition, not something E_MIN_ADA
// created.
//
// #634 gave the eleven ValueAndMint semantics the reference-script deployment
// role the CEK ones already had
// (`VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`),
// so the conviction is now carriable: the harness publishes the
// output-descriptor resolver as a reference script at deployment time -- a
// publication that is itself necessarily oversized, exactly as the CEK ones
// are -- and the resolution reads it instead of embedding it. What this test
// measures is the whole distance the E_MIN_ADA conviction travels on L1: open,
// source, bisection, enter-resolution, prepare-resolution,
// `value_and_mint_v1`'s prepare-selected, and the output-descriptor semantic
// resolution itself, inside the real envelope.
describe("validation-dispute journey to the E_MIN_ADA output-descriptor conviction", () => {
  it("resolves an Accepted claim over a min-Ada rejection on the output-descriptor semantic resolver", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildAcceptedClaimOverMinAdaRejectingTransactionFixture({
          operatorVkey,
          now,
        }),
      { stopAfter: "semantic-resolution" },
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
    // binding forces from `verdict: ForcedTxValid`.
    expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
    expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("rejected");
    expect(fixture.evidence.moves.length).toBeGreaterThan(0);

    // prepare-selected completing on the emulator is the first on-chain half
    // of this measurement: `value_and_mint_v1` accepted the disputed step and
    // routed it to slot 5, the output-descriptor semantic resolver -- not to a
    // neighbouring kind, and not to the retired direct resolver.
    const resolverIndex = validationResolverIndex("ValueAndMint");
    // Stage 3's descriptor step: `validationSemanticResolverIndexV1` maps the
    // `valueOutputDescriptor` witness kind to 5.
    const semanticResolverIndex = 5;
    expect(fixture.evidence.oneStepArgument.resolverIndex).toBe(resolverIndex);
    expect(fixture.evidence.oneStepArgument.semanticResolverIndex).toBe(
      semanticResolverIndex,
    );
    expect(
      Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics)[
        validationSemanticResolverGlobalIndex(
          resolverIndex,
          semanticResolverIndex,
        )
      ],
    ).toBe(
      "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend",
    );

    // #634. The deployment-time publication is honestly oversized: the applied
    // output-descriptor body alone is over the L1 proof envelope, which is
    // precisely why it cannot ride inline and why its own publication cannot
    // fit either.
    const publication = result.valueAndMintSemanticReferencePublication;
    expect(publication).toBeDefined();
    expect(publication!.entryName).toBe(
      "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
    );
    expect(publication!.appliedResolverBytes).toBeGreaterThan(
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
    );
    expect(publication!.publicationMeasurement.l1ByteMargin).toBeLessThan(0);
    expect(publication!.utxo.scriptRef).toBeDefined();

    // THE OUTCOME, NOT JUST THE FIT. The semantic resolution transaction was
    // built, signed, submitted and confirmed on the emulator: the disputed
    // output-descriptor step was resolved against the output-descriptor
    // semantic resolver at its global slot, and the dispute thread moved on to
    // the award stage.
    const semantic = result.semanticResult;
    expect(semantic).toBeDefined();
    expect(semantic!.semanticValidatorCarriage).toBe("reference");
    expect(semantic!.resolverIndex).toBe(resolverIndex);
    expect(semantic!.semanticResolverIndex).toBe(semanticResolverIndex);
    expect(semantic!.semanticResolverGlobalIndex).toBe(
      validationSemanticResolverGlobalIndex(
        resolverIndex,
        semanticResolverIndex,
      ),
    );
    expect(semantic!.txHash).toHaveLength(64);
    expect(semantic!.awaitedConfirmation).toBe(true);
    expect(semantic!.nextThreadOutRef.startsWith(`${semantic!.txHash}#`)).toBe(
      true,
    );

    // THE FIT. Reference carriage is what buys it: the same transaction
    // measured 21,576 bytes when the resolver rode inline.
    const measurement = result.semanticMeasurement;
    expect(measurement).toBeDefined();
    expect(measurement!.completeSignedBytes).toBeLessThan(
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
    );
    expect(measurement!.l1ByteMargin).toBeGreaterThan(0);
    // The resolver body is not in the witness set at all -- it is read from
    // the published reference input.
    expect(measurement!.referenceInputCount).toBeGreaterThan(0);
    expect(measurement!.plutusV3ScriptCount).toBe(0);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            valueAndMintOutputDescriptorSemanticResolution: {
              entryName: publication!.entryName,
              appliedResolverBytes: publication!.appliedResolverBytes,
              publication: publication!.publicationMeasurement,
              resolution: measurement,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 900_000);
});
