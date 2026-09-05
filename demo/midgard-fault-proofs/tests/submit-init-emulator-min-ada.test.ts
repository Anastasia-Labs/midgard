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

// A committed Accepted claim over an underfunded output is resolved through
// the exact ValueAndMint descriptor branch. Both the authenticated reference
// publication and the complete signed resolution must fit real L1 limits.
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

    // Publication is part of the release gate, including the 512-byte reserve.
    const publication = result.valueAndMintSemanticReferencePublication;
    expect(publication).toBeDefined();
    expect(publication!.entryName).toBe(
      "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
    );
    expect(publication!.appliedResolverBytes).toBeLessThan(
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
    );
    expect(
      publication!.publicationMeasurement.l1ByteMargin,
    ).toBeGreaterThanOrEqual(512);
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

    // The complete signed semantic transaction also fits the ledger envelope.
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
