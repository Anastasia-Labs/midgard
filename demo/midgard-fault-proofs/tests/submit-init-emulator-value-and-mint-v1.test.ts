import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "@al-ft/midgard-sdk";
import { PROTOCOL_PARAMETERS_DEFAULT, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  validationResolverIndexV1,
  validationSemanticResolverGlobalIndexV1,
} from "../src/index.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// R5 item 1 (#617): the cek and ValueAndMint resolver indices are prepare +
// semantic decompositions like the other twelve. The journey below disputes
// one honest step of the ValueAndMint phase end to end on the emulator — open,
// source, bisection, enter-resolution, prepare-resolution, the phase's
// `prepare_selected` validator, the kind's semantic resolver, the award and
// the block removal — with every transaction under the literal 16,384-byte L1
// envelope the harness pins. The operator's trace is honest up to the
// disputed step and forged from its successor on, so the challenger (whose
// trace is the honest one) proves the step and wins.
//
// Split out of submit-init-emulator-cek-value-and-mint-v1.test.ts, which keeps
// the cek half: one emulator journey per file, on purpose. Each journey leaks
// wasm linear memory that `@lucid-evolution/uplc` never reclaims, and vitest
// isolates per FILE, so co-locating journeys walks the worker into the wasm32
// ceiling and surfaces it as `EvaluatorError: unreachable`. See
// tests/support/uplc-heap-guard.ts.
describe("validation-dispute journeys through the cek and ValueAndMint decompositions", () => {
  it.each([
    {
      disputedPhase: "valueAndMint" as const,
      resolverName: "ValueAndMint" as const,
      // Stage 0, the replay-schedule seed (`value_and_mint_begin_semantic_v1`).
      semanticResolverIndex: 0,
      semanticModule: "value_and_mint_begin_semantic_v1",
    },
  ])(
    "proves the honest $semanticModule step and removes the forged block",
    async ({
      disputedPhase,
      resolverName,
      semanticResolverIndex,
      semanticModule,
    }) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase,
          }),
      );
      const { fixture, lowIndex, highIndex } = result;
      const resolverIndex = validationResolverIndexV1(resolverName);

      // The bisection landed on exactly the forged boundary: the honest
      // pre-state is the first state of the disputed phase and the operator's
      // successor is the forgery.
      expect(highIndex).toBe(lowIndex + 1);
      expect(fixture.challengerTrace.states[lowIndex]!.phase).toBe(
        disputedPhase,
      );
      expect(fixture.operatorTrace.states[highIndex]!.phase).toBe("terminal");
      expect(
        fixture.operatorTrace.states[highIndex]!.workRoot.equals(
          fixture.challengerTrace.states[highIndex]!.workRoot,
        ),
      ).toBe(false);
      expect(fixture.operatorTrace.tree.descriptor.verdict).toBe("accepted");
      expect(fixture.challengerTrace.tree.descriptor.verdict).toBe("accepted");
      expect(fixture.evidence.moves.length).toBeGreaterThan(0);

      // The proved step rode the decomposition: the phase's prepare resolver
      // and the kind's semantic resolver, not a direct resolver.
      expect(fixture.evidence.oneStepArgument.resolverIndex).toBe(
        resolverIndex,
      );
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
      ).toBe(`fraud_proofs/validation_trace/${semanticModule}.main.spend`);

      // The challenger reached the award and removed the operator's block.
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.awardResult?.fraudProofUnit).toBe(
        toUnit(
          result.contracts.fraudProof.policyId,
          result.initResult.computationThreadAssetName,
        ),
      );
      await expect(
        result.challengerLucid!.utxosAtWithUnit(
          result.contracts.fraudProof.spendingScriptAddress,
          result.awardResult!.fraudProofUnit,
        ),
      ).resolves.toHaveLength(1);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
      const removalMeasurements = result.removalMeasurements ?? [];
      expect(removalMeasurements.length).toBeGreaterThan(0);
      for (const measurement of removalMeasurements) {
        expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
          PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
        );
        expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      }
    },
    900_000,
  );
});
