/**
 * The second adversarial axis of GOAL_SPEC.md 9.1 output 5: spend-input
 * preimage cardinality, measured end to end for Q10 and Q11 (issue #549,
 * finding Q1X-F6).
 *
 * ## What this axis is
 *
 * The membership-depth axis (Q1X-F5, remediated by published-chunk carriage at
 * issue #545) is about how deep a proof the challenged BLOCK can force. This
 * one is about how many inputs the challenged TRANSACTION spends. Two of the
 * four foundational families carry it, and they carry it differently:
 *
 * - Q10 (double-spend) opens tx1's authenticated spend-inputs collection at
 *   step-03 and tx2's at step-04. The preimage reaches each step as a REFERENCE
 *   INPUT published beforehand, so it is not part of the step transaction's own
 *   bytes.
 * - Q11 (no-input) opens the challenged transaction's collection at step-02,
 *   and the preimage is carried in the STEP REDEEMER
 *   (`midgard/fraud_proofs/no_input/step_02.Args.inputs_preimage`), so for this
 *   family it IS part of the step transaction's bytes.
 *
 * Q12 and Q14 do not carry the axis at all: the invalid-range steps operate on
 * the compact transaction, whose fields are hashes, and the zero-input family's
 * challenged transaction spends nothing by construction.
 *
 * ## Why the admissible bound is what it is
 *
 * Three protocol constraints bound how many inputs an admissible challenged
 * transaction may spend, and the smallest of them is the one a fraud proof must
 * survive. All three are read out of their source here rather than asserted:
 *
 * 1. `MIDGARD_CONSENSUS_LIMITS.maxSpendInputCount` = 16,384, which is
 *    `bounded_collection_v1.max_tx_size_derived_item_count`
 *    (onchain/aiken/lib/midgard/bounded-collection-v1.ak). Its own comment says
 *    it is a one-byte-per-item encoding FLOOR and can never reject a shape
 *    Cardano could fit, so it is a guardrail rather than the effective bound.
 * 2. `MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes` = 32,768, twice
 *    the preserved L1 envelope. A canonical `TransactionInput` costs 38 bytes
 *    in the preimage (a two-byte definite-bytes header over 36 bytes of
 *    canonical `TxOutRef` CBOR), so this field bound admits 862 inputs.
 * 3. The Cardano script-spend shape boundary already pinned by this repository:
 *    296 inputs, `maximum_cardano_spend_redeemer_count` in
 *    `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`, which
 *    `validators/fraud-proofs/input-no-idx/step-02.ak` names as the shape a
 *    proof of family Q13 must handle.
 *
 * 296 is therefore the smallest cardinality any of these three admits, and the
 * one this file measures against.
 *
 * ## What is measured, and the verdict
 *
 * The complete correction path is driven through the real prepare/submit
 * pipeline at the largest cardinality that fits and at the first that does not,
 * for both families.
 *
 * ## Finding Q1X-F6 is RESOLVED under the flat reversion (#580, 2026-08-15)
 *
 * The defect this file was written to record read:
 *
 * > Execution MEMORY binds, and it binds an order of magnitude below the
 * > admissible cardinality: re-hashing the authenticated bounded collection
 * > costs a measured ~276,000 memory units per input, so the reserve runs out
 * > in the high thirties. Moving bytes elsewhere cannot remediate this the way
 * > published-chunk carriage remediated Q1X-F5: the cost is the step's own
 * > re-hashing of a collection it must reproduce in full before it may select
 * > one item from it.
 *
 * **The mechanism it names no longer exists.** Under the flat commitment there
 * is no bounded collection to reproduce: the step authenticates the whole field
 * preimage once against its flat hash and reaches an item by arithmetic offset
 * and slice. Measured across the whole admissible range, 40 through 296 inputs,
 * execution memory is **constant in cardinality**:
 *
 * | family | step | mem at N=40 | mem at N=296 | share of the 13.2M basis |
 * | --- | --- | --- | --- | --- |
 * | Q11 no-input | step-02 | 494,909 | 498,121 | 3.8% |
 * | Q10 double-spend | step-04 | 619,787 | 622,999 | 4.7% |
 *
 * The measured per-input memory cost is ~0 where it was ~276,000. Neither
 * family comes within an order of magnitude of the reserve at any admissible
 * cardinality, so the boundary pairs below are no longer execution boundaries
 * at all.
 *
 * ## What binds now, and why it is a different kind of limit
 *
 * **L1 transaction bytes, at tier-1 carriage only.** Both families carry the
 * spend-input field preimage in the step redeemer (§8.3 tier 1), 38 bytes per
 * item plus the §5.1 envelope, and that is what fills the envelope: measured
 * ~41.2 complete-signed bytes per input on both families' binding step. Q10's
 * inline frontier is lower than Q11's because its binding step is step-04,
 * which carries tx2's preimage on top of a larger fixed step, not because it
 * is dearer per input.
 *
 * That is a **carriage-routing** limit rather than an execution one, and the
 * distinction is the whole difference from Q1X-F6: bytes CAN be moved off the
 * step. §8's ladder exists precisely for it — above tier 1 the same preimage is
 * published once as a raw UTxO (tier 2) or as certified chunks (tier 3) and
 * reached by reference, at a redeemer cost of a handful of index bytes. The
 * sentence "moving bytes elsewhere cannot remediate this" was true of the
 * counted scheme and is false of this one.
 *
 * **No inline frontier cardinality is pinned here, deliberately.** Two such
 * pairs used to be (74/75 for Q10, 195/196 for Q11), and #580 and #606
 * re-measured them twice: any validator regeneration that grows a step by a
 * few bytes moves them, with no contract violated. By the time
 * reference-script carriage landed, both members of each "boundary" pair fit,
 * so the pairs asserted nothing about a boundary at all. What they were
 * really guarding — a return of a per-item execution cost on the binding
 * steps — is now measured directly, as a per-input memory spread across two
 * cardinalities on the same route, in the closing row of this file.
 *
 * **#612 (2026-08-17) — the routing gap is closed, and the closure is driven
 * below.** All three legacy submitters (`submitStep03`, `submitStep04`,
 * `neSubmitStep02`) now expose the same programmatic `publishCarriage` option
 * `submitInputNoIdxStep02` shipped with: `publish` demotes the ladder's
 * `Inline` pick to tier 2, the preimage publishes once as raw carriage (§8.7)
 * and the step references it for a handful of index bytes. The routed row at
 * the bottom of this file drives BOTH families through the full admissible
 * 296-input Cardano spend shape and fits — publications included — and does
 * the same at the 365-input shape the ladder routes on size alone.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/consensus-profile";
import "vitest";
import "./support/legacy-submit-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./submit-init-emulator-spend-input-cardinality.run-double-spend-cardinality-journey.js";
import "./submit-init-emulator-spend-input-cardinality.run-no-input-cardinality-journey.js";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import {
  ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES,
  bindingStepMemoryBand,
  CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
  executionCeilings,
  L1_MAX_TX_SIZE,
  maxBindingStepMemoryPerInput,
  printCardinalityFit,
  runDoubleSpendCardinalityJourney,
  TIER2_SIZE_SELECTED_CARDINALITY,
} from "./submit-init-emulator-spend-input-cardinality.run-double-spend-cardinality-journey.js";
import { runNoInputCardinalityJourney } from "./submit-init-emulator-spend-input-cardinality.run-no-input-cardinality-journey.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectProofFit,
  printProofFit,
} from "./support/submit-init-emulator-shared.js";

describe("fault-proof spend-input preimage cardinality", () => {
  it("derives the admissible spend-input cardinality from the consensus profile", () => {
    // (1) The one-byte-per-item guardrail. Its own source calls it a floor.
    expect(MIDGARD_CONSENSUS_LIMITS.maxSpendInputCount).toBe(16_384);
    expect(MIDGARD_CONSENSUS_LIMITS.maxSpendInputCount).toBe(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
    );

    // (2) The field-bytes bound, which is the protocol's effective one.
    expect(MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes).toBe(
      2 * L1_MAX_TX_SIZE,
    );
    const admissibleByPreimageBytes = ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES;
    expect(admissibleByPreimageBytes).toBe(862);
    expect(admissibleByPreimageBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS.maxSpendInputCount,
    );

    // (3) The Cardano script-spend shape, the smallest of the three and hence
    //     the cardinality a fraud proof of these families must survive.
    expect(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY).toBeLessThan(
      admissibleByPreimageBytes,
    );

    // The ceiling the measured boundaries below are judged against, named so
    // the pinned cardinalities cannot be read as protocol-independent: it is
    // the emulator's 16,500,000-unit budget less the shared 20% execution
    // reserve, so it sits BELOW the consensus profile's own 16,500,000-unit
    // capability floor and the boundary is the conservative one.
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem).toBe(16_500_000n);
    expect(executionCeilings().memory).toBe(13_200_000n);
    expect(
      MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionMemoryUnits,
    ).toBe(16_500_000);
  });

  it("reaches the admissible Cardano spend shape on execution and bytes", async () => {
    // **#580 re-take, and the row that carries the Q1X-F6 verdict.**
    //
    // This test used to assert that both journeys REJECT with `/over budget/`
    // at the admissible cardinality — that the step could not be evaluated at
    // all because it exceeded the ledger's own execution-memory cap. Under flat
    // both journeys build and evaluate: nothing is over budget, and the only
    // thing wrong with either step at 296 inputs is its size.
    //
    // Both halves are asserted, because the finding is the pair. Q1X-F6 is
    // resolved on the axis it named; reference-script carriage also removes
    // the former byte gap at this admissible shape.
    const { stages: noInput } = await runNoInputCardinalityJourney(
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
    );
    printCardinalityFit(
      "no-input",
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
      noInput,
    );
    const { stages: doubleSpend } = await runDoubleSpendCardinalityJourney(
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
    );
    printCardinalityFit(
      "double-spend",
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
      doubleSpend,
    );

    const ceilings = executionCeilings();
    const binding = [noInput["step-02"]!, doubleSpend["step-04"]!];
    for (const measurement of binding) {
      expect(measurement.executionMemory).toBeLessThan(bindingStepMemoryBand());
      expect(measurement.executionSteps).toBeLessThan(ceilings.steps / 10n);
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 900_000);

  it("routes both families through §8 tier-2 carriage at both tier-2 cardinalities, at a memory cost flat in cardinality", async () => {
    // **#612 — the closure row, and the Q1X-F6 verdict's own evidence.**
    //
    // Four journeys, two cardinalities, both families:
    //
    //   296 inputs, `publishCarriage` set — the ladder's own pick here is
    //     `Inline` (11,251 preimage bytes, well under §8.4's 14,336-byte
    //     tier-1 bound) and `publish` demotes exactly one rung, so `RawUtxo`
    //     is the recorded tier: the demotion the legacy builders lacked.
    //   365 inputs, no routing input at all — a 14,603-byte field-0 preimage
    //     sits past the tier-1 bound and inside the single-publication
    //     window, so the ladder picks `RawUtxo` on size alone.
    //
    // Every transaction of every journey — the routed steps AND the
    // publications that carry the bytes instead — must fit the envelope. And
    // because the same binding step is measured at two cardinalities on the
    // same route, the per-item execution cost is measured rather than
    // asserted: this is where a return of the Q1X-F6 wall (~276,000 memory
    // units an input under the counted scheme) would show up.
    const ceilings = executionCeilings();
    const runs = {
      "no-input routed 296": {
        binding: "step-02",
        cardinality: CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
        journey: await runNoInputCardinalityJourney(
          CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
          { publishCarriage: true },
        ),
      },
      "double-spend routed 296": {
        binding: "step-04",
        cardinality: CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
        journey: await runDoubleSpendCardinalityJourney(
          CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
          { publishCarriage: true },
        ),
      },
      "no-input size-selected 365": {
        binding: "step-02",
        cardinality: TIER2_SIZE_SELECTED_CARDINALITY,
        journey: await runNoInputCardinalityJourney(
          TIER2_SIZE_SELECTED_CARDINALITY,
        ),
      },
      "double-spend size-selected 365": {
        binding: "step-04",
        cardinality: TIER2_SIZE_SELECTED_CARDINALITY,
        journey: await runDoubleSpendCardinalityJourney(
          TIER2_SIZE_SELECTED_CARDINALITY,
        ),
      },
    } as const;

    for (const [label, { journey, binding }] of Object.entries(runs)) {
      // Tier selection actually happened, on both of the two ways §8 reaches
      // tier 2, and every step that consumes the preimage records it.
      expect(journey.carriageTiers[binding], label).toBe("RawUtxo");
      printProofFit({
        headline: `${label} spend-input carriage`,
        stages: journey.stages,
        extra: { carriageTiers: journey.carriageTiers },
      });
      for (const [stage, measurement] of Object.entries(journey.stages)) {
        expectProofFit({
          stage: `${label} ${stage}`,
          measurement,
          maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
          maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
        });
      }
      const bindingStep = journey.stages[binding];
      if (bindingStep === undefined) {
        throw new Error(`${label} captured no ${binding} measurement`);
      }
      expect(bindingStep.executionMemory, label).toBeLessThan(
        bindingStepMemoryBand(),
      );
      expect(bindingStep.executionSteps, label).toBeLessThan(
        ceilings.steps / 10n,
      );
    }
    // The double-spend family's intermediate step carries tx1's preimage and
    // must be routed too, or the bytes only moved off one of the two.
    expect(
      runs["double-spend routed 296"].journey.carriageTiers["step-03"],
    ).toBe("RawUtxo");
    expect(
      runs["double-spend size-selected 365"].journey.carriageTiers["step-03"],
    ).toBe("RawUtxo");

    // The flatness property, measured across a 23% change in item count on
    // the same route. `Math.abs` on the difference so a cost that shrank with
    // cardinality — which would be just as much a per-item dependence — is
    // caught too.
    for (const [family, at296, at365] of [
      [
        "no-input",
        runs["no-input routed 296"],
        runs["no-input size-selected 365"],
      ],
      [
        "double-spend",
        runs["double-spend routed 296"],
        runs["double-spend size-selected 365"],
      ],
    ] as const) {
      const memoryAt296 = at296.journey.stages[at296.binding]!.executionMemory;
      const memoryAt365 = at365.journey.stages[at365.binding]!.executionMemory;
      const spread =
        memoryAt365 > memoryAt296
          ? memoryAt365 - memoryAt296
          : memoryAt296 - memoryAt365;
      const inputsApart = BigInt(at365.cardinality - at296.cardinality);
      expect(
        spread / inputsApart,
        `${family} binding-step memory per input between ${String(at296.cardinality)} and ${String(at365.cardinality)} inputs (${memoryAt296.toString()} -> ${memoryAt365.toString()})`,
      ).toBeLessThanOrEqual(maxBindingStepMemoryPerInput());
    }
  }, 1_800_000);
});
