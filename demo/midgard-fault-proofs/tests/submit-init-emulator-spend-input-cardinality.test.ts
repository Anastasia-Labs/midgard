/**
 * The second adversarial axis of family output 5
 * (`docs/spec/protocol-decisions.md#family-outputs`): spend-input
 * preimage cardinality, measured end to end for the double-spend and no-input
 * families.
 *
 * ## What this axis is
 *
 * The membership-depth axis (covered by published-chunk carriage, issue #545)
 * is about how deep a proof the challenged BLOCK can force. This
 * one is about how many inputs the challenged TRANSACTION spends. Two of the
 * four foundational families carry it, and they carry it differently:
 *
 * - double-spend opens tx1's authenticated spend-inputs collection at
 *   step-03 and tx2's at step-04. The preimage reaches each step as a REFERENCE
 *   INPUT published beforehand, so it is not part of the step transaction's own
 *   bytes.
 * - no-input opens the challenged transaction's collection at step-02,
 *   and the preimage is carried in the STEP REDEEMER
 *   (`midgard/fraud_proofs/no_input/step_02.Args.inputs_preimage`), so for this
 *   family it IS part of the step transaction's bytes.
 *
 * The invalid-range and zero-input families do not carry the axis at all: the
 * invalid-range steps operate on the compact transaction, whose fields are hashes, and the zero-input family's
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
 *    input-no-idx proof must handle.
 *
 * 296 is therefore the smallest cardinality any of these three admits, and the
 * one this file measures against.
 *
 * ## What is measured
 *
 * The complete correction path is driven through the real prepare/submit
 * pipeline for both families. Under the flat field commitment the step
 * authenticates the whole field preimage once against its flat hash and
 * reaches an item by arithmetic offset and slice, so execution memory is
 * constant in cardinality across the admissible range, 40 through 296 inputs:
 *
 * | family | step | mem at N=40 | mem at N=296 | share of the 13.2M basis |
 * | --- | --- | --- | --- | --- |
 * | no-input | step-02 | 494,909 | 498,121 | 3.8% |
 * | double-spend | step-04 | 619,787 | 622,999 | 4.7% |
 *
 * ## What binds
 *
 * **L1 transaction bytes, at tier-1 carriage only.** Both families carry the
 * spend-input field preimage in the step redeemer (§8.3 tier 1), 38 bytes per
 * item plus the §5.1 envelope, and that is what fills the envelope: measured
 * ~41.2 complete-signed bytes per input on both families' binding step. The
 * double-spend inline frontier is lower than no-input's because its binding
 * step is step-04, which carries tx2's preimage on top of a larger fixed step,
 * not because it is dearer per input.
 *
 * That is a **carriage-routing** limit rather than an execution one: §8's
 * ladder moves the bytes off the step. Above tier 1 the same preimage is
 * published once as a raw UTxO (tier 2) or as certified chunks (tier 3) and
 * reached by reference, at a redeemer cost of a handful of index bytes.
 *
 * **No inline frontier cardinality is pinned here, deliberately.** Any
 * validator regeneration that grows a step by a few bytes moves it, with no
 * contract violated. A per-item execution cost on the binding steps is
 * measured directly instead, as a per-input memory spread across two
 * cardinalities on the same route, in the closing row of this file.
 *
 * The legacy submitters (`buildStep03`, `buildStep04`, `neSubmitStep02`)
 * expose the programmatic `publishCarriage` option `buildInputNoIdxStep02`
 * has: `publish` demotes the ladder's `Inline` pick to tier 2, the preimage
 * publishes once as raw carriage (§8.7) and the step references it for a
 * handful of index bytes. The routed row at the bottom of this file drives
 * BOTH families through the full admissible 296-input Cardano spend shape and
 * fits, publications included, and does the same at the 365-input shape the
 * ladder routes on size alone.
 *
 * Lives in its own file so it runs in its own fresh process.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/consensus-profile";
import "vitest";
import "../src/submit-init.submit-init.js";
import "../src/double-spend/submit-step-01.js";
import "../src/double-spend/submit-step-02.js";
import "../src/double-spend/submit-step-03.js";
import "../src/double-spend/submit-step-04.submit-step04.js";
import "../src/step-support.js";
import "../src/non-existent-input/submit-step-01.js";
import "../src/non-existent-input/submit-step-02.js";
import "./support/submit-init/inclusion.js";
import "./support/submit-init/transaction-material.js";
import "./support/submit-init/non-existent-input.js";
import "./support/emulator/protocol-parameters.js";
import "./support/emulator/proof-fit.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import "./support/emulator/measurement.js";
import "./support/emulator/harness.js";
import "./support/emulator/header-fixtures.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/setup-tx.submit-setup-tx.js";
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
import { expectProofFit, printProofFit } from "./support/emulator/proof-fit.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

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
    // At the admissible cardinality both journeys build and evaluate: the
    // binding step stays inside the execution band and, with reference-script
    // carriage, inside the L1 byte envelope. Both halves are asserted.
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
    // Four journeys, two cardinalities, both families:
    //
    //   296 inputs, `publishCarriage` set — the ladder's own pick here is
    //     `Inline` (11,251 preimage bytes, well under §8.4's 14,336-byte
    //     tier-1 bound) and `publish` demotes exactly one rung, so `RawUtxo`
    //     is the recorded tier.
    //   365 inputs, no routing input at all — a 14,603-byte field-0 preimage
    //     sits past the tier-1 bound and inside the single-publication
    //     window, so the ladder picks `RawUtxo` on size alone.
    //
    // Every transaction of every journey — the routed steps AND the
    // publications that carry the bytes instead — must fit the envelope. And
    // because the same binding step is measured at two cardinalities on the
    // same route, the per-item execution cost is measured rather than
    // asserted: this is where a per-item cost (~276,000 memory units an input
    // for re-hashing a counted collection) would show up.
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
