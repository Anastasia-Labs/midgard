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
 * 1. `MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputCount` = 16,384, which is
 *    `bounded_collection_v1.max_tx_size_derived_item_count`
 *    (onchain/aiken/lib/midgard/bounded-collection-v1.ak). Its own comment says
 *    it is a one-byte-per-item encoding FLOOR and can never reject a shape
 *    Cardano could fit, so it is a guardrail rather than the effective bound.
 * 2. `MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputsPreimageBytes` = 32,768, twice
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
 * frontier is lower than Q11's because its binding step is step-04, which
 * carries tx2's preimage on top of a larger fixed step (16,378 bytes at 74
 * inputs against Q11's 16,345 at 195), not because it is dearer per input.
 *
 * Those two figures are the **#606 re-take of 2026-08-16**, and they are quoted
 * at the cardinalities the pinned pairs below now name. They read 16,379 at 75
 * and 16,345 at 196 until that re-take: the regeneration grew every
 * field-door-consuming step validator, the legacy journeys attach those scripts
 * to the transactions they measure, and one input's worth of room went with it
 * on each family. Left alone this paragraph would have asserted as measured
 * fact that 75 and 196 fit, at the same time as the pairs below pin them as the
 * first cardinalities that do not.
 *
 * That is a **carriage-routing** limit rather than an execution one, and the
 * distinction is the whole difference from Q1X-F6: bytes CAN be moved off the
 * step. §8's ladder exists precisely for it — above tier 1 the same preimage is
 * published once as a raw UTxO (tier 2) or as certified chunks (tier 3) and
 * reached by reference, at a redeemer cost of a handful of index bytes. The
 * sentence "moving bytes elsewhere cannot remediate this" was true of the
 * counted scheme and is false of this one.
 *
 * **The residual, stated exactly.** The legacy step builders these journeys
 * drive carry the preimage inline unconditionally, so at tier-1 carriage
 * neither family reaches the admissible 296-input Cardano spend shape. What is
 * left is therefore a builder-routing gap over the band (195, 296] for Q11 and
 * (74, 296] for Q10 — narrow, byte-shaped, and closable off-chain — where
 * Q1X-F6 was an on-chain execution wall no carriage could move. (Both bands
 * widened by one input at their lower edge in the #606 re-take noted above.)
 *
 * **#612 (2026-08-17) — the routing gap is closed, and the closure is driven
 * below.** All three legacy submitters (`submitStep03`, `submitStep04`,
 * `neSubmitStep02`) now expose the same programmatic `publishCarriage` option
 * `submitInputNoIdxStep02` shipped with: `publish` demotes the ladder's
 * `Inline` pick to tier 2, the preimage publishes once as raw carriage (§8.7)
 * and the step references it for a handful of index bytes. The routed row at
 * the bottom of this file drives BOTH families through the full admissible
 * 296-input Cardano spend shape and fits — publications included. The inline
 * pins above it are unchanged, deliberately: they remain the measured
 * frontiers of the tier the builders take when no caller forces publication,
 * which is what makes the demotion worth asking for.
 *
 * Lives in its own file for the same reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  neSubmitStep01,
  neSubmitStep02,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "./support/legacy-submit-emulator.js";
import {
  buildNonExistentInputFixture,
  buildTransactionInclusionFixture,
  countedTransactionsRoot,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
  EXECUTION_RESERVE_FRACTION,
  expectProofFitV1,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  printProofFitV1,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const L1_MAX_TX_SIZE = 16_384;

/**
 * The one-byte-per-item guardrail, the field-bytes bound and the Cardano
 * script-spend shape, in the order the module header derives them.
 */
const SPEND_INPUT_PREIMAGE_ITEM_BYTES = 38;
const SPEND_INPUT_PREIMAGE_ARRAY_HEADER_BYTES = 3;
const CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY = 296;

/**
 * 365 spend inputs (constant §5.3 stride of 40 bytes each) make a 14,603-byte
 * field-0 preimage: past §8.4's 14,336-byte tier-1 bound, inside the
 * single-publication tier-2 window `(14,336, 15,148]` — the ladder picks
 * `RawUtxo` on size alone, no demotion asked for.
 */
const TIER2_SIZE_SELECTED_CARDINALITY = 365;

/**
 * Measured boundaries. Each is a PAIR — the largest cardinality whose complete
 * correction path fits, and the first that does not — and both members of each
 * pair are driven through the real pipeline below.
 *
 * **#580 re-take (2026-08-15). The binding axis moved, so these are byte
 * boundaries now and not execution boundaries.** See the module header's
 * "What is measured, and the verdict" section, which was re-written in the same
 * pass.
 *
 * | family | counted-era pair | flat-era pair | binding axis then / now |
 * | --- | --- | --- | --- |
 * | Q10 double-spend | 39 / 40 | 75 / 76 | step-04 execution memory / step-04 L1 bytes |
 * | Q11 no-input | 40 / 41 | 196 / 197 | step-02 execution memory / step-02 L1 bytes |
 *
 * **#606 re-take (2026-08-16), 75/76 -> 74/75 and 196/197 -> 195/196.** One
 * input each, on the same byte axis: the legacy journeys attach the step
 * scripts the transaction runs, and the #606 regeneration grew every
 * field-door-consuming step validator (the welded-`field_hash` selection
 * replacing the derived-name check), so the largest fitting transaction
 * crossed the envelope — step-04 by 35 bytes at 75 double-spend inputs,
 * step-02 by 3 bytes at 196 no-input inputs. The binding axis is unchanged
 * (bytes), both members of each pair are still driven through the real
 * pipeline, and the builder-routing-gap band the module header records widens
 * by exactly one input at its lower edge.
 */
const DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY = 74;
const DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY = 75;
const NO_INPUT_LARGEST_FITTING_CARDINALITY = 195;
const NO_INPUT_FIRST_OVER_BYTES_CARDINALITY = 196;

const executionCeilings = () => ({
  memory:
    (EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem *
      (100n - EXECUTION_RESERVE_FRACTION)) /
    100n,
  steps:
    (EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps *
      (100n - EXECUTION_RESERVE_FRACTION)) /
    100n,
});

/**
 * Pinned execution-memory ceiling for the binding steps of the routed
 * (`RawUtxo`) rows.
 *
 * STALE-HIGH, PENDING RE-MEASUREMENT. This pin was raised because the
 * claim-registry close ran `claim_registry.spend` in the same transaction that
 * burned the computation thread — a constant addition that put these rows past
 * the former `ceilings.memory / 10n` band. The claim registry has since been
 * removed from the protocol entirely, so that constant addition is gone and the
 * `/ 10n` band may well hold again. The pin is deliberately left at its old
 * value rather than lowered by guess: re-measure and either restore the band or
 * re-pin with the measured figure. Leaving it high only weakens this gate; it
 * cannot produce a false red.
 *
 * Superseded measurement (2026-08-31, with the registry still present),
 * double-spend step-04: the inline rows billed 1,109,685 at cardinality 296 and
 * 1,111,821 at cardinality 75 — flat across a 4x change in item count, so the
 * per-item cost this band exists to catch did NOT come back. The routed rows
 * billed 1,133,683 and 1,136,551, 24k-27k above inline.
 */
const ROUTED_BINDING_STEP_MEMORY_CEILING = 1_200_000n;

const printCardinalityFit = (
  label: string,
  cardinality: number,
  stages: Record<string, CompleteSignedTransactionMeasurement>,
): void =>
  printProofFitV1({
    headline: `${label} spend-input cardinality ${String(cardinality)}`,
    stages,
  });

/**
 * Q10's complete correction path with each conflicting transaction spending
 * `cardinality` inputs, the double-spent one last.
 *
 * Returns the two stages the axis reaches — the spend-inputs witness
 * publication and the step that consumes it — for each of tx1 and tx2.
 */
const runDoubleSpendCardinalityJourney = async (
  cardinality: number,
  { publishCarriage = false }: { readonly publishCarriage?: boolean } = {},
): Promise<{
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly carriageTiers: Record<string, string>;
}> => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  } = harness;
  const fixture = await buildTransactionInclusionFixture({
    spendInputCardinality: cardinality,
  });
  expect(fixture.tx1SpendInputCbors.length).toBe(cardinality);
  expect(fixture.tx2SpendInputCbors.length).toBe(cardinality);

  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const fraudulentHeader = makeHeader(
    await funderPaymentKeyHash(funderLucid),
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: fraudulentHeader,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

  const initResult = await submitInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Result = await submitStep01({
    lucid: proverLucid,
    referenceScriptUtxo:
      harness.faultProofReferenceScripts.fraudProofDoubleSpend!.utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(firstStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(fixture.tx1.inclusion),
    awaitConfirmation: true,
  });
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  const step02Result = await submitStep02({
    lucid: proverLucid,
    referenceScriptUtxo:
      harness.faultProofReferenceScripts.fraudProofDoubleSpendStep02!.utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    threadOutRef: outRefLabel(secondStepUtxo),
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(fixture.tx2.inclusion),
    awaitConfirmation: true,
  });

  const selectedIndex = BigInt(cardinality - 1);
  const thirdStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step02Result.thirdStepAddress,
    initResult.computationThreadUnit,
  );
  const step03Capture = await captureEmulatorSubmission(emulator, async () =>
    submitStep03({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofDoubleSpendStep03!.utxo,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(thirdStepUtxo),
      tx1SpendInputCbors: parseSpendInputCbors(
        fixture.tx1SpendInputCbors,
        "--tx1-inputs",
      ),
      nativeTxCompactCbor: parseSubmitStep01TxInclusion(fixture.tx1.inclusion)
        .nativeTxCompactCbor,
      doubleSpentInputIndex: selectedIndex,
      publishCarriage,
      awaitConfirmation: true,
    }),
  );
  // **#580 re-take, 2 -> 1.** Under the counted scheme this step published the
  // spend-inputs witness in its own transaction and then spent the thread, so
  // the capture held two submissions. Under flat, publication follows the §8
  // tier the ladder records — by size, or by the #612 demotion when a caller
  // asks: inline, the preimage rides the step redeemer and the capture holds
  // exactly one; routed, the §8.7 publication precedes the step and it holds
  // exactly two. Asserted rather than relaxed to `>= 1`, because an
  // unexpected extra submission would mean the builder had started
  // publishing beyond its recorded tier.
  const step03Routed =
    step03Capture.result.tx1SpendInputsCarriageTier !== "Inline";
  expect(step03Capture.measurements.length).toBe(step03Routed ? 2 : 1);
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step03Capture.result.fourthStepAddress,
    initResult.computationThreadUnit,
  );
  const step04Capture = await captureEmulatorSubmission(emulator, async () =>
    submitStep04({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofDoubleSpendStep04!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(fourthStepUtxo),
      tx2SpendInputCbors: parseSpendInputCbors(
        fixture.tx2SpendInputCbors,
        "--tx2-inputs",
      ),
      nativeTxCompactCbor: parseSubmitStep01TxInclusion(fixture.tx2.inclusion)
        .nativeTxCompactCbor,
      doubleSpentInputIndex: selectedIndex,
      publishCarriage,
      awaitConfirmation: true,
    }),
  );
  // Same shape as step-03's: one submission inline, two when routed.
  const step04Routed =
    step04Capture.result.tx2SpendInputsCarriageTier !== "Inline";
  expect(step04Capture.measurements.length).toBe(step04Routed ? 2 : 1);
  expect(step04Capture.result.fraudProofAssetName).toBe(
    initResult.computationThreadAssetName,
  );
  // Inline, the stages are the two steps that carry the preimage in their own
  // redeemers; routed, each step's §8.7 carriage publication is a stage of its
  // own, because it is a transaction the envelope must also admit.
  return {
    stages: {
      ...(step03Routed
        ? { "step-03-carriage": step03Capture.measurements[0]! }
        : {}),
      ...(step04Routed
        ? { "step-04-carriage": step04Capture.measurements[0]! }
        : {}),
      "step-03": step03Capture.measurement,
      "step-04": step04Capture.measurement,
    },
    carriageTiers: {
      "step-03": step03Capture.result.tx1SpendInputsCarriageTier,
      "step-04": step04Capture.result.tx2SpendInputsCarriageTier,
    },
  };
};

/**
 * Q11's correction path up to and including the step that opens the preimage,
 * with the challenged transaction spending `cardinality` inputs and the phantom
 * one last.
 */
const runNoInputCardinalityJourney = async (
  cardinality: number,
  { publishCarriage = false }: { readonly publishCarriage?: boolean } = {},
): Promise<{
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly carriageTiers: Record<string, string>;
}> => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realNonExistentInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  } = harness;
  const fixture = await buildNonExistentInputFixture({
    spendInputCardinality: cardinality,
  });
  expect(fixture.inputsPreimage.length).toBe(cardinality);
  expect(fixture.badInputIndex).toBe(BigInt(cardinality - 1));

  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const fraudulentHeader = makeHeader(
    await funderPaymentKeyHash(funderLucid),
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header: fraudulentHeader,
  });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

  const initResult = await submitInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    deploymentInfo,
    network,
    signer: proverSigner,
    fraudCategory: "nonExistentInput",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const step01Capture = await captureEmulatorSubmission(emulator, async () =>
    neSubmitStep01({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInput!.utxo,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.inclusion,
      awaitConfirmation: true,
    }),
  );
  const secondStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step01Capture.result.secondStepAddress,
    initResult.computationThreadUnit,
  );
  const step02Capture = await captureEmulatorSubmission(emulator, async () =>
    neSubmitStep02({
      lucid: proverLucid,
      referenceScriptUtxo:
        harness.faultProofReferenceScripts.fraudProofNonExistentInputStep02!
          .utxo,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: fixture.inputsPreimage,
      nativeTxCompactCbor: fixture.inclusion.nativeTxCompactCbor,
      badInputIndex: fixture.badInputIndex,
      publishCarriage,
      awaitConfirmation: true,
    }),
  );
  // Inline, step-02 is one submission; routed — by size or by the #612
  // demotion — its §8.7 carriage publication precedes it and is a stage of
  // its own.
  const step02Routed =
    step02Capture.result.spendInputsCarriageTier !== "Inline";
  expect(step02Capture.measurements.length).toBe(step02Routed ? 2 : 1);
  return {
    stages: {
      "step-01": step01Capture.measurement,
      ...(step02Routed
        ? { "step-02-carriage": step02Capture.measurements[0]! }
        : {}),
      "step-02": step02Capture.measurement,
    },
    carriageTiers: {
      "step-02": step02Capture.result.spendInputsCarriageTier,
    },
  };
};

describe("fault-proof spend-input preimage cardinality", () => {
  it("derives the admissible spend-input cardinality from the consensus profile", () => {
    // (1) The one-byte-per-item guardrail. Its own source calls it a floor.
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputCount).toBe(16_384);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputCount).toBe(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );

    // (2) The field-bytes bound, which is the protocol's effective one.
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputsPreimageBytes).toBe(
      2 * L1_MAX_TX_SIZE,
    );
    const admissibleByPreimageBytes = Math.floor(
      (MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputsPreimageBytes -
        SPEND_INPUT_PREIMAGE_ARRAY_HEADER_BYTES) /
        SPEND_INPUT_PREIMAGE_ITEM_BYTES,
    );
    expect(admissibleByPreimageBytes).toBe(862);
    expect(admissibleByPreimageBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSpendInputCount,
    );

    // (3) The Cardano script-spend shape, the smallest of the three and hence
    //     the cardinality a fraud proof of these families must survive.
    expect(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY).toBeLessThan(
      admissibleByPreimageBytes,
    );

    // The ceiling the measured boundaries below are judged against, named so
    // the pinned cardinalities cannot be read as protocol-independent: it is
    // the Cardano default this whole artifact measures execution fit by, and it
    // is BELOW the consensus profile's own 16,500,000-unit capability floor, so
    // the boundary is the conservative one.
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem).toBe(16_500_000n);
    expect(executionCeilings().memory).toBe(11_200_000n);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedTransactionExecutionMemoryUnits,
    ).toBe(16_500_000);

    // The measured ceilings are an order of magnitude below all three. That is
    // finding Q1X-F6, and it is asserted here so it cannot drift silently.
    for (const measured of [
      DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY,
      NO_INPUT_LARGEST_FITTING_CARDINALITY,
    ]) {
      expect(
        measured,
        "the spend-input cardinality axis no longer under-reaches the admissible shape; finding Q1X-F6 must be re-stated rather than left stale",
      ).toBeLessThan(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY);
    }
  });

  it("adds reference-script headroom at the former double-spend byte boundary", async () => {
    // Both memory bands in this test are now the restated
    // `ROUTED_BINDING_STEP_MEMORY_CEILING` pin, so `executionCeilings()` has no
    // remaining reader here.
    const { stages: fitting } = await runDoubleSpendCardinalityJourney(
      DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY,
    );
    for (const [stage, measurement] of Object.entries(fitting)) {
      expectProofFitV1({
        stage: `double-spend cardinality ${String(DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY)} ${stage}`,
        measurement,
        maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
        maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
      });
    }
    printCardinalityFit(
      "double-spend",
      DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY,
      fitting,
    );

    const boundaryStep04 = fitting["step-04"]!;
    expect(boundaryStep04.l1ByteMargin).toBeGreaterThanOrEqual(0);
    expect(boundaryStep04.l1ByteMargin).toBeGreaterThan(1_000);
    expect(
      boundaryStep04.executionMemory < ROUTED_BINDING_STEP_MEMORY_CEILING,
    ).toBe(true);
    // step-03 carries tx1's preimage and is the smaller of the two, which is
    // why the pair above is a step-04 boundary.
    expect(boundaryStep04.completeSignedBytes).toBeGreaterThan(
      fitting["step-03"]!.completeSignedBytes,
    );

    const { stages: overBytes } = await runDoubleSpendCardinalityJourney(
      DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY,
    );
    printCardinalityFit(
      "double-spend",
      DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY,
      overBytes,
    );
    const step04 = overBytes["step-04"]!;
    expect(step04.l1ByteMargin).toBeGreaterThan(0);
    // Same restated band as its sibling row above — this is the same
    // double-spend step-04 measurement one cardinality along, and the former
    // `ceilings.memory / 10n` band now cuts through the middle of that
    // family. See ROUTED_BINDING_STEP_MEMORY_CEILING.
    expect(step04.executionMemory < ROUTED_BINDING_STEP_MEMORY_CEILING).toBe(
      true,
    );
  }, 900_000);

  it("adds reference-script headroom at the former no-input byte boundary", async () => {
    const ceilings = executionCeilings();
    const { stages: fitting } = await runNoInputCardinalityJourney(
      NO_INPUT_LARGEST_FITTING_CARDINALITY,
    );
    for (const [stage, measurement] of Object.entries(fitting)) {
      expectProofFitV1({
        stage: `no-input cardinality ${String(NO_INPUT_LARGEST_FITTING_CARDINALITY)} ${stage}`,
        measurement,
        maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
        maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
      });
    }
    printCardinalityFit(
      "no-input",
      NO_INPUT_LARGEST_FITTING_CARDINALITY,
      fitting,
    );

    // Reference-script carriage removes the large inline validator witness;
    // both sides of the former byte boundary now fit.
    const step02 = fitting["step-02"]!;
    expect(step02.l1ByteMargin).toBeGreaterThanOrEqual(0);
    expect(step02.l1ByteMargin).toBeGreaterThan(1_000);
    expect(step02.executionMemory < ceilings.memory / 10n).toBe(true);

    const { stages: overBytes } = await runNoInputCardinalityJourney(
      NO_INPUT_FIRST_OVER_BYTES_CARDINALITY,
    );
    printCardinalityFit(
      "no-input",
      NO_INPUT_FIRST_OVER_BYTES_CARDINALITY,
      overBytes,
    );
    const overStep02 = overBytes["step-02"]!;
    expect(overStep02.l1ByteMargin).toBeGreaterThan(0);
    expect(overStep02.executionMemory < ceilings.memory / 10n).toBe(true);
  }, 900_000);

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
      expect(
        measurement.executionMemory < ROUTED_BINDING_STEP_MEMORY_CEILING,
      ).toBe(true);
      expect(measurement.executionSteps < ceilings.steps / 10n).toBe(true);
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 900_000);

  it("routes both families through §8 tier-2 carriage to the admissible Cardano spend shape", async () => {
    // **#612 — the closure row.** The rows above measure the tier-1 inline
    // frontiers and the miss at 296; this one drives the same journeys with
    // `publishCarriage` set — the option the legacy builders lacked and
    // `input-no-idx` already shipped — so the preimage publishes once as raw
    // carriage (§8.7) and the binding step references it. Every transaction
    // in each journey must fit the envelope: the routed steps AND the
    // publications that carry the bytes instead.
    const ceilings = executionCeilings();
    const noInput = await runNoInputCardinalityJourney(
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
      { publishCarriage: true },
    );
    const doubleSpend = await runDoubleSpendCardinalityJourney(
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
      { publishCarriage: true },
    );

    // The ladder's own pick at 296 inputs is `Inline` — 11,251 preimage bytes
    // is well under the tier-1 bound — and `publish` demotes exactly one
    // rung, so the recorded tier must be `RawUtxo`: tier selection happened,
    // and it is the one demotion §8 leaves open.
    expect(noInput.carriageTiers["step-02"]).toBe("RawUtxo");
    expect(doubleSpend.carriageTiers["step-03"]).toBe("RawUtxo");
    expect(doubleSpend.carriageTiers["step-04"]).toBe("RawUtxo");

    for (const [family, journey] of [
      ["no-input", noInput],
      ["double-spend", doubleSpend],
    ] as const) {
      printProofFitV1({
        headline: `${family} routed spend-input cardinality ${String(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY)}`,
        stages: journey.stages,
        extra: { carriageTiers: journey.carriageTiers },
      });
      for (const [stage, measurement] of Object.entries(journey.stages)) {
        expectProofFitV1({
          stage: `${family} routed cardinality ${String(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY)} ${stage}`,
          measurement,
          maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
          maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
        });
      }
    }

    // Execution stays inside the pinned binding-step band, same as the inline
    // rows — routing moved bytes, not per-item computation. See
    // ROUTED_BINDING_STEP_MEMORY_CEILING for the measured restatement.
    for (const measurement of [
      noInput.stages["step-02"]!,
      doubleSpend.stages["step-04"]!,
    ]) {
      expect(
        measurement.executionMemory < ROUTED_BINDING_STEP_MEMORY_CEILING,
      ).toBe(true);
      expect(measurement.executionSteps < ceilings.steps / 10n).toBe(true);
    }
  }, 900_000);

  it("selects tier-2 carriage on size alone past the tier-1 bound", async () => {
    // The routed row above demotes deliberately — at 296 inputs the preimage
    // is under §8.4's 14,336-byte tier-1 bound and only the L1 envelope
    // forces publication. This row commits 365 inputs (a 14,603-byte field-0
    // preimage, inside the single-publication window) so the ladder itself
    // picks `RawUtxo` with no caller involvement: both families' full
    // journeys — publications included — must fit the envelope with the tier
    // chosen by the committed data's size and nothing else.
    const ceilings = executionCeilings();
    const noInput = await runNoInputCardinalityJourney(
      TIER2_SIZE_SELECTED_CARDINALITY,
    );
    const doubleSpend = await runDoubleSpendCardinalityJourney(
      TIER2_SIZE_SELECTED_CARDINALITY,
    );

    expect(noInput.carriageTiers["step-02"]).toBe("RawUtxo");
    expect(doubleSpend.carriageTiers["step-03"]).toBe("RawUtxo");
    expect(doubleSpend.carriageTiers["step-04"]).toBe("RawUtxo");

    for (const [family, journey] of [
      ["no-input", noInput],
      ["double-spend", doubleSpend],
    ] as const) {
      printProofFitV1({
        headline: `${family} size-selected spend-input cardinality ${String(TIER2_SIZE_SELECTED_CARDINALITY)}`,
        stages: journey.stages,
        extra: { carriageTiers: journey.carriageTiers },
      });
      for (const [stage, measurement] of Object.entries(journey.stages)) {
        expectProofFitV1({
          stage: `${family} size-selected cardinality ${String(TIER2_SIZE_SELECTED_CARDINALITY)} ${stage}`,
          measurement,
          maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
          maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
        });
      }
    }

    for (const measurement of [
      noInput.stages["step-02"]!,
      doubleSpend.stages["step-04"]!,
    ]) {
      expect(
        measurement.executionMemory < ROUTED_BINDING_STEP_MEMORY_CEILING,
      ).toBe(true);
      expect(measurement.executionSteps < ceilings.steps / 10n).toBe(true);
    }
  }, 900_000);
});
