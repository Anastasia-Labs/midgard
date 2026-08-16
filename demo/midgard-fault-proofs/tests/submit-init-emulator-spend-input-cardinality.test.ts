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
 * carries tx2's preimage on top of a larger fixed step (16,379 bytes at 75
 * inputs against Q11's 16,345 at 196), not because it is dearer per input.
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
 * left is therefore a builder-routing gap over the band (196, 296] for Q11 and
 * (75, 296] for Q10 — narrow, byte-shaped, and closable off-chain — where
 * Q1X-F6 was an on-chain execution wall no carriage could move.
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
): Promise<Record<string, CompleteSignedTransactionMeasurement>> => {
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  } = await makeFaultProofEmulatorHarnessV1({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
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
      awaitConfirmation: true,
    }),
  );
  // **#580 re-take, 2 -> 1.** Under the counted scheme this step published the
  // spend-inputs witness in its own transaction and then spent the thread, so
  // the capture held two submissions. Under flat there is nothing to publish:
  // the preimage rides the step redeemer, so the capture holds exactly one.
  // Asserted rather than relaxed to `>= 1`, because a second submission
  // reappearing here would mean the builder had started publishing again.
  expect(step03Capture.measurements.length).toBe(1);
  const fourthStepUtxo = await expectSingleUtxoWithUnit(
    proverLucid,
    step03Capture.result.fourthStepAddress,
    initResult.computationThreadUnit,
  );
  const step04Capture = await captureEmulatorSubmission(emulator, async () =>
    submitStep04({
      lucid: proverLucid,
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
      awaitConfirmation: true,
    }),
  );
  // Same re-take as step-03's: one submission, not two.
  expect(step04Capture.measurements.length).toBe(1);
  expect(step04Capture.result.fraudProofAssetName).toBe(
    initResult.computationThreadAssetName,
  );
  // The two witness-publication stages this journey used to return are gone
  // with the publications themselves; the stages that remain are the two steps
  // that carry the preimage in their own redeemers.
  return {
    "step-03": step03Capture.measurement,
    "step-04": step04Capture.measurement,
  };
};

/**
 * Q11's correction path up to and including the step that opens the preimage,
 * with the challenged transaction spending `cardinality` inputs and the phantom
 * one last.
 */
const runNoInputCardinalityJourney = async (
  cardinality: number,
): Promise<Record<string, CompleteSignedTransactionMeasurement>> => {
  const {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  } = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realNonExistentInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
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
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(secondStepUtxo),
      inputsPreimage: fixture.inputsPreimage,
      nativeTxCompactCbor: fixture.inclusion.nativeTxCompactCbor,
      badInputIndex: fixture.badInputIndex,
      awaitConfirmation: true,
    }),
  );
  return {
    "step-01": step01Capture.measurement,
    "step-02": step02Capture.measurement,
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
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem).toBe(14_000_000n);
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

  it("fits the largest double-spend spend-input preimage and measures the first that does not", async () => {
    const ceilings = executionCeilings();
    const fitting = await runDoubleSpendCardinalityJourney(
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

    // **#580 re-take.** The claim this block used to make — that the witness
    // publication transaction is not the constraint — is retired with the
    // publication itself. What replaces it is the claim that matters now:
    // step-04 is the binding step and it binds on BYTES, with execution memory
    // an order of magnitude clear of the reserve at the boundary.
    const boundaryStep04 = fitting["step-04"]!;
    expect(boundaryStep04.l1ByteMargin).toBeGreaterThanOrEqual(0);
    expect(boundaryStep04.l1ByteMargin).toBeLessThan(64);
    expect(boundaryStep04.executionMemory < ceilings.memory / 10n).toBe(true);
    // step-03 carries tx1's preimage and is the smaller of the two, which is
    // why the pair above is a step-04 boundary.
    expect(boundaryStep04.completeSignedBytes).toBeGreaterThan(
      fitting["step-03"]!.completeSignedBytes,
    );

    const overBytes = await runDoubleSpendCardinalityJourney(
      DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY,
    );
    printCardinalityFit(
      "double-spend",
      DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY,
      overBytes,
    );
    const step04 = overBytes["step-04"]!;
    // Deliberately a SUCCESSFUL evaluation that fails the byte policy: one more
    // input pushes the step past `maxTxSize` while execution stays trivial.
    expect(step04.l1ByteMargin).toBeLessThan(0);
    expect(step04.executionMemory < ceilings.memory / 10n).toBe(true);
  }, 900_000);

  it("fits the largest no-input spend-input preimage and measures the first that does not", async () => {
    const ceilings = executionCeilings();
    const fitting = await runNoInputCardinalityJourney(
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

    // **#580 re-take.** This family carries the preimage in the step redeemer,
    // so the axis reaches the step transaction's bytes — and under flat that is
    // now the ONLY axis it reaches. The old assertion here was that ten
    // kilobytes of margin remained at the boundary, which was true when the
    // boundary was an execution boundary at 40 inputs; at the byte boundary the
    // margin is by definition small.
    const step02 = fitting["step-02"]!;
    expect(step02.l1ByteMargin).toBeGreaterThanOrEqual(0);
    expect(step02.l1ByteMargin).toBeLessThan(64);
    expect(step02.executionMemory < ceilings.memory / 10n).toBe(true);

    const overBytes = await runNoInputCardinalityJourney(
      NO_INPUT_FIRST_OVER_BYTES_CARDINALITY,
    );
    printCardinalityFit(
      "no-input",
      NO_INPUT_FIRST_OVER_BYTES_CARDINALITY,
      overBytes,
    );
    const overStep02 = overBytes["step-02"]!;
    expect(overStep02.l1ByteMargin).toBeLessThan(0);
    expect(overStep02.executionMemory < ceilings.memory / 10n).toBe(true);
  }, 900_000);

  it("reaches the admissible Cardano spend shape on execution and misses it on bytes alone", async () => {
    // **#580 re-take, and the row that carries the Q1X-F6 verdict.**
    //
    // This test used to assert that both journeys REJECT with `/over budget/`
    // at the admissible cardinality — that the step could not be evaluated at
    // all because it exceeded the ledger's own execution-memory cap. Under flat
    // both journeys build and evaluate: nothing is over budget, and the only
    // thing wrong with either step at 296 inputs is its size.
    //
    // Both halves are asserted, because the finding is the pair. Q1X-F6 is
    // resolved on the axis it named; what is left is a byte-carriage gap that
    // §8's ladder is built to close and the legacy inline builders do not yet
    // take.
    const noInput = await runNoInputCardinalityJourney(
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
    );
    printCardinalityFit(
      "no-input",
      CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY,
      noInput,
    );
    const doubleSpend = await runDoubleSpendCardinalityJourney(
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
      // Execution: comfortably inside even the conservative emulator reserve,
      // by more than an order of magnitude. This is the assertion Q1X-F6's
      // resolution rests on, and it is the one that would go red first if the
      // per-item cost ever came back.
      expect(measurement.executionMemory < ceilings.memory / 10n).toBe(true);
      expect(measurement.executionSteps < ceilings.steps / 10n).toBe(true);
      // Bytes: and this is what still misses.
      expect(measurement.l1ByteMargin).toBeLessThan(0);
    }
  }, 900_000);
});
