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
 * pipeline at the largest cardinality that clears the GOAL_SPEC.md 3.3 20%
 * execution reserve and at the first that does not, for both families. The
 * result is a DEFECT, recorded here executably so it cannot drift:
 *
 * - The witness publication transaction is NOT the binding constraint. At the
 *   boundary it is under two kilobytes with more than fourteen kilobytes of
 *   margin, and the step transactions that reference it are within five bytes
 *   of their minimal-fixture size.
 * - Execution MEMORY binds, and it binds an order of magnitude below the
 *   admissible cardinality: re-hashing the authenticated bounded collection
 *   costs a measured ~276,000 memory units per input, so the reserve runs out
 *   in the high thirties.
 *
 * Moving bytes elsewhere cannot remediate this the way published-chunk carriage
 * remediated Q1X-F5: the cost is the step's own re-hashing of a collection it
 * must reproduce in full before it may select one item from it.
 *
 * Lives in its own file for the same reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/index.js";
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
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  makeHeader,
  network,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const EXECUTION_RESERVE_FRACTION = 20n;
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
 * correction path clears the 20% reserve, and the first that does not — and
 * both members of each pair are driven through the real pipeline below.
 */
const DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY = 39;
const DOUBLE_SPEND_FIRST_OVER_RESERVE_CARDINALITY = 40;
const NO_INPUT_LARGEST_FITTING_CARDINALITY = 40;
const NO_INPUT_FIRST_OVER_RESERVE_CARDINALITY = 41;

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

const expectProofFitV1 = ({
  stage,
  measurement,
  maxTxExMem,
  maxTxExSteps,
}: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly maxTxExMem: bigint;
  readonly maxTxExSteps: bigint;
}): void => {
  expect(
    measurement.l1ByteMargin,
    `${stage} exceeds the 16,384-byte L1 envelope`,
  ).toBeGreaterThanOrEqual(0);
  const memoryCeiling =
    (maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  const stepCeiling =
    (maxTxExSteps * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  expect(
    measurement.executionMemory <= memoryCeiling,
    `${stage} execution memory ${measurement.executionMemory.toString()} exceeds the 20%-reserve ceiling ${memoryCeiling.toString()}`,
  ).toBe(true);
  expect(
    measurement.executionSteps <= stepCeiling,
    `${stage} execution steps ${measurement.executionSteps.toString()} exceeds the 20%-reserve ceiling ${stepCeiling.toString()}`,
  ).toBe(true);
};

const printCardinalityFit = (
  label: string,
  cardinality: number,
  stages: Record<string, CompleteSignedTransactionMeasurement>,
): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  console.log(
    `${label} spend-input cardinality ${String(cardinality)}: ${JSON.stringify(
      Object.fromEntries(
        Object.entries(stages).map(([stage, measurement]) => [
          stage,
          {
            bytes: measurement.completeSignedBytes,
            l1ByteMargin: measurement.l1ByteMargin,
            memory: measurement.executionMemory.toString(),
            steps: measurement.executionSteps.toString(),
          },
        ]),
      ),
      null,
      2,
    )}`,
  );
};

const newEmulatorParty = async () => {
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  return { emulator, funderLucid, proverLucid, proverSigner };
};

const funderPaymentKeyHash = async (
  funderLucid: Awaited<ReturnType<typeof Lucid>>,
): Promise<string> => {
  const credential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential === undefined || credential.type !== "Key") {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  return credential.hash;
};

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
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const { emulator, funderLucid, proverLucid, proverSigner } =
    await newEmulatorParty();
  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    { alwaysFraudProofCatalogue: true },
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
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
  // The step publishes the witness and then spends the thread; both are ours.
  expect(step03Capture.measurements.length).toBe(2);
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
  expect(step04Capture.measurements.length).toBe(2);
  expect(step04Capture.result.fraudProofAssetName).toBe(
    initResult.computationThreadAssetName,
  );
  return {
    "tx1-witness-publication": step03Capture.measurements[0]!,
    "step-03": step03Capture.measurement,
    "tx2-witness-publication": step04Capture.measurements[0]!,
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
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const { emulator, funderLucid, proverLucid, proverSigner } =
    await newEmulatorParty();
  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    { realNonExistentInput: true, alwaysFraudProofCatalogue: true },
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
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

    // The publication transaction is not the constraint: it is under two
    // kilobytes at the boundary, and the step that references it is within a
    // handful of bytes of its minimal-fixture size.
    const witness = fitting["tx1-witness-publication"]!;
    expect(witness.completeSignedBytes).toBeLessThan(2_048);
    expect(witness.l1ByteMargin).toBeGreaterThan(14_000);
    expect(fitting["step-03"]!.completeSignedBytes).toBeLessThan(4_500);

    const overReserve = await runDoubleSpendCardinalityJourney(
      DOUBLE_SPEND_FIRST_OVER_RESERVE_CARDINALITY,
    );
    printCardinalityFit(
      "double-spend",
      DOUBLE_SPEND_FIRST_OVER_RESERVE_CARDINALITY,
      overReserve,
    );
    const step04 = overReserve["step-04"]!;
    // Deliberately a SUCCESSFUL evaluation that fails the release policy: one
    // more input clears the ledger's hard cap and not the 20% reserve.
    expect(step04.executionMemory > ceilings.memory).toBe(true);
    expect(
      step04.executionMemory < EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    ).toBe(true);
    expect(step04.l1ByteMargin).toBeGreaterThan(0);
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

    // This family carries the preimage in the step redeemer, so the axis DOES
    // reach the step transaction's bytes here — and still does not bind: at the
    // boundary the transaction has more than ten kilobytes of margin.
    const step02 = fitting["step-02"]!;
    expect(step02.l1ByteMargin).toBeGreaterThan(10_000);

    const overReserve = await runNoInputCardinalityJourney(
      NO_INPUT_FIRST_OVER_RESERVE_CARDINALITY,
    );
    printCardinalityFit(
      "no-input",
      NO_INPUT_FIRST_OVER_RESERVE_CARDINALITY,
      overReserve,
    );
    const overStep02 = overReserve["step-02"]!;
    expect(overStep02.executionMemory > ceilings.memory).toBe(true);
    expect(
      overStep02.executionMemory < EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    ).toBe(true);
    expect(overStep02.l1ByteMargin).toBeGreaterThan(0);
  }, 900_000);

  it("cannot build either family's proof at the admissible Cardano spend shape", async () => {
    // Not merely over the release reserve: at the admissible cardinality the
    // step cannot be evaluated at all, because it exceeds the ledger's own
    // execution-memory cap. This is the exposure finding Q1X-F6 records.
    await expect(
      runNoInputCardinalityJourney(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY),
    ).rejects.toThrow(/over budget/u);
    await expect(
      runDoubleSpendCardinalityJourney(CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY),
    ).rejects.toThrow(/over budget/u);
  }, 900_000);
});
