import { outRefLabel } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { expect } from "vitest";

import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
} from "./support/legacy-submit-emulator.js";
import {
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
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  printProofFit,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

export const L1_MAX_TX_SIZE = 16_384;

/**
 * The one-byte-per-item guardrail, the field-bytes bound and the Cardano
 * script-spend shape, in the order the module header derives them.
 */
const SPEND_INPUT_PREIMAGE_ITEM_BYTES = 38;

const SPEND_INPUT_PREIMAGE_ARRAY_HEADER_BYTES = 3;

export const CARDANO_SCRIPT_SPEND_SHAPE_CARDINALITY = 296;

/**
 * 365 spend inputs (constant §5.3 stride of 40 bytes each) make a 14,603-byte
 * field-0 preimage: past §8.4's 14,336-byte tier-1 bound, inside the
 * single-publication tier-2 window `(14,336, 15,148]` — the ladder picks
 * `RawUtxo` on size alone, no demotion asked for.
 */
export const TIER2_SIZE_SELECTED_CARDINALITY = 365;

export const executionCeilings = () => ({
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
 * The band the binding steps' execution memory must stay inside: a tenth of
 * the shared 20%-reserve memory ceiling, derived from the emulator protocol
 * parameters rather than transcribed from a run.
 *
 * A tenth is the Q1X-F6 margin. The counted scheme billed ~276,000 memory
 * units per spend input, so at any admissible cardinality it left this band
 * far behind; under the flat commitment the binding step bills a constant.
 * The band therefore fails the moment a per-item cost of even ~2,500 units
 * an input returns at the 296-input shape — long before the ledger's own cap
 * would notice.
 */
export const bindingStepMemoryBand = (): bigint =>
  executionCeilings().memory / 10n;

/**
 * The largest admissible spend-input cardinality, by the §5.4 field-bytes
 * bound: `maxSpendInputsPreimageBytes` less the preimage array header,
 * divided by the constant 38-byte per-item stride. The first test below
 * re-derives it and states the figure (862).
 */
export const ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES = Math.floor(
  (MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes -
    SPEND_INPUT_PREIMAGE_ARRAY_HEADER_BYTES) /
    SPEND_INPUT_PREIMAGE_ITEM_BYTES,
);

/**
 * How far apart two binding-step memory measurements at different
 * cardinalities may sit, per input of difference, before the cost stops
 * being "constant in cardinality".
 *
 * Derived, not measured: the memory band above, divided by the largest
 * admissible cardinality. A per-input cost at this rate would exhaust the
 * whole band across the admissible range on its own, so anything at or above
 * it is a per-item cost in the Q1X-F6 sense. The counted scheme's ~276,000
 * units an input clears it by more than two orders of magnitude.
 */
export const maxBindingStepMemoryPerInput = (): bigint =>
  bindingStepMemoryBand() / BigInt(ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES);

export const printCardinalityFit = (
  label: string,
  cardinality: number,
  stages: Record<string, CompleteSignedTransactionMeasurement>,
): void =>
  printProofFit({
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
export const runDoubleSpendCardinalityJourney = async (
  cardinality: number,
  { publishCarriage = false }: { readonly publishCarriage?: boolean } = {},
): Promise<{
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly carriageTiers: Record<string, string>;
}> => {
  const harness = await makeFaultProofEmulatorHarness({
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
