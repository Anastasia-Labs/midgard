/**
 * Maximum/adversarial proof-fit fixtures for the four foundational fault-proof
 * families (GOAL_SPEC.md 9.1 output 5, goal ids Q10/Q11/Q12/Q14).
 *
 * The sibling journeys in `submit-init-emulator.test.ts` and
 * `submit-init-emulator-ledger-rules.test.ts` measure proof fit on the MINIMAL
 * fixture: a one- or two-transaction block whose membership proof is a single
 * degenerate step. That answers "does the happy path fit", not "does the worst
 * admissible instance fit", which is what 9.1 output 5 asks.
 *
 * This file runs the same real prepare/submit pipeline over the worst
 * admissible instance of the one axis an adversary actually controls: the depth
 * of the MPF membership proof the challenged block forces into step-01 (and,
 * for the no-input family, into the step-04 non-membership proof). See
 * `adversarialMembershipSiblingKeys` in the shared fixtures module for how the
 * worst case is constructed and why it is the worst case.
 *
 * Three claims are made here, and all three are measured rather than asserted
 * from prose:
 *
 * 1. At the branch depth this fixture constructs, every transaction of every
 *    family's complete correction path still fits the 16,384-byte L1 envelope
 *    and the 20% execution reserve.
 * 2. The marginal cost of one further branch level is a measured CONSTANT — the
 *    MPF proof is a definite list of fixed-shape steps — so the depth at which
 *    each envelope is exhausted follows by exact arithmetic from a measured
 *    transaction. It is measured twice over: as MPF CBOR (139 bytes per level)
 *    and, separately, as complete signed transaction bytes (276 per level, the
 *    number that matters, because the proof reaches the chain as Plutus data).
 * 3. Byte fit, not the execution reserve, is what binds this axis: the derived
 *    byte ceiling is asserted to be the smallest of the three ceilings.
 *
 * The conclusion is NOT that the axis is safe. The measured byte ceiling lands
 * in the low twenties of forced branch levels, and forcing level `i` is a
 * fixed-target search costing ~2^(4i) digests, so the envelope is exhaustible
 * at roughly 2^84 work — expensive, but well inside a 2^128 adversary. That is
 * recorded as a finding (Q1X-F5) and asserted here so it cannot silently
 * change; it is the reason output 5 does not become LOCAL_PASS on the strength
 * of a fixture that merely fits.
 *
 * Lives in its own file for the same reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import { MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES } from "../src/validation-dispute/submit.js";
import {
  neSubmitStep01,
  neSubmitStep02,
  neSubmitStep03,
  neSubmitStep04,
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitInvalidRangeStep01,
  submitInvalidRangeStep02,
  submitStep01,
  submitStep02,
  submitStep03,
  submitStep04,
  submitZeroInputStep01,
  submitZeroInputStep02,
} from "./support/legacy-submit-emulator.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  adversarialMembershipSiblingKeys,
  buildInvalidRangeTransactionInclusionFixture,
  buildNonExistentInputFixture,
  buildTransactionInclusionFixture,
  buildZeroInputTransactionInclusionFixture,
  countedTransactionsRoot,
  expectStateQueueHeaderOrder,
  membershipProofBranchLevelByteCeiling,
  membershipProofBranchLevelsReachableWithWork,
  type MembershipProofShape,
  MPF_BRANCH_PROOF_STEP_CBOR_BYTES,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
  registerPexcludesExclusionRewardAccount,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  EXECUTION_RESERVE_FRACTION,
  expectProofFitV1,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  network,
  printProofFitV1,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * The reference adversary this file measures against: one willing to spend
 * 2^128 digest evaluations. It is deliberately absurd, so that a conclusion
 * drawn against it cannot be accused of assuming a weak attacker. It reaches
 * branch level 32.
 */
const ADVERSARY_LOG2_WORK = 128;

/**
 * The proof really is the adversarial shape it claims to be: one step per
 * forced branch level, each serialized as the largest step shape MPF has.
 */
const expectAdversarialProofShape = (
  label: string,
  shape: MembershipProofShape,
): void => {
  expect(shape.branchLevels, `${label} branch levels`).toBe(
    ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  );
  expect(
    shape.proofSteps,
    `${label} membership proof carries fewer steps than the forced branch levels`,
  ).toBeGreaterThanOrEqual(ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS);
  // Every forced level costs a full `branch` step; the +2 is the CBOR list
  // header the encoding always carries.
  expect(
    shape.proofCborBytes,
    `${label} membership proof is smaller than ${ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS.toString()} branch steps`,
  ).toBeGreaterThanOrEqual(
    2 +
      MPF_BRANCH_PROOF_STEP_CBOR_BYTES *
        ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  );
};

/**
 * From the LARGEST measured transaction of the correction path, the exact
 * branch level at which the L1 envelope is exhausted. The expected value is
 * pinned per family, in the same style as the ordered-field terminal
 * boundaries: a change in proof-carrying transaction size moves this number and
 * must be re-pinned deliberately rather than absorbed.
 */
const expectMembershipDepthCeiling = ({
  label,
  proofFit,
  expectedCeiling,
}: {
  readonly label: string;
  readonly proofFit: Record<string, CompleteSignedTransactionMeasurement>;
  readonly expectedCeiling: number;
}): number => {
  const largest = Math.max(
    ...Object.values(proofFit).map(
      (measurement) => measurement.completeSignedBytes,
    ),
  );
  const byteCeiling = membershipProofBranchLevelByteCeiling({
    measuredTransactionBytes: largest,
    measuredBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    l1MaxTxSize: MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
  });
  expect(
    byteCeiling,
    `${label}: the branch level at which the L1 envelope is exhausted moved`,
  ).toBe(expectedCeiling);
  // Finding Q1X-F5, asserted rather than narrated: forcing branch level `i`
  // costs ~2^(4i) digests, and this ceiling is inside a 2^128 adversary's
  // reach. If a future change lifts the ceiling out of reach this assertion
  // fails, which is exactly when the finding should be revisited.
  expect(
    4 * byteCeiling,
    `${label}: the envelope-exhausting depth is no longer reachable by a 2^${ADVERSARY_LOG2_WORK.toString()} adversary; finding Q1X-F5 needs re-stating`,
  ).toBeLessThan(ADVERSARY_LOG2_WORK);
  expect(
    membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK),
  ).toBe(32);
  return byteCeiling;
};

const printProofFit = (
  label: string,
  proofFit: Record<string, CompleteSignedTransactionMeasurement>,
  extra: Record<string, unknown>,
): void =>
  printProofFitV1({
    headline: `${label} maximum proof fit`,
    stages: proofFit,
    extra,
  });

describe("fault-proof maximum proof fit", () => {
  it("fits a maximum-depth double-spend proof inside the L1 envelope", async () => {
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
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({ lucid: proverLucid, contracts });
    const inclusion = await buildTransactionInclusionFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    expectAdversarialProofShape(
      "double-spend tx1",
      inclusion.tx1MembershipProof,
    );
    expectAdversarialProofShape(
      "double-spend tx2",
      inclusion.tx2MembershipProof,
    );

    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fraudulentHeader = makeHeader(
      await funderPaymentKeyHash(funderLucid),
      headerStartTime,
      await countedTransactionsRoot(
        inclusion.transactionsRoot,
        inclusion.l2TransactionCount,
      ),
      inclusion.l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const { headerHash } = setup;
    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.doubleSpend.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(inclusion.tx1.inclusion),
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    expect(step01Result.nativeTxId).toBe(inclusion.tx1.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(inclusion.tx2.inclusion),
        awaitConfirmation: true,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    const step02Result = step02Capture.result;
    expect(step02Result.nativeTx2Id).toBe(inclusion.tx2.nativeTxId);

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
          inclusion.tx1SpendInputCbors,
          "--tx1-inputs",
        ),
        nativeTxCompactCbor: parseSubmitStep01TxInclusion(
          inclusion.tx1.inclusion,
        ).nativeTxCompactCbor,
        doubleSpentInputIndex: 1n,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-03"] = step03Capture.measurement;
    const step03Result = step03Capture.result;

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
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
          inclusion.tx2SpendInputCbors,
          "--tx2-inputs",
        ),
        nativeTxCompactCbor: parseSubmitStep01TxInclusion(
          inclusion.tx2.inclusion,
        ).nativeTxCompactCbor,
        doubleSpentInputIndex: 1n,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-04"] = step04Capture.measurement;
    const step04Result = step04Capture.result;
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const removeNow = BigInt(emulator.now());
    const removeCapture = await captureEmulatorSubmission(emulator, async () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    proofFit["remove"] = removeCapture.measurement;
    expect(
      removeCapture.result.transactions.map((tx) => tx.removedHeaderHash),
    ).toEqual([headerHash]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expectSingleUtxoWithUnit(
      proverLucid,
      step04Result.fraudProofAddress,
      step04Result.fraudProofUnit,
    );

    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "step-03",
      "step-04",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `double-spend maximum ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    for (const measurement of removeCapture.measurements) {
      expectProofFitV1({
        stage: "double-spend maximum removal transaction",
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    const byteCeiling = expectMembershipDepthCeiling({
      label: "double-spend",
      proofFit,
      expectedCeiling: 20,
    });
    printProofFit("double-spend", proofFit, {
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
      l2TransactionCount: inclusion.l2TransactionCount.toString(),
      tx1ProofCborBytes: inclusion.tx1MembershipProof.proofCborBytes,
      tx2ProofCborBytes: inclusion.tx2MembershipProof.proofCborBytes,
      derivedBranchLevelByteCeiling: byteCeiling,
    });
  }, 300_000);

  it("fits a maximum-depth non-existent-input proof inside the L1 envelope", async () => {
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
      registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
    });
    const fixture = await buildNonExistentInputFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    expectAdversarialProofShape(
      "non-existent-input bad tx",
      fixture.badTxMembershipProof,
    );
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({ lucid: proverLucid, contracts });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
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
    const { headerHash } = setup;
    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "nonExistentInput",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;

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
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    expect(step01Result.nativeTxId).toBe(fixture.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
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
    proofFit["step-02"] = step02Capture.measurement;
    const step02Result = step02Capture.result;

    const thirdStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.thirdStepAddress,
      initResult.computationThreadUnit,
    );
    const step03Capture = await captureEmulatorSubmission(emulator, async () =>
      neSubmitStep03({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(thirdStepUtxo),
        ledgerNonMembershipProofCbor: fixture.ledgerNonMembershipProofCbor,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-03"] = step03Capture.measurement;
    const step03Result = step03Capture.result;

    const fourthStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step03Result.fourthStepAddress,
      initResult.computationThreadUnit,
    );
    const step04Capture = await captureEmulatorSubmission(emulator, async () =>
      neSubmitStep04({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(fourthStepUtxo),
        txsNonMembershipProofCbor: fixture.txsNonMembershipProofCbor,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-04"] = step04Capture.measurement;
    const step04Result = step04Capture.result;
    expect(step04Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const removeNow = BigInt(emulator.now());
    const removeCapture = await captureEmulatorSubmission(emulator, async () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "nonExistentInput",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    proofFit["remove"] = removeCapture.measurement;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "step-03",
      "step-04",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `non-existent-input maximum ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    const byteCeiling = expectMembershipDepthCeiling({
      label: "non-existent-input",
      proofFit,
      expectedCeiling: 20,
    });
    printProofFit("non-existent-input", proofFit, {
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
      l2TransactionCount: fixture.l2TransactionCount.toString(),
      badTxProofCborBytes: fixture.badTxMembershipProof.proofCborBytes,
      txsNonMembershipProofCborBytes: fixture.txsNonMembershipProofCborBytes,
      derivedBranchLevelByteCeiling: byteCeiling,
    });
  }, 300_000);

  // The invalid-range family carries the tightest measured byte margin of the
  // four, so it is the one this file runs at two depths: the difference
  // between them is the measured marginal cost of one forced branch level, in
  // bytes AND in execution units, which is what turns "it fits at depth N"
  // into a bound.
  const runInvalidRangeJourney = async (branchLevels: number) => {
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
      contractOptions: { realInvalidRange: true, alwaysFraudProofCatalogue: true },
    });
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({ lucid: proverLucid, contracts });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const fixture = await buildInvalidRangeTransactionInclusionFixture({
      blockValidFrom: BigInt(headerStartTime),
      blockValidTo: BigInt(headerStartTime + 1_000),
      adversarialBranchLevels: branchLevels,
    });
    expect(fixture.violationReason).toBe("lower-before-block");

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
    const { headerHash } = setup;
    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "invalidRange",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInvalidRangeStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(fixture.badTx.inclusion),
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    expect(step01Result.violationReason).toBe("lower-before-block");

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInvalidRangeStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        awaitConfirmation: true,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    const step02Result = step02Capture.result;
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(
      Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum),
    ).toStrictEqual({
      fraud_prover: getAddressDetails(await proverLucid.wallet().address())
        .paymentCredential!.hash,
    });

    const removeNow = BigInt(emulator.now());
    const removeCapture = await captureEmulatorSubmission(emulator, async () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "invalidRange",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    proofFit["remove"] = removeCapture.measurement;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `invalid-range branch-levels-${branchLevels.toString()} ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    return { proofFit, fixture, maxTxExMem, maxTxExSteps };
  };

  it("fits a maximum-depth invalid-range proof inside the L1 envelope", async () => {
    const { proofFit, fixture } = await runInvalidRangeJourney(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );
    expectAdversarialProofShape(
      "invalid-range bad tx",
      fixture.badTxMembershipProof,
    );
    const byteCeiling = expectMembershipDepthCeiling({
      label: "invalid-range",
      proofFit,
      expectedCeiling: 19,
    });
    printProofFit("invalid-range", proofFit, {
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
      l2TransactionCount: fixture.l2TransactionCount.toString(),
      badTxProofCborBytes: fixture.badTxMembershipProof.proofCborBytes,
      derivedBranchLevelByteCeiling: byteCeiling,
    });
  }, 300_000);

  it("fits a maximum-depth zero-input proof inside the L1 envelope", async () => {
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
      contractOptions: { realZeroInput: true, alwaysFraudProofCatalogue: true },
    });
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({ lucid: proverLucid, contracts });
    const fixture = await buildZeroInputTransactionInclusionFixture({
      adversarialBranchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    expectAdversarialProofShape(
      "zero-input bad tx",
      fixture.badTxMembershipProof,
    );

    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
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
    const { headerHash } = setup;
    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "zeroInput",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitZeroInputStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(fixture.badTx.inclusion),
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    // #604: the thread carries the §2.5 anchor, not the field commitment.
    expect(step01Result.badTxId).toBe(fixture.badTx.nativeTxId);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitZeroInputStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        nativeTxCompactCbor: parseSubmitStep01TxInclusion(
          fixture.badTx.inclusion,
        ).nativeTxCompactCbor,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    const step02Result = step02Capture.result;
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    const removeNow = BigInt(emulator.now());
    const removeCapture = await captureEmulatorSubmission(emulator, async () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "zeroInput",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    proofFit["remove"] = removeCapture.measurement;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });

    expect(Object.keys(proofFit)).toEqual([
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `zero-input maximum ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    const byteCeiling = expectMembershipDepthCeiling({
      label: "zero-input",
      proofFit,
      expectedCeiling: 20,
    });
    printProofFit("zero-input", proofFit, {
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
      l2TransactionCount: fixture.l2TransactionCount.toString(),
      badTxProofCborBytes: fixture.badTxMembershipProof.proofCborBytes,
      derivedBranchLevelByteCeiling: byteCeiling,
    });
  }, 300_000);

  it("charges a constant 139 bytes for every additional forced branch level", async () => {
    const { Store, Trie } = await import(
      "@aiken-lang/merkle-patricia-forestry"
    );
    const target = Buffer.alloc(32, 0x5a);
    const sizes: number[] = [];
    for (
      let branchLevels = 1;
      branchLevels <= ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS;
      branchLevels += 1
    ) {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      await trie.insert(target, Buffer.from("target"));
      for (const key of adversarialMembershipSiblingKeys({
        targetKey: target,
        branchLevels,
        domain: 0x0f01,
      })) {
        await trie.insert(key, Buffer.from("ad", "hex"));
      }
      const proof = await trie.prove(target);
      const steps = proof.toJSON() as readonly { readonly type: string }[];
      // Two siblings per level is what makes every on-path node a >=3-child
      // node, so every step is the largest `branch` shape rather than the
      // cheaper `fork` or `leaf`.
      expect(
        steps.map((step) => step.type),
        `branch levels ${branchLevels.toString()} did not produce all-branch steps`,
      ).toEqual(Array.from({ length: branchLevels }, () => "branch"));
      sizes.push(Buffer.from(proof.toCBOR()).length);
    }
    const deltas = sizes.slice(1).map((size, index) => size - sizes[index]!);
    expect(
      deltas,
      "the MPF proof encoding is a definite list of fixed-shape steps, so each further branch level must cost exactly the same",
    ).toEqual(
      Array.from(
        { length: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS - 1 },
        () => MPF_BRANCH_PROOF_STEP_CBOR_BYTES,
      ),
    );
    // Forcing a branch at level `i` is a fixed-target search over `i` chosen
    // nibbles, so a 2^128 adversary reaches level 32 and no further.
    expect(
      membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK),
    ).toBe(32);
  }, 120_000);

  it("bounds the membership-depth axis with a measured marginal cost per level", async () => {
    // Two real journeys at two depths. Everything below is the DIFFERENCE
    // between them, so nothing here is an assumed rate.
    const shallowLevels = 1;
    const shallow = await runInvalidRangeJourney(shallowLevels);
    const deep = await runInvalidRangeJourney(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );
    const levelSpan =
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS - shallowLevels;
    const shallowStep = shallow.proofFit["step-01"]!;
    const deepStep = deep.proofFit["step-01"]!;

    const marginalBytes =
      (deepStep.completeSignedBytes - shallowStep.completeSignedBytes) /
      levelSpan;
    const marginalMemory =
      Number(deepStep.executionMemory - shallowStep.executionMemory) /
      levelSpan;
    const marginalSteps =
      Number(deepStep.executionSteps - shallowStep.executionSteps) / levelSpan;

    // The proof-carrying transaction grows by a constant per forced level, and
    // that constant is NOT the library's own compact CBOR figure: the proof
    // reaches the chain as Plutus data in the step redeemer, which costs
    // roughly twice as much. Pinning both, and the ratio between them, is what
    // stops the smaller number being used as the envelope arithmetic.
    expect(
      marginalBytes,
      "one forced branch level must cost the pinned constant in the submitted transaction",
    ).toBe(PROOF_TRANSACTION_BRANCH_LEVEL_BYTES);
    expect(marginalBytes).toBeGreaterThan(MPF_BRANCH_PROOF_STEP_CBOR_BYTES);
    expect(marginalMemory).toBeGreaterThan(0);
    expect(marginalSteps).toBeGreaterThan(0);

    const ceilingFrom = (headroom: number, marginal: number): number =>
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS +
      Math.floor(headroom / marginal);
    const byteCeiling = ceilingFrom(
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES - deepStep.completeSignedBytes,
      marginalBytes,
    );
    const memoryCeiling = ceilingFrom(
      Number(
        (deep.maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n -
          deepStep.executionMemory,
      ),
      marginalMemory,
    );
    const stepCeiling = ceilingFrom(
      Number(
        (deep.maxTxExSteps * (100n - EXECUTION_RESERVE_FRACTION)) / 100n -
          deepStep.executionSteps,
      ),
      marginalSteps,
    );

    // Which envelope actually binds this axis. If execution ever became the
    // binding constraint the byte-only bound above would be reporting the wrong
    // number, so the ordering is asserted rather than assumed.
    expect(
      byteCeiling,
      "byte fit must be the binding constraint on membership-proof depth, not execution memory",
    ).toBeLessThan(memoryCeiling);
    expect(
      byteCeiling,
      "byte fit must be the binding constraint on membership-proof depth, not execution steps",
    ).toBeLessThan(stepCeiling);

    // Finding Q1X-F5. Forcing branch level `i` is a fixed-target search over
    // `i` chosen nibbles, so 2^128 digest evaluations buy level 32 — MORE than
    // the level at which the L1 envelope runs out. The membership-depth axis is
    // therefore bounded but not out of reach: exhausting the envelope costs
    // about 2^(4 * byteCeiling) digests, which is recorded rather than rounded
    // up into a pass.
    const reachable =
      membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK);
    expect(reachable).toBe(32);
    expect(
      byteCeiling,
      "the L1 envelope is no longer exhaustible by a 2^128 adversary; finding Q1X-F5 must be re-stated rather than left to drift",
    ).toBeLessThan(reachable);
    const log2WorkToExhaustEnvelope = 4 * byteCeiling;
    expect(
      log2WorkToExhaustEnvelope,
      "exhausting the envelope must still cost at least 2^72 digest evaluations",
    ).toBeGreaterThanOrEqual(72);

    if (process.env["MIDGARD_PRINT_PROOF_FIT"] === "1") {
      console.log(
        `membership-depth bound: ${JSON.stringify(
          {
            shallowLevels,
            deepLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
            shallowStep01Bytes: shallowStep.completeSignedBytes,
            deepStep01Bytes: deepStep.completeSignedBytes,
            marginalBytes,
            marginalMemory,
            marginalSteps,
            byteCeiling,
            memoryCeiling,
            stepCeiling,
            reachableWith2Pow128Work: reachable,
            log2WorkToExhaustEnvelope,
          },
          null,
          2,
        )}`,
      );
    }
  }, 300_000);
});
