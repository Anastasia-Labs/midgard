/**
 * Published-chunk proof carriage, measured end to end in the emulator
 * (issue #545, remediating finding Q1X-F5).
 *
 * ## What the sibling file measured, and what it left open
 *
 * `submit-init-emulator-max-proof-fit.test.ts` measures the four foundational
 * families on the REDEEMER-CARRIED route and shows it is exhaustible: one
 * forced MPF branch level costs a measured 276 complete-signed-transaction
 * bytes, so the preserved 16,384-byte L1 envelope runs out at branch level 21
 * (Q12) to 23 (Q14), and reaching that depth is a fixed-target search costing
 * about 2^84 candidate hashings — expensive, but well inside a 2^128 adversary.
 * That measurement stands unchanged; it is the reason this file exists.
 *
 * ## What this file measures
 *
 * The same real prepare/submit pipeline, with the membership proof taken out of
 * the step transaction entirely: the proof is published beforehand across
 * bounded UTxOs whose inline datum carries nothing but MPF steps, and the step
 * transaction references them and names their order as indices into
 * `tx.reference_inputs`. Three claims, all measured:
 *
 * 1. A depth-22 proof — past the depth at which the direct route exhausts the
 *    envelope for every one of the four families — completes a family's entire
 *    correction path inside the envelope and the 20% execution reserve.
 * 2. The step transaction's marginal cost per proof level is ZERO. Measured by
 *    running the same journey at depth 22 and at depth 32 (the depth a 2^128
 *    adversary reaches) and asserting the step-01 transaction is byte-identical
 *    in size. On the direct route those two depths differ by 10 * 276 bytes.
 * 3. The publication transaction itself fits, and its cost is paid once.
 *
 * The depth is synthesised rather than grinded — see
 * `tests/support/synthetic-deep-proof-v1.ts` for why that is the honest
 * construction for a carriage measurement and what it does and does not claim.
 *
 * Lives in its own file for the same reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import {
  EMPTY_SPEND_INPUTS_HASH,
  MAXIMUM_CHUNK_PROOF_STEP_COUNT,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  publishProofChunksV1,
  resolveProverSigner,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES } from "../src/validation-dispute/submit.js";
import {
  parseSubmitStep01TxInclusion,
  submitInit,
  submitInvalidRangeStep01,
  submitInvalidRangeStep02,
  submitZeroInputStep01,
  submitZeroInputStep02,
} from "./support/legacy-submit-emulator.js";
import {
  buildInvalidRangeTransactionInclusionFixture,
  buildZeroInputTransactionInclusionFixture,
  countedTransactionsRoot,
  expectStateQueueHeaderOrder,
  PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
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
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerChunkedVerifyRewardAccount,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProofV1 } from "./support/synthetic-deep-proof-v1.js";

const EXECUTION_RESERVE_FRACTION = 20n;

/**
 * The depth this file drives. 22 is past the envelope-exhaustion level of every
 * family on the direct route (21 for Q12, 22 for Q10/Q11, 23 for Q14), so no
 * proof of this depth can reach L1 through a step redeemer.
 */
const CARRIAGE_BRANCH_LEVELS = 22;

/** The depth a 2^128 adversary reaches: 128 / 4 bits of forced prefix. */
const ADVERSARY_BRANCH_LEVELS = 32;

/** Both depths need this many publication UTxOs at 16 steps per chunk. */
const EXPECTED_CHUNK_COUNT = 2;

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

const printCarriageFit = (
  label: string,
  proofFit: Record<string, CompleteSignedTransactionMeasurement>,
  extra: Record<string, unknown>,
): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  console.log(
    `${label} chunked carriage fit: ${JSON.stringify(
      {
        ...Object.fromEntries(
          Object.entries(proofFit).map(([stage, measurement]) => [
            stage,
            {
              bytes: measurement.completeSignedBytes,
              l1ByteMargin: measurement.l1ByteMargin,
              memory: measurement.executionMemory.toString(),
              steps: measurement.executionSteps.toString(),
              referenceInputs: measurement.referenceInputCount,
            },
          ]),
        ),
        ...extra,
      },
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
 * Re-commits a fixture's inclusion evidence at `branchLevels` forced branch
 * levels: the same challenged transaction, a synthesised deep proof, and the
 * raw root that proof proves. The header the setup transaction commits is built
 * from the returned root, so the step's counted-root authentication is genuine.
 */
const deepenInclusion = ({
  inclusion,
  branchLevels,
}: {
  readonly inclusion: unknown;
  readonly branchLevels: number;
}) => {
  const parsed = inclusion as {
    readonly nativeTxId: string;
    readonly nativeTxCompactCbor: string;
  };
  const deep = syntheticDeepMembershipProofV1({
    key: Buffer.from(parsed.nativeTxId, "hex"),
    value: Buffer.from(parsed.nativeTxCompactCbor, "hex"),
    branchLevels,
  });
  return {
    deep,
    inclusion: {
      ...(inclusion as Record<string, unknown>),
      transactionsPhasRoot: deep.transactionsPhasRoot,
      txMembershipProofCbor: deep.proofCbor,
    },
  };
};

describe("fault-proof published-chunk proof carriage", () => {
  /**
   * Q14 (zero-input), the whole correction path, with the membership proof
   * carried by published chunks at a depth the direct route cannot reach.
   *
   * Returns the measured stages so the depth-invariance claim can compare two
   * runs of the very same journey.
   */
  const runZeroInputChunkedJourney = async ({
    branchLevels,
    removeBlock,
  }: {
    readonly branchLevels: number;
    readonly removeBlock: boolean;
  }): Promise<{
    readonly proofFit: Record<string, CompleteSignedTransactionMeasurement>;
    readonly chunkCount: number;
    readonly proofCborBytes: number;
  }> => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const { emulator, funderLucid, proverLucid, proverSigner } =
      await newEmulatorParty();

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    await registerChunkedVerifyRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }
    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realZeroInput: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({ lucid: proverLucid, contracts });
    const fixture = await buildZeroInputTransactionInclusionFixture();
    const { deep, inclusion } = deepenInclusion({
      inclusion: fixture.badTx.inclusion,
      branchLevels,
    });

    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const l2TransactionCount = 10_000n;
    const fraudulentHeader = makeHeader(
      await funderPaymentKeyHash(funderLucid),
      headerStartTime,
      await countedTransactionsRoot(
        deep.transactionsPhasRoot,
        l2TransactionCount,
      ),
      l2TransactionCount,
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

    // The proof reaches L1 here, once, and never again.
    const publicationCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        publishProofChunksV1({
          lucid: proverLucid,
          network,
          signer: proverSigner,
          proofCbor: deep.proofCbor,
          awaitConfirmation: true,
        }),
    );
    proofFit["publish-chunks"] = publicationCapture.measurement;
    const publication = publicationCapture.result;
    expect(publication.chunks.length).toBe(EXPECTED_CHUNK_COUNT);
    expect(publication.proofStepCount).toBe(branchLevels);
    expect(publication.chunks[0]?.stepCount).toBe(
      MAXIMUM_CHUNK_PROOF_STEP_COUNT,
    );

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
        txInclusion: parseSubmitStep01TxInclusion(inclusion),
        publishedProofChunks: publication.chunks,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    expect(step01Result.badTxSpendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(step01Result.proofCarriage).toBe("published-chunks");
    expect(step01Result.publishedChunkOutRefs.length).toBe(
      EXPECTED_CHUNK_COUNT,
    );
    // Hub oracle, state-queue node and the two chunks — and nothing else.
    expect(step01Capture.measurement.referenceInputCount).toBe(
      2 + EXPECTED_CHUNK_COUNT,
    );
    // The publication UTxOs survive the step that referenced them.
    const survivors = await proverLucid.utxosByOutRef(
      publication.chunks.map((chunk) => ({
        txHash: chunk.utxo.txHash,
        outputIndex: chunk.utxo.outputIndex,
      })),
    );
    expect(survivors.length).toBe(EXPECTED_CHUNK_COUNT);

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
        awaitConfirmation: true,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    expect(step02Capture.result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );

    if (removeBlock) {
      const removeNow = BigInt(emulator.now());
      const removeCapture = await captureEmulatorSubmission(
        emulator,
        async () =>
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
    }

    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `zero-input chunked ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    return {
      proofFit,
      chunkCount: publication.chunks.length,
      proofCborBytes: deep.proofCborBytes,
    };
  };

  it("carries a zero-input membership proof past the direct route's envelope ceiling", async () => {
    const run = await runZeroInputChunkedJourney({
      branchLevels: CARRIAGE_BRANCH_LEVELS,
      removeBlock: true,
    });
    expect(Object.keys(run.proofFit)).toEqual([
      "publish-chunks",
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    const step01 = run.proofFit["step-01"];
    if (step01 === undefined) {
      throw new Error("zero-input chunked step-01 was not measured");
    }
    // The proof itself is larger than a whole envelope's worth of the direct
    // route's marginal cost at this depth, and yet the step transaction is
    // nowhere near the envelope: the proof is simply not in it.
    expect(run.proofCborBytes).toBeGreaterThan(CARRIAGE_BRANCH_LEVELS * 100);
    expect(step01.completeSignedBytes).toBeLessThan(
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
    );
    printCarriageFit("zero-input", run.proofFit, {
      branchLevels: CARRIAGE_BRANCH_LEVELS,
      chunkCount: run.chunkCount,
      proofCborBytes: run.proofCborBytes,
    });
  }, 600_000);

  it("pins the step transaction's marginal cost per proof level at zero", async () => {
    const shallow = await runZeroInputChunkedJourney({
      branchLevels: CARRIAGE_BRANCH_LEVELS,
      removeBlock: false,
    });
    const deep = await runZeroInputChunkedJourney({
      branchLevels: ADVERSARY_BRANCH_LEVELS,
      removeBlock: false,
    });
    const shallowStep01 = shallow.proofFit["step-01"];
    const deepStep01 = deep.proofFit["step-01"];
    if (shallowStep01 === undefined || deepStep01 === undefined) {
      throw new Error("both depths must measure a step-01 transaction");
    }
    // Ten further forced branch levels. On the redeemer-carried route that is
    // 10 * 276 = 2,760 more signed bytes; here it is none at all, because the
    // step carries one index and one reference input per CHUNK, not per level.
    expect(deep.chunkCount).toBe(shallow.chunkCount);
    expect(
      deepStep01.completeSignedBytes - shallowStep01.completeSignedBytes,
      "the chunked step transaction grew with proof depth",
    ).toBe(0);
    expect(
      (ADVERSARY_BRANCH_LEVELS - CARRIAGE_BRANCH_LEVELS) *
        PROOF_TRANSACTION_BRANCH_LEVEL_BYTES,
      "the direct route's cost for the same ten levels is the number this replaces",
    ).toBe(2_760);
    // The proof did grow; only its carriage did not.
    expect(deep.proofCborBytes).toBeGreaterThan(shallow.proofCborBytes);
    printCarriageFit("zero-input depth-invariance", deep.proofFit, {
      shallowBranchLevels: CARRIAGE_BRANCH_LEVELS,
      deepBranchLevels: ADVERSARY_BRANCH_LEVELS,
      shallowStep01Bytes: shallowStep01.completeSignedBytes,
      deepStep01Bytes: deepStep01.completeSignedBytes,
      shallowProofCborBytes: shallow.proofCborBytes,
      deepProofCborBytes: deep.proofCborBytes,
    });
  }, 900_000);

  it("carries an invalid-range membership proof past the direct route's envelope ceiling", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const { emulator, funderLucid, proverLucid, proverSigner } =
      await newEmulatorParty();

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    await registerChunkedVerifyRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }
    const contracts = await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      { realInvalidRange: true, alwaysFraudProofCatalogue: true },
    );
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
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
    });
    expect(fixture.violationReason).toBe("lower-before-block");
    const { deep, inclusion } = deepenInclusion({
      inclusion: fixture.badTx.inclusion,
      branchLevels: CARRIAGE_BRANCH_LEVELS,
    });
    const l2TransactionCount = 10_000n;
    const fraudulentHeader = makeHeader(
      await funderPaymentKeyHash(funderLucid),
      headerStartTime,
      await countedTransactionsRoot(
        deep.transactionsPhasRoot,
        l2TransactionCount,
      ),
      l2TransactionCount,
    );
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fraudulentHeader,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      contracts,
      catalogue,
      undefined,
      undefined,
      removalReferenceScriptPublications.published,
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;

    const publicationCapture = await captureEmulatorSubmission(
      emulator,
      async () =>
        publishProofChunksV1({
          lucid: proverLucid,
          network,
          signer: proverSigner,
          proofCbor: deep.proofCbor,
          awaitConfirmation: true,
        }),
    );
    proofFit["publish-chunks"] = publicationCapture.measurement;
    const publication = publicationCapture.result;
    expect(publication.chunks.length).toBe(EXPECTED_CHUNK_COUNT);

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
        txInclusion: parseSubmitStep01TxInclusion(inclusion),
        publishedProofChunks: publication.chunks,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01Result = step01Capture.result;
    expect(step01Result.proofCarriage).toBe("published-chunks");
    expect(step01Capture.measurement.referenceInputCount).toBe(
      2 + EXPECTED_CHUNK_COUNT,
    );

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
    expect(step02Capture.result.fraudProofAssetName).toBe(
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
        fraudCategory: "invalidRange",
        fraudulentHeaderHash: setup.headerHash,
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
      "publish-chunks",
      "init",
      "step-01",
      "step-02",
      "remove",
    ]);
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({
        stage: `invalid-range chunked ${stage}`,
        measurement,
        maxTxExMem,
        maxTxExSteps,
      });
    }
    printCarriageFit("invalid-range", proofFit, {
      branchLevels: CARRIAGE_BRANCH_LEVELS,
      chunkCount: publication.chunks.length,
      proofCborBytes: deep.proofCborBytes,
    });
  }, 600_000);
});
