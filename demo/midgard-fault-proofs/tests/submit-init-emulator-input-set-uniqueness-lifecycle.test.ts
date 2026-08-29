/**
 * `input-set-uniqueness` emulator lifecycle: real faults succeed.
 *
 * Three journeys, one per sub-variant of the intra-transaction input-set rule
 * (the W-C14 single-party conversion of the validation machine's InputSets
 * `reject_duplicate_input`):
 *
 * - duplicate SPEND inputs, end to end — init → step-01 bind → step-02
 *   conviction/finalize → permanent fraud-proof token → fraudulent-block
 *   removal (state-queue NFT burned, operator slashed, token retained);
 * - duplicate REFERENCE inputs, through the decisive step-02 conviction;
 * - spend/reference OVERLAP, through the decisive step-02 conviction.
 *
 * Each journey commits its own block on a fresh harness: the thread token's
 * asset name is `categoryId ‖ header_hash`, so one committed header carries
 * exactly one thread of this family.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  requireInputSetUniquenessClaimV1,
  scanInputSetUniquenessV1,
  submitInputSetUniquenessInit,
  submitInputSetUniquenessStep01,
  submitInputSetUniquenessStep02,
} from "../src/input-set-uniqueness/index.js";
import {
  buildInputSetUniquenessFixtureV1,
  isuOutRefV1,
  makeInputSetUniquenessEmulatorHarnessV1,
  publishInputSetUniquenessReferenceScriptsV1,
  setupInputSetUniquenessScenarioV1,
} from "./support/input-set-uniqueness-emulator-v1.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  expectProofFitV1,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("input-set-uniqueness emulator lifecycle", () => {
  it("proves a duplicate spend input end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;
    // One out-ref named twice in field 0, NON-adjacent: a committed
    // accepted-but-invalid transaction owes no ordering.
    const duplicated = isuOutRefV1("11", 7);
    const fixture = await buildInputSetUniquenessFixtureV1({
      spendInputs: [duplicated, isuOutRefV1("22", 3), duplicated],
      referenceInputs: [isuOutRefV1("33", 1)],
    });
    // The prover-side scan constructs the exact claim the validator convicts.
    const claims = scanInputSetUniquenessV1({
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
    });
    expect(claims).toStrictEqual([
      { kind: "duplicateSpendInputs", firstIndex: 0n, secondIndex: 2n },
    ]);
    const claim = requireInputSetUniquenessClaimV1({
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
    });

    const { setup } = await setupInputSetUniquenessScenarioV1({
      harness,
      fixture,
    });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
    // Published only after setup: the harness's one-shot nonce is the
    // funder's first UTxO, so nothing may spend from the funder wallet
    // before `submitSetupTx` consumes it. Removal must source its seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope; the family steps are reference scripts per the standing
    // deployment ruling.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const [step01Ref, step02Ref] =
      await publishInputSetUniquenessReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;
    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInputSetUniquenessInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;
    expect(initResult.computationThreadAssetName).toBe(
      `${category.categoryId}${setup.headerHash}`,
    );

    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInputSetUniquenessStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        contracts: family,
        categoryId: category.categoryId,
        network,
        signer: proverSigner,
        threadOutRef: initResult.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.txInclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01 = step01Capture.result;
    expect(step01.stepState).toStrictEqual({ bad_tx_id: fixture.nativeTxId });

    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInputSetUniquenessStep02({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        claim,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    const step02 = step02Capture.result;
    expect(step02.badTxId).toBe(fixture.nativeTxId);
    expect(step02.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    for (const [stage, measurement] of Object.entries(proofFit)) {
      expectProofFitV1({ stage, measurement, maxTxExMem, maxTxExSteps });
    }

    // The permanent token is minted and the thread NFT is burned: no step
    // address still holds it.
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(outRefLabel(fraudProofUtxo)).toBe(step02.fraudProofOutRef);
    expect(fraudProofUtxo.assets[step02.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg: the minted token is the standing evidence that takes
    // the fraudulent state commitment off the queue. The fraud-proof token
    // itself has no burn path — it survives as permanent evidence — while
    // the state-queue node NFT carrying the fraudulent commitment burns and
    // the committing operator is slashed in the same transaction.
    const removalDeploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferenceScriptPublications.published },
    );
    const removeNow = BigInt(emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo: removalDeploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "inputSetUniqueness",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("inputSetUniqueness");
    expect(removal.fraudCategoryId).toBe(category.categoryId);
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]!.kind).toBe("remove-target");
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    // The fraudulent commitment is gone: its state-queue node NFT is burned
    // and the root no longer links to anything.
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error("Removal did not preserve the state-queue root");
    }
    const finalRoot = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        finalRootUtxo,
        harness.contracts.stateQueue.policyId,
      ),
    );
    expect(finalRoot.datum.next).toBe("Empty");

    // The committing operator (the funder signed the header) is slashed out
    // of the active set, and the scheduler rewinds to the no-operator state.
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.scheduler.spendingScriptAddress,
      toUnit(harness.contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Removal did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SDK.SchedulerDatum)).toBe(
      "NoActiveOperators",
    );

    // The fraud-proof token survives removal untouched at the same out-ref:
    // permanent evidence, not a burnable receipt.
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(step02.fraudProofOutRef);
    expect(retainedFraudProof.assets[step02.fraudProofUnit]).toBe(1n);

    // A second removal claim finds nothing left to remove.
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "inputSetUniqueness",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 600_000);

  it("convicts a duplicate reference input through the decisive step-02 conviction", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;
    // Unique spends; one out-ref named twice in field 1, non-adjacent.
    const duplicated = isuOutRefV1("66", 9);
    const fixture = await buildInputSetUniquenessFixtureV1({
      spendInputs: [isuOutRefV1("44", 0), isuOutRefV1("55", 1)],
      referenceInputs: [duplicated, isuOutRefV1("77", 2), duplicated],
    });
    const claim = requireInputSetUniquenessClaimV1({
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
    });
    expect(claim).toStrictEqual({
      kind: "duplicateReferenceInputs",
      firstIndex: 0n,
      secondIndex: 2n,
    });

    const { setup } = await setupInputSetUniquenessScenarioV1({
      harness,
      fixture,
    });
    const [step01Ref, step02Ref] =
      await publishInputSetUniquenessReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const initResult = await submitInputSetUniquenessInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step01 = await submitInputSetUniquenessStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitInputSetUniquenessStep02({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      claim,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step02.fraudProofUnit]).toBe(1n);
  }, 600_000);

  it("convicts a spend/reference overlap through the decisive step-02 conviction", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;
    // Each list is unique on its own; one out-ref appears in both. The same
    // index in the two lists naming DIFFERENT out-refs is not a fault — the
    // claim points at the matching pair, wherever it sits.
    const shared = isuOutRefV1("99", 6);
    const fixture = await buildInputSetUniquenessFixtureV1({
      spendInputs: [isuOutRefV1("88", 4), shared],
      referenceInputs: [isuOutRefV1("aa", 8), shared],
    });
    const claim = requireInputSetUniquenessClaimV1({
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
    });
    expect(claim).toStrictEqual({
      kind: "spendReferenceOverlap",
      spendIndex: 1n,
      referenceIndex: 1n,
    });

    const { setup } = await setupInputSetUniquenessScenarioV1({
      harness,
      fixture,
    });
    const [step01Ref, step02Ref] =
      await publishInputSetUniquenessReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const initResult = await submitInputSetUniquenessInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step01 = await submitInputSetUniquenessStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitInputSetUniquenessStep02({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      claim,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(fraudProofUtxo.assets[step02.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });
  }, 600_000);
});
