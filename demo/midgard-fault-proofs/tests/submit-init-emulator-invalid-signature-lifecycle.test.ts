/**
 * `invalid-signature` emulator lifecycle (Goal task `Q15`), both polarities.
 *
 * Three journeys against the real Aiken validators:
 *
 * - a real fault end to end — init → step-01 bind → step-02 conviction and
 *   finalize (thread NFT burned, permanent fraud-proof token minted) →
 *   fraudulent-block removal (state-queue node NFT burned, operator slashed,
 *   fraud-proof token retained, second removal finds nothing);
 * - the same fault carried at **tier 2**, selected by §8.4 on the preimage's own
 *   length: 140 committed address witnesses put field 7's §5.1 preimage over the
 *   14,336-byte tier-1 bound, so the plan publishes a `RawUtxo` carriage. No
 *   tier is forced anywhere — the count is the only input; and
 * - the adversarial polarity, where the accused commitment is **honest**. The
 *   submitter's local `verifyAddressWitness` guard refuses first; a raw driver
 *   that omits that guard reaches step-02's on-chain
 *   `verify_ed25519_signature(...) == False`, which is the check that refuses.
 *
 * Every step sources its validator from a published reference script, per the
 * standing deployment ruling.
 *
 * Kept in its own file so the leaked `@lucid-evolution/uplc` wasm heap stays far
 * below the ~4 GiB wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */
import { midgardFieldCarriageBoundsV1, outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpeningV1 } from "../src/field-opening-v1.js";
import {
  submitInvalidSignatureStep01,
  submitInvalidSignatureStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  buildInvalidSignatureSubjectV1,
  INVALID_SIGNATURE_ADDRESS_WITNESS_STRIDE_V1,
  INVALID_SIGNATURE_FIRST_RAW_WITNESS_COUNT_V1,
  makeInvalidSignatureEmulatorHarnessV1,
  publishInvalidSignatureReferenceScriptsV1,
  setupInvalidSignatureScenarioV1,
  submitRawInvalidSignatureStep02V1,
} from "./support/invalid-signature-emulator-v1.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  expectOnchainRefusalV1,
  expectProofFitV1,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("invalid-signature emulator lifecycle", () => {
  it("convicts an invalid address witness end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeInvalidSignatureEmulatorHarnessV1();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      category,
      family,
    } = harness;

    // One committed transaction, one address witness, and that witness does not
    // sign the transaction id: the minimal violation of the rule.
    const subject = await buildInvalidSignatureSubjectV1({
      accused: "invalid",
    });
    expect(subject.addrTxWits).toHaveLength(1);
    expect(
      SDK.verifyAddressWitness({
        txId: subject.nativeTxId,
        witness: subject.addrTxWits[0]!,
      }),
    ).toBe(false);
    expect(
      SDK.findInvalidAddressWitnessIndex({
        txId: subject.nativeTxId,
        addrTxWits: subject.addrTxWits,
      }),
    ).toBe(0);

    const setup = await setupInvalidSignatureScenarioV1({ harness, subject });
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
    // Published only after setup: the harness's one-shot nonce is the funder's
    // first UTxO, so nothing may spend from the funder wallet before
    // `submitSetupTx` consumes it. Removal must source its seven validators
    // from reference inputs to stay inside the 16,384-byte L1 envelope.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const [step01Ref, step02Ref] =
      await publishInvalidSignatureReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferenceScriptPublications.published },
    );

    const proofFit: Record<string, CompleteSignedTransactionMeasurement> = {};
    const { maxTxExMem, maxTxExSteps } = emulator.protocolParameters;

    const initCapture = await captureEmulatorSubmission(emulator, async () =>
      submitInit({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "invalidSignature",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        awaitConfirmation: true,
      }),
    );
    proofFit["init"] = initCapture.measurement;
    const initResult = initCapture.result;
    expect(initResult.fraudCategoryId).toBe(category.categoryId);
    expect(initResult.computationThreadAssetName).toBe(
      `${category.categoryId}${setup.headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInvalidSignatureStep01({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: subject.inclusion,
        badTxWitnessSetCompact: subject.witnessSetCompact,
        referenceScriptUtxo: step01Ref,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-01"] = step01Capture.measurement;
    const step01 = step01Capture.result;
    expect(step01.nativeTxId).toBe(subject.nativeTxId);
    // #604: the thread carries `WitnessAnchor` — the id plus the committed
    // `witness_set_hash` — which is the only reason field 7 can be opened.
    expect(step01.badTxWitnessSetHash).toBe(
      subject.inclusion.nativeTx.witness_set_hash,
    );

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Capture = await captureEmulatorSubmission(emulator, async () =>
      submitInvalidSignatureStep02({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        addrTxWitsPreimage: subject.addrTxWits,
        nativeTxCompactCbor: subject.nativeTxCompactCbor,
        witnessSetCompact: subject.witnessSetCompact,
        badAddrTxWitIndex: subject.badAddrTxWitIndex,
        referenceScriptUtxo: step02Ref,
        awaitConfirmation: true,
      }),
    );
    proofFit["step-02"] = step02Capture.measurement;
    const step02 = step02Capture.result;
    expect(step02.badTxId).toBe(subject.nativeTxId);
    expect(step02.badAddrTxWitsHash).toBe(
      subject.witnessSetCompact.addr_tx_wits_hash,
    );
    expect(step02.addrTxWitsPreimageItemCount).toBe(1);
    expect(step02.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    // A tier-1 opening publishes nothing, so the whole thread is three
    // transactions.
    expect(step02Capture.measurements).toHaveLength(1);
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

    // ——— Removal leg: the minted token is the standing evidence that takes the
    // fraudulent state commitment off the queue. The fraud-proof token itself
    // has no burn path — it survives as permanent evidence — while the
    // state-queue node NFT carrying the fraudulent commitment burns and the
    // committing operator is slashed in the same transaction.
    const removeNow = BigInt(emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidSignature",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("invalidSignature");
    expect(removal.fraudCategoryId).toBe(category.categoryId);
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]!.kind).toBe("remove-target");
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    // The fraudulent commitment is gone: its state-queue node NFT is burned and
    // the root no longer links to anything.
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

    // The committing operator (the funder signed the header) is slashed out of
    // the active set, and the scheduler rewinds to the no-operator state.
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

    // The fraud-proof token survives removal untouched at the same out-ref.
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
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "invalidSignature",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 600_000);

  it("selects a tier-2 RawUtxo carriage from the committed witness count alone and still convicts", async () => {
    const harness = await makeInvalidSignatureEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      category,
      family,
    } = harness;

    // 140 committed address witnesses: 139 that genuinely sign the transaction
    // id, then the one that does not. Nothing forces a tier — the §5.1 preimage
    // is 14,422 bytes and §8.4 partitions on that length alone.
    const witnessCount = INVALID_SIGNATURE_FIRST_RAW_WITNESS_COUNT_V1;
    expect(witnessCount).toBe(140);
    const subject = await buildInvalidSignatureSubjectV1({
      accused: "invalid",
      decoyWitnessCount: witnessCount - 1,
      spendInputByte: "77",
      fee: 17n,
    });
    expect(subject.addrTxWits).toHaveLength(witnessCount);
    expect(subject.badAddrTxWitIndex).toBe(BigInt(witnessCount - 1));
    // Exactly one violation in the whole committed collection, at the end of
    // it: the conviction has to reach witness 139 by §5.3 stride arithmetic.
    expect(
      SDK.findInvalidAddressWitnessIndex({
        txId: subject.nativeTxId,
        addrTxWits: subject.addrTxWits,
      }),
    ).toBe(witnessCount - 1);

    // The same plan the submitter will make, from the same inputs: the tier is
    // read off it, never passed to it.
    const planned = planFaultProofFieldOpeningV1({
      fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.addressWitnesses,
      anchorTxId: subject.nativeTxId,
      nativeTxCompactCbor: subject.nativeTxCompactCbor,
      itemCbors: subject.addrTxWits.map(
        SDK.encodeMidgardAddressWitnessCanonicalV1,
      ),
      owner: proverSigner.paymentKeyHash,
      witnessSet: subject.witnessSetCompact,
      anchorWitnessSetHash: subject.inclusion.nativeTx.witness_set_hash,
      label: "tier-2 selection probe",
    });
    expect(planned.itemCount).toBe(witnessCount);
    expect(planned.preimage.length).toBeGreaterThan(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    // …and one witness fewer would have fitted tier 1, so this is the first
    // count that crosses the bound.
    expect(
      planned.preimage.length - INVALID_SIGNATURE_ADDRESS_WITNESS_STRIDE_V1,
    ).toBeLessThanOrEqual(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    expect(planned.plan.tier).toBe("RawUtxo");
    expect(planned.plan.publications).toHaveLength(1);

    const setup = await setupInvalidSignatureScenarioV1({ harness, subject });
    const [step01Ref, step02Ref] =
      await publishInvalidSignatureReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
    );

    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidSignature",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    expect(initResult.fraudCategoryId).toBe(category.categoryId);
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01 = await submitInvalidSignatureStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: subject.inclusion,
      badTxWitnessSetCompact: subject.witnessSetCompact,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const step02Capture = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        submitInvalidSignatureStep02({
          lucid: proverLucid,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          threadOutRef: outRefLabel(secondStepUtxo),
          addrTxWitsPreimage: subject.addrTxWits,
          nativeTxCompactCbor: subject.nativeTxCompactCbor,
          witnessSetCompact: subject.witnessSetCompact,
          badAddrTxWitIndex: subject.badAddrTxWitIndex,
          referenceScriptUtxo: step02Ref,
          awaitConfirmation: true,
        }),
    );
    // Two submissions: the §8.4 carriage publication, then the step itself. The
    // publication is larger than the preimage and the step is far smaller than
    // it, which is the byte-level evidence that the 14,422 bytes travelled as a
    // reference input rather than in the step's own redeemer.
    expect(step02Capture.measurements).toHaveLength(2);
    const [carriagePublication, stepSubmission] = step02Capture.measurements;
    expect(carriagePublication!.completeSignedBytes).toBeGreaterThan(
      planned.preimage.length,
    );
    expect(stepSubmission!.completeSignedBytes).toBeLessThan(
      planned.preimage.length,
    );
    const step02 = step02Capture.result;
    expect(step02.addrTxWitsPreimageItemCount).toBe(witnessCount);
    expect(step02.badAddrTxWitIndex).toBe(witnessCount - 1);
    expect(step02.badAddrTxWitsHash).toBe(
      subject.witnessSetCompact.addr_tx_wits_hash,
    );
    // The published carriage is a live UTxO at the prover address carrying the
    // preimage as an inline datum; the step referenced it rather than carrying
    // the bytes.
    const carriageUtxos = (
      await proverLucid.utxosAt(proverSigner.address)
    ).filter((utxo) => utxo.datum != null);
    expect(carriageUtxos).toHaveLength(1);

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

  it("refuses an attack on an honest commitment at step-02's on-chain Ed25519 check", async () => {
    const harness = await makeInvalidSignatureEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
    } = harness;

    // The committed transaction is honest: its sole address witness genuinely
    // signs the transaction id. Nothing about the block violates the rule.
    const subject = await buildInvalidSignatureSubjectV1({
      accused: "honest",
      spendInputByte: "99",
      fee: 19n,
    });
    expect(
      SDK.nativeTxHasInvalidSignatureViolation({
        txId: subject.nativeTxId,
        addrTxWits: subject.addrTxWits,
      }),
    ).toBe(false);

    const setup = await setupInvalidSignatureScenarioV1({ harness, subject });
    const [step01Ref, step02Ref] =
      await publishInvalidSignatureReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
    );

    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidSignature",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    // Step-01 binds an honest transaction perfectly well: the rule it forwards
    // is about the witness set's contents, which nothing has read yet.
    const step01 = await submitInvalidSignatureStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: subject.inclusion,
      badTxWitnessSetCompact: subject.witnessSetCompact,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const threadOutRef = outRefLabel(secondStepUtxo);

    // Plane one: the honest builder refuses before paying — both for the
    // accusation itself and for an index the committed collection never had.
    await expect(
      submitInvalidSignatureStep02({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef,
        addrTxWitsPreimage: subject.addrTxWits,
        nativeTxCompactCbor: subject.nativeTxCompactCbor,
        witnessSetCompact: subject.witnessSetCompact,
        badAddrTxWitIndex: 0n,
        referenceScriptUtxo: step02Ref,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/signs transaction .* validly/u);
    await expect(
      submitInvalidSignatureStep02({
        lucid: proverLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef,
        addrTxWitsPreimage: subject.addrTxWits,
        nativeTxCompactCbor: subject.nativeTxCompactCbor,
        witnessSetCompact: subject.witnessSetCompact,
        badAddrTxWitIndex: 5n,
        referenceScriptUtxo: step02Ref,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(/is out of range for a 1-witness preimage/u);

    // Plane two: a patched prover that bypasses the local guard reaches
    // step-02's `verify_ed25519_signature(...) == False` and dies there. The
    // honest operator keeps its block. The crash is a SPEND validator's, not a
    // mint policy's and not the off-chain builder's.
    //
    // The absolute redeemer index is deliberately not asserted. The
    // claim-registry close this step now carries spends the registry singleton
    // alongside the computation thread, and the two script inputs sort against
    // each other by out-ref — which the emulator re-derives on every run, so
    // the thread lands on `Spend[0]` or `Spend[1]` depending on the run
    // (measured 2026-08-31: two of three consecutive runs reported `Spend[0]`,
    // the third `Spend[1]`). Pinning either number makes this row a coin flip.
    // The companion journey below runs this very driver against a genuinely
    // invalid witness and it succeeds, which is what isolates the refusal to
    // the signature check.
    const refusal = await expectOnchainRefusalV1(() =>
      submitRawInvalidSignatureStep02V1({
        harness,
        deploymentInfo,
        threadOutRef,
        subject,
        referenceScriptUtxo: step02Ref,
      }),
    );
    expect(refusal).toMatch(/failed script execution/u);
    expect(refusal).toMatch(/Spend\[\d+\]/u);
    expect(refusal).not.toMatch(/Mint\[/u);

    // Nothing was minted and nothing was burned: the thread is still parked at
    // step-02 and no fraud-proof token exists.
    await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      initResult.computationThreadUnit,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          harness.contracts.fraudProof.policyId,
          initResult.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);
  it("convicts through the same raw driver when the accused witness is genuinely invalid, isolating the refusal above to the signature check", async () => {
    const harness = await makeInvalidSignatureEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
    } = harness;

    // The control for the adversarial journey: identical raw transaction shape,
    // identical §8.8 opening, identical layout — the accused witness is the only
    // thing that differs, and here it genuinely fails to sign the transaction
    // id. It convicts, so nothing but `verify_ed25519_signature(...) == False`
    // can be what refused the honest commitment.
    const subject = await buildInvalidSignatureSubjectV1({
      accused: "invalid",
      spendInputByte: "bb",
      fee: 23n,
    });
    const setup = await setupInvalidSignatureScenarioV1({ harness, subject });
    const [step01Ref, step02Ref] =
      await publishInvalidSignatureReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
    );
    const initResult = await submitInit({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "invalidSignature",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const step01 = await submitInvalidSignatureStep01({
      lucid: proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: subject.inclusion,
      badTxWitnessSetCompact: subject.witnessSetCompact,
      referenceScriptUtxo: step01Ref,
      awaitConfirmation: true,
    });
    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01.secondStepAddress,
      initResult.computationThreadUnit,
    );
    const txHash = await submitRawInvalidSignatureStep02V1({
      harness,
      deploymentInfo,
      threadOutRef: outRefLabel(secondStepUtxo),
      subject,
      referenceScriptUtxo: step02Ref,
    });
    expect(txHash).toHaveLength(64);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      harness.contracts.fraudProof.spendingScriptAddress,
      toUnit(
        harness.contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
  }, 600_000);
});
