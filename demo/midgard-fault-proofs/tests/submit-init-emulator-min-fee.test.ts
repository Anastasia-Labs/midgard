/** Real lifecycle for the registered standalone single-party min-fee proof. */
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/index.js";
import "../src/linear-fault-cancel.js";
import "../src/min-fee-contracts.js";
import "../src/step-support.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./support/submit-init-emulator-shared.js";
import "./submit-init-emulator-min-fee.setup-scenario.js";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitMinFeeStep01,
  submitMinFeeStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { submitLinearFaultCancel } from "../src/linear-fault-cancel.js";
import { MIN_FEE_CATEGORY_LABEL } from "../src/min-fee-contracts.js";
import type { MinFeeFieldItemCbors } from "../src/submit-min-fee-step-02.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  advanceStep01,
  initThread,
  makeHarness,
  setupScenario,
  TIER2_SPEND_INPUT_COUNT,
} from "./submit-init-emulator-min-fee.setup-scenario.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("min-fee emulator lifecycle", () => {
  it("cancels both steps, resumes the same thread, rejects malformed evidence, mints, and removes", async () => {
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 999n,
      headerMinimum: 1_000n,
    });
    const sharedCancel = {
      lucid: harness.proverLucid,
      family: MIN_FEE_CATEGORY_LABEL,
      steps: harness.minFee.steps,
      computationThread: harness.minFee.computationThread,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    };

    const atStep01 = await initThread(harness, scenario);
    const cancel01 = await submitLinearFaultCancel({
      ...sharedCancel,
      threadOutRef: atStep01.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[0],
    });
    expect(cancel01.cancelledStepIndex).toBe(0);

    const again = await initThread(harness, scenario);
    const atStep02ForCancel = await advanceStep01(
      harness,
      scenario,
      again.nextThreadOutRef,
    );
    const cancel02 = await submitLinearFaultCancel({
      ...sharedCancel,
      threadOutRef: atStep02ForCancel.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[1],
    });
    expect(cancel02.cancelledStepIndex).toBe(1);

    const wrongReference = await initThread(harness, scenario);
    await expect(
      submitMinFeeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: wrongReference.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.txInclusion,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/reference script .* hashes to/u);
    await submitLinearFaultCancel({
      ...sharedCancel,
      threadOutRef: wrongReference.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[0],
    });

    const resumed = await initThread(harness, scenario);
    const bound = await advanceStep01(
      harness,
      scenario,
      resumed.nextThreadOutRef,
    );
    expect(bound.computationThreadUnit).toBe(resumed.computationThreadUnit);

    const missing = scenario.fieldItemCbors.slice(0, 8);
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: missing as unknown as MinFeeFieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/exactly nine/u);

    const permuted = [...scenario.fieldItemCbors];
    [permuted[0], permuted[1]] = [permuted[1]!, permuted[0]!];
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: permuted as unknown as MinFeeFieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/field 0|field 1/u);

    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: {
          ...scenario.prepared.tx.witnessSet,
          script_tx_wits_hash: "99".repeat(32),
        },
        fieldItemCbors: scenario.fieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/compact transaction commits/u);

    const wrongSigner = {
      ...harness.proverSigner,
      paymentKeyHash: "99".repeat(28),
    };
    await expect(
      submitLinearFaultCancel({
        ...sharedCancel,
        signer: wrongSigner,
        threadOutRef: bound.nextThreadOutRef,
        referenceScriptUtxo: scenario.refs[1],
      }),
    ).rejects.toThrow(/min-fee: signer does not own thread/u);

    const finalized = await submitMinFeeStep02({
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      witnessSet: scenario.prepared.tx.witnessSet,
      fieldItemCbors: scenario.fieldItemCbors,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(finalized.fee).toBe(999n);
    expect(finalized.minimumFee).toBe(1_000n);
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(finalized.fraudProofOutRef);
    expect(Data.from(proofUtxo.datum!, SDK.FraudProofTokenDatum)).toStrictEqual(
      { fraud_prover: harness.proverSigner.paymentKeyHash },
    );

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "minFee",
      fraudulentHeaderHash: scenario.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudCategory).toBe("minFee");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(finalized.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "minFee",
        fraudulentHeaderHash: scenario.setup.headerHash,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("routes an oversized field-0 preimage through tier-2 published carriage to the conviction", async () => {
    // 365 committed spend inputs put field 0's §5.1 preimage at 14,603 bytes —
    // past §8.4's 14,336-byte tier-1 redeemer bound, inside the
    // single-publication tier-2 window — so the ladder itself routes that one
    // field to `RawUtxo` while the other eight ride inline. Nothing forces
    // the tier; the committed data's size does.
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 999n,
      headerMinimum: 1_000n,
      spendInputs: Array.from({ length: TIER2_SPEND_INPUT_COUNT }, (_, index) =>
        outRefCbor(0x31, BigInt(index)),
      ),
    });
    const preimageBytes = encodeMidgardFieldPreimage(
      scenario.fieldItemCbors[0],
    ).length;
    expect(preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimageBytes).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);

    const init = await initThread(harness, scenario);
    const bound = await advanceStep01(harness, scenario, init.nextThreadOutRef);
    const finalized = await submitMinFeeStep02({
      lucid: harness.proverLucid,
      contracts: harness.minFee,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
      witnessSet: scenario.prepared.tx.witnessSet,
      fieldItemCbors: scenario.fieldItemCbors,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(finalized.fee).toBe(999n);
    expect(finalized.minimumFee).toBe(1_000n);
    expect(finalized.fieldCarriageTiers[0]).toBe("RawUtxo");
    expect(
      finalized.fieldCarriageTiers.slice(1).every((tier) => tier === "Inline"),
    ).toBe(true);
    expect(finalized.fieldPreimageLengths[0]).toBe(preimageBytes);

    // The tier-2 publication really exists at the prover's address as a
    // bytes-only inline datum, referenced rather than spent by the step.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCbor(
      encodeMidgardFieldPreimage(scenario.fieldItemCbors[0]),
    );
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.minFee.fraudProof.spendingScriptAddress,
      finalized.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(finalized.fraudProofOutRef);
  }, 600_000);

  it("reaches step-02 and lets the compiled validator refuse an honest exact fee", async () => {
    const harness = await makeHarness();
    const scenario = await setupScenario({
      harness,
      fee: 1_000n,
      headerMinimum: 1_000n,
    });
    const init = await initThread(harness, scenario);
    const bound = await advanceStep01(harness, scenario, init.nextThreadOutRef);
    await expect(
      submitMinFeeStep02({
        lucid: harness.proverLucid,
        contracts: harness.minFee,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bound.nextThreadOutRef,
        nativeTxCompactCbor: scenario.prepared.tx.nativeTxCompactCbor,
        witnessSet: scenario.prepared.tx.witnessSet,
        fieldItemCbors: scenario.fieldItemCbors,
        referenceScriptUtxo: scenario.refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
        unsafeSkipLocalViolationCheckForTest: true,
      }),
    ).rejects.toThrow();
    const fraudUnit = toUnit(
      harness.minFee.fraudProof.policyId,
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.minFee.fraudProof.spendingScriptAddress,
        fraudUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
    const cancelled = await submitLinearFaultCancel({
      lucid: harness.proverLucid,
      family: MIN_FEE_CATEGORY_LABEL,
      steps: harness.minFee.steps,
      computationThread: harness.minFee.computationThread,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      referenceScriptUtxo: scenario.refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(1);
  }, 600_000);
});
