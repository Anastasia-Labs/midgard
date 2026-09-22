import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitNativeScriptInvalidInit,
  submitNativeScriptInvalidStep01,
  submitNativeScriptInvalidStep02,
  submitNativeScriptInvalidStep03,
  submitNativeScriptInvalidStep03StartSignerScan,
  submitNativeScriptInvalidStep04,
  submitNativeScriptInvalidStep05,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { parseSubmitStep01TxInclusion } from "../src/submit-step-01.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { VAN_ROSSEM_TRANSACTION_LIMITS } from "./support/emulator/protocol-parameters.js";
import {
  buildNativeScriptInvalidEmulatorFixture,
  makeNativeScriptInvalidEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("native-script-invalid standalone emulator lifecycle", () => {
  it("rejects a forced 29-signer direct submission before touching builder dependencies", async () => {
    const fixture = await buildNativeScriptInvalidEmulatorFixture({
      signerCount: 29,
    });
    const unreachable = null as never;
    await expect(
      submitNativeScriptInvalidStep03({
        lucid: unreachable,
        contracts: unreachable,
        categoryId: "unreachable",
        signer: unreachable,
        threadOutRef: "unreachable#0",
        nativeTxCompactCbor: fixture.prepared.nativeTxCompactCbor,
        witnessSet: fixture.witnessSet,
        scriptItemCbor: fixture.scriptItem,
        addressWitnessItems: fixture.addressWitnessItems,
        addressWitnessVerificationKeys: fixture.addressWitnessVerificationKeys,
        referenceScriptUtxo: unreachable,
        witnessReferenceScripts: unreachable,
      }),
    ).rejects.toThrow(
      "native-script-invalid: direct signer limit is 28; use the staged route",
    );
  });

  it("proves a direct invalid native script, removes the header, and retains permanent evidence", async () => {
    const harness = await makeNativeScriptInvalidEmulatorHarness();
    const refs = await publishFinalFamilyReferenceScripts({
      lucid: harness.proverLucid,
      family: harness.family,
      label: "native-script-invalid",
    });
    const fixture = await buildNativeScriptInvalidEmulatorFixture({
      signerCount: 28,
    });
    harness.emulator.awaitSlot(1);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
    const txInclusion = parseSubmitStep01TxInclusion(prepared.txInclusion);
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
    );
    const initParams = {
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    } as const;

    const init = await submitNativeScriptInvalidInit(initParams);
    const step01 = await submitNativeScriptInvalidStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step02 = await submitNativeScriptInvalidStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      nativeTxCompactCbor: prepared.nativeTxCompactCbor,
      witnessSet: fixture.witnessSet,
      scriptWitnessItems: fixture.scriptWitnessItems,
      scriptIndex: prepared.scriptIndex,
      referenceScriptUtxo: refs[1],
    });
    const proof = await submitNativeScriptInvalidStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: prepared.nativeTxCompactCbor,
      witnessSet: fixture.witnessSet,
      scriptItemCbor: fixture.scriptItem,
      addressWitnessItems: fixture.addressWitnessItems,
      addressWitnessVerificationKeys: fixture.addressWitnessVerificationKeys,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      proof.fraudProofUnit,
    );
    expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
    });

    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalRefs.published },
      ),
      network,
      signer: harness.proverSigner,
      fraudCategory: "nativeScriptInvalid",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    expect(
      outRefLabel(
        await expectSingleUtxoWithUnit(
          harness.proverLucid,
          harness.family.fraudProof.spendingScriptAddress,
          proof.fraudProofUnit,
        ),
      ),
    ).toBe(outRefLabel(proofUtxo));
  }, 300_000);

  it.each([29, 33])(
    "routes %i signers through bounded staged transactions with Van Rossem headroom",
    async (signerCount) => {
      const harness = await makeNativeScriptInvalidEmulatorHarness();
      const refs = await publishFinalFamilyReferenceScripts({
        lucid: harness.proverLucid,
        family: harness.family,
        label: "native-script-invalid-staged",
      });
      const fixture = await buildNativeScriptInvalidEmulatorFixture({
        signerCount,
      });
      harness.emulator.awaitSlot(1);
      const setup = await setupFraudulentBlock({
        funderLucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        catalogue: harness.catalogue,
        fixture,
      });
      const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
      const common = {
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
      } as const;
      const init = await submitNativeScriptInvalidInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
        ),
        network,
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const step01 = await submitNativeScriptInvalidStep01({
        ...common,
        blueprint: harness.realBlueprint,
        network,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(prepared.txInclusion),
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const step02 = await submitNativeScriptInvalidStep02({
        ...common,
        threadOutRef: step01.nextThreadOutRef,
        nativeTxCompactCbor: prepared.nativeTxCompactCbor,
        witnessSet: fixture.witnessSet,
        scriptWitnessItems: fixture.scriptWitnessItems,
        scriptIndex: prepared.scriptIndex,
        referenceScriptUtxo: refs[1],
      });
      const start = await captureEmulatorSubmission(
        harness.emulator,
        async () =>
          await submitNativeScriptInvalidStep03StartSignerScan({
            ...common,
            threadOutRef: step02.nextThreadOutRef,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            witnessSet: fixture.witnessSet,
            scriptItemCbor: fixture.scriptItem,
            addressWitnessItems: fixture.addressWitnessItems,
            referenceScriptUtxo: refs[2],
          }),
      );
      expect(start.result.signerCount).toBe(16n);

      let signerThreadOutRef = start.result.nextThreadOutRef;
      const signerMeasurements = [start.measurement];
      const signerActions = ["StartSignerScan"];
      let signerAction = "";
      do {
        const stage = await captureEmulatorSubmission(
          harness.emulator,
          async () =>
            await submitNativeScriptInvalidStep04({
              ...common,
              threadOutRef: signerThreadOutRef,
              nativeTxCompactCbor: prepared.nativeTxCompactCbor,
              witnessSet: fixture.witnessSet,
              addressWitnessItems: fixture.addressWitnessItems,
              referenceScriptUtxo: refs[3],
            }),
        );
        signerThreadOutRef = stage.result.nextThreadOutRef;
        signerAction = stage.result.action;
        signerActions.push(signerAction);
        signerMeasurements.push(stage.measurement);
      } while (signerAction !== "FinalizeSignerScan");

      const evaluatorMeasurements = [];
      const evaluatorActions: string[] = [];
      let evaluatorThreadOutRef = signerThreadOutRef;
      let proofUnit: string | undefined;
      while (proofUnit === undefined) {
        const stage = await captureEmulatorSubmission(
          harness.emulator,
          async () =>
            await submitNativeScriptInvalidStep05({
              ...common,
              threadOutRef: evaluatorThreadOutRef,
              scriptItemCbor: fixture.scriptItem,
              addressWitnessItems: fixture.addressWitnessItems,
              referenceScriptUtxo: refs[4],
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
        );
        evaluatorMeasurements.push(stage.measurement);
        evaluatorActions.push(stage.result.action);
        if ("fraudProofUnit" in stage.result) {
          proofUnit = stage.result.fraudProofUnit;
        } else {
          evaluatorThreadOutRef = stage.result.nextThreadOutRef;
        }
      }
      expect(proofUnit).toHaveLength(120);
      expect(signerActions).toEqual(
        signerCount === 29
          ? ["StartSignerScan", "FinalizeSignerScan"]
          : ["StartSignerScan", "ResumeSignerScan", "FinalizeSignerScan"],
      );
      // Every resumed evaluator call intentionally omits cursor bytes and frames:
      // the builder must reconstruct them solely from the authenticated thread hash.
      expect(evaluatorActions).toEqual([
        "StartScriptScan",
        "ResumeScriptScan",
        "ResumeScriptScan",
        "FinalizeScriptScan",
      ]);
      const measurements = [...signerMeasurements, ...evaluatorMeasurements];
      expect(measurements).toHaveLength(signerCount === 29 ? 6 : 7);
      for (const measurement of measurements) {
        expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
          VAN_ROSSEM_TRANSACTION_LIMITS.maxTxSize / 2,
        );
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          (VAN_ROSSEM_TRANSACTION_LIMITS.maxTxExMem * 3n) / 5n,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_TRANSACTION_LIMITS.maxTxExSteps / 2n,
        );
      }
    },
    300_000,
  );
});
