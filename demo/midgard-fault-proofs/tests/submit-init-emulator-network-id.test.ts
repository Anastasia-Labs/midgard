import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum, NetworkIdStep02Datum } from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  planNetworkIdOutputsOpening,
  submitNetworkIdCancel,
  submitNetworkIdInit,
  submitNetworkIdStep01,
  submitNetworkIdStep02,
} from "../src/network-id/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  buildNetworkIdFixture,
  makeNetworkIdEmulatorHarness,
  NETWORK_ID_EMULATOR_CATEGORY_ID,
  publishNetworkIdReferenceScripts,
} from "./support/network-id-emulator.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const measuredFit = createMeasuredFitRecorder(
  "network-id-wrongful-rejection",
  "accepted",
  "accepted-invalid network nibble, correction",
);

describe("Q35 network-id real-fault lifecycle", () => {
  it.each([
    {
      label: "raw unprotected network nibble 2",
      outputNetworkId: 2,
      protectedAddress: false,
    },
    {
      label: "raw protected network nibble 15",
      outputNetworkId: 7,
      protectedAddress: true,
    },
  ])(
    "mints permanent evidence and removes $label",
    async ({ outputNetworkId, protectedAddress }) => {
      const harness = await makeNetworkIdEmulatorHarness();
      const submitMeasured = async <T>(
        stage: string,
        operation: () => Promise<T>,
      ) => {
        const captured = await captureEmulatorSubmission(
          harness.emulator,
          operation,
        );
        captured.measurements.forEach((measurement, index) =>
          measuredFit.record(
            `${outputNetworkId}-${protectedAddress}/${stage}-${index}`,
            measurement,
            measurement.executionMemory === 0n ? "publication" : "lifecycle",
          ),
        );
        return captured.result;
      };
      const preSubmitStages: string[] = [];
      const preSubmitBoundary =
        (stage: string) =>
        (transaction: {
          readonly txHash: string;
          readonly referenceScripts: readonly unknown[];
        }) => {
          expect(transaction.txHash).toMatch(/^[0-9a-f]{64}$/u);
          expect(transaction.referenceScripts.length).toBeGreaterThan(0);
          preSubmitStages.push(stage);
        };
      const [step01Ref, step02Ref] = await publishNetworkIdReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.networkId,
      });
      const removalReferenceScripts = await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      });
      const fixture = await buildNetworkIdFixture({
        outputNetworkId,
        protectedAddress,
      });
      const setup = await setupFraudulentBlock({
        funderLucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        catalogue: harness.catalogue,
        fixture,
      });
      const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
      const init = await submitMeasured("init", () =>
        submitNetworkIdInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: harness.networkId,
          category: harness.category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          preSubmitBoundary: preSubmitBoundary("init"),
        }),
      );
      expect(init.fraudCategoryId).toBe(NETWORK_ID_EMULATOR_CATEGORY_ID);

      const firstStep = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        init.firstStepAddress,
        init.computationThreadUnit,
      );
      const step01 = await submitMeasured("step01", () =>
        submitNetworkIdStep01({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(firstStep),
          stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
          prepared,
          referenceScriptUtxo: step01Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          preSubmitBoundary: preSubmitBoundary("step01"),
        }),
      );
      const secondStep = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        step01.secondStepAddress,
        init.computationThreadUnit,
      );
      expect(Data.from(secondStep.datum!, NetworkIdStep02Datum)).toEqual({
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: step01.state,
      });

      const opening = planNetworkIdOutputsOpening({
        prepared,
        owner: harness.proverSigner.paymentKeyHash,
      });
      const step02 = await submitMeasured("step02", () =>
        submitNetworkIdStep02({
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(secondStep),
          prepared,
          outputsOpeningPlan: opening,
          referenceScriptUtxo: step02Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          preSubmitBoundary: preSubmitBoundary("step02"),
        }),
      );
      expect(step02.outputOpeningTier).toBe("Inline");
      expect(step02.fraudProofUnit).toBe(
        toUnit(
          harness.networkId.fraudProof.policyId,
          init.computationThreadAssetName,
        ),
      );
      const proofUtxo = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        step02.fraudProofAddress,
        step02.fraudProofUnit,
      );
      const paymentCredential = getAddressDetails(
        await harness.proverLucid.wallet().address(),
      ).paymentCredential;
      if (paymentCredential?.type !== "Key") {
        throw new Error("expected prover payment key credential");
      }
      expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
        fraud_prover: paymentCredential.hash,
      });

      const baseDeployment = buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalReferenceScripts.published },
      );
      const deploymentInfo = {
        ...baseDeployment,
        contracts: {
          ...baseDeployment.contracts,
          fraudProofNetworkId: {
            scriptHash: harness.networkId.steps[0].spendingScriptHash,
          },
          fraudProofNetworkIdStep02: {
            scriptHash: harness.networkId.steps[1].spendingScriptHash,
          },
        },
      };
      const removeNow = BigInt(harness.emulator.now());
      const removal = await submitMeasured("removal", () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "networkId",
          fraudulentHeaderHash: setup.headerHash,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
          preSubmitBoundary: preSubmitBoundary("remove"),
        }),
      );
      expect(removal.fraudCategory).toBe("networkId");
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
            step02.fraudProofAddress,
            step02.fraudProofUnit,
          ),
        ),
      ).toBe(outRefLabel(proofUtxo));
      expect(preSubmitStages).toEqual(["init", "step01", "step02", "remove"]);
    },
    240_000,
  );

  it("refuses a protected matching-network output before submission", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const [step01Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const fixture = await buildNetworkIdFixture({ outputNetworkId: 0 });
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const firstStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    await expect(
      submitNetworkIdStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        prepared,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow("does not contain the requested fault");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 180_000);

  it("cancels the reference-script-only thread without minting evidence", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const [step01Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const fixture = await buildNetworkIdFixture();
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const firstStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    let boundaryCalled = false;
    const cancelled = await submitNetworkIdCancel({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(firstStep),
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      preSubmitBoundary: () => {
        boundaryCalled = true;
      },
    });
    expect(boundaryCalled).toBe(true);
    expect(cancelled.cancelledStepIndex).toBe(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.networkId.fraudProof.spendingScriptAddress,
        toUnit(
          harness.networkId.fraudProof.policyId,
          init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);
});
