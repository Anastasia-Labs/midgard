/** Cancellation, crash-resume, and fail-closed evidence coverage. */
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  proveMissingSignatureFaultV1,
  submitMissingSignatureCancel,
  submitMissingSignatureInit,
  submitMissingSignatureStep01,
  submitMissingSignatureStep02,
  submitMissingSignatureStep03,
  submitMissingSignatureStep04,
} from "../src/missing-signature/index.js";
import {
  makeMissingSignatureEmulatorHarnessV1,
  MISSING_SIGNATURE_TARGET_VKEY_V1,
  missingSignatureFindingV1,
  missingSignatureProverDepsV1,
  publishMissingSignatureReferenceScriptsV1,
  setupMissingSignatureScenarioV1,
} from "./support/missing-signature-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

type Harness = Awaited<
  ReturnType<typeof makeMissingSignatureEmulatorHarnessV1>
>;
type Scenario = Awaited<ReturnType<typeof setupMissingSignatureScenarioV1>>;
type References = readonly [UTxO, UTxO, UTxO, UTxO];

const init = async (harness: Harness, scenario: Scenario) =>
  submitMissingSignatureInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.missingSignature,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  });

const advanceTo = async (
  harness: Harness,
  scenario: Scenario,
  references: References,
  stepIndex: 0 | 1 | 2 | 3,
): Promise<string> => {
  if (scenario.block.txInclusion === null) throw new Error("missing inclusion");
  let outRef = (await init(harness, scenario)).nextThreadOutRef;
  if (stepIndex === 0) return outRef;
  outRef = (
    await submitMissingSignatureStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.block.txInclusion,
      referenceScriptUtxo: references[0],
    })
  ).nextThreadOutRef;
  if (stepIndex === 1) return outRef;
  outRef = (
    await submitMissingSignatureStep02({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRef,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      badRequiredSignerHashIndex: 0n,
      referenceScriptUtxo: references[1],
    })
  ).nextThreadOutRef;
  if (stepIndex === 2) return outRef;
  return (
    await submitMissingSignatureStep03({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRef,
      missingRequiredSignerVkey: MISSING_SIGNATURE_TARGET_VKEY_V1,
      referenceScriptUtxo: references[2],
    })
  ).nextThreadOutRef;
};

describe("missing-signature negatives and resume", () => {
  it("cancels at every step and re-initializes after each explicit abort", async () => {
    const harness = await makeMissingSignatureEmulatorHarnessV1();
    const scenario = await setupMissingSignatureScenarioV1({ harness });
    const references = await publishMissingSignatureReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.missingSignature,
    });
    const unit = toUnit(
      harness.missingSignature.computationThread.policyId,
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
    for (const stepIndex of [0, 1, 2, 3] as const) {
      const threadOutRef = await advanceTo(
        harness,
        scenario,
        references,
        stepIndex,
      );
      const cancelled = await submitMissingSignatureCancel({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex],
      });
      expect(cancelled.cancelledStepIndex).toBe(stepIndex);
      for (const step of harness.missingSignature.steps) {
        await expect(
          harness.proverLucid.utxosAtWithUnit(step.spendingScriptAddress, unit),
        ).resolves.toHaveLength(0);
      }
    }
  }, 600_000);

  it("reconstructs and completes from each of the four holding addresses", async () => {
    for (const stepIndex of [0, 1, 2, 3] as const) {
      const harness = await makeMissingSignatureEmulatorHarnessV1();
      const scenario = await setupMissingSignatureScenarioV1({ harness });
      const references = await publishMissingSignatureReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
      await advanceTo(harness, scenario, references, stepIndex);
      const outcome = await Effect.runPromise(
        proveMissingSignatureFaultV1(
          missingSignatureFindingV1(scenario),
          missingSignatureProverDepsV1({
            harness,
            scenario,
            referenceScriptUtxos: {
              step01: references[0],
              step02: references[1],
              step03: references[2],
              step04: references[3],
            },
          }),
        ),
      );
      expect(outcome.kind).toBe("proven");
      if (outcome.kind === "proven") {
        expect(outcome.txHashes).toHaveLength(4 - stepIndex);
      }
    }
  }, 600_000);

  it("resumes and explicitly cancels from an authenticated interior step-04 checkpoint", async () => {
    const buildInterior = async () => {
      const harness = await makeMissingSignatureEmulatorHarnessV1();
      const scenario = await setupMissingSignatureScenarioV1({
        harness,
        decoyWitnessCount: 64,
      });
      const references = await publishMissingSignatureReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
      const step04OutRef = await advanceTo(harness, scenario, references, 3);
      const scan = await submitMissingSignatureStep04({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step04OutRef,
        addrTxWits: scenario.subject.addrTxWits,
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        witnessSetCompact: scenario.subject.witnessSetCompact,
        referenceScriptUtxo: references[3],
      });
      if (scan.kind !== "advanced") {
        throw new Error("64-witness subject did not produce an interior scan");
      }
      expect(scan.nextItemIndex).toBe(32);
      return { harness, scenario, references, scan };
    };

    const resumable = await buildInterior();
    const resumed = await Effect.runPromise(
      proveMissingSignatureFaultV1(
        missingSignatureFindingV1(resumable.scenario),
        missingSignatureProverDepsV1({
          harness: resumable.harness,
          scenario: resumable.scenario,
          referenceScriptUtxos: {
            step01: resumable.references[0],
            step02: resumable.references[1],
            step03: resumable.references[2],
            step04: resumable.references[3],
          },
        }),
      ),
    );
    expect(resumed.kind).toBe("proven");
    if (resumed.kind === "proven") expect(resumed.txHashes).toHaveLength(1);

    const cancellable = await buildInterior();
    const cancelled = await submitMissingSignatureCancel({
      lucid: cancellable.harness.proverLucid,
      contracts: cancellable.harness.missingSignature,
      categoryId: cancellable.harness.category.categoryId,
      signer: cancellable.harness.proverSigner,
      threadOutRef: cancellable.scan.nextThreadOutRef,
      referenceScriptUtxo: cancellable.references[3],
    });
    expect(cancelled.cancelledStepIndex).toBe(3);
    const unit = toUnit(
      cancellable.harness.missingSignature.computationThread.policyId,
      `${cancellable.harness.category.categoryId}${cancellable.scenario.setup.headerHash}`,
    );
    await expect(
      cancellable.harness.proverLucid.utxosAtWithUnit(
        cancellable.harness.missingSignature.steps[3].spendingScriptAddress,
        unit,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);

  it("refuses out-of-range, stale compact, wrong-vkey, and foreign witness evidence before submission", async () => {
    const harness = await makeMissingSignatureEmulatorHarnessV1();
    const scenario = await setupMissingSignatureScenarioV1({ harness });
    const references = await publishMissingSignatureReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.missingSignature,
    });
    const step02OutRef = await advanceTo(harness, scenario, references, 1);
    await expect(
      submitMissingSignatureStep02({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02OutRef,
        requiredSignerHashes: scenario.subject.requiredSignerHashes,
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        badRequiredSignerHashIndex: 1n,
        referenceScriptUtxo: references[1],
      }),
    ).rejects.toThrow(/outside the 1-item/u);
    const stale = `00${scenario.block.nativeTxCompactCbor.slice(2)}`;
    await expect(
      submitMissingSignatureStep02({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02OutRef,
        requiredSignerHashes: scenario.subject.requiredSignerHashes,
        nativeTxCompactCbor: stale,
        badRequiredSignerHashIndex: 0n,
        referenceScriptUtxo: references[1],
      }),
    ).rejects.toThrow(/re-derives|cbor|compact/iu);
    const step03 = await submitMissingSignatureStep02({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02OutRef,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      badRequiredSignerHashIndex: 0n,
      referenceScriptUtxo: references[1],
    });
    await expect(
      submitMissingSignatureStep03({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step03.nextThreadOutRef,
        missingRequiredSignerVkey: "ff".repeat(32),
        referenceScriptUtxo: references[2],
      }),
    ).rejects.toThrow(/not the thread's accused/u);
    const step04 = await submitMissingSignatureStep03({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      missingRequiredSignerVkey: MISSING_SIGNATURE_TARGET_VKEY_V1,
      referenceScriptUtxo: references[2],
    });
    await expect(
      submitMissingSignatureStep04({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step04.nextThreadOutRef,
        addrTxWits: scenario.subject.addrTxWits,
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        witnessSetCompact: {
          ...scenario.subject.witnessSetCompact,
          addr_tx_wits_hash: "ff".repeat(32),
        },
        referenceScriptUtxo: references[3],
      }),
    ).rejects.toThrow(/compact witness set hashes|not the/u);
  }, 600_000);
});
