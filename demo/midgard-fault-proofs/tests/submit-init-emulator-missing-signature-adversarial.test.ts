/** Honest-commitment polarity: local gates plus raw on-chain fold refusal. */
import { describe, expect, it } from "vitest";

import {
  submitMissingSignatureInit,
  submitMissingSignatureStep01,
  submitMissingSignatureStep02,
  submitMissingSignatureStep03,
  submitMissingSignatureStep04,
} from "../src/missing-signature/index.js";
import {
  makeMissingSignatureEmulatorHarness,
  MISSING_SIGNATURE_TARGET_VKEY,
  publishMissingSignatureReferenceScripts,
  setupMissingSignatureScenario,
  submitRawMissingSignatureStep04,
} from "./support/missing-signature-emulator-v1.js";
import {
  expectOnchainRefusal,
  network,
} from "./support/submit-init-emulator-shared.js";

describe("missing-signature adversarial emulator polarity", () => {
  it("refuses every honest-path local forgery and rejects the guard-bypassing conviction at step-04 on-chain", async () => {
    const harness = await makeMissingSignatureEmulatorHarness({
      useScalusEvaluator: false,
    });
    const scenario = await setupMissingSignatureScenario({
      harness,
      honest: true,
    });
    if (scenario.block.txInclusion === null)
      throw new Error("missing inclusion");
    const [step01Ref, step02Ref, step03Ref, step04Ref] =
      await publishMissingSignatureReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.missingSignature,
      });
    const init = await submitMissingSignatureInit({
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
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const one = await submitMissingSignatureStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    await expect(
      submitMissingSignatureStep02({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: one.nextThreadOutRef,
        requiredSignerHashes: scenario.subject.requiredSignerHashes,
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        badRequiredSignerHashIndex: 1n,
        referenceScriptUtxo: step02Ref,
      }),
    ).rejects.toThrow(/outside the 1-item/u);
    await expect(
      submitMissingSignatureStep02({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: one.nextThreadOutRef,
        requiredSignerHashes: ["aa".repeat(28)],
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        badRequiredSignerHashIndex: 0n,
        referenceScriptUtxo: step02Ref,
      }),
    ).rejects.toThrow(/not the .* commits|not the disputed transaction/u);

    const two = await submitMissingSignatureStep02({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: one.nextThreadOutRef,
      requiredSignerHashes: scenario.subject.requiredSignerHashes,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      badRequiredSignerHashIndex: 0n,
      referenceScriptUtxo: step02Ref,
    });
    await expect(
      submitMissingSignatureStep03({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: two.nextThreadOutRef,
        missingRequiredSignerVkey: "ff".repeat(32),
        referenceScriptUtxo: step03Ref,
      }),
    ).rejects.toThrow(/not the thread's accused/u);
    const three = await submitMissingSignatureStep03({
      lucid: harness.proverLucid,
      contracts: harness.missingSignature,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: two.nextThreadOutRef,
      missingRequiredSignerVkey: MISSING_SIGNATURE_TARGET_VKEY,
      referenceScriptUtxo: step03Ref,
    });

    // Plane one: the honest builder refuses before paying.
    await expect(
      submitMissingSignatureStep04({
        lucid: harness.proverLucid,
        contracts: harness.missingSignature,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: three.nextThreadOutRef,
        addrTxWits: scenario.subject.addrTxWits,
        nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
        witnessSetCompact: scenario.subject.witnessSetCompact,
        referenceScriptUtxo: step04Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/appears in the address-witness preimage/u);

    // Plane two: a patched prover that bypasses that guard reaches the exact
    // `required_signature_is_present == False` fold and dies in step-04.
    await expect(
      expectOnchainRefusal(() =>
        submitRawMissingSignatureStep04({
          harness,
          threadOutRef: three.nextThreadOutRef,
          scenario,
          referenceScriptUtxo: step04Ref,
        }),
      ),
    ).resolves.toMatch(/failed script execution/u);
  }, 600_000);
});
