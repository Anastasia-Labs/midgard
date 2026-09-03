import { encodeMidgardNativeScript } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  prepareMissingNativeScriptTx,
  submitMissingNativeScriptTxCancel,
  submitMissingNativeScriptTxInit,
  submitMissingNativeScriptTxStep01,
  submitMissingNativeScriptTxStep02,
  submitMissingNativeScriptTxStep03,
  submitMissingNativeScriptTxStep04,
  submitMissingNativeScriptTxStep05,
  submitMissingNativeScriptTxStep06,
} from "../src/index.js";
import {
  fundMissingNativeScriptTxOutsider,
  makeMissingNativeScriptTxEmulatorHarness,
  publishMissingNativeScriptTxReferenceScripts,
  setupMissingNativeScriptTxFixture,
  submitRawMissingNativeScriptTxOutsiderCancel,
  submitRawMissingNativeScriptTxStep03,
  submitRawMissingNativeScriptTxStep04,
  submitRawMissingNativeScriptTxStep05,
  submitRawMissingNativeScriptTxStep06,
} from "./support/missing-native-script-tx-emulator-v1.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

describe("missing-native-script-tx adversarial emulator", () => {
  it("refuses an honest present script on-chain and pins every earlier negative/cancel gate", async () => {
    const harness = await makeMissingNativeScriptTxEmulatorHarness();
    const fixture = await setupMissingNativeScriptTxFixture({
      harness,
      scriptPresent: true,
    });
    await fundMissingNativeScriptTxOutsider(harness);
    const refs = await publishMissingNativeScriptTxReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const badInclusion = fixture.block.txInclusion;
    const producingInclusion = fixture.block.txInclusions.get(
      fixture.producingTxId,
    );
    if (badInclusion === null || producingInclusion === undefined) {
      throw new Error("two-transaction fixture is missing inclusion evidence");
    }
    const mismatchedScriptBytes = encodeMidgardNativeScript({
      type: "sig",
      keyHash: Buffer.from("77".repeat(28), "hex"),
    });
    const evidenceArgs = {
      badTxInclusion: badInclusion,
      badTxSpendInputs: fixture.badTxSpendInputs,
      badInputIndex: 0n,
      producingTxInclusion: producingInclusion,
      producingOutputItemCbors: fixture.producingOutputItemCbors,
      badTxWitnessSet: fixture.badTxWitnessSet,
      badTxScriptWitnessItemCbors: fixture.badTxScriptWitnessItemCbors,
      owner: harness.proverSigner.paymentKeyHash,
    } as const;
    expect(() =>
      prepareMissingNativeScriptTx({
        ...evidenceArgs,
        missingNativeScriptBytes: fixture.nativeScriptBytes,
      }),
    ).toThrow(/present|honest/u);
    expect(() =>
      prepareMissingNativeScriptTx({
        ...evidenceArgs,
        missingNativeScriptBytes: mismatchedScriptBytes,
      }),
    ).toThrow(/known native script preimage hashes/u);

    const init = async () =>
      await submitMissingNativeScriptTxInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        category: harness.category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: fixture.setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const bindBadAndSelect = async (badInputIndex: bigint) => {
      const opened = await init();
      const step01 = await submitMissingNativeScriptTxStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: opened.nextThreadOutRef,
        stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
        txInclusion: badInclusion,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      return await submitMissingNativeScriptTxStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
        spendInputs: fixture.badTxSpendInputs,
        badInputIndex,
        referenceScriptUtxo: refs[1],
      });
    };
    const bindProducing = async (threadOutRef: string) =>
      await submitMissingNativeScriptTxStep03({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
        txInclusion: producingInclusion,
        referenceScriptUtxo: refs[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const openScriptOutput = async (threadOutRef: string) =>
      await submitMissingNativeScriptTxStep04({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        nativeTxCompactCbor: producingInclusion.nativeTxCompactCbor,
        outputItemCbors: fixture.producingOutputItemCbors,
        referenceScriptUtxo: refs[3],
      });

    // Honest polarity: the full six-step thread reaches the decisive fold.
    const selected = await bindBadAndSelect(0n);
    const produced = await bindProducing(selected.nextThreadOutRef);
    const openedOutput = await openScriptOutput(produced.nextThreadOutRef);
    const classified = await submitMissingNativeScriptTxStep05({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: openedOutput.nextThreadOutRef,
      missingNativeScriptBytes: fixture.nativeScriptBytes,
      referenceScriptUtxo: refs[4],
    });
    await expect(
      submitMissingNativeScriptTxStep06({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: classified.nextThreadOutRef,
        nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
        witnessSet: fixture.badTxWitnessSet,
        scriptTxWitsItems: fixture.badTxScriptWitnessItemCbors,
        referenceScriptUtxo: refs[5],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/present/u);
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep06({
        harness,
        threadOutRef: classified.nextThreadOutRef,
        nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
        witnessSet: fixture.badTxWitnessSet,
        scriptTxWitsItems: fixture.badTxScriptWitnessItemCbors,
        referenceScriptUtxo: refs[5],
      }),
    );

    // The door refuses a truncated field-6 preimage and a one-sided forged
    // witness set before the absence fold can manufacture a conviction.
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep06({
        harness,
        threadOutRef: classified.nextThreadOutRef,
        nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
        witnessSet: fixture.badTxWitnessSet,
        scriptTxWitsItems: [],
        referenceScriptUtxo: refs[5],
      }),
    );
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep06({
        harness,
        threadOutRef: classified.nextThreadOutRef,
        nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
        witnessSet: {
          ...fixture.badTxWitnessSet,
          script_tx_wits_hash: "de".repeat(32),
        },
        scriptTxWitsItems: fixture.badTxScriptWitnessItemCbors,
        referenceScriptUtxo: refs[5],
      }),
    );

    // A third wallet cannot cancel the prover's still-live step-06 thread.
    await expect(
      submitMissingNativeScriptTxCancel({
        lucid: harness.outsiderLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.outsiderSigner,
        threadOutRef: classified.nextThreadOutRef,
        referenceScriptUtxo: refs[5],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/cannot cancel|names prover/u);
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxOutsiderCancel({
        harness,
        threadOutRef: classified.nextThreadOutRef,
        stepIndex: 5,
        referenceScriptUtxo: refs[5],
      }),
    );
    await submitMissingNativeScriptTxCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: classified.nextThreadOutRef,
      referenceScriptUtxo: refs[5],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    // Step 03 binds only the transaction that produced the accused input.
    const wrongProducing = await bindBadAndSelect(0n);
    await expect(
      submitMissingNativeScriptTxStep03({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: wrongProducing.nextThreadOutRef,
        stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
        txInclusion: badInclusion,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/does not match accused input/u);
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep03({
        harness,
        threadOutRef: wrongProducing.nextThreadOutRef,
        stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
        txInclusion: badInclusion,
        referenceScriptUtxo: refs[2],
      }),
    );
    await submitMissingNativeScriptTxCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: wrongProducing.nextThreadOutRef,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    // Step 04 refuses the second, key-locked producing output at the exact
    // credential pattern match.
    const keySelected = await bindBadAndSelect(1n);
    const keyProduced = await bindProducing(keySelected.nextThreadOutRef);
    await expect(
      openScriptOutput(keyProduced.nextThreadOutRef),
    ).rejects.toThrow(/key-locked/u);
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep04({
        harness,
        threadOutRef: keyProduced.nextThreadOutRef,
        nativeTxCompactCbor: producingInclusion.nativeTxCompactCbor,
        outputItemCbors: fixture.producingOutputItemCbors,
        referenceScriptUtxo: refs[3],
      }),
    );
    await submitMissingNativeScriptTxCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: keyProduced.nextThreadOutRef,
      referenceScriptUtxo: refs[3],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    // Step 05 refuses a known preimage whose tag-0 versioned hash is not the
    // credential, both locally and at the validator equation.
    const mismatchSelected = await bindBadAndSelect(0n);
    const mismatchProduced = await bindProducing(
      mismatchSelected.nextThreadOutRef,
    );
    const mismatchOpened = await openScriptOutput(
      mismatchProduced.nextThreadOutRef,
    );
    await expect(
      submitMissingNativeScriptTxStep05({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: mismatchOpened.nextThreadOutRef,
        missingNativeScriptBytes: mismatchedScriptBytes,
        referenceScriptUtxo: refs[4],
      }),
    ).rejects.toThrow(/not the accused credential/u);
    await expectOnchainRefusal(() =>
      submitRawMissingNativeScriptTxStep05({
        harness,
        threadOutRef: mismatchOpened.nextThreadOutRef,
        missingNativeScriptBytes: mismatchedScriptBytes,
        referenceScriptUtxo: refs[4],
      }),
    );
    await submitMissingNativeScriptTxCancel({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: mismatchOpened.nextThreadOutRef,
      referenceScriptUtxo: refs[4],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  }, 600_000);
});
