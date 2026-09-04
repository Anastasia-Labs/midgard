/**
 * `missing-native-script-tx` emulator tier-2 carriage: the §8.4 size
 * partition exercised with genuinely large committed data.
 *
 * The family's step-02 opens the bad transaction's §2.5 field 0 through the
 * §8.8 door, and §8.4 partitions the carriage tier on the preimage's size
 * alone — the tier is never a caller's argument. This journey commits a bad
 * transaction whose spend-input set is large enough (365 forty-byte out-ref
 * items, a 14,603-byte §5.1 preimage) that tier-1 inline carriage is
 * inadmissible: the preimage exceeds the 14,336-byte tier-1 redeemer bound,
 * so a real prover MUST publish it as a §8.2 `RawUtxo` bytes-only inline
 * datum and hand the door a reference input. Nothing forces the tier — the
 * data's size does. The accused script-locked input is buried among 363
 * fabricated decoys plus the key-locked control, and the six-step conviction
 * still lands on the absent script.
 *
 * Lives in its own file for the reason its siblings do. The split was made
 * while `@lucid-evolution/uplc` (through 0.2.22) leaked wasm linear memory on
 * every script evaluation and vitest isolates per FILE; that leak is fixed
 * upstream, and the split is kept so each file runs in its own fresh process.
 */
import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  prepareMissingNativeScriptTx,
  submitMissingNativeScriptTxInit,
  submitMissingNativeScriptTxStep01,
  submitMissingNativeScriptTxStep02,
  submitMissingNativeScriptTxStep03,
  submitMissingNativeScriptTxStep04,
  submitMissingNativeScriptTxStep05,
  submitMissingNativeScriptTxStep06,
} from "../src/index.js";
import {
  makeMissingNativeScriptTxEmulatorHarness,
  publishMissingNativeScriptTxReferenceScripts,
  setupMissingNativeScriptTxFixture,
} from "./support/missing-native-script-tx-emulator.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

/**
 * 363 decoys plus the accused input and its key-locked control (constant
 * §5.3 stride of 40 bytes each) make a 14,603-byte field-0 preimage: past
 * §8.4's tier-1 bound, inside the single-publication tier-2 window
 * `(14,336, 15,148]` — the size alone selects `RawUtxo`.
 */
const TIER2_DECOY_SPEND_INPUT_COUNT = 363;

describe("missing-native-script-tx emulator tier-2 carriage", () => {
  it("convicts the absent script on an accused input buried in a 14,603-byte spend-input field through a size-selected RawUtxo publication", async () => {
    const harness = await makeMissingNativeScriptTxEmulatorHarness();
    const fixture = await setupMissingNativeScriptTxFixture({
      harness,
      decoySpendInputCount: TIER2_DECOY_SPEND_INPUT_COUNT,
    });
    // The size, not any flag, is what selects tier 2: past the tier-1
    // redeemer bound, within one publication.
    const preimage = encodeMidgardFieldPreimage(
      fixture.badTxSpendInputs.map(SDK.encodeMidgardTxInputCanonical),
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K);

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
    const prepared = prepareMissingNativeScriptTx({
      badTxInclusion: badInclusion,
      badTxSpendInputs: fixture.badTxSpendInputs,
      badInputIndex: BigInt(fixture.badInputIndex),
      producingTxInclusion: producingInclusion,
      producingOutputItemCbors: fixture.producingOutputItemCbors,
      missingNativeScriptBytes: fixture.nativeScriptBytes,
      badTxWitnessSet: fixture.badTxWitnessSet,
      badTxScriptWitnessItemCbors: fixture.badTxScriptWitnessItemCbors,
      owner: harness.proverSigner.paymentKeyHash,
    });
    expect(prepared.expectedMissingScriptHash).toBe(fixture.expectedScriptHash);
    const init = await submitMissingNativeScriptTxInit({
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
    const step01 = await submitMissingNativeScriptTxStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      txInclusion: badInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step02 = await submitMissingNativeScriptTxStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
      spendInputs: fixture.badTxSpendInputs,
      badInputIndex: BigInt(fixture.badInputIndex),
      referenceScriptUtxo: refs[1],
    });
    expect(step02.carriageTier).toBe("RawUtxo");
    expect(step02.inputWithMissingScript.tx_id).toBe(fixture.producingTxId);

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced rather
    // than carried in the step's own redeemer.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCbor(preimage);
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const step03 = await submitMissingNativeScriptTxStep03({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.setup.fraudulentBlockOutRef,
      txInclusion: producingInclusion,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step04 = await submitMissingNativeScriptTxStep04({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      nativeTxCompactCbor: producingInclusion.nativeTxCompactCbor,
      outputItemCbors: fixture.producingOutputItemCbors,
      referenceScriptUtxo: refs[3],
    });
    expect(step04.expectedMissingScriptHash).toBe(fixture.expectedScriptHash);
    const step05 = await submitMissingNativeScriptTxStep05({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      missingNativeScriptBytes: fixture.nativeScriptBytes,
      referenceScriptUtxo: refs[4],
    });
    const step06 = await submitMissingNativeScriptTxStep06({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step05.nextThreadOutRef,
      nativeTxCompactCbor: badInclusion.nativeTxCompactCbor,
      witnessSet: fixture.badTxWitnessSet,
      scriptTxWitsItems: fixture.badTxScriptWitnessItemCbors,
      referenceScriptUtxo: refs[5],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      step06.fraudProofUnit,
    );
  }, 600_000);
});
