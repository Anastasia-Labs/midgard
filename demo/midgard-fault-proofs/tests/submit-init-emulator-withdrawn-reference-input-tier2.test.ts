/**
 * `withdrawn-reference-input` emulator tier-2 carriage: the §8.4 size
 * partition exercised with genuinely large committed data.
 *
 * The family's step-02 opens §2.5 field 1 through the §8.8 door, and §8.4
 * partitions the carriage tier on the preimage's size alone — the tier is
 * never a caller's argument. This journey commits a transaction whose
 * reference-input set is large enough (365 forty-byte out-ref items, a
 * 14,603-byte §5.1 preimage) that tier-1 inline carriage is inadmissible:
 * the preimage exceeds the 14,336-byte tier-1 redeemer bound, so a real
 * prover MUST publish it as a §8.2 `RawUtxo` bytes-only inline datum and
 * hand the door a reference input. Nothing forces the tier — the data's
 * size does. The withdrawn reference input is buried among 364 decoys that
 * are not in the withdrawals set, and the conviction still lands on it.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitWithdrawnReferenceInputInit,
  submitWithdrawnReferenceInputStep01,
  submitWithdrawnReferenceInputStep02,
  submitWithdrawnReferenceInputStep03,
} from "../src/index.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawnReferenceInputEmulatorHarnessV1,
  publishWithdrawnReferenceInputReferenceScriptsV1,
  setupWithdrawnReferenceInputScenarioV1,
} from "./support/withdrawn-reference-input-emulator-v1.js";

/**
 * 364 decoys plus the accused reference input (constant §5.3 stride of 40
 * bytes each) make a 14,603-byte field-1 preimage: past §8.4's tier-1 bound,
 * inside the single-publication tier-2 window `(14,336, 15,148]` — the size
 * alone selects `RawUtxo`.
 */
const TIER2_DECOY_REFERENCE_INPUT_COUNT = 364;

describe("withdrawn-reference-input emulator tier-2 carriage", () => {
  it("convicts a withdrawn reference input buried in a 14,603-byte field through a size-selected RawUtxo publication", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const scenario = await setupWithdrawnReferenceInputScenarioV1({
      harness,
      decoyReferenceInputCount: TIER2_DECOY_REFERENCE_INPUT_COUNT,
    });
    // The size, not any flag, is what selects tier 2: past the tier-1
    // redeemer bound, within one publication.
    const preimage = encodeMidgardFieldPreimageV1(
      scenario.prepared.referenceInputs.map(
        SDK.encodeMidgardTxInputCanonicalV1,
      ),
    );
    expect(preimage.length).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    expect(preimage.length).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K_V1);

    const [step01Ref, step02Ref, step03Ref] =
      await publishWithdrawnReferenceInputReferenceScriptsV1({
        lucid: harness.funderLucid,
        contracts: harness.family,
      });
    const init = await submitWithdrawnReferenceInputInit({
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
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    });
    const step01 = await submitWithdrawnReferenceInputStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.prepared.txInclusion,
      referenceScriptUtxo: step01Ref,
    });
    const step02 = await submitWithdrawnReferenceInputStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceInputs: scenario.prepared.referenceInputs,
      nativeTxCompactCbor: scenario.prepared.txInclusion.nativeTxCompactCbor,
      badReferenceInputIndex: BigInt(scenario.prepared.badReferenceInputIndex),
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.carriageTier).toBe("RawUtxo");
    expect(step02.referenceInputsItemCount).toBe(
      TIER2_DECOY_REFERENCE_INPUT_COUNT + 1,
    );
    expect(step02.missingReferenceInput).toStrictEqual(
      scenario.prepared.missingReferenceInput,
    );

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, referenced rather
    // than carried in the step's own redeemer.
    const expectedDatum = SDK.fieldPreimagePublicationDatumCborV1(preimage);
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    const step03 = await submitWithdrawnReferenceInputStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      withdrawalMembership: scenario.prepared.withdrawalMembership,
      referenceScriptUtxo: step03Ref,
    });
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.family.fraudProof.spendingScriptAddress,
      step03.fraudProofUnit,
    );
  }, 600_000);
});
