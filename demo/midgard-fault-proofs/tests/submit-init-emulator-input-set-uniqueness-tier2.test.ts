/**
 * `input-set-uniqueness` emulator tier-2 carriage: the §8.4 size partition
 * exercised with genuinely large committed data.
 *
 * The family's step-02 opens §2.5 field 0 through the §8.8 door, and §8.4
 * partitions the carriage tier on the preimage's size alone — the tier is
 * never a caller's argument. Both journeys here commit a transaction whose
 * spend-input set is large enough (365 forty-byte out-ref items, a 14,603-byte
 * §5.1 preimage) that tier-1 inline carriage is inadmissible: the preimage
 * exceeds the 14,336-byte tier-1 redeemer bound, so a real prover MUST publish
 * it as a §8.2 `RawUtxo` bytes-only inline datum and hand the door a reference
 * input. Nothing forces the tier — the data's size does.
 *
 * - Journey 1 (real fault): duplicate spend inputs buried in the large set;
 *   init → step-01 → step-02 plans tier-2, publishes the preimage at the
 *   prover's address, and convicts through the reference input — permanent
 *   fraud-proof token minted.
 * - Journey 2 (tampered publication): the prover publishes a byte-flipped
 *   preimage and names it as the `RawUtxo` carriage. The door's
 *   `field_commitment` re-hash refuses the transaction on-chain; the honest
 *   publication then convicts on the same thread.
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
import {
  fieldPreimagePublicationDatumCborV1,
  type MidgardTxInput,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  requireInputSetUniquenessClaimV1,
  scanInputSetUniquenessV1,
  submitInputSetUniquenessInit,
  submitInputSetUniquenessStep01,
  submitInputSetUniquenessStep02,
} from "../src/input-set-uniqueness/index.js";
import { expectOnchainRefusalV1 } from "./support/emulator/expect-onchain-refusal-v1.js";
import {
  buildInputSetUniquenessFixtureV1,
  isuOutRefV1,
  makeInputSetUniquenessEmulatorHarnessV1,
  publishInputSetUniquenessReferenceScriptsV1,
  setupInputSetUniquenessScenarioV1,
} from "./support/input-set-uniqueness-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

/**
 * 365 out-refs (constant §5.3 stride of 40 bytes each) with the item at
 * `secondIndex` duplicating the one at `firstIndex`: a 14,603-byte field-0
 * preimage — past the tier-1 bound, inside the single-publication tier-2
 * window `(14,336, 15,148]`.
 */
const LARGE_SPEND_INPUT_COUNT = 365;
const DUPLICATE_FIRST_INDEX = 40;
const DUPLICATE_SECOND_INDEX = 320;

const largeDuplicateSpendInputsV1 = (): readonly MidgardTxInput[] => {
  const inputs = Array.from({ length: LARGE_SPEND_INPUT_COUNT }, (_, index) =>
    isuOutRefV1("a1", index),
  );
  inputs[DUPLICATE_SECOND_INDEX] = inputs[DUPLICATE_FIRST_INDEX]!;
  return inputs;
};

/** The §5.1 field-0 preimage the fixture commits, canonical producer form. */
const fieldPreimageV1 = (itemCbors: readonly string[]): Buffer =>
  encodeMidgardFieldPreimageV1(
    itemCbors.map((item) => Buffer.from(item, "hex")),
  );

const driveToStep01 = async () => {
  const harness = await makeInputSetUniquenessEmulatorHarnessV1();
  const fixture = await buildInputSetUniquenessFixtureV1({
    spendInputs: largeDuplicateSpendInputsV1(),
    referenceInputs: [isuOutRefV1("33", 1)],
  });
  // The size, not any flag, is what selects tier 2: past the tier-1 redeemer
  // bound, within one publication.
  const preimageBytes = fieldPreimageV1(fixture.spendInputItemCbors).length;
  expect(preimageBytes).toBeGreaterThan(
    MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  );
  expect(preimageBytes).toBeLessThanOrEqual(MIDGARD_CHUNK_BYTES_K_V1);

  const claims = scanInputSetUniquenessV1({
    spendInputItemCbors: fixture.spendInputItemCbors,
    referenceInputItemCbors: fixture.referenceInputItemCbors,
  });
  expect(claims).toStrictEqual([
    {
      kind: "duplicateSpendInputs",
      firstIndex: BigInt(DUPLICATE_FIRST_INDEX),
      secondIndex: BigInt(DUPLICATE_SECOND_INDEX),
    },
  ]);
  const claim = requireInputSetUniquenessClaimV1({
    spendInputItemCbors: fixture.spendInputItemCbors,
    referenceInputItemCbors: fixture.referenceInputItemCbors,
  });

  const { setup } = await setupInputSetUniquenessScenarioV1({
    harness,
    fixture,
  });
  const [step01Ref, step02Ref] =
    await publishInputSetUniquenessReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });

  const initResult = await submitInputSetUniquenessInit({
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
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: setup.witnessReferenceScripts,
  });
  const step01 = await submitInputSetUniquenessStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    network,
    signer: harness.proverSigner,
    threadOutRef: initResult.nextThreadOutRef,
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    txInclusion: fixture.txInclusion,
    referenceScriptUtxo: step01Ref,
    witnessReferenceScripts: setup.witnessReferenceScripts,
  });
  expect(step01.stepState).toStrictEqual({ bad_tx_id: fixture.nativeTxId });

  return {
    harness,
    fixture,
    claim,
    step01,
    step02Ref,
    witnessReferenceScripts: setup.witnessReferenceScripts,
  };
};

describe("input-set-uniqueness emulator tier-2 carriage", () => {
  it("convicts a duplicate buried in a 14,603-byte spend-input field through a size-forced RawUtxo publication", async () => {
    const {
      harness,
      fixture,
      claim,
      step01,
      step02Ref,
      witnessReferenceScripts,
    } = await driveToStep01();

    const step02 = await submitInputSetUniquenessStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      claim,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts,
    });
    expect(step02.badTxId).toBe(fixture.nativeTxId);

    // The tier-2 publication really exists: the whole §5.1 preimage sits at
    // the prover's address as a bytes-only inline datum, and the step
    // transaction consumed it as a reference input rather than carrying
    // 14,603 bytes in its own redeemer.
    const expectedDatum = fieldPreimagePublicationDatumCborV1(
      fieldPreimageV1(fixture.spendInputItemCbors),
    );
    const publications = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).filter((utxo) => utxo.datum === expectedDatum);
    expect(publications).toHaveLength(1);

    // Terminal step: thread burned everywhere, permanent token minted.
    for (const step of harness.family.steps) {
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          step02.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const fraudProofUtxos = await harness.proverLucid.utxosAtWithUnit(
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(fraudProofUtxos).toHaveLength(1);
  });

  it("refuses a tampered predeployed preimage at the door's field_commitment re-hash, then convicts through the honest publication", async () => {
    const {
      harness,
      fixture,
      claim,
      step01,
      step02Ref,
      witnessReferenceScripts,
    } = await driveToStep01();

    // Publish a byte-flipped twin of the honest preimage at the prover's own
    // address — same length, same shape, wrong content.
    const tampered = Buffer.from(fieldPreimageV1(fixture.spendInputItemCbors));
    tampered[tampered.length - 1] = tampered[tampered.length - 1]! ^ 0xff;
    const tamperedDatum = fieldPreimagePublicationDatumCborV1(tampered);
    harness.proverSigner.selectWallet(harness.proverLucid);
    const publishTx = await harness.proverLucid
      .newTx()
      .pay.ToAddressWithData(
        harness.proverSigner.address,
        { kind: "inline", value: tamperedDatum },
        {},
      )
      .complete();
    const publishSigned = await publishTx.sign.withWallet().complete();
    await harness.proverLucid.awaitTx(await publishSigned.submit(), 100);
    const tamperedUtxo = (
      await harness.proverLucid.utxosAt(harness.proverSigner.address)
    ).find((utxo) => utxo.datum === tamperedDatum);
    if (tamperedUtxo === undefined) {
      throw new Error("the tampered publication did not confirm");
    }

    // The redeemer names the tampered UTxO as its RawUtxo carriage; the
    // door re-hashes the referenced bytes against the committed field hash
    // and the validator refuses.
    const refusal = await expectOnchainRefusalV1(() =>
      submitInputSetUniquenessStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        claim,
        nativeTxCompactCbor: fixture.nativeTxCompactCbor,
        spendInputItemCbors: fixture.spendInputItemCbors,
        referenceInputItemCbors: fixture.referenceInputItemCbors,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts,
        unsafeSpendFieldRawUtxoForTest: tamperedUtxo,
      }),
    );
    expect(refusal).toMatch(/failed script execution/u);

    // Same thread, honest publication: the conviction goes through, proving
    // the refusal above was the tamper and nothing else.
    const step02 = await submitInputSetUniquenessStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      claim,
      nativeTxCompactCbor: fixture.nativeTxCompactCbor,
      spendInputItemCbors: fixture.spendInputItemCbors,
      referenceInputItemCbors: fixture.referenceInputItemCbors,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts,
    });
    expect(step02.badTxId).toBe(fixture.nativeTxId);
  });
});
