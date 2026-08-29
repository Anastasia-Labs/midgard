/**
 * `mint-authorization` size-forced tier-2 carriage: large doors ride RawUtxo.
 *
 * Both journeys select tier-2 published carriage from PREIMAGE LENGTH ALONE —
 * no override flag exists in these submitters — and drive the whole lifecycle
 * against the published UTxO read back as a reference input:
 *
 * - the field-5 mint door, forced past `maxTier1RedeemerPreimageBytes` by
 *   distinct absent policies, through the direction-A finalize;
 * - the field-7 address-witness door, forced past the ceiling by decoy
 *   signers, through the direction-B EvaluateUnsatisfied finalize.
 *
 * Each asserts the door actually crossed into the RawUtxo window and then
 * that the on-chain door accepts the published preimage end to end.
 */
import { midgardFieldCarriageBoundsV1 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { MIDGARD_FIELD_INDEX_V1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpeningV1 } from "../src/field-opening-v1.js";
import {
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep03WitnessAbsence,
  submitMintAuthorizationStep04AdvanceComplete,
  submitMintAuthorizationStep05,
} from "../src/mint-authorization/index.js";
import {
  buildMintAuthorizationSubjectV1,
  directionBNativeScriptV1,
  largeAddressWitnessItemCborsV1,
  largeMintItemCborsV1,
  makeMintAuthorizationEmulatorHarnessV1,
  publishMintAuthorizationReferenceScriptsV1,
  setupMintAuthorizationScenarioV1,
} from "./support/mint-authorization-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

const ACCUSED_POLICY_INDEX = 0n;

describe("mint-authorization size-forced tier-2 carriage", () => {
  it("forces the field-5 mint door onto RawUtxo carriage by size and finalizes direction A", async () => {
    const harness = await makeMintAuthorizationEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;

    const largeMint = largeMintItemCborsV1();
    expect(largeMint.preimageByteLength).toBeGreaterThan(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    expect(largeMint.preimageByteLength).toBeLessThanOrEqual(
      midgardFieldCarriageBoundsV1.maxPublishableCarriageBytes,
    );
    const subject = buildMintAuthorizationSubjectV1({
      mintItemCbors: largeMint.itemCbors,
    });
    const scenario = await setupMintAuthorizationScenarioV1({
      harness,
      subject,
    });
    const { block, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }

    // The planner selects the tier purely from the mint field's own length.
    const plannedMint = planFaultProofFieldOpeningV1({
      fieldIndex: MIDGARD_FIELD_INDEX_V1.mint,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      itemCbors: largeMint.itemCbors.map((hex) => Buffer.from(hex, "hex")),
      owner: proverSigner.paymentKeyHash,
      label: "mint size assertion",
    });
    expect(plannedMint.plan.tier).toBe("RawUtxo");

    const [step01Ref, step02Ref, step03Ref, step04Ref, step05Ref] =
      await publishMintAuthorizationReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const initResult = await submitMintAuthorizationInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step01 = await submitMintAuthorizationStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitMintAuthorizationStep02({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: ACCUSED_POLICY_INDEX,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: step02Ref,
    });
    const step03 = await submitMintAuthorizationStep03WitnessAbsence({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      witnessSet: subject.witnessSetCompact,
      scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
      referenceScriptUtxo: step03Ref,
    });
    const step04 = await submitMintAuthorizationStep04AdvanceComplete({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      referenceInputsItemCbors: subject.referenceInputItemCbors,
      referenceScriptUtxo: step04Ref,
    });
    const step05 = await submitMintAuthorizationStep05({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: step05Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const [fraudProofUtxo] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (fraudProofUtxo === undefined) {
      throw new Error("size-forced mint lifecycle minted no fraud-proof token");
    }
    expect(fraudProofUtxo.assets[step05.fraudProofUnit]).toBe(1n);
  }, 600_000);

  it("forces the field-7 address-witness door onto RawUtxo carriage by size and finalizes direction B", async () => {
    const harness = await makeMintAuthorizationEmulatorHarnessV1();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;

    const largeWitnesses = largeAddressWitnessItemCborsV1();
    expect(largeWitnesses.preimageByteLength).toBeGreaterThan(
      midgardFieldCarriageBoundsV1.maxTier1RedeemerPreimageBytes,
    );
    expect(largeWitnesses.preimageByteLength).toBeLessThanOrEqual(
      midgardFieldCarriageBoundsV1.maxPublishableCarriageBytes,
    );
    const directionB = directionBNativeScriptV1();
    const subject = buildMintAuthorizationSubjectV1({
      mintItemCbors: [directionB.mintItemCbor],
      addrWitnessItemCbors: largeWitnesses.itemCbors,
    });
    const scenario = await setupMintAuthorizationScenarioV1({
      harness,
      subject,
    });
    const { block, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }

    const [step01Ref, step02Ref, step03Ref, , step05Ref] =
      await publishMintAuthorizationReferenceScriptsV1({
        lucid: funderLucid,
        contracts: family,
      });
    const initResult = await submitMintAuthorizationInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step01 = await submitMintAuthorizationStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitMintAuthorizationStep02({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: ACCUSED_POLICY_INDEX,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: step02Ref,
    });

    // The field-7 door's tier follows the address-witness field's own length.
    const plannedField07 = planFaultProofFieldOpeningV1({
      fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      itemCbors: subject.addrWitnessItemCbors.map((hex) =>
        Buffer.from(hex, "hex"),
      ),
      owner: proverSigner.paymentKeyHash,
      witnessSet: subject.witnessSetCompact,
      anchorWitnessSetHash: step02.step03State.bad_tx_witness_set_hash,
      label: "field-7 size assertion",
    });
    expect(plannedField07.plan.tier).toBe("RawUtxo");

    const step03 = await submitMintAuthorizationStep03EvaluateUnsatisfied({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      witnessSet: subject.witnessSetCompact,
      scriptBytesHex: directionB.scriptBytesHex,
      addrTxWitsItemCbors: subject.addrWitnessItemCbors,
      referenceScriptUtxo: step03Ref,
    });
    const step05 = await submitMintAuthorizationStep05({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: step05Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const [fraudProofUtxo] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (fraudProofUtxo === undefined) {
      throw new Error(
        "size-forced field-7 lifecycle minted no fraud-proof token",
      );
    }
    expect(fraudProofUtxo.assets[step05.fraudProofUnit]).toBe(1n);
  }, 600_000);
});
