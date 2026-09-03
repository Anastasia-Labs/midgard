/**
 * `mint-authorization` adversarial polarity: an honest commitment is refused
 * at the exact check, and a tampered predeployed carriage is refused on-chain.
 *
 * Four negatives, each against a commitment that carries NO fault:
 *
 * - a committed leaf the operator honestly RECORDED AS REJECTED can never be
 *   bound — step-01's §2.4.3(d) acceptance guard refuses it;
 * - a direction-A "the script is absent" claim is FALSE when the committed
 *   field 6 actually consulted a script hashing to the policy — step-03's
 *   absence fold refuses it;
 * - a direction-B "the native script is unsatisfied" claim is FALSE when the
 *   committed signer set SATISFIES it — step-03's machine-twin refuses it;
 * - a TAMPERED tier-2 mint publication (planted by a raw builder that bypasses
 *   the honest content-matching path) is refused on-chain when the door
 *   recomputes the §8.8 field commitment and disagrees with the anchored slot.
 *
 * The first three surface at the submitters' local guards (the exact on-chain
 * check mirrored off-chain); the last reaches local UPLC evaluation and fails
 * inside the validator. Own file for the wasm-leak isolation its siblings cite.
 */
import { midgardFieldCarriageBounds } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.js";
import {
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep03WitnessAbsence,
} from "../src/mint-authorization/index.js";
import {
  addressWitnessItemCbors,
  buildMintAuthorizationSubject,
  directionAPresentScript,
  directionBSatisfiedNativeScript,
  expectOnchainRefusal,
  largeMintItemCbors,
  makeMintAuthorizationEmulatorHarness,
  type MintAuthorizationHarness,
  publishMintAuthorizationReferenceScripts,
  setupMintAuthorizationScenario,
  smallMintItemCbors,
  submitRawMintAuthorizationStep02TamperedMint,
  tamperFieldPreimageBytes,
} from "./support/mint-authorization-emulator.js";
import { network } from "./support/submit-init-emulator-shared.js";

const ACCUSED_POLICY_INDEX = 0n;

/** The honest init leg every negative shares, plus published references. */
const driveInit = async (
  harness: MintAuthorizationHarness,
  setup: Awaited<ReturnType<typeof setupMintAuthorizationScenario>>["setup"],
): Promise<{
  readonly initResult: Awaited<ReturnType<typeof submitMintAuthorizationInit>>;
  readonly refs: readonly [
    step01Ref: Awaited<
      ReturnType<typeof publishMintAuthorizationReferenceScripts>
    >[number],
    step02Ref: Awaited<
      ReturnType<typeof publishMintAuthorizationReferenceScripts>
    >[number],
    step03Ref: Awaited<
      ReturnType<typeof publishMintAuthorizationReferenceScripts>
    >[number],
  ];
}> => {
  const { realBlueprint, funderLucid, proverLucid, proverSigner, catalogue } =
    harness;
  const [step01Ref, step02Ref, step03Ref] =
    await publishMintAuthorizationReferenceScripts({
      lucid: funderLucid,
      contracts: harness.family,
    });
  const initResult = await submitMintAuthorizationInit({
    lucid: proverLucid,
    blueprint: realBlueprint,
    network,
    contracts: harness.family,
    category: harness.category,
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
  return { initResult, refs: [step01Ref, step02Ref, step03Ref] };
};

describe("mint-authorization emulator adversarial polarity", () => {
  it("never binds a committed leaf the operator honestly recorded as rejected", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness({
      useScalusEvaluator: false,
    });
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: smallMintItemCbors(),
      validity: "TxIsInvalid",
    });
    const { block, setup } = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error("rejected mint-authorization fixture has no inclusion");
    }
    const { initResult, refs } = await driveInit(harness, setup);

    // §2.4.3(d): an operator-rejected leaf carries a non-zero validity code, so
    // step-01 refuses to bind it — the honest no-op can never convict.
    await expect(
      submitMintAuthorizationStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: initResult.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: setup.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/is not an acceptance/u);
  }, 600_000);

  it("refuses a false absence claim when the committed field 6 consulted the policy's script", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness({
      useScalusEvaluator: false,
    });
    const present = directionAPresentScript();
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: [present.mintItemCbor],
      scriptWitnessItemCbors: [present.scriptWitnessItemCbor],
    });
    const { block, setup } = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }
    const { initResult, refs } = await driveInit(harness, setup);
    const step01 = await submitMintAuthorizationStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitMintAuthorizationStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: ACCUSED_POLICY_INDEX,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: refs[1],
    });
    expect(step02.step03State.policy_id).toBe(present.policyIdHex);

    await expect(
      submitMintAuthorizationStep03WitnessAbsence({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        witnessSet: subject.witnessSetCompact,
        scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/absence claim is false/u);
  }, 600_000);

  it("refuses an unsatisfied claim when the committed signers SATISFY the native script", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness({
      useScalusEvaluator: false,
    });
    const satisfied = directionBSatisfiedNativeScript();
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: [satisfied.mintItemCbor],
      addrWitnessItemCbors: [satisfied.addrWitnessItemCbor],
    });
    const { block, setup } = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }
    const { initResult, refs } = await driveInit(harness, setup);
    const step01 = await submitMintAuthorizationStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    const step02 = await submitMintAuthorizationStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: ACCUSED_POLICY_INDEX,
      direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: refs[1],
    });
    expect(step02.step03State.policy_id).toBe(satisfied.policyIdHex);

    await expect(
      submitMintAuthorizationStep03EvaluateUnsatisfied({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        witnessSet: subject.witnessSetCompact,
        scriptBytesHex: satisfied.scriptBytesHex,
        addrTxWitsItemCbors: subject.addrWitnessItemCbors,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/SATISFY the policy's native script/u);
  }, 600_000);

  it("refuses a tampered predeployed tier-2 mint publication at the field re-hash", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness({
      useScalusEvaluator: false,
    });
    const largeMint = largeMintItemCbors();
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: largeMint.itemCbors,
      addrWitnessItemCbors: addressWitnessItemCbors(1),
    });
    const { block, setup } = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }

    // The honest tier-2 mint preimage, tampered in its last content byte: the
    // published bytes still decode, but no longer re-hash to the committed slot.
    const plannedMint = planFaultProofFieldOpening({
      fieldIndex: MIDGARD_FIELD_INDEX.mint,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      itemCbors: largeMint.itemCbors.map((hex) => Buffer.from(hex, "hex")),
      owner: harness.proverSigner.paymentKeyHash,
      label: "adversarial mint tamper",
    });
    expect(plannedMint.plan.tier).toBe("RawUtxo");
    expect(plannedMint.preimage.length).toBeGreaterThan(
      midgardFieldCarriageBounds.maxTier1RedeemerPreimageBytes,
    );
    const tamperedPreimageBytes = tamperFieldPreimageBytes(
      plannedMint.preimage,
    );

    const [step01Ref, step02Ref] =
      await publishMintAuthorizationReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.family,
      });
    const initResult = await submitMintAuthorizationInit({
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
    const step01 = await submitMintAuthorizationStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });

    await expectOnchainRefusal(() =>
      submitRawMintAuthorizationStep02TamperedMint({
        harness,
        threadOutRef: step01.nextThreadOutRef,
        block,
        tamperedPreimageBytes,
        referenceScriptUtxo: step02Ref,
      }),
    );
  }, 600_000);
});
