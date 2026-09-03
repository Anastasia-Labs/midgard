/**
 * `mint-authorization` direction-B emulator lifecycle: real faults succeed.
 *
 * Direction B convicts an operator-ACCEPTED committed transaction that mints
 * under a policy whose native script IS present but evaluates unsatisfied
 * against the committed field-7 signer set and validity interval. The journey
 * pins the policy's native payload by hash, opens the committed address
 * witnesses, and the machine-twin evaluator refutes the script — closing
 * straight from step-03 to step-05, with no reference-input scan:
 *
 * init → step-01 bind → step-02 claim (direction 1) → step-03
 * EvaluateUnsatisfied → step-05 permanent fraud-proof token →
 * fraudulent-block removal.
 *
 * Its own file for the wasm-leak isolation reason its siblings cite.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep05,
} from "../src/mint-authorization/index.js";
import {
  addressWitnessItemCbors,
  buildMintAuthorizationSubject,
  directionBNativeScript,
  makeMintAuthorizationEmulatorHarness,
  publishMintAuthorizationReferenceScripts,
  setupMintAuthorizationScenario,
} from "./support/mint-authorization-emulator-v1.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const ACCUSED_POLICY_INDEX = 0n;
const DIRECTION_SCRIPT_UNSATISFIED =
  SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED;

describe("mint-authorization direction-B emulator lifecycle", () => {
  it("proves an unsatisfied native mint policy end to end and removes the fraudulent commitment", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;

    // A native `sig` timelock whose key hash signs no committed witness: the
    // two decoy address witnesses do not name it, so the machine-twin refutes.
    const directionB = directionBNativeScript();
    const subject = buildMintAuthorizationSubject({
      mintItemCbors: [directionB.mintItemCbor],
      addrWitnessItemCbors: addressWitnessItemCbors(2),
    });
    const scenario = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    const { block, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }

    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const [step01Ref, step02Ref, step03Ref, , step05Ref] =
      await publishMintAuthorizationReferenceScripts({
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
      direction: DIRECTION_SCRIPT_UNSATISFIED,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.step03State.policy_id).toBe(directionB.policyIdHex);
    expect(step02.step03State.direction).toBe(DIRECTION_SCRIPT_UNSATISFIED);

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
    expect(step03.nextStepAddress).toBe(family.steps[4].spendingScriptAddress);

    const step05 = await submitMintAuthorizationStep05({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step03.nextThreadOutRef,
      referenceScriptUtxo: step05Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(step05.verdictState.direction).toBe(DIRECTION_SCRIPT_UNSATISFIED);

    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const [fraudProofUtxo] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (fraudProofUtxo === undefined) {
      throw new Error("step-05 did not mint the fraud-proof token");
    }
    expect(fraudProofUtxo.assets[step05.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg.
    const removalDeploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferenceScriptPublications.published },
    );
    const removeNow = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo: removalDeploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "mintAuthorization",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("mintAuthorization");
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error("Removal did not preserve the state-queue root");
    }
    const finalRoot = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        finalRootUtxo,
        harness.contracts.stateQueue.policyId,
      ),
    );
    expect(finalRoot.datum.next).toBe("Empty");

    // The fraud-proof token survives removal untouched at the same out-ref.
    const [retainedFraudProof] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (retainedFraudProof === undefined) {
      throw new Error("Removal burned the permanent fraud-proof token");
    }
    expect(outRefLabel(retainedFraudProof)).toBe(step05.fraudProofOutRef);
  }, 600_000);
});
