/**
 * Transition-trace fraud-proof journey: submit the one-step transition fault
 * and remove the block it condemns.
 *
 * Split out of `submit-init-emulator.test.ts` to keep each file's leaked wasm
 * heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import { outRefLabel } from "@al-ft/midgard-core";
import {
  createReferenceScriptAuthPolicy,
  FraudProofTokenDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  resolveProverSigner,
  submitRemoveFraudulentBlock,
  submitTransitionTraceProof,
} from "../src/index.js";
import {
  publishFaultProofWitnessReferenceScriptsV1,
  publishOperatorLifecycleReferenceScriptsV1,
} from "./support/emulator/reference-scripts.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  buildInvalidForcedTransitionTraceFixture,
  expectStateQueueHeaderOrder,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  fundedProverEmulatorAccount,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
  TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES,
} from "./support/submit-init-emulator-shared.js";

describe("fault-proof emulator integration", () => {
  it("submits and removes a tail transition-trace fraud proof end to end", async () => {
    const realBlueprint = readBlueprint(realBlueprintPath);
    const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = fundedProverEmulatorAccount(20_000_000_000n);
    const emulator = new Emulator(
      [funder, prover],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const funderLucid = await Lucid(emulator, "Custom");
    const proverLucid = await Lucid(emulator, "Custom");
    funderLucid.selectWallet.fromSeed(funder.seedPhrase);
    const proverSigner = resolveProverSigner({
      network,
      walletSeedPhrase: prover.seedPhrase,
    });
    // Selected through the signer so the prover Lucid instance and every
    // `signer.selectWallet(lucid)` call site address the same funded wallet.
    proverSigner.selectWallet(proverLucid);

    await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
    const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
    if (nonceUtxo === undefined) {
      throw new Error("Expected funder wallet to expose a nonce UTxO");
    }

    const baseContracts = {
      ...(await buildMinimalFaultProofContracts(
        realBlueprint,
        alwaysBlueprint,
        nonceUtxo,
        { realTransitionTrace: true, alwaysFraudProofCatalogue: true },
      )),
      referenceScriptAuth: createReferenceScriptAuthPolicy(
        funderLucid,
        emulator.now(),
      ),
    };
    // Operator registration and activation source their four directory
    // validators from published reference scripts. Published from the prover
    // wallet before the header clock is sampled so the funder's nonce UTxO
    // survives and the whole fixture timeline shifts uniformly.
    const contracts = {
      ...baseContracts,
      operatorLifecycleReferenceScripts:
        await publishOperatorLifecycleReferenceScriptsV1({
          lucid: proverLucid,
          contracts: baseContracts,
        }),
    };
    const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
    const witnessReferenceScripts =
      await publishFaultProofWitnessReferenceScriptsV1({
        lucid: proverLucid,
        realBlueprint,
        claimRegistrySpendingScript: contracts.claimRegistry.spendingScript,
        computationThreadMintingScript:
          contracts.computationThread.mintingScript,
        fraudProofMintingScript: contracts.fraudProof.mintingScript,
      });
    // See `publishRemovalReferenceScripts`: removal must source these seven
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });
    const transitionTraceReferenceScripts =
      await publishFraudProofChainReferenceScripts({
        lucid: proverLucid,
        steps: contracts.fraudProofContracts.transitionTrace.steps,
        entryNames: FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transitionTrace,
        familyLabel: "transition-trace",
        oversizedEntryNames:
          TRANSITION_TRACE_OVERSIZED_REFERENCE_SCRIPT_ENTRIES,
      });
    const funderPaymentCredential = getAddressDetails(
      await funderLucid.wallet().address(),
    ).paymentCredential;
    if (
      funderPaymentCredential === undefined ||
      funderPaymentCredential.type !== "Key"
    ) {
      throw new Error("Expected funder wallet to expose a payment key hash");
    }
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const traceFixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: funderPaymentCredential.hash,
      now: headerStartTime,
    });
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: traceFixture.header,
    });
    expect(setup.headerHash).toBe(traceFixture.headerHash);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [traceFixture.headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferenceScriptPublications.published,
      fraudProofReferenceScripts: transitionTraceReferenceScripts,
      claimRegistrySpendReference: witnessReferenceScripts.claimRegistrySpend,
    });
    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts,
      awaitConfirmation: true,
    });

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(initResult.fraudCategoryName).toBe("transitionTrace");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.transitionTrace.categoryId}${traceFixture.headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    const proofResult = await submitTransitionTraceProof({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      proof: traceFixture.proof,
      witnessReferenceScripts,
      awaitConfirmation: true,
    });

    expect(proofResult.txHash).toHaveLength(64);
    expect(proofResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
    expect(proofResult.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(proofResult.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    expect(proofResult.fraudProofMintRedeemerIndex).not.toBe(
      proofResult.computationThreadMintRedeemerIndex,
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    const removeNow = BigInt(emulator.now());
    const removeResult = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "transitionTrace",
      fraudulentHeaderHash: traceFixture.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });

    expect(removeResult.fraudCategory).toBe("transitionTrace");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.transitionTrace.categoryId,
    );
    expect(removeResult.stateQueueMutationLease).toBeNull();
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [traceFixture.headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      proofResult.fraudProofAddress,
      proofResult.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[proofResult.fraudProofUnit]).toBe(1n);
  }, 180_000);
});
