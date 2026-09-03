import { outRefLabel } from "@al-ft/midgard-core";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  L2TxMistagStep02Datum,
  submitL2TxMistagInit,
  submitL2TxMistagStep01,
  submitL2TxMistagStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  buildL2TxMistagBlockFixture,
  l2TxMistagCategory,
  publishL2TxMistagReferenceScripts,
} from "./support/l2-tx-mistag-emulator-v1.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";

describe("l2-tx-mistag emulator lifecycle", () => {
  it("mints permanent evidence for a committed code-1 normal leaf and removes the fraudulent block", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realL2TxMistag: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = harness.contracts.l2TxMistag;
    if (contracts === undefined) {
      throw new Error("l2-tx-mistag contracts missing");
    }
    const category = l2TxMistagCategory(harness);
    const [step01Ref, step02Ref] = await publishL2TxMistagReferenceScripts({
      harness,
    });
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const fixture = await buildL2TxMistagBlockFixture("TxIsInvalid");
    expect(fixture.inclusion.nativeTx.validity_code).toBe(1n);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });

    const init = await submitL2TxMistagInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitL2TxMistagStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts,
      categoryId: category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.inclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(step01.state).toStrictEqual({
      bad_tx_id: fixture.nativeTxId,
      committed_validity_code: 1n,
    });
    const step02Utxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      contracts.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    const proverKeyHash = getAddressDetails(
      await harness.proverLucid.wallet().address(),
    ).paymentCredential?.hash;
    expect(Data.from(step02Utxo.datum!, L2TxMistagStep02Datum)).toStrictEqual({
      fraud_prover: proverKeyHash,
      data: { bad_tx_id: fixture.nativeTxId, committed_validity_code: 1n },
    });

    const step02 = await submitL2TxMistagStep02({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const evidence = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(Data.from(evidence.datum!, FraudProofTokenDatum)).toStrictEqual({
      fraud_prover: proverKeyHash,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        contracts.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudCategory: "l2TxMistag",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
      awaitConfirmation: true,
    });
    expect(removal.fraudCategory).toBe("l2TxMistag");
    expect(removal.transactions[0]?.slashingApproach).toBe(
      "SlashActiveOperator",
    );
    await expectStateQueueHeaderOrder({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      harness.funderLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(outRefLabel(evidence));
  }, 600_000);
});
