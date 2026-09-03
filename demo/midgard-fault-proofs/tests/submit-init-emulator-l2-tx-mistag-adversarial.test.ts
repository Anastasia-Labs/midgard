import { outRefLabel } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { submitL2TxMistagInit, submitL2TxMistagStep01 } from "../src/index.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  buildL2TxMistagBlockFixture,
  forceL2TxMistagStep01ForAdversarialTest,
  l2TxMistagCategory,
  publishL2TxMistagReferenceScripts,
} from "./support/l2-tx-mistag-emulator.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";

describe("l2-tx-mistag adversarial refusal", () => {
  it("refuses an honest code-0 leaf at the exact on-chain check and a scalar flip at membership", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realL2TxMistag: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = harness.contracts.l2TxMistag;
    if (contracts === undefined) throw new Error("l2-tx-mistag missing");
    const category = l2TxMistagCategory(harness);
    const [step01Ref] = await publishL2TxMistagReferenceScripts({ harness });
    const honest = await buildL2TxMistagBlockFixture("TxIsValid");
    const scalarFlip = await buildL2TxMistagBlockFixture("TxIsInvalid");
    expect(honest.nativeTxId).toBe(scalarFlip.nativeTxId);
    expect(honest.compactCbor).not.toBe(scalarFlip.compactCbor);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture: honest,
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
    const firstStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      contracts.steps[0].spendingScriptAddress,
      init.computationThreadUnit,
    );

    // Offchain plane: the production submitter refuses before building.
    await expect(
      submitL2TxMistagStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts,
        categoryId: category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: honest.inclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(
      /code-0 leaf is an honest acceptance; a valid block cannot be challenged/u,
    );

    // Onchain plane: the test-only forced builder omits that one local gate;
    // the real step-01 validator refuses its authenticated code 0.
    await expect(
      forceL2TxMistagStep01ForAdversarialTest({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts,
        categoryId: category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: honest.inclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/failed script execution/u);

    // Anti-framing plane: the forged compact re-derives the same id and passes
    // the submitter's codec gate, but it is not the value in the honest root.
    await expect(
      submitL2TxMistagStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts,
        categoryId: category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: {
          ...scalarFlip.inclusion,
          transactionsPhasRoot: honest.transactionsRoot,
          txMembershipProof: honest.inclusion.txMembershipProof,
          txMembershipProofCbor: honest.inclusion.txMembershipProofCbor,
        },
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/failed script execution Withdraw/u);

    const untouched = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      contracts.steps[0].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(untouched)).toBe(outRefLabel(firstStep));
    await expectStateQueueHeaderOrder({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 600_000);
});
