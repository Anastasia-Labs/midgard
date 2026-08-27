import { outRefLabel } from "@al-ft/midgard-core";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitL2TxMistagCancel,
  submitL2TxMistagInit,
  submitL2TxMistagStep01,
  submitL2TxMistagStep02,
} from "../src/index.js";
import { network } from "./support/emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import {
  buildL2TxMistagBlockFixtureV1,
  l2TxMistagCategoryV1,
  publishL2TxMistagReferenceScriptsV1,
} from "./support/l2-tx-mistag-emulator-v1.js";
import { setupFraudulentBlockV1 } from "./support/submit-init-emulator-fixtures.js";

describe("l2-tx-mistag cancellation and resume controls", () => {
  it("cancels at either step, then re-inits and completes the proof", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: {
        realL2TxMistag: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const contracts = harness.contracts.l2TxMistag;
    if (contracts === undefined) throw new Error("l2-tx-mistag missing");
    const category = l2TxMistagCategoryV1(harness);
    const [step01Ref, step02Ref] = await publishL2TxMistagReferenceScriptsV1({
      harness,
    });
    const fixture = await buildL2TxMistagBlockFixtureV1("TxIsInvalid");
    const setup = await setupFraudulentBlockV1({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const init = () =>
      submitL2TxMistagInit({
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
      });
    const step01 = (threadOutRef: string) =>
      submitL2TxMistagStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts,
        categoryId: category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.inclusion,
        referenceScriptUtxo: step01Ref,
      });

    const first = await init();
    const cancel01 = await submitL2TxMistagCancel({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: first.nextThreadOutRef,
      referenceScriptUtxo: step01Ref,
    });
    expect(cancel01.cancelledStepIndex).toBe(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        contracts.steps[0].spendingScriptAddress,
        first.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const second = await init();
    const advanced = await step01(second.nextThreadOutRef);
    const cancel02 = await submitL2TxMistagCancel({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: advanced.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
    });
    expect(cancel02.cancelledStepIndex).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        contracts.fraudProof.spendingScriptAddress,
        toUnit(
          contracts.fraudProof.policyId,
          second.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);

    const third = await init();
    const resumed01 = await step01(third.nextThreadOutRef);
    const resumed02Utxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      contracts.steps[1].spendingScriptAddress,
      third.computationThreadUnit,
    );
    expect(outRefLabel(resumed02Utxo)).toBe(resumed01.nextThreadOutRef);
    const completed = await submitL2TxMistagStep02({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: resumed01.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
    });
    expect(completed.fraudProofUnit).toContain(
      third.computationThreadAssetName,
    );
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      completed.fraudProofAddress,
      completed.fraudProofUnit,
    );
  }, 600_000);
});
