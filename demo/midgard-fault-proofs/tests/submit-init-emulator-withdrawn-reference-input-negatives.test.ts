import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  submitWithdrawnReferenceInputCancel,
  submitWithdrawnReferenceInputInit,
  submitWithdrawnReferenceInputStep01,
  submitWithdrawnReferenceInputStep02,
  submitWithdrawnReferenceInputStep03,
} from "../src/withdrawn-reference-input/index.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawnReferenceInputEmulatorHarness,
  publishWithdrawnReferenceInputReferenceScripts,
  setupWithdrawnReferenceInputScenario,
  submitRawWithdrawnReferenceInputCancel,
  submitRawWithdrawnReferenceInputStep02,
} from "./support/withdrawn-reference-input-emulator-v1.js";

describe("withdrawn-reference-input cancel, restart, resume and outsider negatives", () => {
  it("cancels at every step, restarts after each cancel, and resumes mid-thread", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const scenario = await setupWithdrawnReferenceInputScenario({ harness });
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const threadUnit = toUnit(
      harness.family.computationThread.policyId,
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
    const initThread = async () =>
      await submitWithdrawnReferenceInputInit({
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
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const step01 = async (threadOutRef: string) =>
      await submitWithdrawnReferenceInputStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.prepared.txInclusion,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
    const step02 = async (threadOutRef: string) =>
      await submitWithdrawnReferenceInputStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceInputs: scenario.prepared.referenceInputs,
        nativeTxCompactCbor: scenario.prepared.txInclusion.nativeTxCompactCbor,
        badReferenceInputIndex: BigInt(
          scenario.prepared.badReferenceInputIndex,
        ),
        referenceScriptUtxo: refs[1],
      });

    for (const cancelAt of [0, 1, 2] as const) {
      let threadOutRef = (await initThread()).nextThreadOutRef;
      if (cancelAt >= 1) {
        threadOutRef = (await step01(threadOutRef)).nextThreadOutRef;
      }
      if (cancelAt >= 2) {
        threadOutRef = (await step02(threadOutRef)).nextThreadOutRef;
      }
      const cancelled = await submitWithdrawnReferenceInputCancel({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: refs[cancelAt],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      expect(cancelled.cancelledStepIndex).toBe(cancelAt);
      for (const step of harness.family.steps) {
        await expect(
          harness.proverLucid.utxosAtWithUnit(
            step.spendingScriptAddress,
            threadUnit,
          ),
        ).resolves.toHaveLength(0);
      }
    }

    // Crash/resume seam: the second process needs only the persisted out-ref
    // and immutable evidence; each submitter re-reads state from the thread.
    const restarted = await initThread();
    const bound = await step01(restarted.nextThreadOutRef);
    await Promise.resolve();
    const opened = await step02(bound.nextThreadOutRef);
    const finalized = await submitWithdrawnReferenceInputStep03({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: opened.nextThreadOutRef,
      withdrawalMembership: scenario.prepared.withdrawalMembership,
      referenceScriptUtxo: refs[2],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(finalized.fraudProofUnit).toContain(
      `${harness.category.categoryId}${scenario.setup.headerHash}`,
    );
  }, 600_000);

  it("prevents an outsider from taking over or cancelling the prover's thread", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const scenario = await setupWithdrawnReferenceInputScenario({ harness });
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
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
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const bound = await submitWithdrawnReferenceInputStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.prepared.txInclusion,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });

    const outsiderAccount = generateEmulatorAccount({ lovelace: 0n });
    const outsiderLucid = await Lucid(harness.emulator, "Custom");
    outsiderLucid.selectWallet.fromSeed(outsiderAccount.seedPhrase);
    const outsider = resolveProverSigner({
      network,
      walletSeedPhrase: outsiderAccount.seedPhrase,
    });
    // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
    // derives the seed's base address while `resolveProverSigner` derives its
    // enterprise address, and the drivers below re-select through the signer,
    // so funding only the base address strands their transactions.
    const outsiderAddress = await outsiderLucid.wallet().address();
    const funding = await harness.funderLucid
      .newTx()
      .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
      .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
      .pay.ToAddress(outsider.address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(outsider.address, { lovelace: 1_000_000_000n })
      .complete();
    const signedFunding = await funding.sign.withWallet().complete();
    await harness.funderLucid.awaitTx(await signedFunding.submit());

    await expect(
      submitWithdrawnReferenceInputStep02({
        lucid: outsiderLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: outsider,
        threadOutRef: bound.nextThreadOutRef,
        referenceInputs: scenario.prepared.referenceInputs,
        nativeTxCompactCbor: scenario.prepared.txInclusion.nativeTxCompactCbor,
        badReferenceInputIndex: 0n,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(/not the signing wallet/);
    await expect(
      submitWithdrawnReferenceInputCancel({
        lucid: outsiderLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: outsider,
        threadOutRef: bound.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/only the prover can cancel/);

    const takeoverDatum = Data.to(
      {
        fraud_prover: outsider.paymentKeyHash,
        data: {
          missing_reference_input: scenario.prepared.missingReferenceInput,
          blocks_withdrawals_root: scenario.header.withdrawalsRoot,
          blocks_withdrawal_count: scenario.header.withdrawalCount,
        },
      },
      SDK.WithdrawnReferenceInputStep03Datum,
    );
    const opening: SDK.FieldOpening = {
      BodyFieldOpening: {
        native_tx_compact_cbor:
          scenario.prepared.txInclusion.nativeTxCompactCbor,
        carriage: {
          Inline: {
            preimage: encodeMidgardFieldPreimage(
              scenario.prepared.referenceInputs.map((input) =>
                SDK.encodeMidgardTxInputCanonical(input),
              ),
            ).toString("hex"),
          },
        },
      },
    };
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep02({
        lucid: outsiderLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: outsider,
        threadOutRef: bound.nextThreadOutRef,
        nextDatumCbor: takeoverDatum,
        buildRedeemer: ({ inputIndex, outputIndex }) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  reference_inputs_opening: opening,
                  bad_reference_input_index: 0n,
                },
              ],
            },
            SDK.WithdrawnReferenceInputStep02SpendRedeemer,
          ),
        referenceScriptUtxo: refs[1],
      }),
    );
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputCancel({
        lucid: outsiderLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: outsider,
        stepIndex: 1,
        threadOutRef: bound.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

    const resumed = await submitWithdrawnReferenceInputStep02({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: bound.nextThreadOutRef,
      referenceInputs: scenario.prepared.referenceInputs,
      nativeTxCompactCbor: scenario.prepared.txInclusion.nativeTxCompactCbor,
      badReferenceInputIndex: 0n,
      referenceScriptUtxo: refs[1],
    });
    expect(resumed.nextThreadOutRef).toBeTruthy();
  }, 600_000);
});
