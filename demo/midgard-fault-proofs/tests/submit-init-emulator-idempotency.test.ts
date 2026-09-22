import {
  encodeMidgardNativeTxCanonical,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  captureLocallyEvaluatedTransaction,
  prepareMinFeeFromTransactions,
  submitCapturedTransaction,
  submitMinFeeInit,
  submitMinFeeStep01,
  submitMinFeeStep02,
  submitRemoveFraudulentBlock,
  workflowTransactionInputOutRefs,
} from "../src/index.js";
import type { MinFeeFieldItemCbors } from "../src/submit-min-fee-step-02.js";
import { parseSubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  buildProvedDoubleSpendFixture,
  expectRemovedFraudProofState,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("Q53 fraud-proof reward idempotency", () => {
  const expectExactNonZeroFullSlashEconomics = (
    signed: Awaited<
      ReturnType<typeof captureLocallyEvaluatedTransaction>
    >["signed"],
  ) => {
    const economics = SDK.getProtocolParameters(network);
    expect(economics.slashing_penalty).toBeGreaterThan(0n);
    expect(economics.fraud_prover_reward).toBeGreaterThan(0n);
    expect(economics.required_bond).toBe(
      economics.slashing_penalty + economics.fraud_prover_reward,
    );
    expect(signed.toTransaction().body().fee()).toBe(
      economics.slashing_penalty,
    );
  };

  it("lets only one signed replacement removal consume the queue and operator bond", async () => {
    const fixture = await buildProvedDoubleSpendFixture();

    const capture = async (label: string) => {
      try {
        return await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) =>
            await submitRemovalForFixture(fixture, { preSubmitBoundary }),
        );
      } catch (cause) {
        throw new Error(`${label}: ${String(cause)}`);
      }
    };
    const first = await capture("first");
    fixture.emulator.awaitSlot(1);
    const replacement = await capture("replacement");

    expect(replacement.txHash).not.toBe(first.txHash);
    expectExactNonZeroFullSlashEconomics(first.signed);
    expectExactNonZeroFullSlashEconomics(replacement.signed);
    const firstInputs = workflowTransactionInputOutRefs(first.signed);
    const replacementInputs = workflowTransactionInputOutRefs(
      replacement.signed,
    );
    expect(
      firstInputs.filter((outRef) => replacementInputs.includes(outRef)),
    ).toEqual(
      expect.arrayContaining([
        fixture.fraudulentBlockOutRef,
        outRefLabel(fixture.setup.activeOperatorNode),
      ]),
    );

    const winnerHash = await submitCapturedTransaction(first);
    await expect(fixture.emulator.awaitTx(winnerHash)).resolves.toBe(true);
    await expect(submitCapturedTransaction(replacement)).rejects.toThrow(
      /already spent|input.*not found|utxo/iu,
    );
    await expectRemovedFraudProofState(fixture);
  }, 300_000);

  it("lets only one of two different-family proofs for the same header consume the bond and queue", async () => {
    const atStage = async <T>(label: string, action: () => Promise<T>) => {
      try {
        return await action();
      } catch (cause) {
        throw new Error(`${label}: ${String(cause)}`);
      }
    };
    const fixture = await buildProvedDoubleSpendFixture({
      headerMinimumFee: 1n,
    });
    const minFeeChain = fixture.contracts.fraudProofContracts.minFee;
    const category = fixture.catalogue.categories.minFee;
    if (minFeeChain === undefined || category === undefined) {
      throw new Error("Q53 fixture omitted the registered min-fee family");
    }
    const minFee = {
      steps: minFeeChain.steps,
      computationThread: {
        policyId: fixture.contracts.computationThread.policyId,
        mintingScript: fixture.contracts.computationThread.mintingScript,
      },
      fraudProof: {
        policyId: fixture.contracts.fraudProof.policyId,
        mintingScript: fixture.contracts.fraudProof.mintingScript,
        spendingScriptAddress:
          fixture.contracts.fraudProof.spendingScriptAddress,
      },
      hubOraclePolicyId: fixture.contracts.hubOracle.policyId,
      stateQueuePolicyId: fixture.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        fixture.contracts.fieldPreimageCertificate.policyId,
    };
    const prepared = await prepareMinFeeFromTransactions({
      headerHash: fixture.headerHash,
      transactions: [
        {
          nodeTxId: fixture.transactionInclusion.tx1.nativeTxId,
          txCbor: encodeMidgardNativeTxCanonical(
            fixture.transactionInclusion.tx1Full,
          ).toString("hex"),
        },
        {
          nodeTxId: fixture.transactionInclusion.tx2.nativeTxId,
          txCbor: encodeMidgardNativeTxCanonical(
            fixture.transactionInclusion.tx2Full,
          ).toString("hex"),
        },
      ],
      // The authenticated root is the counted commitment the header carries,
      // not the bare trie root `transactionInclusion.transactionsRoot` holds.
      expectedTransactionsRoot: fixture.fraudulentHeader.transactionsRoot,
      minFeeA: 0n,
      minFeeB: 1n,
      categoryId: category.categoryId,
    });
    const stepReferenceEntryNames = [
      "fraudProofMinFee",
      "fraudProofMinFeeStep02",
    ] as const;
    const stepReferences = await publishFraudProofChainReferenceScripts({
      lucid: fixture.proverLucid,
      steps: minFeeChain.steps,
      entryNames: stepReferenceEntryNames,
      familyLabel: "Q53 min-fee",
    });
    const catalogue = {
      policyId: fixture.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        fixture.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: fixture.catalogue.root,
    };
    const init = await atStage("min-fee init", async () =>
      submitMinFeeInit({
        lucid: fixture.proverLucid,
        blueprint: fixture.realBlueprint,
        network,
        contracts: minFee,
        category,
        catalogue,
        signer: fixture.proverSigner,
        fraudulentBlockOutRef: fixture.fraudulentBlockOutRef,
        witnessReferenceScripts: fixture.witnessReferenceScripts,
      }),
    );
    const step01 = await atStage("min-fee step-01", async () =>
      submitMinFeeStep01({
        lucid: fixture.proverLucid,
        blueprint: fixture.realBlueprint,
        contracts: minFee,
        categoryId: category.categoryId,
        network,
        signer: fixture.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: fixture.fraudulentBlockOutRef,
        txInclusion: parseSubmitStep01TxInclusion(prepared.tx.txInclusion),
        referenceScriptUtxo: stepReferences[stepReferenceEntryNames[0]].utxo,
        witnessReferenceScripts: fixture.witnessReferenceScripts,
      }),
    );
    const fieldItemCbors = prepared.tx.fieldItemCbors.map((field) =>
      field.map((item) => Buffer.from(item, "hex")),
    ) as unknown as MinFeeFieldItemCbors;
    const minFeeProof = await atStage("min-fee step-02", async () =>
      submitMinFeeStep02({
        lucid: fixture.proverLucid,
        contracts: minFee,
        categoryId: category.categoryId,
        signer: fixture.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
        witnessSet: prepared.tx.witnessSet,
        fieldItemCbors,
        referenceScriptUtxo: stepReferences[stepReferenceEntryNames[1]].utxo,
        witnessReferenceScripts: fixture.witnessReferenceScripts,
      }),
    );
    expect(minFeeProof.minimumFee).toBe(1n);
    expect(minFeeProof.fee).toBe(0n);
    expect(minFeeProof.fraudProofUnit).not.toBe(
      fixture.step04Result.fraudProofUnit,
    );
    expect(minFeeProof.fraudProofUnit.endsWith(fixture.headerHash)).toBe(true);
    expect(
      fixture.step04Result.fraudProofUnit.endsWith(fixture.headerHash),
    ).toBe(true);

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: fixture.proverLucid,
      contracts: fixture.contracts,
    });
    const removalDeploymentInfo = buildRemovalDeploymentInfo(
      { ...fixture.contracts, minFee },
      fixture.catalogue,
      {
        removalReferenceScripts: removalReferences.published,
        fraudProofReferenceScripts: stepReferences,
      },
    );

    const doubleSpendRemoval = await captureLocallyEvaluatedTransaction(
      async (preSubmitBoundary) =>
        await submitRemovalForFixture(fixture, { preSubmitBoundary }),
    );
    const removeNow = BigInt(fixture.emulator.now());
    const minFeeRemoval = await captureLocallyEvaluatedTransaction(
      async (preSubmitBoundary) =>
        await submitRemoveFraudulentBlock({
          lucid: fixture.proverLucid,
          blueprint: fixture.realBlueprint,
          deploymentInfo: removalDeploymentInfo,
          network,
          signer: fixture.proverSigner,
          fraudCategory: "minFee",
          fraudulentHeaderHash: fixture.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
          preSubmitBoundary,
        }),
    );
    expectExactNonZeroFullSlashEconomics(doubleSpendRemoval.signed);
    expectExactNonZeroFullSlashEconomics(minFeeRemoval.signed);
    const doubleSpendInputs = workflowTransactionInputOutRefs(
      doubleSpendRemoval.signed,
    );
    const minFeeInputs = workflowTransactionInputOutRefs(minFeeRemoval.signed);
    expect(
      doubleSpendInputs.filter((outRef) => minFeeInputs.includes(outRef)),
    ).toEqual(
      expect.arrayContaining([
        fixture.fraudulentBlockOutRef,
        outRefLabel(fixture.setup.activeOperatorNode),
      ]),
    );

    const winnerHash = await submitCapturedTransaction(doubleSpendRemoval);
    await expect(fixture.emulator.awaitTx(winnerHash)).resolves.toBe(true);
    await expect(submitCapturedTransaction(minFeeRemoval)).rejects.toThrow(
      /already spent|input.*not found|utxo/iu,
    );
    await expectRemovedFraudProofState(fixture);
    await expectSingleUtxoWithUnit(
      fixture.proverLucid,
      minFee.fraudProof.spendingScriptAddress,
      minFeeProof.fraudProofUnit,
    );
    await expectSingleUtxoWithUnit(
      fixture.proverLucid,
      minFee.fraudProof.spendingScriptAddress,
      fixture.step04Result.fraudProofUnit,
    );
    expect(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minFee).toBe(
      category.categoryId,
    );
  }, 600_000);
});
