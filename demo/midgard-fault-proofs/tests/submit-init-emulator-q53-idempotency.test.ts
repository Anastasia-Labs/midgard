import {
  encodeMidgardNativeTxCanonicalV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  captureLocallyEvaluatedTransactionV1,
  prepareMinFeeFromTransactions,
  submitCapturedTransactionV1,
  submitMinFeeInit,
  submitMinFeeStep01,
  submitMinFeeStep02,
  submitRemoveFraudulentBlock,
  workflowTransactionInputOutRefsV1,
} from "../src/index.js";
import type { MinFeeFieldItemCborsV1 } from "../src/submit-min-fee-step-02.js";
import { parseSubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  buildProvedDoubleSpendFixture,
  expectRemovedFraudProofState,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import {
  expectSingleUtxoWithUnit,
  network,
  publishFraudProofChainReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("Q53 fraud-proof reward idempotency", () => {
  it("lets only one signed replacement removal consume the queue and operator bond", async () => {
    const fixture = await buildProvedDoubleSpendFixture();

    const capture = async (label: string) => {
      try {
        return await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) =>
            await submitRemovalForFixture(fixture, { preSubmitBoundary }),
        );
      } catch (cause) {
        throw new Error(`${label}: ${String(cause)}`);
      }
    };
    const first = await capture("first");
    await fixture.emulator.awaitSlot(1);
    const replacement = await capture("replacement");

    expect(replacement.txHash).not.toBe(first.txHash);
    const firstInputs = workflowTransactionInputOutRefsV1(first.signed);
    const replacementInputs = workflowTransactionInputOutRefsV1(
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

    const winnerHash = await submitCapturedTransactionV1(first);
    await expect(fixture.emulator.awaitTx(winnerHash)).resolves.toBe(true);
    await expect(submitCapturedTransactionV1(replacement)).rejects.toThrow(
      /already spent|input.*not found|utxo/iu,
    );
    await expectRemovedFraudProofState(fixture);
  }, 300_000);

  it.skip("lets only one of two different-family proofs for the same header consume the bond and queue", async () => {
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
          txCbor: encodeMidgardNativeTxCanonicalV1(
            fixture.transactionInclusion.tx1Full,
          ).toString("hex"),
        },
        {
          nodeTxId: fixture.transactionInclusion.tx2.nativeTxId,
          txCbor: encodeMidgardNativeTxCanonicalV1(
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
    const stepReferences = await publishFraudProofChainReferenceScripts({
      lucid: fixture.proverLucid,
      steps: minFeeChain.steps,
      entryNames: ["fraudProofMinFee", "fraudProofMinFeeStep02"],
      familyLabel: "Q53 min-fee",
    });
    const catalogue = {
      policyId: fixture.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        fixture.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: fixture.catalogue.root,
    };
    const init = await submitMinFeeInit({
      lucid: fixture.proverLucid,
      blueprint: fixture.realBlueprint,
      network,
      contracts: minFee,
      category,
      catalogue,
      signer: fixture.proverSigner,
      fraudulentBlockOutRef: fixture.fraudulentBlockOutRef,
      witnessReferenceScripts: fixture.witnessReferenceScripts,
    });
    const step01 = await submitMinFeeStep01({
      lucid: fixture.proverLucid,
      blueprint: fixture.realBlueprint,
      contracts: minFee,
      categoryId: category.categoryId,
      network,
      signer: fixture.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: fixture.fraudulentBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(prepared.tx.txInclusion),
      referenceScriptUtxo: stepReferences[0].utxo,
      witnessReferenceScripts: fixture.witnessReferenceScripts,
    });
    const fieldItemCbors = prepared.tx.fieldItemCbors.map((field) =>
      field.map((item) => Buffer.from(item, "hex")),
    ) as unknown as MinFeeFieldItemCborsV1;
    const minFeeProof = await submitMinFeeStep02({
      lucid: fixture.proverLucid,
      contracts: minFee,
      categoryId: category.categoryId,
      signer: fixture.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
      witnessSet: prepared.tx.witnessSet,
      fieldItemCbors,
      referenceScriptUtxo: stepReferences[1].utxo,
      witnessReferenceScripts: fixture.witnessReferenceScripts,
    });
    expect(minFeeProof.minimumFee).toBe(1n);
    expect(minFeeProof.fee).toBe(0n);
    expect(minFeeProof.fraudProofUnit).not.toBe(
      fixture.step04Result.fraudProofUnit,
    );
    expect(minFeeProof.fraudProofUnit.endsWith(fixture.headerHash)).toBe(true);
    expect(
      fixture.step04Result.fraudProofUnit.endsWith(fixture.headerHash),
    ).toBe(true);

    const doubleSpendRemoval = await captureLocallyEvaluatedTransactionV1(
      async (preSubmitBoundary) =>
        await submitRemovalForFixture(fixture, { preSubmitBoundary }),
    );
    const removeNow = BigInt(fixture.emulator.now());
    const minFeeRemoval = await captureLocallyEvaluatedTransactionV1(
      async (preSubmitBoundary) =>
        await submitRemoveFraudulentBlock({
          lucid: fixture.proverLucid,
          blueprint: fixture.realBlueprint,
          deploymentInfo: fixture.deploymentInfo,
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
    const doubleSpendInputs = workflowTransactionInputOutRefsV1(
      doubleSpendRemoval.signed,
    );
    const minFeeInputs = workflowTransactionInputOutRefsV1(
      minFeeRemoval.signed,
    );
    expect(
      doubleSpendInputs.filter((outRef) => minFeeInputs.includes(outRef)),
    ).toEqual(
      expect.arrayContaining([
        fixture.fraudulentBlockOutRef,
        outRefLabel(fixture.setup.activeOperatorNode),
      ]),
    );

    const winnerHash = await submitCapturedTransactionV1(doubleSpendRemoval);
    await expect(fixture.emulator.awaitTx(winnerHash)).resolves.toBe(true);
    await expect(submitCapturedTransactionV1(minFeeRemoval)).rejects.toThrow(
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
