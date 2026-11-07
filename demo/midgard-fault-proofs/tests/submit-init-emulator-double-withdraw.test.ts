/**
 * Both-polarity emulator coverage for the registered `double-withdraw`
 * family: payable duplicate -> permanent proof -> block removal, and honest
 * non-payable duplicate / same-leaf adversaries refused in the terminal script.
 */
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/lucid-data";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/double-withdraw/contracts.js";
import "../src/double-withdraw/index.js";
import "../src/double-withdraw/submit-double-withdraw-step-01.js";
import "../src/linear-fault-cancel.js";
import "../src/prepare-double-withdraw.js";
import "../src/remove-fraudulent-block.js";
import "../src/runtime.js";
import "../src/step-support.js";
import "../src/transition-trace/phas.js";
import "../src/tx-layout.js";
import "./support/native-script-decoding-emulator.js";
import "./support/submit-init-emulator-shared.js";
import "./submit-init-emulator-double-withdraw.setup-block.js";
import "./submit-init-emulator-double-withdraw.submit-raw-terminal.js";

import { outRefLabel } from "@al-ft/midgard-core";
import { generateEmulatorAccount, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { DOUBLE_WITHDRAW_CATEGORY_LABEL } from "../src/double-withdraw/contracts.js";
import {
  submitDoubleWithdrawInit,
  submitDoubleWithdrawStep01,
  submitDoubleWithdrawStep02,
} from "../src/double-withdraw/index.js";
import { parseSubmitDoubleWithdrawInclusion } from "../src/double-withdraw/submit-double-withdraw-step-01.js";
import { submitLinearFaultCancel } from "../src/linear-fault-cancel.js";
import { prepareDoubleWithdrawFromCommittedLeaves } from "../src/prepare-double-withdraw.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { fetchUtxoByOutRef, parseOutRef } from "../src/runtime.js";
import {
  FIRST_ID,
  HONEST_DUPLICATE_INFO,
  inclusionFor,
  makeHarness,
  PAYABLE_INFO,
  publishStepReferences,
  SECOND_ID,
  setupBlock,
} from "./submit-init-emulator-double-withdraw.setup-block.js";
import {
  submitRawCancel,
  submitRawTerminal,
} from "./submit-init-emulator-double-withdraw.submit-raw-terminal.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("double-withdraw emulator lifecycle", () => {
  it("proves the payable duplicate, resumes from step-02, and removes the fraudulent block", async () => {
    const harness = await makeHarness();
    const block = await setupBlock({ harness, secondInfo: PAYABLE_INFO });
    const refs = await publishStepReferences({
      lucid: harness.funderLucid,
      contracts: harness.doubleWithdraw,
    });
    const plan = await prepareDoubleWithdrawFromCommittedLeaves({
      headerHash: block.setup.headerHash,
      committedWithdrawalsRoot: block.counted.root,
      withdrawalCount: block.counted.count,
      entries: block.entries,
    });
    expect(plan.firstLeaf.withdrawalId).toEqual(FIRST_ID);
    expect(plan.secondLeaf.withdrawalId).toEqual(SECOND_ID);

    const init = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitDoubleWithdrawStep01({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: parseSubmitDoubleWithdrawInclusion(plan.firstInclusion),
      referenceScriptUtxo: refs[0],
    });
    // Crash/resume surface: everything required is re-read from the surviving
    // thread, state-queue node and retained prepared inclusion.
    const resumedThread = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step01.secondStepAddress,
      init.computationThreadUnit,
    );
    expect(outRefLabel(resumedThread)).toBe(step01.nextThreadOutRef);
    const terminal = await submitDoubleWithdrawStep02({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(resumedThread),
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: parseSubmitDoubleWithdrawInclusion(plan.secondInclusion),
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        step01.secondStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      terminal.fraudProofAddress,
      terminal.fraudProofUnit,
    );
    expect(outRefLabel(proofUtxo)).toBe(terminal.fraudProofOutRef);

    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    const removed = await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: deployment,
      network,
      signer: harness.proverSigner,
      fraudCategory: "doubleWithdraw",
      fraudulentHeaderHash: block.setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removed.fraudCategory).toBe("doubleWithdraw");
    expect(removed.transactions[0]?.slashingApproach).toBe(
      "SlashActiveOperator",
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        block.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retained = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      terminal.fraudProofAddress,
      terminal.fraudProofUnit,
    );
    expect(outRefLabel(retained)).toBe(terminal.fraudProofOutRef);
    await expect(
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: deployment,
        network,
        signer: harness.proverSigner,
        fraudCategory: "doubleWithdraw",
        fraudulentHeaderHash: block.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/u);
  }, 600_000);

  it("refuses an honest non-payable duplicate and same-leaf pairing on chain, and enforces cancel ownership", async () => {
    const harness = await makeHarness();
    const block = await setupBlock({
      harness,
      secondInfo: HONEST_DUPLICATE_INFO,
    });
    const refs = await publishStepReferences({
      lucid: harness.funderLucid,
      contracts: harness.doubleWithdraw,
    });
    await expect(
      prepareDoubleWithdrawFromCommittedLeaves({
        headerHash: block.setup.headerHash,
        committedWithdrawalsRoot: block.counted.root,
        withdrawalCount: block.counted.count,
        entries: block.entries,
      }),
    ).rejects.toThrow(/no_payable_duplicate_pair/u);
    const [firstInclusion, secondInclusion] = await Promise.all([
      inclusionFor({ counted: block.counted, leaf: block.entries[0]! }),
      inclusionFor({ counted: block.counted, leaf: block.entries[1]! }),
    ]);
    const init = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const step01 = await submitDoubleWithdrawStep01({
      lucid: harness.proverLucid,
      contracts: harness.doubleWithdraw,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: init.nextThreadOutRef,
      stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
      inclusion: firstInclusion,
      referenceScriptUtxo: refs[0],
    });
    await expect(
      submitDoubleWithdrawStep01({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: firstInclusion,
        referenceScriptUtxo: refs[0],
      }),
    ).rejects.toThrow(/not locked at double-withdraw step 01/u);
    await expect(
      submitDoubleWithdrawStep02({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: secondInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/second leaf is identical.*not payable/su);
    expect(
      await expectOnchainRefusal(() =>
        submitRawTerminal({
          harness,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          blockOutRef: block.setup.fraudulentBlockOutRef,
          inclusion: secondInclusion,
          referenceScript: refs[1],
        }),
      ),
    ).not.toBe("");
    await expect(
      submitDoubleWithdrawStep02({
        lucid: harness.proverLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: firstInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/second leaf is identical/u);
    expect(
      await expectOnchainRefusal(() =>
        submitRawTerminal({
          harness,
          signer: harness.proverSigner,
          threadOutRef: step01.nextThreadOutRef,
          blockOutRef: block.setup.fraudulentBlockOutRef,
          inclusion: firstInclusion,
          referenceScript: refs[1],
        }),
      ),
    ).not.toBe("");

    const outsider = generateEmulatorAccount({ lovelace: 0n });
    const outsiderLucid = await Lucid(harness.emulator, "Custom");
    outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
    const outsiderSigner = (
      await import("../src/runtime.js")
    ).resolveProverSigner({
      network,
      walletSeedPhrase: outsider.seedPhrase,
    });
    // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
    // derives the seed's base address while `resolveProverSigner` derives its
    // enterprise address, and the cancel submitter re-selects through the
    // signer, so funding only the base address strands the transaction.
    const funding = await harness.funderLucid
      .newTx()
      .pay.ToAddress(await outsiderLucid.wallet().address(), {
        lovelace: 1_000_000_000n,
      })
      .pay.ToAddress(outsiderSigner.address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(outsiderSigner.address, { lovelace: 1_000_000_000n })
      .complete();
    await harness.funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    await expect(
      submitLinearFaultCancel({
        lucid: outsiderLucid,
        family: DOUBLE_WITHDRAW_CATEGORY_LABEL,
        steps: harness.doubleWithdraw.steps,
        computationThread: harness.doubleWithdraw.computationThread,
        categoryId: harness.category.categoryId,
        signer: outsiderSigner,
        threadOutRef: step01.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/double-withdraw: signer does not own thread/u);
    await expect(
      submitDoubleWithdrawStep02({
        lucid: outsiderLucid,
        contracts: harness.doubleWithdraw,
        categoryId: harness.category.categoryId,
        network,
        signer: outsiderSigner,
        threadOutRef: step01.nextThreadOutRef,
        stateQueueBlockOutRef: block.setup.fraudulentBlockOutRef,
        inclusion: secondInclusion,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/not the signing wallet/u);
    const threadUtxo = await fetchUtxoByOutRef({
      lucid: harness.proverLucid,
      outRef: parseOutRef(step01.nextThreadOutRef, "thread"),
      label: "double-withdraw step-02 thread",
    });
    expect(
      await expectOnchainRefusal(() =>
        submitRawCancel({
          lucid: outsiderLucid,
          contracts: harness.doubleWithdraw,
          signer: outsiderSigner,
          threadUtxo,
          categoryId: harness.category.categoryId,
          referenceScript: refs[1],
          computationThreadReference:
            harness.witnessReferenceScripts.computationThreadMint!,
        }),
      ),
    ).not.toBe("");
    const cancelled = await submitLinearFaultCancel({
      lucid: harness.proverLucid,
      family: DOUBLE_WITHDRAW_CATEGORY_LABEL,
      steps: harness.doubleWithdraw.steps,
      computationThread: harness.doubleWithdraw.computationThread,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      referenceScriptUtxo: refs[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelled.cancelledStepIndex).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.doubleWithdraw.steps[1].spendingScriptAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    // Re-init after the cancelled NFT burn and exercise the step-01 cancel arm.
    const retry = await submitDoubleWithdrawInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.doubleWithdraw,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const cancelledAtEntry = await submitLinearFaultCancel({
      lucid: harness.proverLucid,
      family: DOUBLE_WITHDRAW_CATEGORY_LABEL,
      steps: harness.doubleWithdraw.steps,
      computationThread: harness.doubleWithdraw.computationThread,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: retry.nextThreadOutRef,
      referenceScriptUtxo: refs[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(cancelledAtEntry.cancelledStepIndex).toBe(0);
  }, 600_000);
});
