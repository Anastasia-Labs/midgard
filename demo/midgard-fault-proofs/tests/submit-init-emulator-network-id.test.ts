import { outRefLabel } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  EMPTY_MERKLE_TREE_ROOT,
  FraudProofTokenDatum,
  NetworkIdStep02Datum,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  planNetworkIdOutputsOpening,
  submitNetworkIdCancel,
  submitNetworkIdInit,
  submitNetworkIdPostUtxoStep01,
  submitNetworkIdStep01,
  submitNetworkIdStep02,
} from "../src/network-id/index.js";
import { publishProofChunks } from "../src/publish-proof-chunks.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  buildNetworkIdFixture,
  buildNetworkIdPostUtxoFixture,
  makeNetworkIdEmulatorHarness,
  NETWORK_ID_EMULATOR_CATEGORY_ID,
  publishNetworkIdReferenceScripts,
} from "./support/network-id-emulator.js";
import {
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  makeHeader,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import {
  MAXIMUM_PROOF_STEP_COUNT,
  syntheticDeepMembershipProof,
  syntheticDeepSharedRootProofs,
} from "./support/synthetic-deep-proof.js";

describe("Q35 network-id real-fault lifecycle", () => {
  it.each([
    {
      label: "raw unprotected network nibble 2",
      outputNetworkId: 2,
      protectedAddress: false,
    },
    {
      label: "raw protected network nibble 15",
      outputNetworkId: 7,
      protectedAddress: true,
    },
  ])(
    "mints permanent evidence and removes $label",
    async ({ outputNetworkId, protectedAddress }) => {
      const harness = await makeNetworkIdEmulatorHarness();
      const preSubmitStages: string[] = [];
      const preSubmitBoundary =
        (stage: string) =>
        (transaction: {
          readonly txHash: string;
          readonly referenceScripts: readonly unknown[];
        }) => {
          expect(transaction.txHash).toMatch(/^[0-9a-f]{64}$/u);
          expect(transaction.referenceScripts.length).toBeGreaterThan(0);
          preSubmitStages.push(stage);
        };
      const [step01Ref, step02Ref] = await publishNetworkIdReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.networkId,
      });
      const removalReferenceScripts = await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      });
      const fixture = await buildNetworkIdFixture({
        outputNetworkId,
        protectedAddress,
      });
      const setup = await setupFraudulentBlock({
        funderLucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        catalogue: harness.catalogue,
        fixture,
      });
      const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
      const init = await submitNetworkIdInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.networkId,
        category: harness.category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        preSubmitBoundary: preSubmitBoundary("init"),
      });
      expect(init.fraudCategoryId).toBe(NETWORK_ID_EMULATOR_CATEGORY_ID);

      const firstStep = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        init.firstStepAddress,
        init.computationThreadUnit,
      );
      const step01 = await submitNetworkIdStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        prepared,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        preSubmitBoundary: preSubmitBoundary("step01"),
      });
      const secondStep = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        step01.secondStepAddress,
        init.computationThreadUnit,
      );
      expect(Data.from(secondStep.datum!, NetworkIdStep02Datum)).toEqual({
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: step01.state,
      });

      const opening = planNetworkIdOutputsOpening({
        prepared,
        owner: harness.proverSigner.paymentKeyHash,
      });
      const step02 = await submitNetworkIdStep02({
        lucid: harness.proverLucid,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(secondStep),
        prepared,
        outputsOpeningPlan: opening,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        preSubmitBoundary: preSubmitBoundary("step02"),
      });
      expect(step02.outputOpeningTier).toBe("Inline");
      expect(step02.fraudProofUnit).toBe(
        toUnit(
          harness.networkId.fraudProof.policyId,
          init.computationThreadAssetName,
        ),
      );
      const proofUtxo = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        step02.fraudProofAddress,
        step02.fraudProofUnit,
      );
      const paymentCredential = getAddressDetails(
        await harness.proverLucid.wallet().address(),
      ).paymentCredential;
      if (paymentCredential?.type !== "Key") {
        throw new Error("expected prover payment key credential");
      }
      expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
        fraud_prover: paymentCredential.hash,
      });

      const baseDeployment = buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalReferenceScripts.published },
      );
      const deploymentInfo = {
        ...baseDeployment,
        contracts: {
          ...baseDeployment.contracts,
          fraudProofNetworkId: {
            scriptHash: harness.networkId.steps[0].spendingScriptHash,
          },
          fraudProofNetworkIdStep02: {
            scriptHash: harness.networkId.steps[1].spendingScriptHash,
          },
        },
      };
      const removeNow = BigInt(harness.emulator.now());
      const removal = await submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "networkId",
        fraudulentHeaderHash: setup.headerHash,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
        preSubmitBoundary: preSubmitBoundary("remove"),
      });
      expect(removal.fraudCategory).toBe("networkId");
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.contracts.stateQueue.spendingScriptAddress,
          setup.stateQueueBlockUnit,
        ),
      ).resolves.toHaveLength(0);
      expect(
        outRefLabel(
          await expectSingleUtxoWithUnit(
            harness.proverLucid,
            step02.fraudProofAddress,
            step02.fraudProofUnit,
          ),
        ),
      ).toBe(outRefLabel(proofUtxo));
      expect(preSubmitStages).toEqual(["init", "step01", "step02", "remove"]);
    },
    240_000,
  );

  it("proves a zero-transaction post-UTxO introduction and retains evidence through correction", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const preSubmitStages: string[] = [];
    const preSubmitBoundary =
      (stage: string) =>
      (transaction: {
        readonly txHash: string;
        readonly referenceScripts: readonly unknown[];
      }) => {
        expect(transaction.txHash).toMatch(/^[0-9a-f]{64}$/u);
        expect(transaction.referenceScripts.length).toBeGreaterThan(0);
        preSubmitStages.push(stage);
      };
    const [step01Ref, step02Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const removalReferenceScripts = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const fixture = await buildNetworkIdPostUtxoFixture();
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      preSubmitBoundary: preSubmitBoundary("init"),
    });
    const firstStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    const step01 = await submitNetworkIdPostUtxoStep01({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.networkId,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(firstStep),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      prepared,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      preSubmitBoundary: preSubmitBoundary("step01-post-utxo"),
    }).catch((error: unknown) => {
      throw new Error(
        `post-UTxO step-01 failed after boundaries ${preSubmitStages.join(",")}: ${String(error)}`,
      );
    });
    const secondStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step01.secondStepAddress,
      init.computationThreadUnit,
    );
    const step02 = await submitNetworkIdStep02({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      contracts: harness.networkId,
      categoryId: harness.category.categoryId,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(secondStep),
      prepared,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      preSubmitBoundary: preSubmitBoundary("step02"),
    }).catch((error: unknown) => {
      throw new Error(
        `post-UTxO step-02 failed after boundaries ${preSubmitStages.join(",")}: ${String(error)}`,
      );
    });
    const proofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalReferenceScripts.published },
    );
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        fraudProofNetworkId: {
          scriptHash: harness.networkId.steps[0].spendingScriptHash,
        },
        fraudProofNetworkIdStep02: {
          scriptHash: harness.networkId.steps[1].spendingScriptHash,
        },
      },
    };
    const removeNow = BigInt(harness.emulator.now());
    await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      fraudCategory: "networkId",
      fraudulentHeaderHash: setup.headerHash,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
      preSubmitBoundary: preSubmitBoundary("remove"),
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    expect(
      outRefLabel(
        await expectSingleUtxoWithUnit(
          harness.proverLucid,
          step02.fraudProofAddress,
          step02.fraudProofUnit,
        ),
      ),
    ).toBe(outRefLabel(proofUtxo));
    expect(preSubmitStages).toEqual([
      "init",
      "step01-post-utxo",
      "step02",
      "remove",
    ]);
  }, 240_000);

  it("fits both staged maximum-depth post-UTxO proofs through published chunks", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const [step01Ref, step02Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const base = await buildNetworkIdPostUtxoFixture({
      outputNetworkId: 7,
      protectedAddress: true,
    });
    const key = Buffer.from(base.prepared.outRefKeyCbor, "hex");
    const post = syntheticDeepMembershipProof({
      key,
      value: Buffer.from(base.prepared.descriptorCbor, "hex"),
      branchLevels: MAXIMUM_PROOF_STEP_COUNT,
    });
    const keyFirstNibble = computeHash32(key)[0]! >> 4;
    let dummyKey = Buffer.alloc(key.length, 0xa5);
    for (let suffix = 0; suffix <= 0xff; suffix += 1) {
      const candidate = Buffer.from(dummyKey);
      candidate[candidate.length - 1] = suffix;
      if (computeHash32(candidate)[0]! >> 4 !== keyFirstNibble) {
        dummyKey = candidate;
        break;
      }
    }
    const previous = syntheticDeepSharedRootProofs({
      claims: [{ key }, { key: dummyKey }],
      branchLevels: MAXIMUM_PROOF_STEP_COUNT,
    });
    const predecessorOpening = previous.openings[0];
    if (predecessorOpening === undefined || predecessorOpening.isMembership) {
      throw new Error("expected a maximum-depth predecessor absence opening");
    }
    const prepared = {
      ...base.prepared,
      postUtxosRoot: post.transactionsPhasRoot,
      prevUtxosRoot: previous.root,
      membershipProofCbor: post.proofCbor,
      membershipProof: Data.from(post.proofCbor, Proof),
      predecessorProofCbor: predecessorOpening.proofCbor,
      predecessorProof: Data.from(predecessorOpening.proofCbor, Proof),
    };
    const predecessorSetup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture: {
        transactionsRoot: base.transactionsRoot,
        l2TransactionCount: base.l2TransactionCount,
        prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
        utxosRoot: previous.root,
        headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
      },
    });
    const targetStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: predecessorSetup.header.endTime,
      emulator: harness.emulator,
    });
    const targetHeader = {
      ...makeHeader(
        predecessorSetup.header.operatorVkey,
        targetStart,
        EMPTY_MERKLE_TREE_ROOT,
      ),
      prevHeaderHash: predecessorSetup.headerHash,
      prevUtxosRoot: previous.root,
      utxosRoot: post.transactionsPhasRoot,
    };
    const target = await submitSuccessorBlockTx({
      lucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      anchorBlockUnit: predecessorSetup.stateQueueBlockUnit,
      header: targetHeader,
      hubOracle: predecessorSetup.hubOracle,
      scheduler: predecessorSetup.scheduler,
      activeOperatorNode: predecessorSetup.activeOperatorNode,
      activeOperatorNodeUnit: predecessorSetup.activeOperatorNodeUnit,
    });
    const rebound = { ...prepared, headerHash: target.successorHeaderHash };
    const postPublication = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        publishProofChunks({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          proofCbor: post.proofCbor,
        }),
    );
    const predecessorPublication = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        publishProofChunks({
          lucid: harness.proverLucid,
          network,
          signer: harness.proverSigner,
          proofCbor: predecessorOpening.proofCbor,
        }),
    );
    expect(postPublication.result.proofStepCount).toBe(
      MAXIMUM_PROOF_STEP_COUNT,
    );
    expect(predecessorPublication.result.proofStepCount).toBe(
      MAXIMUM_PROOF_STEP_COUNT,
    );
    expect(postPublication.result.chunks).toHaveLength(4);
    expect(predecessorPublication.result.chunks).toHaveLength(4);

    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: target.successorOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const firstStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    const step01 = await captureEmulatorSubmission(harness.emulator, async () =>
      submitNetworkIdPostUtxoStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: target.successorOutRef,
        prepared: rebound,
        referenceScriptUtxo: step01Ref,
        publishedProofChunks: postPublication.result.chunks,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const secondStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step01.result.secondStepAddress,
      init.computationThreadUnit,
    );
    const step02 = await captureEmulatorSubmission(harness.emulator, async () =>
      submitNetworkIdStep02({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(secondStep),
        prepared: rebound,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        publishedPredecessorProofChunks: predecessorPublication.result.chunks,
      }),
    );
    const { maxTxSize, maxTxExMem, maxTxExSteps } =
      harness.emulator.protocolParameters;
    for (const measured of [step01.measurement, step02.measurement]) {
      expect(measured.completeSignedBytes).toBeLessThanOrEqual(maxTxSize);
      expect(measured.executionMemory).toBeLessThanOrEqual(maxTxExMem);
      expect(measured.executionSteps).toBeLessThanOrEqual(maxTxExSteps);
      expect(measured.plutusV3ScriptCount).toBe(0);
    }
    expect(step01.measurement.referenceInputCount).toBe(8);
    expect(step02.measurement.referenceInputCount).toBe(8);
    expect(step02.result.fraudProofUnit).toBe(
      toUnit(
        harness.networkId.fraudProof.policyId,
        init.computationThreadAssetName,
      ),
    );
  }, 240_000);

  it("refuses a protected matching-network output before submission", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const [step01Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const fixture = await buildNetworkIdFixture({ outputNetworkId: 0 });
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const prepared = { ...fixture.prepared, headerHash: setup.headerHash };
    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
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
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    await expect(
      submitNetworkIdStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: outRefLabel(firstStep),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        prepared,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow("does not contain the requested fault");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 180_000);

  it("cancels the reference-script-only thread without minting evidence", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const [step01Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const fixture = await buildNetworkIdFixture();
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: harness.catalogue,
      fixture,
    });
    const init = await submitNetworkIdInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.networkId,
      category: harness.category,
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
      init.firstStepAddress,
      init.computationThreadUnit,
    );
    let boundaryCalled = false;
    const cancelled = await submitNetworkIdCancel({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(firstStep),
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      preSubmitBoundary: () => {
        boundaryCalled = true;
      },
    });
    expect(boundaryCalled).toBe(true);
    expect(cancelled.cancelledStepIndex).toBe(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        init.firstStepAddress,
        init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.networkId.fraudProof.spendingScriptAddress,
        toUnit(
          harness.networkId.fraudProof.policyId,
          init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
  }, 180_000);
});
