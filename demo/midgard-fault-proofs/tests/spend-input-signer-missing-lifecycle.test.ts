import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  EMPTY_MERKLE_TREE_ROOT,
  forcedVerdictSubject,
  hashBlockHeader,
  missingSignatureVkeyHash,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  applySpendInputSignerMissingScripts,
  prepareSpendInputSignerMissingEvidence,
  SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  SPEND_INPUT_SIGNER_MISSING_ID,
  type SpendInputSignerMissingContracts,
  submitSpendInputSignerMissingCancel,
  submitSpendInputSignerMissingStep01Accepted,
  submitSpendInputSignerMissingStep01Forced,
  submitSpendInputSignerMissingStep02,
  submitSpendInputSignerMissingStep03,
  submitSpendInputSignerMissingStep04,
  submitSpendInputSignerMissingStep05,
} from "../src/spend-input-signer-missing/index.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const coverage = createLifecycleCoverageRecorder();

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
const registeredContracts = async (
  harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.spendInputSignerMissing;
  const category = harness.catalogue.categories.spendInputSignerMissing;
  expectRegisteredChainParity({
    registered,
    applied: applySpendInputSignerMissingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const contracts: SpendInputSignerMissingContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { steps, contracts, catalogue: harness.catalogue, category };
};

describe("spendInputSignerMissing registered-chain lifecycle", () => {
  it("runs the accepted 318-witness maximum from Init through proof mint", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realSpendInputSignerMissing: true,
        alwaysFraudProofCatalogue: true,
      },
      lucidOptions: { evaluator: createScalusEvaluator() },
    });
    const { steps, contracts, catalogue, category } =
      await registeredContracts(harness);

    const paymentCredential = Buffer.alloc(28, 0x51);
    const priorOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), paymentCredential]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const priorTxId = "ab".repeat(32);
    const outRefBytes = encodeMidgardSpendInputItem({
      txId: Buffer.from(priorTxId, "hex"),
      outputIndex: 0,
    });
    const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: priorOutput,
    });
    const priorStore = new Store(undefined);
    await priorStore.ready();
    const priorTrie = new Trie(priorStore);
    await priorTrie.insert(outRefBytes, outputMaterial.descriptorCbor);
    const priorProof = await priorTrie.prove(outRefBytes);
    const priorRoot = Buffer.from(priorTrie.hash).toString("hex");
    const witnessPreimage = encodeCbor(
      Array.from({ length: 318 }, (_unused, index) => {
        const verificationKey = Buffer.alloc(32);
        verificationKey.writeUInt32BE(index + 1, 28);
        return encodeMidgardAddressWitnessItem({
          verificationKey,
          signature: Buffer.alloc(64, 0xff),
        });
      }),
    );
    const nativeTx = makeNativeTx({
      spendInputCbors: [outRefBytes],
      fee: 7n,
      addrTxWitsPreimageCbor: witnessPreimage,
    });
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
    const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
    ).toString("hex");
    const sourceCbor = l2TransactionSourceCborV1(nativeTx);
    const txStore = new Store(undefined);
    await txStore.ready();
    const txTrie = new Trie(txStore);
    await txTrie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const txProof = await txTrie.prove(Buffer.from(nativeTxId, "hex"));
    const transactionsRoot = Buffer.from(txTrie.hash).toString("hex");
    const txInclusion = {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(txProof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: txProof.toCBOR().toString("hex"),
    };
    const predecessor = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: {
        transactionsRoot,
        l2TransactionCount: 1n,
        utxosRoot: priorRoot,
        headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
      },
    });
    const targetHeader = {
      ...makeHeader(
        predecessor.header.operatorVkey,
        emulatorSuccessorHeaderStart({
          predecessorEndTime: predecessor.header.endTime,
          emulator: harness.emulator,
        }),
        await countedTransactionsRoot(transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: priorRoot,
    };
    const target = await submitSuccessorBlockTx({
      lucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      anchorBlockUnit: predecessor.stateQueueBlockUnit,
      header: targetHeader,
      hubOracle: predecessor.hubOracle,
      scheduler: predecessor.scheduler,
      activeOperatorNode: predecessor.activeOperatorNode,
      activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
    });
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: acceptedVerdictSubject(nativeTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: {
        priorRoot,
        transactionId: priorTxId,
        outputIndex: 0,
        descriptorCborHex: outputMaterial.descriptorCbor.toString("hex"),
        outputCborHex: priorOutput.toString("hex"),
        membershipProofCborHex: priorProof.toCBOR().toString("hex"),
        membershipProof: Data.from(priorProof.toCBOR().toString("hex"), Proof),
      },
    });
    expect(evidence.witnessCarriage).toBe("Certified");
    coverage.scenario("maximum_supported_evidence");

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `spend-input-signer-missing-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "spend-input-signer-missing-certificate",
      })
    ).utxo;
    const cancellationMeasurements: [string, unknown][] = [];
    for (const cancelTarget of [
      "step01",
      "step02",
      "step03",
      "step04-initial",
      "step04-resumed",
      "step05",
    ] as const) {
      const cancelInit = await submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: target.successorOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      let cancelThreadOutRef = `${cancelInit.txHash}#${cancelInit.firstStepOutputIndex.toString()}`;
      if (cancelTarget !== "step01") {
        const [cancelThreadUtxo] = await harness.proverLucid.utxosByOutRef([
          {
            txHash: cancelInit.txHash,
            outputIndex: cancelInit.firstStepOutputIndex,
          },
        ]);
        if (cancelThreadUtxo === undefined)
          throw new Error("cancel step01 thread absent");
        cancelThreadOutRef = (
          await submitSpendInputSignerMissingStep01Accepted({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            signer: harness.proverSigner,
            evidence,
            threadUtxo: cancelThreadUtxo,
            threadToken: {
              unit: cancelInit.computationThreadUnit,
              fraudulentHeaderHash: cancelInit.fraudulentHeaderHash,
            },
            stateQueueBlockOutRef: target.successorOutRef,
            txInclusion,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          })
        ).nextThreadOutRef;
      }
      if (cancelTarget !== "step01" && cancelTarget !== "step02") {
        cancelThreadOutRef = (
          await submitSpendInputSignerMissingStep02({
            lucid: harness.proverLucid,
            network,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: cancelThreadOutRef,
            evidence,
            nativeTxCompactCbor: compactCbor.toString("hex"),
            witnessSetCompactCbor,
            referenceScriptUtxo: references[1]!,
            membershipReferenceScriptUtxo:
              harness.witnessReferenceScripts.phasMembershipWithdraw!,
          })
        ).nextThreadOutRef;
      }
      if (
        cancelTarget === "step04-initial" ||
        cancelTarget === "step04-resumed" ||
        cancelTarget === "step05"
      ) {
        cancelThreadOutRef = (
          await submitSpendInputSignerMissingStep03({
            lucid: harness.proverLucid,
            network,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: cancelThreadOutRef,
            evidence,
            nativeTxCompactCbor: compactCbor.toString("hex"),
            witnessSetCompactCbor,
            referenceScriptUtxo: references[2]!,
            certificateReferenceScriptUtxo: certificateReference,
          })
        ).nextThreadOutRef;
      }
      if (cancelTarget === "step04-resumed" || cancelTarget === "step05") {
        for (;;) {
          const scan = await submitSpendInputSignerMissingStep04({
            lucid: harness.proverLucid,
            network,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: cancelThreadOutRef,
            evidence,
            nativeTxCompactCbor: compactCbor.toString("hex"),
            witnessSetCompactCbor,
            referenceScriptUtxo: references[3]!,
            certificateReferenceScriptUtxo: certificateReference,
          });
          cancelThreadOutRef = scan.nextThreadOutRef;
          if (cancelTarget === "step04-resumed" || scan.stage === "step05")
            break;
        }
      }
      const referenceIndex =
        cancelTarget === "step01"
          ? 0
          : cancelTarget === "step02"
            ? 1
            : cancelTarget === "step03"
              ? 2
              : cancelTarget === "step05"
                ? 4
                : 3;
      const cancellation = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitSpendInputSignerMissingCancel({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: cancelThreadOutRef,
            referenceScriptUtxo: references[referenceIndex]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
      );
      expect(cancellation.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(cancellation.measurement.executionMemory).toBeGreaterThan(0n);
      coverage.cancelled(`step-0${(referenceIndex + 1).toString()}`);
      cancellationMeasurements.push([
        `cancel-${cancelTarget}`,
        cancellation.measurement,
      ]);
    }
    const init = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: target.successorOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    const step01Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: target.successorOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const step02Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep02({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[1]!,
        membershipReferenceScriptUtxo:
          harness.witnessReferenceScripts.phasMembershipWithdraw!,
      }),
    );
    const step03Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep03({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[2]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    const scans: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] = [];
    let threadOutRef = step03Result.result.nextThreadOutRef;
    for (;;) {
      const scan = await captureEmulatorSubmission(harness.emulator, () =>
        submitSpendInputSignerMissingStep04({
          lucid: harness.proverLucid,
          network,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: compactCbor.toString("hex"),
          witnessSetCompactCbor,
          referenceScriptUtxo: references[3]!,
          certificateReferenceScriptUtxo: certificateReference,
        }),
      );
      scans.push(scan);
      threadOutRef = scan.result.nextThreadOutRef;
      if (scan.result.stage === "step05") break;
    }
    expect(scans).toHaveLength(20);
    coverage.resumed();
    const step05Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(step05Result.result.fraudProofUnit).toBeTruthy();
    coverage.reason("SpendInputSignerMissing", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofSpendInputSignerMissing entries carry the
    // registered chain the harness built.
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const removalNow = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "spendInputSignerMissing",
        fraudulentHeaderHash: target.successorHeaderHash,
        requireReferenceScripts: true,
        awaitConfirmation: true,
        validFrom: removalNow > 120_000n ? removalNow - 120_000n : 0n,
        validTo: removalNow + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe(SPEND_INPUT_SIGNER_MISSING_ID);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
    for (const capture of [
      init,
      step01Result,
      step02Result,
      step03Result,
      ...scans,
      step05Result,
      removal,
    ]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    console.info(
      `[spend-input-signer-missing-lifecycle] ${JSON.stringify(
        [
          ["init", init.measurement],
          ["step01", step01Result.measurement],
          ...step02Result.measurements.map((measurement, index) => [
            `step02-${index.toString()}`,
            measurement,
          ]),
          ["step02", step02Result.measurement],
          ...step03Result.measurements.map((measurement, index) => [
            `step03-${index.toString()}`,
            measurement,
          ]),
          ["step03", step03Result.measurement],
          ...scans.map((capture, index) => [
            `step04-${index.toString().padStart(2, "0")}`,
            capture.measurement,
          ]),
          ["step05", step05Result.measurement],
          ["removal", removal.measurement],
          ...cancellationMeasurements,
        ],
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      )}`,
    );
  }, 600_000);

  it("runs a forced wrongful rejection with a valid matching signature through removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realSpendInputSignerMissing: true,
        alwaysFraudProofCatalogue: true,
      },
      lucidOptions: { evaluator: createScalusEvaluator() },
    });
    const { steps, contracts, catalogue, category } =
      await registeredContracts(harness);

    const seed = Buffer.alloc(32, 7);
    const privateKey = createPrivateKey({
      key: Buffer.concat([
        Buffer.from("302e020100300506032b657004220420", "hex"),
        seed,
      ]),
      format: "der",
      type: "pkcs8",
    });
    const verificationKey = createPublicKey(privateKey)
      .export({ format: "der", type: "spki" })
      .subarray(-32);
    const paymentCredential = missingSignatureVkeyHash(
      verificationKey.toString("hex"),
    );
    const priorTxId = "cd".repeat(32);
    const priorOutput = encodeMidgardTxOutput({
      address: Buffer.concat([
        Buffer.from([0x60]),
        Buffer.from(paymentCredential, "hex"),
      ]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const outRefBytes = encodeMidgardSpendInputItem({
      txId: Buffer.from(priorTxId, "hex"),
      outputIndex: 0,
    });
    const outputMaterial = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: priorOutput,
    });
    const priorStore = new Store(undefined);
    await priorStore.ready();
    const priorTrie = new Trie(priorStore);
    await priorTrie.insert(outRefBytes, outputMaterial.descriptorCbor);
    const priorProof = await priorTrie.prove(outRefBytes);
    const priorRoot = Buffer.from(priorTrie.hash).toString("hex");

    const unsigned = makeNativeTx({ spendInputCbors: [outRefBytes], fee: 7n });
    const signature = sign(
      null,
      computeMidgardNativeTxId(unsigned),
      privateKey,
    );
    const nativeTx = makeNativeTx({
      spendInputCbors: [outRefBytes],
      fee: 7n,
      addrTxWitsPreimageCbor: encodeCbor([
        encodeMidgardAddressWitnessItem({ verificationKey, signature }),
      ]),
    });
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    expect(nativeTxId).toBe(computeMidgardNativeTxId(unsigned).toString("hex"));
    const proofSource = deriveMidgardNativeTxProofSource(
      adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
    );
    const reason = {
      SpendInputSignerMissing: { input_index: 0n },
    } as const;
    const sourceKey = transitionTraceOutRef("f7");
    const predecessor = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: {
        transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        l2TransactionCount: 0n,
        utxosRoot: priorRoot,
        headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
      },
    });
    const forcedBlock = await buildDecodingBlockFixture({
      operatorVkey: predecessor.header.operatorVkey,
      startTime: BigInt(
        emulatorSuccessorHeaderStart({
          predecessorEndTime: predecessor.header.endTime,
          emulator: harness.emulator,
        }),
      ),
      priorLedgerRoot: priorRoot,
      subject: {
        kind: "forced",
        nativeTx,
        orderKey: sourceKey,
        verdict: { ForcedTxInvalid: { reason } },
      },
    });
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forcedBlock.reconstruction,
      eventKey: { ForcedTransactionEventKey: { tx_order_id: sourceKey } },
    });
    const forcedHeader = {
      ...forcedBlock.header,
      prevUtxosRoot: priorRoot,
      utxosRoot: priorRoot,
      prevHeaderHash: predecessor.headerHash,
    };
    const forcedSetup = await submitSuccessorBlockTx({
      lucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      anchorBlockUnit: predecessor.stateQueueBlockUnit,
      header: forcedHeader,
      hubOracle: predecessor.hubOracle,
      scheduler: predecessor.scheduler,
      activeOperatorNode: predecessor.activeOperatorNode,
      activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
    });
    expect(forcedSetup.successorHeaderHash).toBe(
      await Effect.runPromise(hashBlockHeader(forcedHeader)),
    );
    const evidence = prepareSpendInputSignerMissingEvidence({
      subject: forcedVerdictSubject({
        transactionId: nativeTxId,
        sourceKey,
        rejectionReason: reason,
      }),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: {
        priorRoot,
        transactionId: priorTxId,
        outputIndex: 0,
        descriptorCborHex: outputMaterial.descriptorCbor.toString("hex"),
        outputCborHex: priorOutput.toString("hex"),
        membershipProofCborHex: priorProof.toCBOR().toString("hex"),
        membershipProof: Data.from(priorProof.toCBOR().toString("hex"), Proof),
      },
    });
    expect(evidence.signerMissing).toBe(false);
    expect(evidence.validSignerHashes).toEqual([paymentCredential]);

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `spend-input-signer-missing-forced-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "spend-input-signer-missing-forced-certificate",
      })
    ).utxo;
    const forcedInit = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: forcedSetup.successorOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const forced01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${forcedInit.result.txHash}#${forcedInit.result.firstStepOutputIndex.toString()}`,
        evidence,
        forcedSource: { header: forcedHeader, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
    const compactCbor = proofSource.compactCbor.toString("hex");
    const witnessSetCompactCbor =
      proofSource.witnessSetCompactCbor.toString("hex");
    const forced02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep02({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forced01.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor,
        witnessSetCompactCbor,
        referenceScriptUtxo: references[1]!,
        membershipReferenceScriptUtxo:
          harness.witnessReferenceScripts.phasMembershipWithdraw!,
      }),
    );
    const forced03 = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep03({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forced02.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor,
        witnessSetCompactCbor,
        referenceScriptUtxo: references[2]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    const forced04 = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep04({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forced03.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor,
        witnessSetCompactCbor,
        referenceScriptUtxo: references[3]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    expect(forced04.result.stage).toBe("step05");
    const forced05 = await captureEmulatorSubmission(harness.emulator, () =>
      submitSpendInputSignerMissingStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forced04.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(forced05.result.fraudProofUnit).toBeTruthy();
    coverage.reason("SpendInputSignerMissing", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const removalNow = BigInt(harness.emulator.now());
    const forcedRemoval = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "spendInputSignerMissing",
          fraudulentHeaderHash: forcedSetup.successorHeaderHash,
          requireReferenceScripts: true,
          awaitConfirmation: true,
          validFrom: removalNow > 120_000n ? removalNow - 120_000n : 0n,
          validTo: removalNow + 300_000n,
        }),
    );
    expect(forcedRemoval.result.fraudCategoryId).toBe(
      SPEND_INPUT_SIGNER_MISSING_ID,
    );
    for (const capture of [
      forcedInit,
      forced01,
      forced02,
      forced03,
      forced04,
      forced05,
      forcedRemoval,
    ]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    console.info(
      `[spend-input-signer-missing-forced-lifecycle] ${JSON.stringify(
        [
          ["forced-init", forcedInit.measurement],
          ["forced-step01", forced01.measurement],
          ...forced02.measurements.map((measurement, index) => [
            `forced-step02-${index.toString()}`,
            measurement,
          ]),
          ["forced-step02", forced02.measurement],
          ...forced03.measurements.map((measurement, index) => [
            `forced-step03-${index.toString()}`,
            measurement,
          ]),
          ["forced-step03", forced03.measurement],
          ["forced-step04", forced04.measurement],
          ["forced-step05", forced05.measurement],
          ["forced-remove", forcedRemoval.measurement],
        ],
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      )}`,
    );
  }, 600_000);

  it("declares the lifecycle coverage it exercised and the gaps it leaves open", () => {
    // Recorded while the suites above ran, never pre-filled. The gate lists
    // every omission; the suite pins that list so a silent regression of what
    // it does cover, or an unannounced closure of a gap, both fail here.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: ["SpendInputSignerMissing"],
        authenticationSeams: [
          "tx_membership",
          "prior_output_membership",
          "field_certificate",
          "forced_leaf",
        ],
        cancellablePhysicalSteps: [
          "step-01",
          "step-02",
          "step-03",
          "step-04",
          "step-05",
        ],
        resumable: true,
        hasAdjacentConsensusBound: false,
      }),
    ).toThrow(
      "incomplete fault-proof lifecycle coverage: scenarios: honest_accepted_block_refusal, honest_forced_rejection_refusal, reason_or_subject_coordinate_mutation; authentication seams: tx_membership, prior_output_membership, field_certificate, forced_leaf",
    );
  });
});
