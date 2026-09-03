import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  computeMidgardNativeTxId,
  decodeMidgardDatum,
  decodeMidgardLedgerOutputCommitment,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardLedgerOutputCommitment,
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
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  prepareResolvedOutputNonCanonicalEvidence,
  RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  RESOLVED_OUTPUT_NON_CANONICAL_ID,
  type ResolvedOutputNonCanonicalContracts,
  submitResolvedOutputNonCanonicalCancel,
  submitResolvedOutputNonCanonicalStep01Accepted,
  submitResolvedOutputNonCanonicalStep02,
  submitResolvedOutputNonCanonicalStep03,
  submitResolvedOutputNonCanonicalStep04,
  submitResolvedOutputNonCanonicalStep05,
} from "../src/resolved-output-non-canonical/index.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { applyCompiledScript } from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { makeSpendingValidator } from "./support/emulator/validators.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  insertAdversarialMembershipSiblings,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const firstStepDeploymentEntry = "fraudProofResolvedOutputNonCanonical";

describe("resolvedOutputNonCanonical local-catalogue lifecycle", () => {
  it("runs accepted Init through scan, final mint, and removal before central registration", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const titles = RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES;
    const step05 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[4], [
        harness.contracts.fraudProof.policyId,
        addressData,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step04 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[3], [
        step05.spendingScriptHash,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step03 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[2], [
        step04.spendingScriptHash,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step02 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[1], [
        step03.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.fieldPreimageCertificate.policyId,
      ]),
    );
    const step01 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[0], [
        step02.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.hubOracle.policyId,
      ]),
    );
    const steps = [step01, step02, step03, step04, step05] as const;
    const contracts: ResolvedOutputNonCanonicalContracts = {
      steps: steps.map((step, index) => ({
        ...step,
        blueprintTitle: titles[index]!,
        referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
      })) as unknown as ResolvedOutputNonCanonicalContracts["steps"],
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      fieldPreimageCertificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
    };
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
      {
        resolvedOutputNonCanonical: {
          categoryId: RESOLVED_OUTPUT_NON_CANONICAL_ID,
          scriptHash: step01.spendingScriptHash,
        },
      },
    );
    const category = catalogue.extraCategories.resolvedOutputNonCanonical!;

    // The selected predecessor output is exactly the family maximum and is
    // non-canonical. Repeated input items take field 0 through Certified
    // carriage while the selected coordinate and out-ref remain exact.
    const canonicalPrefix = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
      datum: decodeMidgardDatum(
        Buffer.from(Data.to("ab".repeat(7_000)), "hex"),
      ),
      script_ref: {
        language: "PlutusV3",
        scriptBytes: Buffer.alloc(7_000, 0x6b),
      },
    });
    expect(canonicalPrefix.length).toBeLessThan(16_384);
    const malformedOutput = Buffer.concat([
      canonicalPrefix,
      Buffer.alloc(16_384 - canonicalPrefix.length),
    ]);
    const priorTxId = "ab".repeat(32);
    const outRefBytes = encodeMidgardSpendInputItem({
      txId: Buffer.from(priorTxId, "hex"),
      outputIndex: 0,
    });
    const templateOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const template = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: templateOutput,
    });
    const descriptor = decodeMidgardLedgerOutputCommitment(
      template.descriptorCbor,
    );
    const bounded = buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: malformedOutput,
    });
    const descriptorCbor = encodeMidgardLedgerOutputCommitment({
      ...descriptor,
      totalLength: malformedOutput.length,
      itemCommitment: bounded.commitment,
    });
    const priorStore = new Store(undefined);
    await priorStore.ready();
    const priorTrie = new Trie(priorStore);
    await priorTrie.insert(outRefBytes, descriptorCbor);
    await insertAdversarialMembershipSiblings({
      trie: priorTrie,
      targets: [{ key: outRefBytes, domain: 0x2601 }],
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
    const priorProof = await priorTrie.prove(outRefBytes);
    const priorRoot = Buffer.from(priorTrie.hash).toString("hex");
    const nativeTx = makeNativeTx({
      spendInputCbors: Array.from({ length: 800 }, () => outRefBytes),
      fee: 7n,
      outputCbors: [],
    });
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
    const sourceCbor = l2TransactionSourceCborV1(nativeTx);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
    const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
    const transactionsRoot = Buffer.from(trie.hash).toString("hex");
    const txInclusion = {
      nativeTxId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      l2TransactionSourceCbor: sourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
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
    const targetStart = emulatorSuccessorHeaderStart({
      predecessorEndTime: predecessor.header.endTime,
      emulator: harness.emulator,
    });
    const targetHeader = {
      ...makeHeader(
        predecessor.header.operatorVkey,
        targetStart,
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
    const setup = {
      ...target,
      fraudulentBlockOutRef: target.successorOutRef,
      headerHash: target.successorHeaderHash,
    };
    const evidence = prepareResolvedOutputNonCanonicalEvidence({
      subject: acceptedVerdictSubject(nativeTxId),
      coordinate: { sourceKind: 0, inputIndex: 0 },
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
      resolved: {
        priorRoot,
        transactionId: priorTxId,
        outputIndex: 0,
        descriptorCborHex: descriptorCbor.toString("hex"),
        outputCborHex: malformedOutput.toString("hex"),
        membershipProofCborHex: priorProof.toCBOR().toString("hex"),
        membershipProof: Data.from(priorProof.toCBOR().toString("hex"), Proof),
      },
    });
    expect(evidence.outputIsNonCanonical).toBe(true);
    expect(Buffer.from(evidence.resolved.outputCborHex, "hex")).toHaveLength(
      16_384,
    );
    expect(evidence.carriage).toBe("Certified");

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `resolved-output-non-canonical-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "resolved-output-non-canonical-certificate",
      })
    ).utxo;
    const cancelInit = await captureEmulatorSubmission(harness.emulator, () =>
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
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const cancellation = await captureEmulatorSubmission(harness.emulator, () =>
      submitResolvedOutputNonCanonicalCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${cancelInit.result.txHash}#${cancelInit.result.firstStepOutputIndex.toString()}`,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(cancellation.measurement.l1ByteMargin).toBeGreaterThan(0);
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
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
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
      submitResolvedOutputNonCanonicalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: init.result.computationThreadUnit,
          fraudulentHeaderHash: init.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const step02Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitResolvedOutputNonCanonicalStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
          deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
        ).toString("hex"),
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    const step03Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitResolvedOutputNonCanonicalStep03({
        lucid: harness.proverLucid,
        network,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02Result.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[2]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    const step04Results: Awaited<
      ReturnType<typeof captureEmulatorSubmission>
    >[] = [];
    let reconstructionThreadOutRef = step03Result.result.nextThreadOutRef;
    for (;;) {
      const result = await captureEmulatorSubmission(harness.emulator, () =>
        submitResolvedOutputNonCanonicalStep04({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: reconstructionThreadOutRef,
          evidence,
          referenceScriptUtxo: references[3]!,
        }),
      );
      step04Results.push(result);
      reconstructionThreadOutRef = result.result.nextThreadOutRef;
      if (result.result.terminal) break;
    }
    expect(step04Results.length).toBeGreaterThan(1);
    const step05Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitResolvedOutputNonCanonicalStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: reconstructionThreadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(step05Result.result.fraudProofUnit).toBeTruthy();

    for (const capture of [
      init,
      step01Result,
      step02Result,
      step03Result,
      ...step04Results,
      step05Result,
    ]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    if (process.env.MIDGARD_PRINT_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            lifecycle: [
              ...step02Result.measurements.map((measurement, index) => [
                `step02-submission-${index.toString()}`,
                measurement,
              ]),
              ["init", init.measurement],
              ["step01", step01Result.measurement],
              ["step02", step02Result.measurement],
              ["step03", step03Result.measurement],
              ...step04Results.map((result, index) => [
                `step04-${index.toString()}`,
                result.measurement,
              ]),
              ["step05", step05Result.measurement],
              ["cancel-init", cancelInit.measurement],
              ["cancel", cancellation.measurement],
            ],
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const baseDeployment = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const deploymentInfo = {
      ...baseDeployment,
      contracts: {
        ...baseDeployment.contracts,
        [firstStepDeploymentEntry]: {
          scriptHash: step01.spendingScriptHash,
          contract: {
            type: step01.spendingScript.type,
            cborHex: step01.spendingScript.script,
          },
        },
      },
    };
    const now = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: {
          name: "resolvedOutputNonCanonical",
          categoryId: category.categoryId,
          firstStepDeploymentEntry,
          firstStepScriptHash: step01.spendingScriptHash,
          fraudProof: {
            policyId: harness.contracts.fraudProof.policyId,
            spendingScriptHash: harness.contracts.fraudProof.spendingScriptHash,
            spendingScriptAddress:
              harness.contracts.fraudProof.spendingScriptAddress,
          },
        },
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("00000026");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1") {
      console.info(
        JSON.stringify(
          { lifecycle: [["removal", removal.measurement]] },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 180_000);
});
