import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeCbor,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  AddressData,
  addressDataFromBech32,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  prepareProtectedOutputSignerMissingEvidenceV1,
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES_V1,
  PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1,
  PROTECTED_OUTPUT_SIGNER_MISSING_ID_V1,
  type ProtectedOutputSignerMissingContractsV1,
  submitProtectedOutputSignerMissingCancelV1,
  submitProtectedOutputSignerMissingStep01AcceptedV1,
  submitProtectedOutputSignerMissingStep02V1,
  submitProtectedOutputSignerMissingStep03V1,
  submitProtectedOutputSignerMissingStep04V1,
  submitProtectedOutputSignerMissingStep05V1,
} from "../src/protected-output-signer-missing/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { makeProtectedOutputSignerIsolatedEvaluatorV1 } from "./protected-output-signer-missing-isolated-evaluator.js";
import { applyCompiledScript } from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCborV1 } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { makeSpendingValidator } from "./support/emulator/validators.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS_V1,
  emulatorSuccessorHeaderStartV1,
  setupFraudulentBlockV1,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const firstStepDeploymentEntry = "fraudProofProtectedOutputSignerMissing";

describe("protectedOutputSignerMissing real-blueprint lifecycle", () => {
  it("runs maximum-carriage evidence through cancel, restartable scan, mint and leased removal", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: { alwaysFraudProofCatalogue: true },
      lucidOptions: {
        evaluator: makeProtectedOutputSignerIsolatedEvaluatorV1(),
      },
    });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:harness-ready");
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const titles = PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1;
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
        harness.contracts.fieldPreimageCertificate.policyId,
      ]),
    );
    const step03 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[2], [
        step04.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.fieldPreimageCertificate.policyId,
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
    const contracts: ProtectedOutputSignerMissingContractsV1 = {
      steps: steps.map((step, index) => ({
        ...step,
        blueprintTitle: titles[index]!,
        referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
      })) as unknown as ProtectedOutputSignerMissingContractsV1["steps"],
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
        protectedOutputSignerMissing: {
          categoryId: PROTECTED_OUTPUT_SIGNER_MISSING_ID_V1,
          scriptHash: step01.spendingScriptHash,
        },
      },
    );
    const category = catalogue.extraCategories.protectedOutputSignerMissing!;
    const credential = Buffer.alloc(28, 0xa7);
    const output = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x68]), credential]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const certifiedWitnessCount = PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES_V1;
    expect(certifiedWitnessCount).toBeLessThanOrEqual(
      PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES_V1,
    );
    const witnesses = Array.from(
      { length: certifiedWitnessCount },
      (_unused, index) => {
        const key = Buffer.alloc(32);
        key.writeUInt32BE(index + 1, 28);
        return encodeMidgardAddressWitnessItemV1({
          verificationKey: key,
          signature: Buffer.alloc(64, 0xff),
        });
      },
    );
    const nativeTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbor: output,
      addrTxWitsPreimageCbor: encodeCbor(witnesses),
    });
    const nativeTxId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompactV1(nativeTx.compact);
    const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompactV1(
      deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
    ).toString("hex");
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
    const predecessor = await setupFraudulentBlockV1({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: {
        transactionsRoot,
        l2TransactionCount: 1n,
        headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS_V1,
      },
    });
    const targetStart = emulatorSuccessorHeaderStartV1({
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
      fraudulentBlockOutRef: target.successorOutRef,
      headerHash: target.successorHeaderHash,
    };
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:block-ready");
    const evidence = prepareProtectedOutputSignerMissingEvidenceV1({
      subject: acceptedVerdictSubjectV1(nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(nativeTx),
    });
    expect(evidence.witnessCarriage).toBe("Certified");
    expect(evidence.validSignerHashes).toEqual([]);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:evidence-ready");

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `protected-output-signer-missing-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "protected-output-signer-missing-certificate",
      })
    ).utxo;
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:references-ready");
    const initThread = async () => {
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
      return init;
    };
    const cancelInit = await initThread();
    const cancellation = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingCancelV1({
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

    const init = await initThread();
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: init.result.txHash,
        outputIndex: init.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    const step01Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep01AcceptedV1({
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
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:step01-ready");
    const step02Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:step02-ready");
    const step03Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep03V1({
        lucid: harness.proverLucid,
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
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info("protected-output-signer-missing:max:certificate-ready");
    const scanResults: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] =
      [];
    let scanThreadOutRef = step03Result.result.nextThreadOutRef;
    for (;;) {
      const result = await captureEmulatorSubmission(harness.emulator, () =>
        submitProtectedOutputSignerMissingStep04V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: scanThreadOutRef,
          evidence,
          nativeTxCompactCbor: compactCbor.toString("hex"),
          witnessSetCompactCbor,
          referenceScriptUtxo: references[3]!,
          certificateReferenceScriptUtxo: certificateReference,
          publishedCarriageUtxos: step03Result.result.carriageUtxos,
          certificateUtxo: step03Result.result.certificateUtxo,
        }),
      );
      scanResults.push(result);
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          `protected-output-signer-missing:max:scan-${scanResults.length.toString()}`,
        );
      scanThreadOutRef = result.result.nextThreadOutRef;
      if (result.result.terminal) break;
    }
    expect(scanResults).toHaveLength(10);
    const step05Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitProtectedOutputSignerMissingStep05V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanThreadOutRef,
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
      ...scanResults,
      step05Result,
    ]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            lifecycle: [
              ["init", init.measurement],
              ["step01", step01Result.measurement],
              ["step02", step02Result.measurement],
              ["step03", step03Result.measurement],
              ...scanResults.map((result, index) => [
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
          name: "protectedOutputSignerMissing" as never,
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
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "protected-output-signer-missing-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("0000002b");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            auxiliary: step03Result.measurements,
            removal: removal.measurement,
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
  }, 300_000);
});
