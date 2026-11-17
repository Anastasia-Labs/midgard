import "node:crypto";
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/field-opening.js";
import "../src/linear-fault-family.js";
import "../src/linear-fault-finalize.js";
import "../src/protected-output-signer-missing/index.js";
import "../src/remove-fraudulent-block.js";
import "../src/signer-witness-scan/index.js";
import "../src/step-support.js";
import "../src/testing/complete-lifecycle.js";
import "../src/transition-trace/witnesses.js";
import "./protected-output-signer-missing-isolated-evaluator.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/emulator/harness.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/removal-deployment.js";
import "./support/emulator/signer-witness-scan-raw-turn.js";
import "./support/lifecycle-coverage.js";
import "./support/native-script-decoding-emulator.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import "./protected-output-signer-missing-lifecycle.make-scenario.js";
import "./protected-output-signer-missing-lifecycle.with-swapped-certificate-slot.js";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  type MidgardFieldCarriagePlan,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  planProtectedOutputSignerOutputOpening,
  prepareProtectedOutputSignerMissingEvidence,
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES,
  type ProtectedOutputSignerMissingEvidence,
  ProtectedOutputSignerStep02RedeemerSchema,
  submitProtectedOutputSignerMissingCancel,
  submitProtectedOutputSignerMissingStep01Accepted,
  submitProtectedOutputSignerMissingStep02,
} from "../src/protected-output-signer-missing/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  planSignerWitnessOpening,
  SIGNER_WITNESS_SCAN_BATCH,
  SignerWitnessScanDatumSchema,
  SignerWitnessTerminalDatumSchema,
  submitSignerWitnessScan,
  submitSignerWitnessTerminal,
} from "../src/signer-witness-scan/index.js";
import { nativeTxFromCoreCompact } from "../src/step-support.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { makeProtectedOutputSignerIsolatedEvaluator } from "./protected-output-signer-missing-isolated-evaluator.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";
import {
  AUTHENTICATION_SEAMS,
  compactHex,
  coverage,
  decoyWitness,
  evidenceFor,
  FORCED_ORDER_KEY,
  forgedWitness,
  honestEvidenceFor,
  nativeTxWith,
  PHYSICAL_STEPS,
  protectedOutputCbor,
  REASON,
  registeredContracts,
  signerCredentialHex,
  validWitness,
  witnessSetCompactHex,
  witnessSetOf,
} from "./protected-output-signer-missing-lifecycle.registered-contracts.js";
import {
  scanToTerminal,
  withSwappedCertificateSlot,
} from "./protected-output-signer-missing-lifecycle.with-swapped-certificate-slot.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitRawSignerWitnessScanTurn } from "./support/emulator/signer-witness-scan-raw-turn.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  makeHeader,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("protectedOutputSignerMissing registered-chain lifecycle", () => {
  it.each([
    {
      shape: "decoy keys",
      credentialHex: "a7".repeat(28),
      witness: (index: number) => decoyWitness(index),
    },
    {
      // Every witness carries the protected key, so every one passes the key
      // hash and pays for an Ed25519 verification that fails: the costliest
      // field for the scan.
      shape: "matching keys with forged signatures",
      credentialHex: signerCredentialHex,
      witness: (index: number) => forgedWitness(index + 1),
    },
  ])(
    "runs maximum-carriage evidence ($shape) through cancel, restartable scan, mint and leased removal",
    async ({ shape, credentialHex, witness }) => {
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realProtectedOutputSignerMissing: true,
          alwaysFraudProofCatalogue: true,
        },
        lucidOptions: {
          evaluator: makeProtectedOutputSignerIsolatedEvaluator(),
        },
      });
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info("protected-output-signer-missing:max:harness-ready");
      const { steps, contracts, catalogue, category } =
        await registeredContracts(harness);
      const nativeTx = nativeTxWith({
        fee: 7n,
        credentialHex,
        witnesses: () =>
          Array.from(
            { length: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES },
            (_unused, index) => witness(index),
          ),
      });
      const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
      const compactCbor = encodeMidgardNativeTxCompact(nativeTx.compact);
      const witnessSetCompactCbor = witnessSetCompactHex(nativeTx);
      const sourceCbor = l2TransactionSourceCbor(nativeTx);
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
      const evidence = prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(nativeTxId),
        outputIndex: 0n,
        canonicalTransactionCbor:
          "validity" in nativeTx
            ? encodeMidgardNativeTxCanonical(nativeTx)
            : encodeMidgardForcedTxCanonical(nativeTx),
      });
      expect(evidence.witnessCarriage).toBe("Certified");
      expect(evidence.signerPresent).toBe(false);
      coverage.scenario("maximum_supported_evidence");
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
      const cancellation = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitProtectedOutputSignerMissingCancel({
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
      coverage.cancelled("step-01");

      const init = await initThread();
      const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
        {
          txHash: init.result.txHash,
          outputIndex: init.result.firstStepOutputIndex,
        },
      ]);
      if (threadUtxo === undefined) throw new Error("init thread absent");
      const step01Result = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitProtectedOutputSignerMissingStep01Accepted({
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
      const step02Result = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitProtectedOutputSignerMissingStep02({
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
      const scanCommon = {
        lucid: harness.proverLucid,
        family: "protected-output-signer-missing",
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        referenceScriptUtxo: references[2]!,
        certificateReferenceScriptUtxo: certificateReference,
      };
      // The opening turn publishes the field's chunks, mints its certificate
      // and scans the first batch.
      const openingScan = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitSignerWitnessScan({
            ...scanCommon,
            threadOutRef: step02Result.result.nextThreadOutRef,
          }),
      );
      expect(openingScan.result.carriageTier).toBe("Certified");
      expect(openingScan.result.nextCursor).toBe(SIGNER_WITNESS_SCAN_BATCH);
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info("protected-output-signer-missing:max:certificate-ready");

      // Field certificate: the tier-3 opening names its certificate by slot. A
      // redeemer pointing the certificate slot at a chunk (and the chunk slot at
      // the certificate) is refused before any item is read.
      const witnessPlan = planSignerWitnessOpening({
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        owner: harness.proverSigner.paymentKeyHash,
        label: "protected-output max scan opening",
      });
      const checkpointAt = (nextItemIndex: number) =>
        SDK.missingSignatureFieldWalkCheckpoint({
          txId: nativeTxId,
          itemCount: witnessPlan.itemCount,
          totalLength: witnessPlan.preimage.length,
          nextItemIndex,
        });
      const honestReferenceInputs = [
        ...openingScan.result.carriageUtxos,
        references[2]!,
        openingScan.result.certificateUtxo!,
      ];
      const honestOpening = (ctx: RedeemerContext) =>
        faultProofFieldOpening({
          planned: witnessPlan,
          referenceInputs: ctx.referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: "protected-output max scan opening",
        });
      // The resumed turn from the first batch boundary to the second, with the
      // caller's opening and checkpoint.
      const rawScan = (
        openingFor: (ctx: RedeemerContext) => SDK.FieldOpening,
        checkpointCbor: string,
      ) =>
        submitRawSignerWitnessScanTurn({
          lucid: harness.proverLucid,
          family: "protected-output-signer-missing",
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: openingScan.result.nextThreadOutRef,
          terminal: false,
          nextDatum: Data.to(
            {
              fraud_prover: harness.proverSigner.paymentKeyHash,
              data: {
                subject: evidence.subject,
                witness_set_hash: evidence.witnessSetHashHex,
                payment_credential: evidence.paymentCredentialHex,
                checkpoint_hash: checkpointAt(2 * SIGNER_WITNESS_SCAN_BATCH)
                  .checkpointHash,
              },
            } as never,
            SignerWitnessScanDatumSchema as never,
          ),
          openingFor,
          checkpointCbor,
          referenceScriptUtxo: references[2]!,
          carriageReferenceInputs: honestReferenceInputs,
        });
      await expectOnchainRefusal(() =>
        rawScan(
          (ctx) => withSwappedCertificateSlot(honestOpening(ctx)),
          checkpointAt(SIGNER_WITNESS_SCAN_BATCH).checkpointCbor,
        ),
      );
      coverage.seamMutated("field_certificate");
      // Checkpoint: a position the thread never committed (one batch ahead)
      // under the committed digest, then malformed bytes.
      await expectOnchainRefusal(() =>
        rawScan(
          honestOpening,
          checkpointAt(2 * SIGNER_WITNESS_SCAN_BATCH).checkpointCbor,
        ),
      );
      await expectOnchainRefusal(() =>
        rawScan(
          honestOpening,
          checkpointAt(SIGNER_WITNESS_SCAN_BATCH).checkpointCbor.slice(0, 80),
        ),
      );
      coverage.seamMutated("checkpoint");

      const scanResults = [openingScan];
      let scanThreadOutRef = openingScan.result.nextThreadOutRef;
      while (!scanResults.at(-1)!.result.terminal) {
        const result = await captureEmulatorSubmission(harness.emulator, () =>
          submitSignerWitnessScan({
            ...scanCommon,
            threadOutRef: scanThreadOutRef,
            publishedCarriageUtxos: openingScan.result.carriageUtxos,
            certificateUtxo: openingScan.result.certificateUtxo,
          }),
        );
        scanResults.push(result);
        if (process.env.MIDGARD_PRINT_FIT === "1")
          console.info(
            `protected-output-signer-missing:max:scan-${scanResults.length.toString()}`,
          );
        scanThreadOutRef = result.result.nextThreadOutRef;
      }
      // 318 witnesses in batches of 32: nine full batches and one of 30.
      expect(scanResults.map((result) => result.result.nextCursor)).toEqual([
        32, 64, 96, 128, 160, 192, 224, 256, 288, 318,
      ]);
      // Every resume above started from nothing but the chain's committed digest
      // and the deterministic frontier: a real interruption at every batch.
      coverage.resumed();
      const terminalResult = await captureEmulatorSubmission(
        harness.emulator,
        () =>
          submitSignerWitnessTerminal({
            lucid: harness.proverLucid,
            family: "protected-output-signer-missing",
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: scanThreadOutRef,
            evidence,
            referenceScriptUtxo: references[3]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
      );
      expect(terminalResult.result.fraudProofUnit).toBeTruthy();
      coverage.reason(REASON, "accepted_invalid");
      coverage.scenario("wrongful_acceptance_success");
      // The scan turn is the last transaction each capture submitted; the
      // opening capture also holds the chunk and certificate transactions.
      for (const capture of [
        init,
        step01Result,
        step02Result,
        ...scanResults,
        terminalResult,
      ]) {
        expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
        expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
        expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
      }
      const scanTurns = scanResults.map((result) => result.measurement);
      console.info(
        `[protected-output-signer-missing-max-scan] ${JSON.stringify({
          shape,
          turns: scanTurns.length,
          opening: {
            memory: scanTurns[0]!.executionMemory.toString(),
            cpu: scanTurns[0]!.executionSteps.toString(),
            bytes: scanTurns[0]!.completeSignedBytes,
          },
          maxResumedMemory: scanTurns
            .slice(1)
            .reduce(
              (max, m) => (m.executionMemory > max ? m.executionMemory : max),
              0n,
            )
            .toString(),
          maxResumedCpu: scanTurns
            .slice(1)
            .reduce(
              (max, m) => (m.executionSteps > max ? m.executionSteps : max),
              0n,
            )
            .toString(),
          terminal: {
            memory: terminalResult.measurement.executionMemory.toString(),
            cpu: terminalResult.measurement.executionSteps.toString(),
          },
        })}`,
      );
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          JSON.stringify(
            {
              lifecycle: [
                ["init", init.measurement],
                ["step01", step01Result.measurement],
                ["step02", step02Result.measurement],
                ...scanResults.map((result, index) => [
                  `scan-${index.toString()}`,
                  result.measurement,
                ]),
                ["terminal", terminalResult.measurement],
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
      // A registered family resolves removal through the canonical catalogue:
      // the manifest's fraudProofProtectedOutputSignerMissing entries carry the
      // registered chain the harness built.
      const deploymentInfo = buildRemovalDeploymentInfo(
        harness.contracts,
        catalogue,
        { removalReferenceScripts: removalReferences.published },
      );
      const now = BigInt(harness.emulator.now());
      const removal = await captureEmulatorSubmission(harness.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "protectedOutputSignerMissing",
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
      coverage.scenario("permanent_proof_token_and_descendant_removal");
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          JSON.stringify(
            {
              auxiliary: openingScan.measurements,
              removal: removal.measurement,
            },
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
          ),
        );
    },
    900_000,
  );

  it("proves an exact forced ProtectedOutputSignerMissing rejection and refuses its mutations", async () => {
    // The operator rejected a transaction whose protected output IS signed:
    // the valid witness sits behind four unverifiable decoys, so the complete
    // scan has to reach the last position to find it.
    const nativeTx = nativeTxWith({
      fee: 11n,
      witnesses: (txId) => [
        decoyWitness(0),
        decoyWitness(1),
        decoyWitness(2),
        decoyWitness(3),
        validWitness(txId),
      ],
    });
    const s = await makeScenario({
      nativeTx,
      forcedReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const adjudicated = materializeMidgardForcedTxFromCanonical(nativeTx);
    const subject = forcedVerdictSubject({
      transactionId: s.block.nativeTxId,
      sourceKey: FORCED_ORDER_KEY,
      rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const evidence = evidenceFor(subject, adjudicated);
    expect(evidence.signerPresent).toBe(true);
    const membership = await s.forcedMembership();
    const thread = await s.init();
    // Reason coordinate: the leaf rejects output 0; a claim bound to output 1
    // passes every off-chain shape check and fails the exact-reason bind.
    await expectOnchainRefusal(() =>
      s.forced01(
        thread,
        Object.freeze({
          ...evidence,
          outputIndex: 1n,
          subject: forcedVerdictSubject({
            transactionId: s.block.nativeTxId,
            sourceKey: FORCED_ORDER_KEY,
            rejectionReason: {
              ProtectedOutputSignerMissing: { output_index: 1n },
            },
          }),
        }),
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Direction: a rejecting leaf cannot open a wrongful-acceptance thread.
    await expectOnchainRefusal(() =>
      s.forced01(thread, {
        ...evidence,
        subject: forcedVerdictSubject({
          transactionId: evidence.subject.transaction_id,
          sourceKey: FORCED_ORDER_KEY,
          rejectionReason: null,
        }),
      }),
    );
    // Forced leaf: a leaf carrying another verdict is not in the forced root.
    await expectOnchainRefusal(() =>
      s.forced01(thread, evidence, {
        membership: {
          ...membership,
          value: { ...membership.value, verdict: "ForcedTxValid" },
        },
      }),
    );
    // Header: a header committing another forced count is not the thread's.
    await expectOnchainRefusal(() =>
      s.forced01(thread, evidence, {
        header: {
          ...s.block.header,
          forcedTransactionCount: s.block.header.forcedTransactionCount + 1n,
        },
      }),
    );
    coverage.seamMutated("forced_leaf");
    const bound = await captureEmulatorSubmission(s.harness.emulator, () =>
      s.forced01(thread, evidence),
    );
    expect(bound.measurement.l1ByteMargin).toBeGreaterThan(0);
    console.info(
      `[protected-output-signer-missing-forced-step01] ${JSON.stringify({ bytes: bound.measurement.completeSignedBytes, memory: bound.measurement.executionMemory.toString(), cpu: bound.measurement.executionSteps.toString() })}`,
    );
    await captureEmulatorSubmission(s.harness.emulator, async () => {
      const credential = await s.step02(
        bound.result.nextThreadOutRef,
        evidence,
        adjudicated,
      );
      const terminal = await scanToTerminal(
        s,
        credential.nextThreadOutRef,
        evidence,
        adjudicated,
      );
      const minted = await s.terminal(terminal.threadOutRef, evidence);
      expect(minted.fraudProofUnit).toContain(s.category.categoryId);
    });
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
  }, 900_000);

  it("refuses an honest accepted block, every substituted accepted seam, the adjacent over-bound field, and cancels every step", async () => {
    const ownOutputs = createOwnOutputJournal();
    // Honest: the protected output's signer is present behind one decoy, so
    // the accepted block is correct and no acceptance thread may close.
    const honestTx = nativeTxWith({
      fee: 13n,
      witnesses: (txId) => [decoyWitness(0), validWitness(txId)],
    });
    // One witness past the canonical 318 maximum: 33,000 field-7 bytes, past
    // the 32,768-byte aggregate bound every carriage tier enforces on chain.
    const overBoundCredentialHex = "a7".repeat(28);
    const overBoundTx = nativeTxWith({
      fee: 17n,
      credentialHex: overBoundCredentialHex,
      witnesses: () =>
        Array.from(
          { length: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES + 1 },
          (_unused, index) => decoyWitness(index),
        ),
    });
    const s = await makeScenario({
      nativeTx: honestTx,
      additionalTransactions: [overBoundTx],
    });
    const subject = acceptedVerdictSubject(s.block.nativeTxId);
    const evidence = honestEvidenceFor(
      subject,
      forcedVerdictSubject({
        transactionId: s.block.nativeTxId,
        sourceKey: FORCED_ORDER_KEY,
        rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
      }),
      honestTx,
    );
    expect(evidence.signerPresent).toBe(true);
    const thread = await s.init();
    // Transaction membership: a foreign transactions root cannot bind the
    // header's counted root, whatever proof rides with it.
    await expectOnchainRefusal(() =>
      s.accepted01(thread, evidence, {
        ...s.block.txInclusion!,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const bound = await s.accepted01(thread, evidence);
    const owner = s.harness.proverSigner.paymentKeyHash;
    const outputPlan = planProtectedOutputSignerOutputOpening({
      evidence,
      nativeTxCompactCbor: compactHex(honestTx),
      owner,
    });
    const outputOpening = faultProofFieldOpening({
      planned: outputPlan,
      referenceInputs: [s.refs[1]!],
      certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
      label: "honest output opening",
    });
    // Step 02 writes the unopened scan state; the shared scan reads only
    // the subject, witness-set hash and credential from it.
    const scanStartDatum = (data: Record<string, unknown>) =>
      s.datum(SignerWitnessScanDatumSchema, {
        subject,
        witness_set_hash: evidence.witnessSetHashHex,
        payment_credential: evidence.paymentCredentialHex,
        checkpoint_hash: "",
        ...data,
      });
    // Subject coordinate: step 02 re-derives the scan state from the bound
    // subject and output, and refuses a datum naming another transaction or a
    // credential the bound output does not carry.
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: scanStartDatum({
          subject: acceptedVerdictSubject("99".repeat(32)),
        }),
        opening: outputOpening,
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: scanStartDatum({ payment_credential: "4c".repeat(28) }),
        opening: outputOpening,
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    // Compact transaction: another transaction's bytes under the anchored id.
    const foreignPlan = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: 2,
      anchorTxId: computeMidgardNativeTxId(overBoundTx).toString("hex"),
      nativeTxCompactCbor: compactHex(overBoundTx),
      itemCbors: [protectedOutputCbor(overBoundCredentialHex)],
      owner,
      label: "foreign output opening",
    });
    await expectOnchainRefusal(() =>
      s.rawTransition({
        threadOutRef: bound,
        stepIndex: 1,
        nextStepIndex: 2,
        nextDatum: scanStartDatum({}),
        opening: faultProofFieldOpening({
          planned: foreignPlan,
          referenceInputs: [s.refs[1]!],
          certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
          label: "foreign output opening",
        }),
        carriageReferenceInputs: [s.refs[1]!],
        redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema,
      }),
    );
    coverage.seamMutated("compact_tx");
    const credential = (await s.step02(bound, evidence, honestTx))
      .nextThreadOutRef;
    const witnessPlan = planSignerWitnessOpening({
      evidence,
      nativeTxCompactCbor: compactHex(honestTx),
      witnessSetCompactCbor: witnessSetCompactHex(honestTx),
      owner,
      label: "honest address witnesses",
    });
    expect(witnessPlan.plan.tier).toBe("Inline");
    // The honest field ends in its first batch, so the opening turn writes
    // the verdict: the signer behind one decoy is present.
    const honestVerdict = s.datum(SignerWitnessTerminalDatumSchema, {
      subject,
      signer_required: true,
      signer_present: true,
    });
    // Witness-set anchor: the genuine compact bytes paired with another
    // transaction's witness set fail the anchored `witness_set_hash`.
    await expectOnchainRefusal(() =>
      s.rawScan({
        threadOutRef: credential,
        terminal: true,
        nextDatum: honestVerdict,
        opening: {
          WitnessFieldOpening: {
            native_tx_compact_cbor: compactHex(honestTx),
            witness_set: witnessSetOf(overBoundTx),
            carriage: {
              Inline: { preimage: witnessPlan.preimage.toString("hex") },
            },
          },
        },
        checkpointCbor: null,
        carriageReferenceInputs: [s.refs[2]!],
      }),
    );
    coverage.seamMutated("witness_set_anchor");
    // Field preimage: a witness collection the transaction did not commit
    // fails the flat commitment under the anchored witness set.
    await expectOnchainRefusal(() =>
      s.rawScan({
        threadOutRef: credential,
        terminal: true,
        nextDatum: honestVerdict,
        opening: {
          WitnessFieldOpening: {
            native_tx_compact_cbor: compactHex(honestTx),
            witness_set: witnessSetOf(honestTx),
            carriage: {
              Inline: {
                preimage: encodeCbor([forgedWitness()]).toString("hex"),
              },
            },
          },
        },
        checkpointCbor: null,
        carriageReferenceInputs: [s.refs[2]!],
      }),
    );
    coverage.seamMutated("field_preimage");
    const terminal = await scanToTerminal(s, credential, evidence, honestTx);
    expect(terminal.turns).toBe(1);
    // The complete scan found the valid signer; the honest block stands.
    await expectOnchainRefusal(() => s.rawTerminal(terminal.threadOutRef));
    coverage.scenario("honest_accepted_block_refusal");
    coverage.reason(REASON);

    // Cancel from every physical step after the first.
    const boundThread = async () =>
      await s.accepted01(await s.init(), evidence);
    const credentialThread = async () =>
      (await s.step02(await boundThread(), evidence, honestTx))
        .nextThreadOutRef;
    await s.cancel(await boundThread(), 1);
    coverage.cancelled("step-02");
    await s.cancel(await credentialThread(), 2);
    coverage.cancelled("step-03");
    await s.cancel(
      (await scanToTerminal(s, await credentialThread(), evidence, honestTx))
        .threadOutRef,
      3,
    );
    coverage.cancelled("step-04");

    // Adjacent consensus bound: the 319-witness transaction is committed and
    // binds through step 02, but its field 7 has no admissible carriage. The
    // production planner refuses it before any transaction; a carriage
    // assembled below the planner is refused on chain by the certificate mint,
    // so no frontier over that field can ever be initialized.
    const overBoundId = computeMidgardNativeTxId(overBoundTx).toString("hex");
    const overBoundMaterial = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeMidgardNativeTxCanonical(overBoundTx),
    );
    const overBoundPreimage = overBoundMaterial.fieldPreimages[7]!;
    expect(overBoundPreimage.length).toBeGreaterThan(32_768);
    const overBoundEvidence: ProtectedOutputSignerMissingEvidence =
      Object.freeze({
        subject: acceptedVerdictSubject(overBoundId),
        outputIndex: 0n,
        route: "witness_scan" as const,
        signerRequired: true,
        canonicalTransactionCborHex:
          encodeMidgardNativeTxCanonical(overBoundTx).toString("hex"),
        outputCborHex: protectedOutputCbor(overBoundCredentialHex).toString(
          "hex",
        ),
        paymentCredentialHex: overBoundCredentialHex,
        witnessSetHashHex: Buffer.from(
          overBoundMaterial.compact.transactionWitnessSetHash,
        ).toString("hex"),
        addressWitnessFieldPreimageHex: overBoundPreimage.toString("hex"),
        signerPresent: false,
        outputCarriage: "Inline",
        witnessCarriage: "Certified",
        checkpoints: [],
      });
    expect(() =>
      prepareProtectedOutputSignerMissingEvidence({
        subject: acceptedVerdictSubject(overBoundId),
        outputIndex: 0n,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(overBoundTx),
      }),
    ).toThrow(/frontier exceeds the canonical maximum/u);
    const overBoundInclusion = s.block.txInclusions.get(overBoundId);
    if (overBoundInclusion === undefined)
      throw new Error("over-bound inclusion missing");
    const overBoundCredential = await s.step02(
      await s.accepted01(await s.init(), overBoundEvidence, overBoundInclusion),
      overBoundEvidence,
      overBoundTx,
    );
    expect(overBoundCredential.nextThreadOutRef).toMatch(/^[0-9a-f]{64}#\d+$/u);
    expect(() =>
      planSignerWitnessOpening({
        evidence: overBoundEvidence,
        nativeTxCompactCbor: compactHex(overBoundTx),
        witnessSetCompactCbor: witnessSetCompactHex(overBoundTx),
        owner,
        label: "over-bound address witnesses",
      }),
    ).toThrow(/aggregate bound/u);
    const chunks: Buffer[] = [];
    for (
      let start = 0;
      start < overBoundPreimage.length;
      start += MIDGARD_CHUNK_BYTES_K
    )
      chunks.push(
        overBoundPreimage.subarray(
          start,
          Math.min(start + MIDGARD_CHUNK_BYTES_K, overBoundPreimage.length),
        ),
      );
    // The §8.6 certificate exactly as the core derives it for an admissible
    // field (`deriveMidgardFieldPreimageCertificate`), assembled by hand only
    // because the core refuses to split an over-bound preimage at all.
    const certificate: MidgardFieldPreimageCertificate = {
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(overBoundId, "hex"),
      fieldIndex: 7,
      fieldHash: midgardFieldCommitment(overBoundPreimage),
      totalLength: overBoundPreimage.length,
      chunkDigests: chunks.map((chunk) => midgardFieldCommitment(chunk)),
    };
    const overBoundPlan: MidgardFieldCarriagePlan = {
      tier: "Certified",
      fieldIndex: 7,
      txId: Buffer.from(overBoundId, "hex"),
      totalLength: overBoundPreimage.length,
      commitment: midgardFieldCommitment(overBoundPreimage),
      inlinePreimage: null,
      publications: chunks.map((bytes, chunkIndex) => ({
        chunkIndex,
        bytes,
        digest: certificate.chunkDigests[chunkIndex]!,
      })),
      certificate,
      certificateAssetName: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    };
    const overBoundPlanned: FaultProofFieldOpeningPlan = {
      sourceKind: 0n,
      fieldIndex: 7,
      nativeTxId: overBoundId,
      nativeTxCompactCbor: compactHex(overBoundTx),
      preimage: overBoundPreimage,
      itemCount: PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES + 1,
      commitment: midgardFieldCommitment(overBoundPreimage).toString("hex"),
      plan: overBoundPlan,
      witnessSet: witnessSetOf(overBoundTx),
      witnessSetHash: overBoundEvidence.witnessSetHashHex,
    };
    const overBoundChunks = await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid: s.harness.proverLucid,
      signer: s.harness.proverSigner,
      planned: overBoundPlanned,
      publisherAddress: s.harness.proverSigner.address,
      label: "over-bound witness carriage",
    });
    expect(overBoundChunks).toHaveLength(3);
    await expectOnchainRefusal(() =>
      certifyFaultProofFieldCarriage({
        ownOutputs,
        lucid: s.harness.proverLucid,
        network,
        signer: s.harness.proverSigner,
        planned: overBoundPlanned,
        certificatePolicyId: s.contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          s.contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: s.certificateRef,
        chunkUtxos: overBoundChunks,
        compactCbor: compactHex(overBoundTx),
        witnessSetCompactCbor: witnessSetCompactHex(overBoundTx),
      }),
    );
    coverage.adjacentOverBoundRefused();
  }, 1_200_000);

  it("refuses an honest forced rejection whose signer really is missing", async () => {
    // The right key with a forged signature: canonical validation admits no
    // signer, so the operator's rejection is exactly right and the
    // wrongful-rejection thread walks the field to `signer_present = False`
    // and cannot close.
    const nativeTx = nativeTxWith({
      fee: 19n,
      witnesses: () => [forgedWitness(), decoyWitness(0)],
    });
    const s = await makeScenario({
      nativeTx,
      forcedReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const adjudicated = materializeMidgardForcedTxFromCanonical(nativeTx);
    const subject = forcedVerdictSubject({
      transactionId: s.block.nativeTxId,
      sourceKey: FORCED_ORDER_KEY,
      rejectionReason: { ProtectedOutputSignerMissing: { output_index: 0n } },
    });
    const evidence = honestEvidenceFor(
      subject,
      acceptedVerdictSubject(s.block.nativeTxId),
      adjudicated,
    );
    expect(evidence.signerPresent).toBe(false);
    const bound = await s.forced01(await s.init(), evidence);
    const credential = await s.step02(
      bound.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    const terminal = await scanToTerminal(
      s,
      credential.nextThreadOutRef,
      evidence,
      adjudicated,
    );
    await expectOnchainRefusal(() => s.rawTerminal(terminal.threadOutRef));
    coverage.scenario("honest_forced_rejection_refusal");
  }, 900_000);

  it("proves forced rejections over an unprotected and a script-locked output through the direct terminal route and refuses the direct exit for a protected key", async () => {
    // Canonical validation authorizes an unprotected output, and a protected
    // script output, with no signer at all: the operator's rejection is
    // wrong without any witness, and step 02 writes the verdict straight to
    // the shared terminal.
    const forcedReason = {
      ProtectedOutputSignerMissing: { output_index: 0n },
    } as const;
    for (const [label, addressHeader, route] of [
      ["unprotected", 0x60, "unprotected_output"],
      ["script-locked", 0x78, "script_credential"],
    ] as const) {
      const nativeTx = nativeTxWith({
        fee: 23n,
        addressHeader,
        witnesses: () => [],
      });
      const s = await makeScenario({ nativeTx, forcedReason });
      const adjudicated = materializeMidgardForcedTxFromCanonical(nativeTx);
      const evidence = evidenceFor(
        forcedVerdictSubject({
          transactionId: s.block.nativeTxId,
          sourceKey: FORCED_ORDER_KEY,
          rejectionReason: forcedReason,
        }),
        adjudicated,
      );
      expect(evidence.route).toBe(route);
      expect(evidence.signerRequired).toBe(false);
      expect(evidence.paymentCredentialHex).toBeUndefined();
      const bound = await s.forced01(await s.init(), evidence);
      // Credential seam: the same forced claim cannot take the scan door,
      // and an in-range coordinate cannot claim the out-of-range arm.
      await expectOnchainRefusal(() =>
        s.step02(
          bound.nextThreadOutRef,
          Object.freeze({
            ...evidence,
            route: "witness_scan",
            signerRequired: true,
            paymentCredentialHex: signerCredentialHex,
          }),
          adjudicated,
        ),
      );
      await expectOnchainRefusal(() =>
        s.step02(
          bound.nextThreadOutRef,
          Object.freeze({ ...evidence, route: "coordinate_out_of_range" }),
          adjudicated,
        ),
      );
      const direct = await captureEmulatorSubmission(s.harness.emulator, () =>
        s.step02(bound.nextThreadOutRef, evidence, adjudicated),
      );
      expect(direct.result.stage).toBe("terminal");
      expect(direct.measurement.l1ByteMargin).toBeGreaterThan(0);
      console.info(
        `[protected-output-signer-missing-forced-direct-${label}] ${JSON.stringify({ bytes: direct.measurement.completeSignedBytes, memory: direct.measurement.executionMemory.toString(), cpu: direct.measurement.executionSteps.toString() })}`,
      );
      const minted = await s.terminal(direct.result.nextThreadOutRef, evidence);
      expect(minted.fraudProofUnit).toContain(s.category.categoryId);
    }
    coverage.reason(REASON, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");

    // Credential seam, the other way: a protected pub-key output whose
    // signer really is missing cannot skip the scan through the direct exit.
    const unsignedTx = nativeTxWith({
      fee: 29n,
      witnesses: () => [forgedWitness()],
    });
    const s = await makeScenario({ nativeTx: unsignedTx, forcedReason });
    const adjudicated = materializeMidgardForcedTxFromCanonical(unsignedTx);
    const honest = honestEvidenceFor(
      forcedVerdictSubject({
        transactionId: s.block.nativeTxId,
        sourceKey: FORCED_ORDER_KEY,
        rejectionReason: forcedReason,
      }),
      acceptedVerdictSubject(s.block.nativeTxId),
      adjudicated,
    );
    expect(honest.route).toBe("witness_scan");
    const bound = await s.forced01(await s.init(), honest);
    await expectOnchainRefusal(() =>
      s.step02(
        bound.nextThreadOutRef,
        Object.freeze({
          ...honest,
          route: "unprotected_output",
          signerRequired: false,
          signerPresent: false,
        }),
        adjudicated,
      ),
    );
    coverage.seamMutated("credential");
  }, 900_000);

  it("declares the complete lifecycle coverage it exercised", () => {
    // Recorded while the suites above ran, never pre-filled.
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
  });
});
