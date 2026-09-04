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
  applyResolvedOutputNonCanonicalScripts,
  prepareResolvedOutputNonCanonicalEvidence,
  RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  type ResolvedOutputNonCanonicalContracts,
  submitResolvedOutputNonCanonicalCancel,
  submitResolvedOutputNonCanonicalStep01Accepted,
  submitResolvedOutputNonCanonicalStep02,
  submitResolvedOutputNonCanonicalStep03,
  submitResolvedOutputNonCanonicalStep04,
  submitResolvedOutputNonCanonicalStep05,
} from "../src/resolved-output-non-canonical/index.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
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
    harness.contracts.fraudProofContracts.resolvedOutputNonCanonical;
  const category = harness.catalogue.categories.resolvedOutputNonCanonical;
  expectRegisteredChainParity({
    registered,
    applied: applyResolvedOutputNonCanonicalScripts({
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
    RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  );
  const contracts: ResolvedOutputNonCanonicalContracts = {
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

describe("resolvedOutputNonCanonical registered-chain lifecycle", () => {
  it("runs accepted Init through scan, final mint, and removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realResolvedOutputNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const { steps, contracts, catalogue, category } =
      await registeredContracts(harness);

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
    coverage.scenario("maximum_supported_evidence");

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
    coverage.cancelled("step-01");
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
    coverage.resumed();
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
    coverage.reason("InputSpentOutputNonCanonical", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

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
    // A registered family resolves removal through the canonical catalogue:
    // the manifest's fraudProofResolvedOutputNonCanonical entries carry the
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
        fraudCategory: "resolvedOutputNonCanonical",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("00000026");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
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

  it("declares the lifecycle coverage it exercised and the gaps it leaves open", () => {
    // Recorded while the suite above ran, never pre-filled. The gate lists
    // every omission; the suite pins that list so a silent regression of what
    // it does cover, or an unannounced closure of a gap, both fail here.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: ["InputSpentOutputNonCanonical"],
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
      "incomplete fault-proof lifecycle coverage: InputSpentOutputNonCanonical success directions: forced_rejection_wrong; scenarios: wrongful_forced_rejection_success, honest_accepted_block_refusal, honest_forced_rejection_refusal, reason_or_subject_coordinate_mutation; authentication seams: tx_membership, prior_output_membership, field_certificate, forced_leaf; cancel steps: step-02, step-03, step-04, step-05",
    );
  });
});
