import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  acceptedVerdictSubjectV1,
  AddressData,
  addressDataFromBech32,
  DA_PAYLOAD_V1_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayloadV1,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubjectV1,
  hashBlockHeaderV1,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorV1Schema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import {
  prepareTransactionOutputEvidenceV1,
  submitTransactionOutputNonCanonicalCancelV1,
  submitTransactionOutputNonCanonicalStep01AcceptedV1,
  submitTransactionOutputNonCanonicalStep01ForcedV1,
  submitTransactionOutputNonCanonicalStep02V1,
  submitTransactionOutputNonCanonicalStep03V1,
  submitTransactionOutputNonCanonicalStep04V1,
  TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1,
  TRANSACTION_OUTPUT_NON_CANONICAL_PROPOSED_ID_V1,
  type TransactionOutputNonCanonicalContractsV1,
} from "../src/transaction-output-non-canonical/index.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { reconstructDaPayloadV1 } from "../src/transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { applyCompiledScript } from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCborV1 } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { makeSpendingValidator } from "./support/emulator/validators.js";
import {
  outputReferenceCbor,
  setupFraudulentBlockV1,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  h32,
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
const firstStepDeploymentEntry = "fraudProofTransactionOutputNonCanonical";

const forcedFixture = async (operatorVkey: string, now: number) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalRoot = await keyValuePhasRootWithCount([
    { key: Buffer.from(finalUtxo[0], "hex"), value: descriptor },
  ]);
  const nativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputCbor: Buffer.from(
      "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
      "hex",
    ),
    witnessByte: "b8",
  });
  const source = deriveMidgardNativeTxProofSourceV1(
    adjudicateMidgardNativeTxFullV1Validity(nativeTx, "TxIsInvalid"),
  );
  const rejectionReason = {
    OutputNonCanonical: { output_index: 0n },
  } as const;
  const transaction = {
    tx_id: computeMidgardNativeTxIdV1(nativeTx).toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason: rejectionReason } },
  } as const;
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: transaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const transitionEntries = [
    transitionTraceDaEntry({
      key: 0n,
      keySchema: Data.Integer() as never,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "ForcedTransaction",
        pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: finalRoot.root,
      },
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: { step_index: 0n, phase: "ForcedTransaction" },
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const validationEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: {
        schema_version: 1n,
        machine_version: 1n,
        trace_root: h32("c1"),
        step_count: 1n,
        initial_state_hash: h32("c2"),
        terminal_state_hash: h32("c3"),
        verdict: "Rejected",
        rejection_code_hash: h32("c4"),
      },
      valueSchema: ValidationTraceDescriptorV1Schema,
    }),
  ];
  const counted = async (
    domain: Parameters<typeof buildCountedRoot>[0],
    entries: readonly (readonly [string, string])[],
  ) =>
    await buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [forcedRoot, transitionRoot, eventRoot, validationRoot] =
    await Promise.all([
      counted(ROOT_DOMAINS.forcedTransactionsV1, forcedEntries),
      counted(ROOT_DOMAINS.transitionTrace, transitionEntries),
      counted(ROOT_DOMAINS.eventToStep, eventEntries),
      counted(ROOT_DOMAINS.validationTraces, validationEntries),
    ]);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventRoot.root,
    validationTracesRoot: validationRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    encodeDaPayloadV1({
      version: DA_PAYLOAD_V1_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: sortedDaEntries([finalUtxo]),
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(transitionEntries),
        event_to_step: sortedDaEntries(eventEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries([
          transitionTraceRawEntry(
            forcedEntries[0]![0],
            encodeMidgardNativeTxCanonicalV1(nativeTx).toString("hex"),
          ),
        ]),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationEntries),
        validation_trace_witnesses: [],
        counts,
      },
    }),
    { mode: "identity" },
  );
  return {
    header,
    reconstruction: await reconstructDaPayloadV1({
      payloadEnvelopeCbor,
      expectedHeaderHash: headerHash,
      committedHeader: header,
    }),
    eventKey,
    nativeTx,
    transaction,
    rejectionReason,
  };
};

describe("transactionOutputNonCanonical local-catalogue lifecycle", () => {
  it("runs accepted and forced Init through scan, final mint, and removal before central registration", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const titles = TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1;
    const step04 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles.step04, [
        harness.contracts.fraudProof.policyId,
        addressData,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step03 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles.step03, [
        step04.spendingScriptHash,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step02 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles.step02, [
        step03.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.fieldPreimageCertificate.policyId,
      ]),
    );
    const step01 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles.step01, [
        step02.spendingScriptHash,
        harness.contracts.computationThread.policyId,
        harness.contracts.hubOracle.policyId,
      ]),
    );
    const steps = [step01, step02, step03, step04] as const;
    const contracts: TransactionOutputNonCanonicalContractsV1 = {
      steps: steps.map((step, index) => ({
        ...step,
        blueprintTitle: Object.values(titles)[index]!,
        referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
      })) as unknown as TransactionOutputNonCanonicalContractsV1["steps"],
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
        transactionOutputNonCanonical: {
          categoryId: TRANSACTION_OUTPUT_NON_CANONICAL_PROPOSED_ID_V1,
          scriptHash: step01.spendingScriptHash,
        },
      },
    );
    const category = catalogue.extraCategories.transactionOutputNonCanonical!;

    // The selected raw CBOR item is exactly the family maximum. Its leading
    // unsigned integer is not an output map, so the scanner rejects it. A
    // second unselected item takes the authenticated field into Certified
    // carriage without crossing the 32,768-byte field ceiling.
    const malformedOutput = Buffer.alloc(16_384);
    const nativeTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbors: [malformedOutput, Buffer.alloc(16_377)],
    });
    const nativeTxId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    const compactCbor = encodeMidgardNativeTxCompactV1(nativeTx.compact);
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
    const setup = await setupFraudulentBlockV1({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: { transactionsRoot, l2TransactionCount: 1n },
    });
    const evidence = prepareTransactionOutputEvidenceV1({
      finding: {
        subject: acceptedVerdictSubjectV1(nativeTxId),
        fieldIndex: 2,
        itemIndex: 0,
      },
      fieldPreimage: nativeTx.body.outputsPreimageCbor,
      committedFieldHashHex: midgardFieldCommitmentV1(
        nativeTx.body.outputsPreimageCbor,
      ).toString("hex"),
    });
    expect(evidence.canonical).toBe(false);
    expect(evidence.itemLength).toBe(16_384);
    expect(Buffer.from(evidence.fieldPreimageHex, "hex")).toHaveLength(32_768);
    expect(evidence.carriage).toBe("Certified");

    const references: UTxO[] = [];
    for (const [index, step] of steps.entries()) {
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `transaction-output-non-canonical-${index.toString()}`,
          })
        ).utxo,
      );
    }
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "transaction-output-non-canonical-certificate",
      })
    ).utxo;
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
      submitTransactionOutputNonCanonicalStep01AcceptedV1({
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
      submitTransactionOutputNonCanonicalStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
          deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
        ).toString("hex"),
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    const step03Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep03V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor: "",
        referenceScriptUtxo: references[2]!,
      }),
    );
    expect(step03Result.result.terminal).toBe(true);
    const step04Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep04V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step03Result.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(step04Result.result.fraudProofUnit).toBeTruthy();

    for (const capture of [
      init,
      step01Result,
      step02Result,
      step03Result,
      step04Result,
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
              ["init", init.measurement],
              ["step01", step01Result.measurement],
              ["step02", step02Result.measurement],
              ["step03", step03Result.measurement],
              ["step04", step04Result.measurement],
            ],
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }

    const cancellationMeasurements: unknown[] = [];
    for (const targetStep of [0, 1, 2, 3] as const) {
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
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      let cancelThread = `${cancelInit.txHash}#${cancelInit.firstStepOutputIndex.toString()}`;
      if (targetStep >= 1) {
        const [cancelThreadUtxo] = await harness.proverLucid.utxosByOutRef([
          {
            txHash: cancelInit.txHash,
            outputIndex: cancelInit.firstStepOutputIndex,
          },
        ]);
        if (cancelThreadUtxo === undefined)
          throw new Error("cancel thread absent");
        cancelThread = (
          await submitTransactionOutputNonCanonicalStep01AcceptedV1({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network,
            contracts,
            signer: harness.proverSigner,
            finding: evidence,
            threadUtxo: cancelThreadUtxo,
            threadToken: {
              unit: cancelInit.computationThreadUnit,
              fraudulentHeaderHash: cancelInit.fraudulentHeaderHash,
            },
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          })
        ).nextThreadOutRef;
      }
      if (targetStep >= 2) {
        cancelThread = (
          await submitTransactionOutputNonCanonicalStep02V1({
            lucid: harness.proverLucid,
            contracts,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: cancelThread,
            evidence,
            nativeTxCompactCbor: compactCbor.toString("hex"),
            witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompactV1(
              deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
            ).toString("hex"),
            referenceScriptUtxo: references[1]!,
            certificateReferenceScriptUtxo: certificateReference,
          })
        ).nextThreadOutRef;
      }
      if (targetStep >= 3) {
        const scan = await submitTransactionOutputNonCanonicalStep03V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: cancelThread,
          evidence,
          nativeTxCompactCbor: compactCbor.toString("hex"),
          witnessSetCompactCbor: "",
          referenceScriptUtxo: references[2]!,
        });
        expect(scan.terminal).toBe(true);
        cancelThread = scan.nextThreadOutRef;
      }
      const cancelled = await captureEmulatorSubmission(harness.emulator, () =>
        submitTransactionOutputNonCanonicalCancelV1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: cancelThread,
          referenceScriptUtxo: references[targetStep]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expect(cancelled.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(cancelled.measurement.executionMemory).toBeGreaterThan(0n);
      cancellationMeasurements.push([
        `cancel-step0${(targetStep + 1).toString()}`,
        cancelled.measurement,
      ]);
    }
    if (process.env.MIDGARD_PRINT_FIT === "1") {
      console.info(
        JSON.stringify(
          { lifecycle: cancellationMeasurements },
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
          name: "transactionOutputNonCanonical",
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
    expect(removal.result.fraudCategoryId).toBe("00000029");
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

    {
      const harness = await makeFaultProofEmulatorHarnessV1({
        contractOptions: { alwaysFraudProofCatalogue: true },
      });
      const addressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(
          Effect.map((address) => Data.from(Data.to(address, AddressData))),
        ),
      );
      const titles = TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1;
      const step04 = makeSpendingValidator(
        applyCompiledScript(harness.realBlueprint, titles.step04, [
          harness.contracts.fraudProof.policyId,
          addressData,
          harness.contracts.computationThread.policyId,
        ]),
      );
      const step03 = makeSpendingValidator(
        applyCompiledScript(harness.realBlueprint, titles.step03, [
          step04.spendingScriptHash,
          harness.contracts.computationThread.policyId,
        ]),
      );
      const step02 = makeSpendingValidator(
        applyCompiledScript(harness.realBlueprint, titles.step02, [
          step03.spendingScriptHash,
          harness.contracts.computationThread.policyId,
          harness.contracts.fieldPreimageCertificate.policyId,
        ]),
      );
      const step01 = makeSpendingValidator(
        applyCompiledScript(harness.realBlueprint, titles.step01, [
          step02.spendingScriptHash,
          harness.contracts.computationThread.policyId,
          harness.contracts.hubOracle.policyId,
        ]),
      );
      const steps = [step01, step02, step03, step04] as const;
      const contracts: TransactionOutputNonCanonicalContractsV1 = {
        steps: steps.map((step, index) => ({
          ...step,
          blueprintTitle: Object.values(titles)[index]!,
          referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
        })) as unknown as TransactionOutputNonCanonicalContractsV1["steps"],
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
          transactionOutputNonCanonical: {
            categoryId: TRANSACTION_OUTPUT_NON_CANONICAL_PROPOSED_ID_V1,
            scriptHash: step01.spendingScriptHash,
          },
        },
      );
      const category = catalogue.extraCategories.transactionOutputNonCanonical!;
      const funderCredential = (
        await harness.funderLucid.wallet().address()
      ).match(/^addr/u)
        ? (await import("@lucid-evolution/lucid")).getAddressDetails(
            await harness.funderLucid.wallet().address(),
          ).paymentCredential
        : undefined;
      if (funderCredential?.type !== "Key")
        throw new Error("forced fixture funder key absent");
      const forced = await forcedFixture(
        funderCredential.hash,
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
      );
      const forcedSetup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header: forced.header,
      });
      const references: UTxO[] = [];
      for (const [index, step] of steps.entries()) {
        references.push(
          (
            await publishPlainReferenceScriptUtxo({
              lucid: harness.funderLucid,
              script: step.spendingScript,
              label: `transaction-output-forced-${index.toString()}`,
            })
          ).utxo,
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
      const membership = await buildForcedTransactionLeafMembershipProof({
        reconstruction: forced.reconstruction,
        eventKey: forced.eventKey,
      });
      const forcedEvidence = prepareTransactionOutputEvidenceV1({
        finding: {
          subject: forcedVerdictSubjectV1({
            transactionId: forced.transaction.tx_id,
            sourceKey: membership.key,
            rejectionReason: forced.rejectionReason,
          }),
          fieldIndex: 2,
          itemIndex: 0,
        },
        fieldPreimage: forced.nativeTx.body.outputsPreimageCbor,
        committedFieldHashHex: midgardFieldCommitmentV1(
          forced.nativeTx.body.outputsPreimageCbor,
        ).toString("hex"),
      });
      expect(forcedEvidence.canonical).toBe(true);
      const forcedInit = await submitCommittedFieldShapeInit({
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
        fraudulentBlockOutRef: forcedSetup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const forced01 = await submitTransactionOutputNonCanonicalStep01ForcedV1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${forcedInit.txHash}#${forcedInit.firstStepOutputIndex.toString()}`,
        finding: forcedEvidence,
        forcedSource: { header: forced.header, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      });
      const forced02 = await submitTransactionOutputNonCanonicalStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forced01.nextThreadOutRef,
        evidence: forcedEvidence,
        nativeTxCompactCbor: forced.transaction.source.compact_cbor,
        witnessSetCompactCbor:
          forced.transaction.source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1]!,
      });
      let forcedThread = forced02.nextThreadOutRef;
      for (;;) {
        const scan = await submitTransactionOutputNonCanonicalStep03V1({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: forcedThread,
          evidence: forcedEvidence,
          nativeTxCompactCbor: forced.transaction.source.compact_cbor,
          witnessSetCompactCbor:
            forced.transaction.source.witness_set_compact_cbor,
          referenceScriptUtxo: references[2]!,
        });
        forcedThread = scan.nextThreadOutRef;
        if (scan.terminal) break;
      }
      const forced04 = await submitTransactionOutputNonCanonicalStep04V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: forcedThread,
        evidence: forcedEvidence,
        referenceScriptUtxo: references[3]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      expect(forced04.fraudProofUnit).toBeTruthy();
      const forcedNow = BigInt(harness.emulator.now());
      const forcedRemoval = await submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: {
          name: "transactionOutputNonCanonical",
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
        fraudulentHeaderHash: forcedSetup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: forcedNow > 120_000n ? forcedNow - 120_000n : 0n,
        validTo: forcedNow + 300_000n,
      });
      expect(forcedRemoval.fraudCategoryId).toBe("00000029");
    }
  }, 180_000);
});
