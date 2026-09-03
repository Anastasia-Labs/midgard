import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  hashBlockHeader,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  OUTPUT_REFERENCE_SCRIPT_DECODING_ID,
  type OutputReferenceScriptDecodingContracts,
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
  submitOutputReferenceScriptDecodingCancel,
  submitOutputReferenceScriptDecodingStep01Accepted,
  submitOutputReferenceScriptDecodingStep01Forced,
  submitOutputReferenceScriptDecodingStep02,
  submitOutputReferenceScriptDecodingStep03,
  submitOutputReferenceScriptDecodingStep04,
  submitOutputReferenceScriptDecodingStep05,
  submitOutputReferenceScriptDecodingStep06,
} from "../src/output-reference-script-decoding/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { applyCompiledScript } from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { l2TransactionSourceCbor as l2TransactionSourceCborV1 } from "./support/emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { makeSpendingValidator } from "./support/emulator/validators.js";
import {
  outputReferenceCbor,
  setupFraudulentBlock,
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
const firstStepDeploymentEntry = "fraudProofOutputReferenceScriptDecoding";

const forcedFixture = async (operatorVkey: string, now: number) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalRoot = await keyValuePhasRootWithCount([
    { key: Buffer.from(finalUtxo[0], "hex"), value: descriptor },
  ]);
  const nativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
      value: { lovelace: 5_000_000n, assets: new Map() },
      script_ref: {
        language: "NativeCardano",
        scriptBytes: Buffer.alloc(0),
        nativeScript: { type: "sig", keyHash: Buffer.alloc(28, 0x22) },
      },
    }),
  });
  const source = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
  );
  const rejectionReason = {
    OutputReferenceScriptMalformed: { output_index: 0n },
  } as const;
  const transaction = {
    tx_id: computeMidgardNativeTxId(nativeTx).toString("hex"),
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
      valueSchema: ForcedInclusionTxSchema,
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
      valueSchema: ValidationTraceDescriptorSchema,
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
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    encodeDaPayload({
      version: DA_PAYLOAD_VERSION,
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
            encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
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
    reconstruction: await reconstructDaPayload({
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

describe("outputReferenceScriptDecoding local-catalogue lifecycle", () => {
  it("runs maximum accepted output through resume and permanent proof mint", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const titles = OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES;
    const step06 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[5], [
        harness.contracts.fraudProof.policyId,
        addressData,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step05 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[4], [
        step06.spendingScriptHash,
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
    const validators = [
      step01,
      step02,
      step03,
      step04,
      step05,
      step06,
    ] as const;
    const contracts: OutputReferenceScriptDecodingContracts = {
      steps: validators.map((step, index) => ({
        ...step,
        blueprintTitle: titles[index]!,
        referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
      })) as unknown as OutputReferenceScriptDecodingContracts["steps"],
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
        outputReferenceScriptDecoding: {
          categoryId: OUTPUT_REFERENCE_SCRIPT_DECODING_ID,
          scriptHash: step01.spendingScriptHash,
        },
      },
    );
    const category = catalogue.extraCategories.outputReferenceScriptDecoding!;
    let output = Buffer.alloc(0);
    for (let payload = 16_320; payload > 15_000; payload -= 1) {
      const candidate = encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
        value: { lovelace: 2_000_000n, assets: new Map() },
        script_ref: {
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(payload, 0),
        },
      });
      if (candidate.length <= 16_384) {
        output = Buffer.from(candidate);
        break;
      }
    }
    const marker = Buffer.from("8203", "hex");
    const markerOffset = output.indexOf(marker);
    if (markerOffset < 0) throw new Error("versioned script marker absent");
    output[markerOffset + 1] = 0;
    expect(output.length).toBeGreaterThan(16_300);
    const nativeTx = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbors: [output],
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
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue,
      fixture: { transactionsRoot, l2TransactionCount: 1n },
    });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(nativeTx),
    });
    const references: UTxO[] = [];
    for (const [index, step] of validators.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `output-reference-${index.toString()}`,
          })
        ).utxo,
      );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "output-reference-certificate",
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
    const s1 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep01Accepted({
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
    const witnessSetCompactCbor = encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
    ).toString("hex");
    const s2 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: s1.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        publishCarriage: true,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    let scanOutRef = s2.result.nextThreadOutRef;
    const scans = [];
    for (;;) {
      const scan = await captureEmulatorSubmission(harness.emulator, () =>
        submitOutputReferenceScriptDecodingStep03({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: scanOutRef,
          evidence,
          referenceScriptUtxo: references[2]!,
        }),
      );
      scans.push(scan);
      scanOutRef = scan.result.nextThreadOutRef;
      if (scan.result.terminal) break;
    }
    const s4 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: scanOutRef,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        publishedCarriageUtxos: s2.result.carriageUtxos,
        certificateUtxo: s2.result.certificateUtxo,
        referenceScriptUtxo: references[3]!,
      }),
    );
    const s5 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: s4.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
      }),
    );
    expect(s5.result.closed).toBe(true);
    const s6 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep06({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: s5.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(s6.result.fraudProofUnit).toBeTruthy();
    for (const capture of [init, s1, s2, ...scans, s4, s5, s6]) {
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(capture.measurement.executionMemory).toBeGreaterThan(0n);
      expect(capture.measurement.executionSteps).toBeGreaterThan(0n);
    }

    const startThreadAtStep01 = async () => {
      const branchInit = await submitCommittedFieldShapeInit({
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
      return branchInit;
    };
    const startThreadAtStep02 = async () => {
      const branchInit = await startThreadAtStep01();
      const [branchThread] = await harness.proverLucid.utxosByOutRef([
        {
          txHash: branchInit.txHash,
          outputIndex: branchInit.firstStepOutputIndex,
        },
      ]);
      if (branchThread === undefined) throw new Error("branch thread absent");
      const branch01 = await submitOutputReferenceScriptDecodingStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo: branchThread,
        threadToken: {
          unit: branchInit.computationThreadUnit,
          fraudulentHeaderHash: branchInit.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      return branch01.nextThreadOutRef;
    };
    const startThreadAtOutputScan = async () => {
      const branch02 = await submitOutputReferenceScriptDecodingStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: await startThreadAtStep02(),
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        witnessSetCompactCbor,
        publishCarriage: true,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      });
      return branch02;
    };

    const cancelStep01Init = await startThreadAtStep01();
    const cancelStep01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${cancelStep01Init.txHash}#${cancelStep01Init.firstStepOutputIndex.toString()}`,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(cancelStep01.measurement.l1ByteMargin).toBeGreaterThan(0);

    const cancelStep02 = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitOutputReferenceScriptDecodingCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: await startThreadAtStep02(),
          referenceScriptUtxo: references[1]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(cancelStep02.measurement.l1ByteMargin).toBeGreaterThan(0);

    const cancelOutputScan = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitOutputReferenceScriptDecodingCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: (await startThreadAtOutputScan()).nextThreadOutRef,
          referenceScriptUtxo: references[2]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(cancelOutputScan.measurement.l1ByteMargin).toBeGreaterThan(0);

    const referenceBindStart = await startThreadAtOutputScan();
    let referenceBindThread = referenceBindStart.nextThreadOutRef;
    for (;;) {
      const scan = await submitOutputReferenceScriptDecodingStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: referenceBindThread,
        evidence,
        referenceScriptUtxo: references[2]!,
      });
      referenceBindThread = scan.nextThreadOutRef;
      if (scan.terminal) break;
    }
    const cancelReferenceBind = await captureEmulatorSubmission(
      harness.emulator,
      () =>
        submitOutputReferenceScriptDecodingCancel({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: referenceBindThread,
          referenceScriptUtxo: references[3]!,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(cancelReferenceBind.measurement.l1ByteMargin).toBeGreaterThan(0);

    const step05Start = await startThreadAtOutputScan();
    let step05Thread = step05Start.nextThreadOutRef;
    for (;;) {
      const scan = await submitOutputReferenceScriptDecodingStep03({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step05Thread,
        evidence,
        referenceScriptUtxo: references[2]!,
      });
      step05Thread = scan.nextThreadOutRef;
      if (scan.terminal) break;
    }
    step05Thread = (
      await submitOutputReferenceScriptDecodingStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step05Thread,
        evidence,
        nativeTxCompactCbor: compactCbor.toString("hex"),
        publishedCarriageUtxos: step05Start.carriageUtxos,
        certificateUtxo: step05Start.certificateUtxo,
        referenceScriptUtxo: references[3]!,
      })
    ).nextThreadOutRef;
    const cancelStep05 = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingCancel({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step05Thread,
        referenceScriptUtxo: references[4]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(cancelStep05.measurement.l1ByteMargin).toBeGreaterThan(0);

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
          name: "outputReferenceScriptDecoding" as never,
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
            token: "output-reference-script-decoding-emulator",
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
    expect(removal.result.fraudCategoryId).toBe("0000002a");
    expect(removal.measurement.l1ByteMargin).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            lifecycle: [
              ["init", init.measurement],
              ["step01", s1.measurement],
              ["step02", s2.measurement],
              ...scans.map((scan, index) => [
                `step03-${index.toString()}`,
                scan.measurement,
              ]),
              ["step04", s4.measurement],
              ["step05", s5.measurement],
              ["step06", s6.measurement],
              ["cancel-step01", cancelStep01.measurement],
              ["cancel-step02", cancelStep02.measurement],
              ["cancel-step03", cancelOutputScan.measurement],
              ["cancel-step04", cancelReferenceBind.measurement],
              ["cancel-step05", cancelStep05.measurement],
              ["removal", removal.measurement],
            ],
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
  }, 600_000);

  it("runs a decodable forced wrongful rejection through the real chain", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
    );
    const titles = OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES;
    const step06 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[5], [
        harness.contracts.fraudProof.policyId,
        addressData,
        harness.contracts.computationThread.policyId,
      ]),
    );
    const step05 = makeSpendingValidator(
      applyCompiledScript(harness.realBlueprint, titles[4], [
        step06.spendingScriptHash,
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
    const validators = [
      step01,
      step02,
      step03,
      step04,
      step05,
      step06,
    ] as const;
    const contracts: OutputReferenceScriptDecodingContracts = {
      steps: validators.map((step, index) => ({
        ...step,
        blueprintTitle: titles[index]!,
        referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
      })) as unknown as OutputReferenceScriptDecodingContracts["steps"],
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
        outputReferenceScriptDecoding: {
          categoryId: OUTPUT_REFERENCE_SCRIPT_DECODING_ID,
          scriptHash: step01.spendingScriptHash,
        },
      },
    );
    const category = catalogue.extraCategories.outputReferenceScriptDecoding!;
    const funderCredential = (
      await import("@lucid-evolution/lucid")
    ).getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (funderCredential?.type !== "Key")
      throw new Error("forced fixture funder key absent");
    const forced = await forcedFixture(
      funderCredential.hash,
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    );
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header: forced.header,
    });
    const references: UTxO[] = [];
    for (const [index, step] of validators.entries())
      references.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid: harness.funderLucid,
            script: step.spendingScript,
            label: `output-reference-forced-${index.toString()}`,
          })
        ).utxo,
      );
    const certificateReference = (
      await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: harness.contracts.fieldPreimageCertificate.mintingScript,
        label: "output-reference-forced-certificate",
      })
    ).utxo;
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: forced.reconstruction,
      eventKey: forced.eventKey,
    });
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(forced.transaction.tx_id),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(
          forced.nativeTx,
        ),
      }),
    ).toThrow(/agrees with operator verdict/u);
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: forcedVerdictSubject({
          transactionId: forced.transaction.tx_id,
          sourceKey: membership.key,
          rejectionReason: {
            OutputReferenceScriptMalformed: { output_index: 1n },
          },
        }),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(
          forced.nativeTx,
        ),
      }),
    ).toThrow(/coordinate differs/u);
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: forcedVerdictSubject({
          transactionId: "ff".repeat(32),
          sourceKey: membership.key,
          rejectionReason: forced.rejectionReason,
        }),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(
          forced.nativeTx,
        ),
      }),
    ).toThrow(/identity was substituted/u);
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: forcedVerdictSubject({
        transactionId: forced.transaction.tx_id,
        sourceKey: membership.key,
        rejectionReason: forced.rejectionReason,
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(forced.nativeTx),
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
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
    const step01Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep01Forced({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${init.result.txHash}#${init.result.firstStepOutputIndex.toString()}`,
        evidence,
        forcedSource: { header: forced.header, membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
    const step02Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep02({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01Result.result.nextThreadOutRef,
        evidence,
        nativeTxCompactCbor: forced.transaction.source.compact_cbor,
        witnessSetCompactCbor:
          forced.transaction.source.witness_set_compact_cbor,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
    let threadOutRef = step02Result.result.nextThreadOutRef;
    const scans = [];
    for (;;) {
      const scan = await captureEmulatorSubmission(harness.emulator, () =>
        submitOutputReferenceScriptDecodingStep03({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          referenceScriptUtxo: references[2]!,
        }),
      );
      scans.push(scan);
      threadOutRef = scan.result.nextThreadOutRef;
      if (scan.result.terminal) break;
    }
    const step04Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep04({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: forced.transaction.source.compact_cbor,
        publishedCarriageUtxos: step02Result.result.carriageUtxos,
        certificateUtxo: step02Result.result.certificateUtxo,
        referenceScriptUtxo: references[3]!,
      }),
    );
    threadOutRef = step04Result.result.nextThreadOutRef;
    const step05Result = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep05({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
      }),
    );
    expect(step05Result.result.closed).toBe(true);
    const result = await captureEmulatorSubmission(harness.emulator, () =>
      submitOutputReferenceScriptDecodingStep06({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step05Result.result.nextThreadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expect(result.result.fraudProofUnit).toBeTruthy();
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        JSON.stringify(
          {
            forcedLifecycle: [
              ["forced-init", init.measurement],
              ["forced-step01", step01Result.measurement],
              ["forced-step02", step02Result.measurement],
              ...scans.map((scan, index) => [
                `forced-step03-${index.toString()}`,
                scan.measurement,
              ]),
              ["forced-step04", step04Result.measurement],
              ["forced-step05", step05Result.measurement],
              ["forced-step06", result.measurement],
            ],
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
  }, 300_000);
});
