/**
 * Emulator fixtures and stage drivers for the `outputReferenceScriptDecoding`
 * lifecycle suite: canonical accepted and forced blocks whose subject outputs
 * carry a versioned reference script, the registered six-step chain, and one
 * recorder for every complete signed measurement the Van Rossem fit ledger
 * keeps.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
  type MidgardNativeScript,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  AddressData,
  addressDataFromBech32,
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  hashBlockHeader,
  OutputReference,
  Proof,
  type RejectionReason,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  applyOutputReferenceScriptDecodingScripts,
  OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  type OutputReferenceScriptDecodingContracts,
  type OutputReferenceScriptDecodingEvidence,
  type OutputReferenceScriptScanArgs,
  type OutputReferenceScriptScanState,
  submitOutputReferenceScriptDecodingCancel,
  submitOutputReferenceScriptDecodingStep01Accepted,
  submitOutputReferenceScriptDecodingStep01Forced,
  submitOutputReferenceScriptDecodingStep01ForcedRaw,
  submitOutputReferenceScriptDecodingStep02,
  submitOutputReferenceScriptDecodingStep02Raw,
  submitOutputReferenceScriptDecodingStep03,
  submitOutputReferenceScriptDecodingStep04,
  submitOutputReferenceScriptDecodingStep05,
  submitOutputReferenceScriptDecodingStep05Raw,
  submitOutputReferenceScriptDecodingStep06,
  submitOutputReferenceScriptDecodingStep06Raw,
} from "../../src/output-reference-script-decoding/index.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import {
  outputReferenceCbor,
  setupFraudulentBlock,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  h32,
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
  submitSetupTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

export const network = "Custom" as const;
export const OUTPUT_REFERENCE_CATEGORY_ID = "0000002a";
/** `ledger_output_v1.max_output_canonical_cbor_bytes`: the consensus bound. */
export const OUTPUT_REFERENCE_MAX_OUTPUT_BYTES = 16_384;
export const OUTPUT_REFERENCE_REASON_ARMS = [
  "OutputReferenceScriptMalformed",
  "OutputReferenceScriptNodeLimit",
  "OutputReferenceScriptDepthLimit",
] as const;
export type OutputReferenceReasonArm =
  (typeof OUTPUT_REFERENCE_REASON_ARMS)[number];

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
export type Measurement = CompleteSignedTransactionMeasurement;

// ## Output fixtures

const subjectAddress = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 1),
]);
const subjectValue = { lovelace: 2_000_000n, assets: new Map() };

export const signatureScript = (fill = 2): MidgardNativeScript => ({
  type: "sig",
  keyHash: Buffer.alloc(28, fill),
});

/** `all [all [... sig]]` nested `depth` containers deep. */
export const nestedScript = (depth: number): MidgardNativeScript =>
  depth === 0
    ? signatureScript(3)
    : { type: "all", scripts: [nestedScript(depth - 1)] };

/** `all [sig × count]`. */
export const wideScript = (count: number): MidgardNativeScript => ({
  type: "all",
  scripts: Array.from({ length: count }, (_, index) =>
    signatureScript(4 + (index % 200)),
  ),
});

export const outputWithNativeScript = (script: MidgardNativeScript): Buffer =>
  Buffer.from(
    encodeMidgardTxOutput({
      address: subjectAddress,
      value: subjectValue,
      script_ref: {
        language: "NativeCardano",
        scriptBytes: Buffer.alloc(0),
        nativeScript: script,
      },
    }),
  );

/**
 * A canonical output whose reference script is `[0, payload]` for arbitrary
 * payload bytes: encoded as PlutusV3 and re-tagged to the native language, so
 * malformed and empty native payloads reach the descriptor unchanged.
 */
export const outputWithRawNativePayload = (payload: Buffer): Buffer => {
  const output = Buffer.from(
    encodeMidgardTxOutput({
      address: subjectAddress,
      value: subjectValue,
      script_ref: { language: "PlutusV3", scriptBytes: payload },
    }),
  );
  const marker = output.indexOf(Buffer.from("8203", "hex"));
  if (marker < 0) throw new Error("versioned script marker absent");
  output[marker + 1] = 0;
  return output;
};

/** The zero-payload output of exactly `length` bytes (malformed at token 0). */
export const rawNativeOutputOfLength = (length: number): Buffer => {
  for (let payload = length; payload > length - 64; payload -= 1) {
    const candidate = outputWithRawNativePayload(Buffer.alloc(payload, 0));
    if (candidate.length === length) return candidate;
  }
  throw new Error(`no zero-payload output of ${length.toString()} bytes`);
};

/** The widest `all [sig × n]` output that still fits the consensus bound. */
export const maximumWideScriptOutput = (): {
  readonly output: Buffer;
  readonly childCount: number;
} => {
  for (let count = 520; count > 0; count -= 1) {
    const output = outputWithNativeScript(wideScript(count));
    if (output.length <= OUTPUT_REFERENCE_MAX_OUTPUT_BYTES)
      return { output, childCount: count };
  }
  throw new Error("no wide script output fits the bound");
};

export const subjectTransaction = (
  outputs: readonly Buffer[],
  fee = 7n,
): MidgardNativeTxFull =>
  makeNativeTx({ spendInputCbors: [], fee, outputCbors: outputs });

// ## Registered chain

export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.outputReferenceScriptDecoding;
  const category = harness.catalogue.categories.outputReferenceScriptDecoding;
  expectRegisteredChainParity({
    registered,
    applied: applyOutputReferenceScriptDecodingScripts({
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
  const validators = familyStepsFromRegisteredChain(
    registered.steps,
    OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  );
  const contracts: OutputReferenceScriptDecodingContracts = {
    steps: validators,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { validators, contracts, catalogue: harness.catalogue, category };
};

export const makeOutputReferenceHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realOutputReferenceScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  return { harness, ...(await registeredContracts(harness)) };
};
export type OutputReferenceContext = Awaited<
  ReturnType<typeof makeOutputReferenceHarness>
>;

// ## Fit-ledger recorder

export const MAXIMUM_SHAPE =
  "16,384-byte accepted output (zero native payload) with Certified field-2 carriage and eight resumable descriptor windows; widest all-of native script that fits the bound (16-step resumable scans, adjacent chunk windows); nested containers; cancellation from every step; mint and leased removal";

export const createMeasurementRecorder = () => {
  const measurements: VanRossemFitMeasurement[] = [];
  const names = new Set<string>();
  const record = (
    name: string,
    measurement: Measurement,
    { runsScripts = true, maximumShape = MAXIMUM_SHAPE } = {},
  ) => {
    expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    if (runsScripts) {
      expect(measurement.executionMemory, name).toBeGreaterThan(0n);
      expect(measurement.executionSteps, name).toBeGreaterThan(0n);
    }
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "lifecycle",
      maximumShape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  const recordPublication = (stepIndex: number, measurement: Measurement) => {
    const name = `publish-step0${(stepIndex + 1).toString()}`;
    expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "publication",
      maximumShape: "fully applied testnet validator",
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  /** Step 02/04 carriage publications and the certificate mint. */
  const recordCarriage = (
    prefix: string,
    captured: { readonly measurements: readonly Measurement[] },
  ) => {
    const auxiliary = captured.measurements.slice(0, -1);
    auxiliary.forEach((measurement, index) => {
      const last = index === auxiliary.length - 1;
      record(
        last
          ? `${prefix}-carriage-certificate`
          : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
        { runsScripts: last },
      );
    });
  };
  return { measurements, record, recordPublication, recordCarriage };
};
export type MeasurementRecorder = ReturnType<typeof createMeasurementRecorder>;

// ## Blocks

export type AcceptedSubject = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxId: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly canonicalCbor: Buffer;
  readonly txInclusion: SubmitStep01TxInclusion;
};

/** Commits accepted transactions under one fraudulent state-queue block. */
export const commitAcceptedBlock = async (
  { harness, catalogue }: OutputReferenceContext,
  nativeTxs: readonly MidgardNativeTxFull[],
) => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const prepared = nativeTxs.map((nativeTx) => {
    const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
    return {
      nativeTx,
      nativeTxId,
      sourceCbor: l2TransactionSourceCbor(nativeTx),
    };
  });
  for (const { nativeTxId, sourceCbor } of prepared)
    await trie.insert(
      Buffer.from(nativeTxId, "hex"),
      Buffer.from(sourceCbor, "hex"),
    );
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const subjects: AcceptedSubject[] = [];
  for (const { nativeTx, nativeTxId, sourceCbor } of prepared) {
    const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
    subjects.push({
      nativeTx,
      nativeTxId,
      compactCborHex: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
        "hex",
      ),
      witnessSetCompactCborHex: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
      ).toString("hex"),
      canonicalCbor: Buffer.from(encodeMidgardNativeTxCanonical(nativeTx)),
      txInclusion: {
        nativeTxId,
        nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          nativeTx.compact,
        ).toString("hex"),
        l2TransactionSourceCbor: sourceCbor,
        transactionsPhasRoot: transactionsRoot,
        txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
    });
  }
  const setup = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: BigInt(nativeTxs.length),
    },
  });
  return { subjects, setup, transactionsRoot };
};

export type ForcedLeafSpec = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly reason: RejectionReason;
};

/**
 * Commits forced leaves under one header: every leaf is an operator rejection
 * carrying its own typed reason, one transition step and event per leaf.
 */
export const commitForcedBlock = async (
  { harness, catalogue }: OutputReferenceContext,
  leaves: readonly ForcedLeafSpec[],
) => {
  const funderCredential = (
    await import("@lucid-evolution/lucid")
  ).getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (funderCredential?.type !== "Key")
    throw new Error("forced fixture funder key absent");
  const now =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
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
  const built = leaves.map(({ nativeTx, reason }, index) => {
    const txOrderId = transitionTraceOutRef(`f${(index + 1).toString()}`);
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
    const source = deriveMidgardForcedTxProofSource(
      materializeMidgardForcedTxFromCanonical(nativeTx),
    );
    const transaction = {
      tx_id: computeMidgardNativeTxId(nativeTx).toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason } },
    } as const;
    return {
      index,
      txOrderId,
      eventKey,
      nativeTx,
      transaction,
      reason,
      forcedEntry: transitionTraceDaEntry({
        key: txOrderId,
        keySchema: OutputReference as never,
        value: transaction,
        valueSchema: ForcedInclusionTxV1Schema,
      }),
      transitionEntry: transitionTraceDaEntry({
        key: BigInt(index),
        keySchema: Data.Integer() as never,
        value: {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: eventKey,
          phase: "ForcedTransaction",
          pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: finalRoot.root,
        },
        valueSchema: TransitionStepSchema,
      }),
      eventEntry: transitionTraceDaEntry({
        key: eventKey,
        keySchema: EventKeySchema,
        value: { step_index: BigInt(index), phase: "ForcedTransaction" },
        valueSchema: EventToStepValueSchema,
      }),
      validationEntry: transitionTraceDaEntry({
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
      canonicalHex: encodeMidgardForcedTxCanonical(nativeTx).toString("hex"),
    };
  });
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
  const forcedEntries = built.map((leaf) => leaf.forcedEntry);
  const transitionEntries = built.map((leaf) => leaf.transitionEntry);
  const eventEntries = built.map((leaf) => leaf.eventEntry);
  const validationEntries = built.map((leaf) => leaf.validationEntry);
  const [forcedRoot, transitionRoot, eventRoot, validationRoot] =
    await Promise.all([
      counted(ROOT_DOMAINS.forcedTransactionsV1, forcedEntries),
      counted(ROOT_DOMAINS.transitionTrace, transitionEntries),
      counted(ROOT_DOMAINS.eventToStep, eventEntries),
      counted(ROOT_DOMAINS.validationTraces, validationEntries),
    ]);
  const count = BigInt(leaves.length);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: count,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: count,
    transitionStepCount: count,
    validationTraceCount: count,
  };
  const header = {
    ...makeHeader(funderCredential.hash, now),
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
        forced_transaction_preimages: sortedDaEntries(
          built.map((leaf) =>
            transitionTraceRawEntry(leaf.forcedEntry[0], leaf.canonicalHex),
          ),
        ),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationEntries),
        validation_trace_witnesses: [],
        counts,
      },
    }),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const forcedLeaves = [];
  for (const leaf of built) {
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction,
      eventKey: leaf.eventKey,
    });
    forcedLeaves.push({
      nativeTx: leaf.nativeTx,
      transaction: leaf.transaction,
      reason: leaf.reason,
      eventKey: leaf.eventKey,
      membership,
      canonicalCbor: Buffer.from(encodeMidgardForcedTxCanonical(leaf.nativeTx)),
    });
  }
  return { header, headerHash, reconstruction, setup, leaves: forcedLeaves };
};
export type ForcedLeaf = Awaited<
  ReturnType<typeof commitForcedBlock>
>["leaves"][number];

/**
 * A forced root that carries the same leaf but which the header never
 * committed: the membership proof verifies against its own root and the
 * counted-root binding to the header refuses it.
 */
export const foreignForcedMembership = async (
  membership: ForcedLeaf["membership"],
) => {
  const key = Buffer.from(Data.to(membership.key, OutputReference), "hex");
  const value = Buffer.from(
    Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const decoy = Buffer.from(
    Data.to(
      { transactionId: "ab".repeat(32), outputIndex: 7n },
      OutputReference,
    ),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key, value },
    { key: decoy, value },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(key, value);
  await trie.insert(decoy, value);
  return {
    ...membership,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: Data.from((await trie.prove(key)).toCBOR().toString("hex"), Proof),
  };
};

// ## Stages

export const publishFamilyReferences = async (
  { harness, validators }: OutputReferenceContext,
  recorder: MeasurementRecorder,
  label: string,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of validators.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${label}-${index.toString()}`,
    });
    recorder.recordPublication(index, published.publicationMeasurement);
    references.push(published.utxo);
  }
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${label}-certificate`,
    })
  ).utxo;
  return { references, certificateReference };
};

export const makeOutputReferenceStages = ({
  context,
  fraudulentBlockOutRef,
  references,
  certificateReference,
}: {
  readonly context: OutputReferenceContext;
  readonly fraudulentBlockOutRef: string;
  readonly references: readonly UTxO[];
  readonly certificateReference: UTxO;
}) => {
  const { harness, contracts, catalogue, category } = context;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const captured = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);

  const init = async () =>
    await captured(() =>
      submitCommittedFieldShapeInit({
        lucid,
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
        signer,
        fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const threadOf = (
    initialized: Awaited<ReturnType<typeof init>>["result"],
  ): string =>
    `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`;
  const threadUtxoOf = async (threadOutRef: string): Promise<UTxO> => {
    const [txHash, outputIndex] = threadOutRef.split("#");
    const [utxo] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    if (utxo === undefined) throw new Error("thread absent");
    return utxo;
  };

  const step01Accepted = async (
    initialized: Awaited<ReturnType<typeof init>>["result"],
    finding: {
      readonly subject: OutputReferenceScriptDecodingEvidence["subject"];
      readonly outputIndex: number;
    },
    txInclusion: SubmitStep01TxInclusion,
  ) =>
    await captured(async () =>
      submitOutputReferenceScriptDecodingStep01Accepted({
        lucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer,
        finding,
        threadUtxo: await threadUtxoOf(threadOf(initialized)),
        threadToken: {
          unit: initialized.computationThreadUnit,
          fraudulentHeaderHash: initialized.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step01Forced = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    leaf: ForcedLeaf,
    header: Parameters<typeof submitSetupTx>[0]["header"],
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep01Forced({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        forcedSource: { header, membership: leaf.membership, direction: 1n },
        referenceScriptUtxo: references[0]!,
      }),
    );
  const step01ForcedRaw = async (
    args: Omit<
      Parameters<typeof submitOutputReferenceScriptDecodingStep01ForcedRaw>[0],
      "lucid" | "contracts" | "categoryId" | "signer" | "referenceScriptUtxo"
    >,
  ) =>
    await submitOutputReferenceScriptDecodingStep01ForcedRaw({
      lucid,
      contracts,
      categoryId,
      signer,
      referenceScriptUtxo: references[0]!,
      ...args,
    });
  const step02 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    source: {
      readonly compactCborHex: string;
      readonly witnessSetCompactCborHex: string;
    },
    options: {
      readonly publishedCarriageUtxos?: readonly UTxO[];
      readonly certificateUtxo?: UTxO;
    } = {},
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep02({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: source.compactCborHex,
        witnessSetCompactCbor: source.witnessSetCompactCborHex,
        publishCarriage: options.publishedCarriageUtxos === undefined,
        ...options,
        referenceScriptUtxo: references[1]!,
        certificateReferenceScriptUtxo: certificateReference,
      }),
    );
  /** Every descriptor window until the output scan closes canonical. */
  const step03Loop = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    onWindow?: (
      window: {
        readonly result: {
          readonly terminal: boolean;
          readonly nextThreadOutRef: string;
        };
        readonly measurement: Measurement;
      },
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let windows = 0;
    for (;;) {
      const scan = await captured(() =>
        submitOutputReferenceScriptDecodingStep03({
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef: current,
          evidence,
          referenceScriptUtxo: references[2]!,
        }),
      );
      onWindow?.(scan, windows);
      windows += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.terminal) break;
    }
    return { threadOutRef: current, windows };
  };
  const step04 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    compactCborHex: string,
    opened: {
      readonly carriageUtxos: readonly UTxO[];
      readonly certificateUtxo?: UTxO;
    },
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep04({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        nativeTxCompactCbor: compactCborHex,
        publishedCarriageUtxos: opened.carriageUtxos,
        certificateUtxo: opened.certificateUtxo,
        referenceScriptUtxo: references[3]!,
      }),
    );
  const step05 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep05({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[4]!,
      }),
    );
  /** Every scan transaction until the native scan closes. */
  const step05Loop = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    onScan?: (
      captured: Awaited<ReturnType<typeof step05>>,
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let scans = 0;
    for (;;) {
      const scan = await step05(current, evidence);
      onScan?.(scan, scans);
      scans += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.closed) break;
    }
    return { threadOutRef: current, scans };
  };
  const step05Raw = async (
    threadOutRef: string,
    args: OutputReferenceScriptScanArgs,
    nextState: OutputReferenceScriptScanState,
    nextStepIndex: 4 | 5,
  ) =>
    await submitOutputReferenceScriptDecodingStep05Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      args,
      nextState,
      nextStepIndex,
      referenceScriptUtxo: references[4]!,
    });
  const step06 = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
  ) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingStep06({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[5]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step06Raw = async (threadOutRef: string) =>
    await submitOutputReferenceScriptDecodingStep06Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      referenceScriptUtxo: references[5]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await captured(() =>
      submitOutputReferenceScriptDecodingCancel({
        lucid,
        contracts,
        categoryId,
        signer,
        threadOutRef,
        referenceScriptUtxo: references[stepIndex]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async (fraudulentHeaderHash: string) => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    return await captured(() =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "outputReferenceScriptDecoding",
        fraudulentHeaderHash,
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
  };
  /**
   * A genuine certified field-2 carriage of another transaction: same chunk
   * bytes, but a certificate anchored to the other transaction id.
   */
  const certifyForeignField = async (
    other: AcceptedSubject,
    outputFieldPreimageHex: string,
  ) => {
    const items = (
      await import("@al-ft/midgard-core")
    ).decodeMidgardFieldPreimage(Buffer.from(outputFieldPreimageHex, "hex"));
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: 2,
      anchorTxId: other.nativeTxId,
      nativeTxCompactCbor: other.compactCborHex,
      itemCbors: items,
      owner: signer.paymentKeyHash,
      publish: true,
      label: "output-reference foreign field 2",
    });
    signer.selectWallet(lucid);
    const carriageUtxos = await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "output-reference foreign field 2",
    });
    if (planned.plan.tier !== "Certified")
      throw new Error("foreign field is not certified");
    const { certificateUtxo } = await certifyFaultProofFieldCarriage({
      lucid,
      network,
      signer,
      planned,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos: carriageUtxos,
      compactCbor: other.compactCborHex,
      witnessSetCompactCbor: other.witnessSetCompactCborHex,
    });
    return { planned, carriageUtxos, certificateUtxo };
  };
  /** Step 02 over a caller-planned carriage (see `certifyForeignField`). */
  const step02Raw = async (
    threadOutRef: string,
    evidence: OutputReferenceScriptDecodingEvidence,
    carriage: Awaited<ReturnType<typeof certifyForeignField>>,
  ) =>
    await submitOutputReferenceScriptDecodingStep02Raw({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef,
      evidence,
      ...carriage,
      referenceScriptUtxo: references[1]!,
    });

  return {
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03Loop,
    step04,
    step05,
    step05Loop,
    step05Raw,
    step06,
    step06Raw,
    cancel,
    remove,
    certifyForeignField,
  };
};
export type OutputReferenceStages = ReturnType<
  typeof makeOutputReferenceStages
>;
