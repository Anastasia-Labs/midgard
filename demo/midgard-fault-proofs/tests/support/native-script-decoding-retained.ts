import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputMaterial,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeHash28,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import { makeReferenceNativeTx as makeNativeTx } from "./no-reference-input-native.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";

const provenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/no-reference-input-retained-da",
  grade: "security",
} as const;
export const nativeDecodingFixture = async ({
  index = 0n,
  consumed = false,
  item = Buffer.from("820043820400", "hex"),
  direction = 1,
  reasonName = "ResolvedReferenceScriptMalformed",
  now = 1_000_000,
  operatorVkey = "aa".repeat(28),
}: {
  now?: number;
  operatorVkey?: string;
  index?: bigint;
  consumed?: boolean;
  item?: Buffer;
  direction?: 0 | 1;
  reasonName?:
    | "ResolvedReferenceScriptMalformed"
    | "ResolvedReferenceScriptNodeLimit"
    | "ResolvedReferenceScriptDepthLimit";
} = {}) => {
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey,
    now: now + 1_000,
  });
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x55),
    outputIndex: 0,
  });
  const plainOutput = Buffer.from(
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
    "hex",
  );
  const output = Buffer.concat([
    Buffer.from("a3", "hex"),
    plainOutput.subarray(1),
    Buffer.from("03", "hex"),
    item,
  ]);
  const baseline = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [{ key, value: plainOutput }],
  });
  const baseDescriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: key,
    outputCbor: plainOutput,
  }).descriptor;
  const {
    version: _version,
    outputIndex: _index,
    totalLength: _length,
    itemCommitment: _commitment,
    ...baseFacts
  } = baseDescriptor;
  const outputMaterial = buildMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: output,
    facts: {
      ...baseFacts,
      referenceScriptLanguage: item[1] === 0 ? 0 : 3,
      referenceScriptHash: computeHash28(
        Buffer.concat([Buffer.from([item[1]!]), item.subarray(3)]),
      ),
      referenceScriptTotalLength: item.length,
      referenceScriptItemCommitment: buildMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: 0,
        bytes: item,
      }).commitment,
    },
  });
  // For canonical items derive every descriptor fact; malformed subjects retain
  // the operator's committed descriptor instead of declaring it correct.
  let descriptor: Buffer;
  try {
    descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key,
      outputCbor: output,
    }).descriptorCbor;
  } catch {
    descriptor = outputMaterial.descriptorCbor;
  }
  const ledger = await keyValuePhasRootWithCount([{ key, value: descriptor }]);
  const z = "00".repeat(32);
  const retained: SDK.RetainedValidationWitness = {
    machine_state: {
      machine_version: 1n,
      event_key_hash: z,
      transaction_id: "55".repeat(32),
      transaction_commitment: z,
      validation_context_hash: z,
      source_kind: "Normal",
      prior_ledger_root: ledger.root,
      phase: "ResolveInputs",
      program_counter: 0n,
      work_root: z,
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: z,
      ledger_delta_root: z,
    },
    trace_proof: { state_index: 0n, state_hash: z, siblings: [] },
    phase: 7n,
    program_counter: 0n,
    witness_cbor: "80",
    auxiliary: {
      ScheduledLedgerMembershipWitness: {
        source_kind: 1n,
        key: key.toString("hex"),
        next_schedule_hash: z,
        value: descriptor.toString("hex"),
        proof: await keyValuePhasProof(ledger, key, descriptor),
        signer_proof: "NoSignerSetProof",
      },
    },
  };
  const retainedEntry: SDK.DaPayloadEntry = [
    SDK.encodeRetainedValidationWitnessKey({
      event_key: { L2TransactionEventKey: { tx_id: "55".repeat(32) } },
      execution_index: -1n,
    }).toString("hex"),
    SDK.encodeRetainedValidationWitness(retained).toString("hex"),
  ];
  const predecessorHeader = {
    ...baseline.header,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    operatorVkey,
    startTime: BigInt(now),
    endTime: BigInt(now) + 1000n,
    blockSlot: 9n,
    utxosRoot: ledger.root,
  };
  const predecessorHash = computeHash28(
    SDK.encodeHeaderCbor(predecessorHeader),
  ).toString("hex");
  const predecessorPayload = {
    ...baseline.payload,
    block_body: {
      ...baseline.payload.block_body,
      header: predecessorHeader,
      header_hash: predecessorHash,
      utxos: [[key.toString("hex"), output.toString("hex")]],
      validation_trace_witnesses: [retainedEntry],
    },
  } as SDK.DaPayload;
  const predecessorFixture = {
    ...baseline,
    header: predecessorHeader,
    headerHash: predecessorHash,
    payload: predecessorPayload,
    payloadEnvelopeCbor: await reencodeFixturePayload(predecessorPayload),
  };
  const native = adjudicateMidgardNativeTxFullValidity(
    makeNativeTx({ spendInputCbors: [], referenceInputCbors: [key], fee: 0n }),
    direction === 1 ? "TxIsInvalid" : "TxIsValid",
  );
  const proofSource = deriveMidgardNativeTxProofSource(native);
  const leaf = {
    tx_id: computeMidgardNativeTxId(native).toString("hex"),
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict:
      direction === 0
        ? "ForcedTxValid"
        : {
            ForcedTxInvalid: {
              reason: { [reasonName]: { source_kind: 1n, input_index: index } },
            },
          },
  } as const;
  const sourceKey = Data.to(
    base.eventKey.ForcedTransactionEventKey.tx_order_id,
    SDK.OutputReference,
  );
  const sourceValue = Data.to(
    leaf as SDK.ForcedInclusionTxV1,
    SDK.ForcedInclusionTxV1,
  );
  const entries: SDK.DaPayloadEntry[] = [[sourceKey, sourceValue]];
  const preimages: SDK.DaPayloadEntry[] = [
    [
      sourceKey,
      encodeMidgardNativeTxCanonical(
        adjudicateMidgardNativeTxFullValidity(native, "TxIsValid"),
      ).toString("hex"),
    ],
  ];
  const finalRoot = consumed
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : predecessorFixture.header.utxosRoot;
  const targetIndex = consumed ? 1n : 0n;
  const step = {
    ...base.reconstruction.transitionTrace[0]!.value,
    step_index: targetIndex,
    pre_utxos_root: finalRoot,
    post_utxos_root: finalRoot,
  };
  const traces: SDK.DaPayloadEntry[] = [
    [Data.to(targetIndex), Data.to(step, SDK.TransitionStep)],
  ];
  const events: SDK.DaPayloadEntry[] = [
    [
      Data.to(base.eventKey, SDK.EventKey),
      Data.to(
        { step_index: targetIndex, phase: "ForcedTransaction" },
        SDK.EventToStepValue,
      ),
    ],
  ];
  const validations = [
    ...base.reconstruction.payload.block_body.validation_traces,
  ];
  if (consumed) {
    const priorNative = makeNativeTx({ spendInputCbors: [key], fee: 1n });
    const priorSource = deriveMidgardNativeTxProofSource(priorNative);
    const priorKey = { transactionId: "aa".repeat(32), outputIndex: 0n };
    const priorEvent: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: priorKey },
    };
    const priorKeyCbor = Data.to(priorKey, SDK.OutputReference);
    entries.unshift([
      priorKeyCbor,
      Data.to(
        {
          tx_id: computeMidgardNativeTxId(priorNative).toString("hex"),
          source: {
            compact_cbor: priorSource.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              priorSource.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              priorSource.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: "ForcedTxValid",
        },
        SDK.ForcedInclusionTxV1,
      ),
    ]);
    preimages.unshift([
      priorKeyCbor,
      encodeMidgardNativeTxCanonical(priorNative).toString("hex"),
    ]);
    traces.unshift([
      Data.to(0n),
      Data.to(
        {
          ...step,
          step_index: 0n,
          event_key: priorEvent,
          pre_utxos_root: predecessorFixture.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
    events.unshift([
      Data.to(priorEvent, SDK.EventKey),
      Data.to(
        { step_index: 0n, phase: "ForcedTransaction" },
        SDK.EventToStepValue,
      ),
    ]);
    const descriptor = Data.from(
      validations[0]![1],
      SDK.ValidationTraceDescriptor,
    );
    validations.unshift([
      Data.to(priorEvent, SDK.EventKey),
      Data.to(
        { ...descriptor, verdict: "Accepted" },
        SDK.ValidationTraceDescriptor,
      ),
    ]);
  }
  const rootFor = async (domain: SDK.RootDomain, rows: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      rows.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const root = await rootFor(SDK.ROOT_DOMAINS.forcedTransactionsV1, entries);
  const traceRoot = await rootFor(SDK.ROOT_DOMAINS.transitionTrace, traces);
  const eventRoot = await rootFor(SDK.ROOT_DOMAINS.eventToStep, events);
  const validationRoot = await rootFor(
    SDK.ROOT_DOMAINS.validationTraces,
    validations,
  );
  const counts = {
    ...base.reconstruction.payload.block_body.counts,
    forcedTransactionCount: BigInt(entries.length),
    totalEventCount: BigInt(entries.length),
    transitionStepCount: BigInt(entries.length),
    validationTraceCount: BigInt(entries.length),
  };
  const header = {
    ...base.header,
    blockSlot: 10n,
    endTime: BigInt(now) + 61_000n,
    ...counts,
    prevHeaderHash: predecessorFixture.headerHash,
    prevUtxosRoot: predecessorFixture.header.utxosRoot,
    utxosRoot: finalRoot,
    forcedTransactionsRoot: root.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventRoot.root,
    validationTracesRoot: validationRoot.root,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload = {
    ...base.reconstruction.payload,
    block_body: {
      ...base.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      utxos: consumed ? [] : predecessorFixture.payload.block_body.utxos,
      validation_trace_witnesses: [retainedEntry],
      forced_transactions: entries,
      forced_transaction_preimages: preimages,
      transition_trace: traces,
      event_to_step: events,
      validation_traces: validations,
    },
  } as SDK.DaPayload;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture, {
      header,
      headerHash,
    }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      value: {
        observation: authenticatedHeaderObservation(predecessorFixture),
        payloadEnvelopeCborHex:
          predecessorFixture.payloadEnvelopeCbor.toString("hex"),
        daProvenance: provenance,
      },
      currentEvidence: evidence,
      minimumConfirmationDepth: 1,
    }),
  };
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture),
    payloadEnvelopeCbor: predecessorFixture.payloadEnvelopeCbor,
    daProvenance: provenance,
  });
  return {
    evidence,
    context,
    predecessor,
    descriptor,
    output,
    ledger,
    key,
    item,
  };
};
