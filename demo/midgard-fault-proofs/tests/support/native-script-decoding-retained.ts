import {
  buildMidgardBoundedItem,
  buildMidgardLedgerOutputMaterial,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  commitMidgardScriptContextTxOut,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import { withPlaceholderWithdrawalBlock } from "../helpers/canonical-block-evidence-fixture.with-placeholder-withdrawal.js";
import { fixtureBlockSlot } from "./emulator/header-fixtures.js";
import { recordFixtureLedger } from "./fixture-ledger.js";
import { makeReferenceNativeTx as makeNativeTx } from "./no-reference-input-native.js";
import { payloadLedgerAt } from "./predecessor-ledger.js";
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
  sourceKind = 1,
}: {
  now?: number;
  operatorVkey?: string;
  sourceKind?: 0 | 1;
  index?: bigint;
  consumed?: boolean;
  item?: Buffer;
  direction?: 0 | 1;
  reasonName?:
    | "ResolvedReferenceScriptMalformed"
    | "ResolvedReferenceScriptNodeLimit"
    | "ResolvedReferenceScriptDepthLimit"
    | "InputSpentOutputNonCanonical";
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
  const baseline = await withPlaceholderWithdrawalBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key, value: plainOutput }],
    }),
  );
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
  // The script hash covers the language tag and the script bytes the item's
  // byte string carries, whatever the width of its length header.
  const [, scriptBytes] = decodeSingleCbor(item) as [bigint, Uint8Array];
  const referenceScriptHash = computeHash28(
    Buffer.concat([Buffer.from([item[1]!]), Buffer.from(scriptBytes)]),
  );
  // The TxOut summaries carry the reference script hash, as the on-chain
  // terminal descriptor derives them.
  const txOutSummary = (encoding: "cardano" | "midgard") => {
    const { root, cborLength, memory } = commitMidgardScriptContextTxOut(
      decodeMidgardTxOutput(plainOutput),
      encoding,
      referenceScriptHash.toString("hex"),
    );
    return { root: Buffer.from(root), cborLength, memory };
  };
  const outputMaterial = buildMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor: output,
    facts: {
      ...baseFacts,
      referenceScriptLanguage: item[1] === 0 ? 0 : 3,
      referenceScriptHash,
      referenceScriptTotalLength: item.length,
      referenceScriptItemCommitment: buildMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: 0,
        bytes: item,
      }).commitment,
      cardanoTxOut: txOutSummary("cardano"),
      midgardTxOut: txOutSummary("midgard"),
    },
  });
  // Canonical items use the canonical descriptor; malformed native items use the
  // descriptor computed here from the output bytes, independently of the
  // fault-proof derivation under test.
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
  recordFixtureLedger(ledger.root, [{ key, value: output }]);
  const predecessorHeader = {
    ...baseline.header,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    operatorVkey,
    startTime: BigInt(now),
    endTime: BigInt(now) + 1000n,
    blockSlot: fixtureBlockSlot(BigInt(now) + 1000n, 9n),
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
      validation_trace_witnesses: [],
    },
  } as SDK.DaPayload;
  const predecessorFixture = {
    ...baseline,
    header: predecessorHeader,
    headerHash: predecessorHash,
    payload: predecessorPayload,
    payloadEnvelopeCbor: await reencodeFixturePayload(predecessorPayload),
  };
  const native = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputCbors: sourceKind === 0 ? [key] : [],
      referenceInputCbors: sourceKind === 1 ? [key] : [],
      fee: 0n,
    }),
  );
  const proofSource = deriveMidgardForcedTxProofSource(native);
  const leaf = {
    tx_id: computeMidgardNativeTxId(native).toString("hex"),
    submitted_source: {
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
              reason: {
                [reasonName]: {
                  source_kind: BigInt(sourceKind),
                  input_index: index,
                },
              },
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
      encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(native),
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
    const priorSource = deriveMidgardForcedTxProofSource(priorNative);
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
          submitted_source: {
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
      encodeMidgardForcedTxCanonical(priorNative).toString("hex"),
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
  const counts = {
    ...base.reconstruction.payload.block_body.counts,
    forcedTransactionCount: BigInt(entries.length),
    totalEventCount: BigInt(entries.length),
    transitionStepCount: BigInt(entries.length),
    validationTraceCount: BigInt(entries.length),
  };
  const header = {
    ...base.header,
    blockSlot: fixtureBlockSlot(BigInt(now) + 61_000n, 10n),
    endTime: BigInt(now) + 61_000n,
    ...counts,
    prevHeaderHash: predecessorFixture.headerHash,
    prevUtxosRoot: predecessorFixture.header.utxosRoot,
    utxosRoot: finalRoot,
    forcedTransactionsRoot: root.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventRoot.root,
    // The header pins the validation-trace pair; the body keeps its traces.
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
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
      validation_trace_witnesses: [],
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
      ledgerAt: payloadLedgerAt(predecessorFixture.payloadEnvelopeCbor),
      currentEvidence: evidence,
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
