import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  hashBlockHeader,
  Header,
  invalidOneStepTransitionFault,
  OutputReference,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  buildInvalidForcedTransactionNoOpWitness,
  buildTransitionFaultProof,
  keyValuePhasRootWithCount,
  reconstructDaPayload,
} from "../../src/index.js";
import {
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.build-non-existent-input-fixture.js";
import { outputReferenceCbor } from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  h32,
  makeHeader,
  makeNativeTx,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

export const buildInvalidForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
  headerDurationMs,
  fieldPreimageLengthMismatchIndex,
  fieldItemWidthIllegalCoordinate,
  redeemerMalformedIndex,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly headerDurationMs?: number;
  readonly fieldPreimageLengthMismatchIndex?: number;
  readonly fieldItemWidthIllegalCoordinate?: {
    readonly fieldIndex: number;
    readonly itemIndex: number;
  };
  readonly redeemerMalformedIndex?: number;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const finalDescriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalUtxosRoot = await keyValuePhasRootWithCount([
    {
      key: Buffer.from(finalUtxo[0], "hex"),
      value: finalDescriptor,
    },
  ]);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputByte: "b2",
    witnessByte: "b8",
    ...(redeemerMalformedIndex === undefined
      ? {}
      : {
          redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimage([
            encodeMidgardRedeemerWitnessItem({
              purpose: "Spend",
              index: BigInt(redeemerMalformedIndex),
              redeemerCbor: Buffer.from("00", "hex"),
              executionUnits: { memory: 1n, steps: 2n },
            }),
          ]),
        }),
  });
  const forcedCanonicalCbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(forcedNativeTx),
  );
  // DA and the verdict-bearing leaf commit the same immutable submission.
  const forcedSource = deriveMidgardForcedTxProofSource(
    materializeMidgardForcedTxFromCanonical(forcedNativeTx),
  );
  const forcedTransaction = {
    tx_id: computeMidgardNativeTxId(forcedNativeTx).toString("hex"),
    submitted_source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason:
          redeemerMalformedIndex !== undefined
            ? {
                RedeemerMalformed: {
                  redeemer_index: BigInt(redeemerMalformedIndex),
                },
              }
            : fieldItemWidthIllegalCoordinate !== undefined
              ? {
                  FieldItemWidthIllegal: {
                    field_index: BigInt(
                      fieldItemWidthIllegalCoordinate.fieldIndex,
                    ),
                    item_index: BigInt(
                      fieldItemWidthIllegalCoordinate.itemIndex,
                    ),
                  },
                }
              : fieldPreimageLengthMismatchIndex === undefined
                ? { PlutusExecutionFailed: { execution_index: 0n } }
                : {
                    FieldPreimageLengthMismatch: {
                      field_index: BigInt(fieldPreimageLengthMismatchIndex),
                    },
                  },
      },
    },
  };
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction",
    pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
    post_utxos_root: finalUtxosRoot.root,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction",
  };
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: forcedTransaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const forcedPreimageEntries = [
    transitionTraceRawEntry(
      forcedEntries[0]![0],
      forcedCanonicalCbor.toString("hex"),
    ),
  ];
  const validationTraceEntries = [
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
  const traceEntries = [
    transitionTraceDaEntry({
      key: step.step_index,
      keySchema: Data.Integer() as never,
      value: step,
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventToStepEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: eventToStepValue,
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const forcedRoot = await buildCountedRoot(
    ROOT_DOMAINS.forcedTransactionsV1,
    forcedEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const traceRoot = await buildCountedRoot(
    ROOT_DOMAINS.transitionTrace,
    traceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const eventToStepRoot = await buildCountedRoot(
    ROOT_DOMAINS.eventToStep,
    eventToStepEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const validationTracesRoot = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    validationTraceEntries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header: Header = {
    ...makeHeader(operatorVkey, now),
    ...(headerDurationMs === undefined
      ? {}
      : { endTime: BigInt(now + headerDurationMs) }),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
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
        transition_trace: sortedDaEntries(traceEntries),
        event_to_step: sortedDaEntries(eventToStepEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries(forcedPreimageEntries),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationTraceEntries),
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
  const fault = invalidOneStepTransitionFault(
    await buildInvalidForcedTransactionNoOpWitness({
      reconstruction,
      stepIndex: 0n,
    }),
  );
  return {
    header,
    headerHash,
    reconstruction,
    eventKey,
    forcedNativeTx,
    forcedTransaction,
    proof: buildTransitionFaultProof({ reconstruction, fault }),
  };
};
