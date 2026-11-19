import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
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
  type RejectionReason,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../../../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import { buildInvalidForcedTransactionNoOpWitness } from "../../../src/transition-trace/witnesses.build-omitted-due-l1-event-fault.js";
import { buildTransitionFaultProof } from "../../../src/transition-trace/witnesses.build-out-of-window-source-event-fault.js";
import {
  fixtureBlockSlot,
  h32,
  makeHeader,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "../emulator/header-fixtures.js";
import { makeNativeTx } from "../emulator/native-tx.js";
import { recordFixtureLedger } from "../fixture-ledger.js";
import {
  outputReferenceCbor,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./transaction-material.js";

type ForcedTransitionTraceFixtureInput = {
  readonly operatorVkey: string;
  readonly now: number;
  readonly headerDurationMs?: number;
  readonly fieldPreimageLengthMismatchIndex?: number;
  readonly fieldItemWidthIllegalCoordinate?: {
    readonly fieldIndex: number;
    readonly itemIndex: number;
  };
  readonly redeemerMalformedIndex?: number;
  /**
   * The forced transaction's field-8 items, replacing the one redeemer the
   * fixture otherwise derives; the rejection reason is unchanged.
   */
  readonly redeemers?: readonly Buffer[];
  /**
   * Commits the forced transaction ForcedTxValid with one Spend redeemer whose
   * data is exactly these bytes. Such a block carries no invalid-forced no-op
   * step, so the fixture has no transition fault proof.
   */
  readonly acceptedRedeemerDataHex?: string;
  /**
   * Fields of the forced transaction that replace the fixture's own, such as
   * its mint field or its witness scripts.
   */
  readonly nativeTx?: Partial<
    Omit<Parameters<typeof makeNativeTx>[0], "spendInputCbors" | "fee">
  >;
  /** The rejection the forced leaf commits, replacing the derived one. */
  readonly rejectionReason?: RejectionReason;
};

type ForcedTransitionTraceFixture = Awaited<
  ReturnType<typeof buildForcedTransitionTraceFixture>
>;

export function buildInvalidForcedTransitionTraceFixture(
  input: ForcedTransitionTraceFixtureInput & {
    readonly acceptedRedeemerDataHex: string;
  },
): Promise<Omit<ForcedTransitionTraceFixture, "proof">>;

export function buildInvalidForcedTransitionTraceFixture(
  input: ForcedTransitionTraceFixtureInput & {
    readonly acceptedRedeemerDataHex?: undefined;
  },
): Promise<
  Omit<ForcedTransitionTraceFixture, "proof"> & {
    readonly proof: NonNullable<ForcedTransitionTraceFixture["proof"]>;
  }
>;

export function buildInvalidForcedTransitionTraceFixture(
  input: ForcedTransitionTraceFixtureInput,
): Promise<ForcedTransitionTraceFixture> {
  return buildForcedTransitionTraceFixture(input);
}

const buildForcedTransitionTraceFixture = async ({
  operatorVkey,
  now,
  headerDurationMs,
  fieldPreimageLengthMismatchIndex,
  fieldItemWidthIllegalCoordinate,
  redeemerMalformedIndex,
  redeemers,
  acceptedRedeemerDataHex,
  nativeTx,
  rejectionReason,
}: ForcedTransitionTraceFixtureInput) => {
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
  recordFixtureLedger(finalUtxosRoot.root, [finalUtxo]);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputByte: "b2",
    witnessByte: "b8",
    ...(redeemers !== undefined
      ? { redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimage(redeemers) }
      : redeemerMalformedIndex === undefined &&
          acceptedRedeemerDataHex === undefined
        ? {}
        : {
            redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimage([
              encodeMidgardRedeemerWitnessItem({
                purpose: "Spend",
                index: BigInt(redeemerMalformedIndex ?? 0),
                redeemerCbor: Buffer.from(
                  acceptedRedeemerDataHex ?? "00",
                  "hex",
                ),
                executionUnits: { memory: 1n, steps: 2n },
              }),
            ]),
          }),
    ...nativeTx,
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
    verdict:
      acceptedRedeemerDataHex !== undefined
        ? ("ForcedTxValid" as const)
        : {
            ForcedTxInvalid: {
              reason:
                rejectionReason !== undefined
                  ? rejectionReason
                  : redeemerMalformedIndex !== undefined
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
                        ? {
                            PlutusExecutionFailed: {
                              purpose_kind: 0n,
                              purpose_index: 0n,
                            },
                          }
                        : {
                            FieldPreimageLengthMismatch: {
                              field_index: BigInt(
                                fieldPreimageLengthMismatchIndex,
                              ),
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
        verdict:
          acceptedRedeemerDataHex === undefined ? "Rejected" : "Accepted",
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
      : {
          endTime: BigInt(now + headerDurationMs),
          blockSlot: fixtureBlockSlot(BigInt(now + headerDurationMs)),
        }),
    utxosRoot: finalUtxosRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    ...counts,
    // The header pins the validation-trace pair; the body keeps its traces.
    validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    encodeDaPayload({
      version: DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(traceEntries),
        event_to_step: sortedDaEntries(eventToStepEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries(forcedPreimageEntries),
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
  return {
    header,
    headerHash,
    reconstruction,
    eventKey,
    forcedNativeTx,
    forcedTransaction,
    proof:
      acceptedRedeemerDataHex === undefined
        ? buildTransitionFaultProof({
            reconstruction,
            fault: invalidOneStepTransitionFault(
              await buildInvalidForcedTransactionNoOpWitness({
                reconstruction,
                stepIndex: 0n,
              }),
            ),
          })
        : undefined,
  };
};
