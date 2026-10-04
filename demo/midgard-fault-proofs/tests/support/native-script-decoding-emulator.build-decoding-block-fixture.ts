import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  encodeMidgardForcedTxCompact,
} from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { SubmitStep01TxInclusion } from "../../src/step-support.js";
import { nativeTxFromCoreCompact } from "../../src/step-support.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import {
  encodeData,
  reconstructDaPayload,
} from "../../src/transition-trace/reconstruct.js";
import {
  bufferEntries,
  type DecodingBlockFixture,
  type DecodingSubjectSource,
  decodingSubjectTransaction,
  entry,
  sorted,
} from "./native-script-decoding-emulator.build-decoding-ledger-fixture.js";

/**
 * The committed block the thread disputes: one event (the accused
 * transaction, normal or forced), one dense transition step carrying
 * `priorLedgerRoot` as its `pre_utxos_root`, and the matching event→step and
 * validation-trace leaves so `header_v1_is_valid` admits the header.
 */
export const buildDecodingBlockFixture = async ({
  operatorVkey,
  startTime,
  priorLedgerRoot,
  subject,
  decoyTransactionCount = 0,
  additionalTransactions = [],
  commitEventToStep = (entries) => entries,
  orderEvents = (events) => events,
}: {
  readonly operatorVkey: string;
  readonly startTime: bigint;
  readonly priorLedgerRoot: string;
  readonly subject: DecodingSubjectSource;
  /**
   * Extra committed L2 transactions, present only to give the header's
   * `transactions_root` more than one leaf: a single-leaf MPF proof has zero
   * steps, and the #545 published-chunk carriage has nothing to publish.
   */
  readonly decoyTransactionCount?: number;
  /** Caller-supplied normal transactions committed beside the subject. */
  readonly additionalTransactions?: readonly MidgardNativeTxFull[];
  /**
   * The `event_to_step` entries the block commits, derived from the honest
   * ones. The header and payload commit whatever this returns.
   */
  readonly commitEventToStep?: (
    entries: readonly SDK.DaPayloadEntry[],
  ) => readonly SDK.DaPayloadEntry[];
  /**
   * The order the trace steps the events in (subject first, then the L2
   * transactions). The event_to_step entries follow the same order.
   */
  readonly orderEvents?: <T>(events: readonly T[]) => readonly T[];
}): Promise<DecodingBlockFixture> => {
  const submitted = materializeMidgardForcedTxFromCanonical(subject.nativeTx);
  const canonicalCbor =
    subject.kind === "normal"
      ? encodeMidgardNativeTxCanonical(subject.nativeTx)
      : encodeMidgardForcedTxCanonical(submitted);
  const nativeTxId = computeMidgardNativeTxId(subject.nativeTx).toString("hex");
  const compactCbor =
    subject.kind === "normal"
      ? encodeMidgardNativeTxCompact(subject.nativeTx.compact)
      : encodeMidgardForcedTxCompact(submitted.compact);

  let transactions: SDK.DaPayloadEntry[] = [];
  let transactionPreimages: SDK.DaPayloadEntry[] = [];
  let forcedTransactions: SDK.DaPayloadEntry[] = [];
  let forcedTransactionPreimages: SDK.DaPayloadEntry[] = [];
  let eventKey: SDK.EventKey;
  const phase: SDK.TransitionPhase =
    subject.kind === "normal" ? "L2Transaction" : "ForcedTransaction";

  if (subject.kind === "normal") {
    // The DA payload and header commit the same exact canonical
    // `Data(L2TransactionSource)` leaf value.
    const source =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const sourceValue: SDK.L2TransactionSource = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    transactions = [
      [
        nativeTxId,
        encodeData(
          sourceValue,
          SDK.L2TransactionSourceSchema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [[nativeTxId, canonicalCbor.toString("hex")]];
    eventKey = { L2TransactionEventKey: { tx_id: nativeTxId } };
  } else {
    const source = deriveMidgardForcedTxProofSource(submitted);
    const leaf: SDK.ForcedInclusionTxV1 = {
      tx_id: nativeTxId,
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: subject.verdict,
    };
    const key = encodeData(subject.orderKey, SDK.OutputReference as never);
    forcedTransactions = [
      entry(key, encodeData(leaf, SDK.ForcedInclusionTxV1 as never)),
    ];
    forcedTransactionPreimages = [entry(key, canonicalCbor)];
    eventKey = { ForcedTransactionEventKey: { tx_order_id: subject.orderKey } };
  }

  const normalTransactionsForInclusion: MidgardNativeTxFull[] =
    subject.kind === "normal" ? [subject.nativeTx] : [];

  // Decoys are ordinary committed L2 transactions. They exist only so the
  // transactions trie holds more than one leaf.
  // The placeholder validation-trace descriptor is stamped by the COMMITTED
  // LEAF's verdict, not by what a replay would conclude — the #640
  // convention. A `ForcedTxInvalid` leaf therefore carries `Rejected`, so a
  // direction-B fixture never presents a block that accepts and rejects the
  // same event at once.
  const events: {
    readonly eventKey: SDK.EventKey;
    readonly phase: SDK.TransitionPhase;
    readonly verdict: "Accepted" | "Rejected";
  }[] = [
    {
      eventKey,
      phase,
      verdict:
        subject.kind === "forced" && subject.verdict !== "ForcedTxValid"
          ? "Rejected"
          : "Accepted",
    },
  ];
  const decoys = [
    ...additionalTransactions,
    ...Array.from({ length: decoyTransactionCount }, (_, index) =>
      decodingSubjectTransaction({ fee: BigInt(5_000 + index) }),
    ),
  ];
  for (const decoy of decoys) {
    const decoyCanonical = encodeMidgardNativeTxCanonical(decoy);
    const decoyId = computeMidgardNativeTxId(decoy).toString("hex");
    const decoySource =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(decoyCanonical);
    transactions = [
      ...transactions,
      [
        decoyId,
        encodeData(
          {
            tx_id: decoyId,
            source: {
              compact_cbor: decoySource.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                decoySource.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                decoySource.fieldPreimageLengthsCbor.toString("hex"),
            },
          } satisfies SDK.L2TransactionSource,
          SDK.L2TransactionSourceSchema as never,
        ).toString("hex"),
      ],
    ];
    transactionPreimages = [
      ...transactionPreimages,
      [decoyId, decoyCanonical.toString("hex")],
    ];
    normalTransactionsForInclusion.push(decoy);
    events.push({
      eventKey: { L2TransactionEventKey: { tx_id: decoyId } },
      phase: "L2Transaction",
      verdict: "Accepted",
    });
  }

  const transitionTrace: SDK.DaPayloadEntry[] = [];
  const eventToStep: SDK.DaPayloadEntry[] = [];
  const validationTraces: SDK.DaPayloadEntry[] = [];
  for (const [stepIndex, event] of orderEvents(events).entries()) {
    const step: SDK.TransitionStep = {
      schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
      step_index: BigInt(stepIndex),
      event_key: event.eventKey,
      phase: event.phase,
      pre_utxos_root: priorLedgerRoot,
      post_utxos_root: priorLedgerRoot,
    };
    transitionTrace.push(
      entry(
        encodeData(step.step_index, Data.Integer() as never),
        encodeData(step, SDK.TransitionStepSchema as never),
      ),
    );
    eventToStep.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            step_index: BigInt(stepIndex),
            phase: event.phase,
          } satisfies SDK.EventToStepValue,
          SDK.EventToStepValueSchema as never,
        ),
      ),
    );
    validationTraces.push(
      entry(
        encodeData(event.eventKey, SDK.EventKeySchema as never),
        encodeData(
          {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: "1a".repeat(32),
            step_count: 1n,
            initial_state_hash: "1b".repeat(32),
            terminal_state_hash: "1c".repeat(32),
            verdict: event.verdict,
            rejection_code_hash:
              event.verdict === "Rejected" ? "1d".repeat(32) : "00".repeat(32),
          } satisfies SDK.ValidationTraceDescriptor,
          SDK.ValidationTraceDescriptorSchema as never,
        ),
      ),
    );
  }

  const committedEventToStep = commitEventToStep(eventToStep);
  const utxoRoot = await keyValuePhasRootWithCount([]);
  const roots = {
    withdrawals: await buildCountedRoot(SDK.ROOT_DOMAINS.withdrawals, []),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      bufferEntries(forcedTransactions),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      bufferEntries(transactions),
    ),
    deposits: await buildCountedRoot(SDK.ROOT_DOMAINS.deposits, []),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      bufferEntries(transitionTrace),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      bufferEntries(committedEventToStep),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      bufferEntries(validationTraces),
    ),
  };
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: 0n,
    totalEventCount: BigInt(events.length),
    transitionStepCount: BigInt(events.length),
    validationTraceCount: BigInt(events.length),
  };
  const header: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: roots.transactions.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime,
    endTime: startTime + 1_000n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      deposits: [],
      transition_trace: sorted(transitionTrace),
      event_to_step: sorted(committedEventToStep),
      transaction_preimages: sorted(transactionPreimages),
      forced_transaction_preimages: sorted(forcedTransactionPreimages),
      cek_program_material: [],
      validation_traces: sorted(validationTraces),
      validation_trace_witnesses: [],
      counts,
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDK.encodeDaPayload(payload),
    { mode: "identity" },
  );
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: headerHash,
    committedHeader: header,
  });

  const txInclusions = new Map<string, SubmitStep01TxInclusion>();
  for (const nativeTx of normalTransactionsForInclusion) {
    const includedId = computeMidgardNativeTxId(nativeTx).toString("hex");
    const includedCompact = encodeMidgardNativeTxCompact(nativeTx.compact);
    const transactionEntry = transactions.find(([key]) => key === includedId);
    if (transactionEntry === undefined) {
      throw new Error(`Missing retained transaction source for ${includedId}`);
    }
    const includedSourceCbor = Buffer.from(transactionEntry[1], "hex");
    const membership = await keyValuePhasProof(
      { ...roots.transactions, root: roots.transactions.phasRoot },
      Buffer.from(includedId, "hex"),
      includedSourceCbor,
    );
    const proofCbor = Data.to(membership, SDK.Proof);
    txInclusions.set(includedId, {
      nativeTxId: includedId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: includedCompact.toString("hex"),
      l2TransactionSourceCbor: includedSourceCbor.toString("hex"),
      transactionsPhasRoot: roots.transactions.phasRoot,
      txMembershipProof: membership,
      txMembershipProofCbor: proofCbor,
    });
  }
  const txInclusion =
    subject.kind === "normal" ? (txInclusions.get(nativeTxId) ?? null) : null;

  return {
    header,
    headerHash,
    payloadEnvelopeCbor,
    reconstruction,
    nativeTxId,
    nativeTxCompactCbor: compactCbor.toString("hex"),
    txInclusion,
    txInclusions,
    forcedOrderKey: subject.kind === "forced" ? subject.orderKey : null,
    transactionsPhasRoot: roots.transactions.phasRoot,
  };
};
