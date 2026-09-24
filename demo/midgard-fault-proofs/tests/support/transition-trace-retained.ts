import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { type Assets, Data } from "@lucid-evolution/lucid";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
import { buildAcceptedClaimTransitionFixture } from "./transition-trace-final-fixtures.js";

const sealDepositPayload = async (payload: SDK.DaPayload) => {
  const header = payload.block_body.header;
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const sealed = {
    ...payload,
    block_body: { ...payload.block_body, header_hash: headerHash },
  };
  return {
    payload: sealed,
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(sealed), {
      mode: "identity",
    }),
  };
};

type DepositEventInput =
  | { event: SDK.DepositEvent; eventCbor?: never }
  | { eventCbor: string; event?: never };

export type RetainedDepositEvent = DepositEventInput & {
  originalAssets: Assets;
  honest: boolean;
};

const retainedDepositEvent = (input: DepositEventInput) => {
  if (input.event !== undefined && input.eventCbor !== undefined)
    throw new Error(
      "Retained deposit fixture received competing event encodings",
    );
  const eventCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
    input.eventCbor ?? Data.to(input.event!, SDK.DepositEvent),
  );
  const event = Data.from(eventCbor, SDK.DepositEvent);
  return { event, infoCbor: plutusConstrFieldCbor(eventCbor, [1]) };
};

/** Build deposit transitions from actual event outputs and the retained prior ledger. */
export const depositEventsRetainedBlock = async (input: {
  operatorVkey: string;
  startTime: bigint;
  endTime: bigint;
  blockSlot: bigint;
  prevHeaderHash: string;
  prevUtxosRoot: string;
  priorLedger: readonly SDK.DaPayloadEntry[];
  events: readonly RetainedDepositEvent[];
}) => {
  const ledger = new Map(input.priorLedger);
  const buildLedger = () =>
    buildCanonicalBlockFixture({
      transactions: [],
      utxos: [...ledger].map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
      startTime: input.startTime,
      endTime: input.endTime,
      prevHeaderHash: input.prevHeaderHash,
      prevUtxosRoot: input.prevUtxosRoot,
    });
  let base = await buildLedger();
  if (base.header.utxosRoot !== input.prevUtxosRoot)
    throw new Error("Retained prior ledger differs from the predecessor root");
  const entries: Record<
    "deposits" | "event_to_step" | "transition_trace",
    SDK.DaPayloadEntry[]
  > = { deposits: [], event_to_step: [], transition_trace: [] };
  const events = input.events
    .map((item) => ({ ...item, ...retainedDepositEvent(item) }))
    .sort((a, b) =>
      Data.to(a.event.id, SDK.OutputReference).localeCompare(
        Data.to(b.event.id, SDK.OutputReference),
      ),
    );
  for (const [index, item] of events.entries()) {
    const { id, info } = item.event;
    const effect = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: "Custom",
      eventId: id,
      l2Address: info.l2_address,
      l2NetworkId: info.l2_network_id,
      l2DatumCbor:
        info.l2_datum === null
          ? null
          : Buffer.from(plutusConstrFieldCbor(item.infoCbor, [2, 0]), "hex"),
      originalAssets: {
        ...item.originalAssets,
        lovelace: item.originalAssets.lovelace! + (item.honest ? 0n : 1n),
      },
    });
    const operation = effect.operations[0];
    if (operation?.type !== "insert")
      throw new Error("Deposit fixture has no insertion");
    const key = encodeMidgardSpendInputItem({
      txId: Buffer.from(id.transactionId, "hex"),
      outputIndex: Number(id.outputIndex),
    }).toString("hex");
    if (ledger.has(key))
      throw new Error("Deposit fixture repeats an existing ledger output");
    const preRoot = base.header.utxosRoot;
    ledger.set(key, operation.outputCbor.toString("hex"));
    base = await buildLedger();
    const eventKey: SDK.EventKey = { DepositEventKey: { deposit_id: id } };
    const stepIndex = BigInt(index);
    entries.deposits.push([SDK.committedDepositKeyBytes(id), item.infoCbor]);
    entries.event_to_step.push([
      Data.to(eventKey, SDK.EventKey),
      Data.to(
        { step_index: stepIndex, phase: "Deposit" },
        SDK.EventToStepValue,
      ),
    ]);
    entries.transition_trace.push([
      Data.to(stepIndex),
      Data.to(
        {
          schema_version: 1n,
          step_index: stepIndex,
          event_key: eventKey,
          phase: "Deposit",
          pre_utxos_root: preRoot,
          post_utxos_root: base.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
  }
  for (const values of Object.values(entries))
    values.sort(([a], [b]) => a.localeCompare(b));
  const root = async (domain: SDK.RootDomain, values: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        values.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    ...base.payload.block_body.counts,
    depositCount: BigInt(events.length),
    totalEventCount: BigInt(events.length),
    transitionStepCount: BigInt(events.length),
  };
  const header = {
    ...base.header,
    ...counts,
    operatorVkey: input.operatorVkey,
    blockSlot: input.blockSlot,
    depositsRoot: await root(SDK.ROOT_DOMAINS.deposits, entries.deposits),
    eventToStepRoot: await root(
      SDK.ROOT_DOMAINS.eventToStep,
      entries.event_to_step,
    ),
    transitionTraceRoot: await root(
      SDK.ROOT_DOMAINS.transitionTrace,
      entries.transition_trace,
    ),
  };
  return sealDepositPayload({
    ...base.payload,
    block_body: { ...base.payload.block_body, ...entries, counts, header },
  });
};

export const transitionTraceDepositRetainedFixture = async (
  input: DepositEventInput & {
    operatorVkey: string;
    now: number;
    originalAssets: Assets;
    honest?: boolean;
  },
) => {
  const { operatorVkey, now, originalAssets, honest = false } = input;
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: BigInt(now),
    endTime: BigInt(now + 60_000),
    blockSlot: 9n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });
  const current = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: predecessor.header.endTime,
    endTime: BigInt(now + 120_000),
    blockSlot: 10n,
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    priorLedger: predecessor.payload.block_body.utxos,
    events: [{ ...input, originalAssets, honest }],
  });
  return { predecessor, current };
};

export const transitionTraceAcceptedRetainedFixture = async ({
  operatorVkey,
  now,
  honest = false,
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
}) => {
  const seal = async (payload: SDK.DaPayload) => {
    const headerHash = computeHash28(
      SDK.encodeHeaderCbor(payload.block_body.header),
    ).toString("hex");
    const sealed = {
      ...payload,
      block_body: { ...payload.block_body, header_hash: headerHash },
    };
    return {
      payload: sealed,
      header: sealed.block_body.header,
      headerHash,
      payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(sealed), {
        mode: "identity",
      }),
    };
  };
  const first = await buildCanonicalBlockFixture({
    transactions: [],
    startTime: BigInt(now),
    endTime: BigInt(now + 60000),
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  const predecessor = await seal({
    ...first.payload,
    block_body: {
      ...first.payload.block_body,
      header: { ...first.header, operatorVkey, blockSlot: 9n },
    },
  });
  const output = Buffer.from(
    "a200581d60" + "aa".repeat(28) + "01821a001e8480a0",
    "hex",
  );
  const transaction = buildFixtureTransaction({
    spendInputs: [],
    outputs: [output],
    fee: 0n,
    networkId: 0n,
  });
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(transaction.txId, "hex"),
    outputIndex: 0,
  });
  const claimed = honest
    ? output
    : Buffer.from("a200581d60" + "aa".repeat(28) + "01821a001e8481a0", "hex");
  const base = await buildCanonicalBlockFixture({
    transactions: [transaction],
    utxos: [{ key, value: claimed }],
    startTime: BigInt(now + 60000),
    endTime: BigInt(now + 120000),
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
  });
  const claimFixture = await buildAcceptedClaimTransitionFixture({
    operatorVkey,
    now: now + 60000,
    honest,
    endTime: BigInt(now + 120000),
    blockSlot: 10n,
  });
  if (!("AcceptedTransactionTransitionMismatch" in claimFixture.proof.fault))
    throw new Error("Missing accepted claim fixture");
  const witness =
    claimFixture.proof.fault.AcceptedTransactionTransitionMismatch.witness;
  const claim = witness.claim;
  if (
    !("NormalValidationSource" in claim.source_membership) ||
    Data.to(
      claim.source_membership.NormalValidationSource.membership.value,
      SDK.L2TransactionSource,
    ) !== Data.to(transaction.source, SDK.L2TransactionSource)
  )
    throw new Error(
      "Retained accepted source differs from canonical transaction",
    );
  const eventKey = claim.descriptor_membership.key;
  const step = {
    ...claim.transition_step_membership.value,
    post_utxos_root: base.header.utxosRoot,
  };
  const traces: SDK.DaPayloadEntry[] = [
    [Data.to(0n), Data.to(step, SDK.TransitionStep)],
  ];
  const descriptors: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to(claim.descriptor_membership.value, SDK.ValidationTraceDescriptor),
    ],
  ];
  const source = transaction.source.source;
  const initialWitness = encodeCbor([
    Buffer.from(source.compact_cbor, "hex"),
    Buffer.from(source.witness_set_compact_cbor, "hex"),
    Buffer.from(source.field_preimage_lengths_cbor, "hex"),
    Buffer.from(claim.validation_context_cbor, "hex"),
    0n,
    0n,
    0n,
    -1n,
    0n,
  ]).toString("hex");
  const retained: SDK.DaPayloadEntry[] = [];
  for (const terminal of [false, true]) {
    const state = terminal ? claim.terminal_state : claim.initial_state;
    const proof = terminal
      ? claim.terminal_state_proof
      : claim.initial_state_proof;
    const record = {
      phase: terminal ? 14n : 0n,
      program_counter: state.program_counter,
      machine_state: state,
      trace_proof: proof,
      witness_cbor: terminal
        ? witness.terminal_acceptance_witness_cbor
        : initialWitness,
      auxiliary: "NoAuxiliaryWitness" as const,
    };
    retained.push([
      SDK.encodeRetainedValidationWitnessKey({
        event_key: eventKey,
        execution_index: SDK.retainedValidationStateCoordinate(
          1n,
          terminal ? 1n : 0n,
        ),
      }).toString("hex"),
      SDK.encodeRetainedValidationWitness(record).toString("hex"),
    ]);
    retained.push([
      SDK.encodeRetainedValidationWitnessKey({
        event_key: eventKey,
        execution_index: SDK.retainedValidationEndpointCoordinate(
          1n,
          terminal ? "terminal" : "initial",
        ),
      }).toString("hex"),
      SDK.encodeRetainedValidationWitness({
        ...record,
        phase: terminal ? 14n : -1n,
        witness_cbor: terminal
          ? record.witness_cbor
          : claim.validation_context_cbor,
      }).toString("hex"),
    ]);
  }
  retained.sort(([a], [b]) => a.localeCompare(b));
  const root = async (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const header = {
    ...base.header,
    operatorVkey,
    blockSlot: 10n,
    transitionStepCount: 1n,
    transitionTraceRoot: await root(SDK.ROOT_DOMAINS.transitionTrace, traces),
    validationTracesRoot: await root(
      SDK.ROOT_DOMAINS.validationTraces,
      descriptors,
    ),
  };
  const current = await seal({
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      counts: { ...base.payload.block_body.counts, transitionStepCount: 1n },
      transition_trace: traces,
      validation_traces: descriptors,
      validation_trace_witnesses: retained,
    },
  });
  return { predecessor, current };
};

/** Structurally complete timing evidence; L1 admission supplies event authority. */
export const transitionTraceTimingRetainedFixture = async (input: {
  operatorVkey: string;
  now: number;
  payload: SDK.EventHistoryPayload;
  originalAssets: Assets;
  omitted: boolean;
}) => {
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    startTime: BigInt(input.now),
    endTime: BigInt(input.now + 60_000),
    blockSlot: 9n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });
  const current = await depositEventsRetainedBlock({
    operatorVkey: input.operatorVkey,
    startTime: predecessor.header.endTime,
    endTime: BigInt(input.now + 120_000),
    blockSlot: 10n,
    prevHeaderHash: predecessor.headerHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
    priorLedger: predecessor.payload.block_body.utxos,
    events:
      !input.omitted && "DepositPayload" in input.payload
        ? [
            {
              event: input.payload.DepositPayload.event,
              originalAssets: input.originalAssets,
              honest: true,
            },
          ]
        : [],
  });
  if (input.omitted || "DepositPayload" in input.payload)
    return { predecessor, current };
  const { event } = input.payload.WithdrawalPayload;
  // The committed verdict is independent of the submitted body. Timing proof
  // validates the source body and signature while preserving that distinction.
  const info: SDK.WithdrawalInfo = {
    ...event.info,
    validity: "IncorrectWithdrawalSignature",
  };
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: event.id },
  };
  const withdrawals: SDK.DaPayloadEntry[] = [
    [
      Data.to(event.id, SDK.OutputReference),
      SDK.committedWithdrawalValueBytes(info),
    ],
  ];
  const eventToStep: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase: "Withdrawal" }, SDK.EventToStepValue),
    ],
  ];
  const transitionTrace: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Withdrawal",
          pre_utxos_root: current.header.utxosRoot,
          post_utxos_root: current.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const root = async (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const counts = {
    ...current.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const header = {
    ...current.header,
    ...counts,
    withdrawalsRoot: await root(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
    eventToStepRoot: await root(SDK.ROOT_DOMAINS.eventToStep, eventToStep),
    transitionTraceRoot: await root(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTrace,
    ),
  };
  return {
    predecessor,
    current: await sealDepositPayload({
      ...current.payload,
      block_body: {
        ...current.payload.block_body,
        withdrawals,
        event_to_step: eventToStep,
        transition_trace: transitionTrace,
        header,
        counts,
      },
    }),
  };
};
