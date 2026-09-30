import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
import { buildAcceptedClaimTransitionFixture } from "./transition-trace-final-fixtures.js";

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
