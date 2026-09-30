import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxProofCommitment,
  hashMidgardValidationContext,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { ensureHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { encodeValidationTerminalWitnessCbor } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildAcceptedTransitionFixture } from "./transition-trace-final-fixtures.build-accepted-transition-fixture.js";

export const buildAcceptedClaimTransitionFixture = async ({
  operatorVkey,
  now,
  honest = false,
  endTime,
  blockSlot,
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
  endTime?: bigint;
  blockSlot?: bigint;
}) => {
  const base = await buildAcceptedTransitionFixture({
    operatorVkey,
    now,
    honest,
  });
  if (
    !("InvalidOneStepTransition" in base.proof.fault) ||
    !(
      "L2TransactionTransition" in
      base.proof.fault.InvalidOneStepTransition.witness
    )
  )
    throw new Error("Expected accepted fixture");
  const opening =
    base.proof.fault.InvalidOneStepTransition.witness.L2TransactionTransition;
  const nativeSource = Data.from(
    opening.source_membership.value,
    SDK.L2TransactionSource,
  );
  const source = nativeSource.source;
  const header = {
    ...base.header,
    ...(endTime === undefined ? {} : { endTime }),
    ...(blockSlot === undefined ? {} : { blockSlot }),
  };
  const context = Buffer.from(
    Data.to([
      1n,
      Buffer.from("midgard-consensus-v1").toString("hex"),
      header.endTime,
      header.expectedNetworkId,
      header.minFeeA,
      header.minFeeB,
      header.blockSlot,
    ]),
    "hex",
  );
  const initialWitness = encodeCbor([
    Buffer.from(source.compact_cbor, "hex"),
    Buffer.from(source.witness_set_compact_cbor, "hex"),
    Buffer.from(source.field_preimage_lengths_cbor, "hex"),
    context,
    0n,
    0n,
    0n,
    -1n,
    0n,
  ]);
  const root = Buffer.from(
    honest ? opening.trace_proof.value.post_utxos_root : "cc".repeat(32),
    "hex",
  );
  const terminalWitness = encodeValidationTerminalWitnessCbor({
    verdict: "accepted",
    postLedgerRoot: root,
    ledgerDeltaFrontier: { count: 0, peaks: [] },
  });
  const initial: MidgardValidationMachineState = {
    machineVersion: 1,
    eventKeyHash: hashMidgardValidationEventKey(
      Buffer.from(
        Data.to(opening.trace_proof.value.event_key, SDK.EventKey),
        "hex",
      ),
    ),
    transactionId: ensureHash32(Buffer.from(nativeSource.tx_id, "hex"), "tx"),
    transactionCommitment: ensureHash32(
      computeMidgardNativeTxProofCommitment({
        compactCbor: Buffer.from(source.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          source.field_preimage_lengths_cbor,
          "hex",
        ),
      }),
      "source",
    ),
    validationContextHash: hashMidgardValidationContext(context),
    sourceKind: "normal",
    priorLedgerRoot: ensureHash32(
      Buffer.from(opening.trace_proof.value.pre_utxos_root, "hex"),
      "prior",
    ),
    phase: "canonicalDecode",
    programCounter: 0,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "canonicalDecode",
      programCounter: 0,
      witnessCbor: initialWitness,
    }),
    executionCpu: 0n,
    executionMemory: 0n,
    verdict: "pending",
    rejectionCodeHash: ensureHash32(Buffer.alloc(32), "reject"),
    ledgerDeltaRoot: ensureHash32(Buffer.alloc(32), "delta"),
  };
  const terminal: MidgardValidationMachineState = {
    ...initial,
    phase: "terminal",
    programCounter: 1,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "terminal",
      programCounter: 1,
      witnessCbor: terminalWitness,
    }),
    verdict: "accepted",
  };
  const tree = buildMidgardValidationTraceTree(
    [
      hashMidgardValidationMachineState(initial),
      hashMidgardValidationMachineState(terminal),
    ],
    "accepted",
  );
  const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
  const state = (
    value: MidgardValidationMachineState,
  ): SDK.ValidationMachineState => ({
    machine_version: 1n,
    event_key_hash: hex(value.eventKeyHash),
    transaction_id: hex(value.transactionId),
    transaction_commitment: hex(value.transactionCommitment),
    validation_context_hash: hex(value.validationContextHash),
    source_kind: "Normal",
    prior_ledger_root: hex(value.priorLedgerRoot),
    phase: value.phase === "terminal" ? "Terminal" : "CanonicalDecode",
    program_counter: BigInt(value.programCounter),
    work_root: hex(value.workRoot),
    execution_cpu: 0n,
    execution_memory: 0n,
    verdict: value.verdict === "accepted" ? "Accepted" : "Pending",
    rejection_code_hash: hex(value.rejectionCodeHash),
    ledger_delta_root: hex(value.ledgerDeltaRoot),
  });
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: 1n,
    machine_version: 1n,
    trace_root: hex(tree.descriptor.traceRoot),
    step_count: 1n,
    initial_state_hash: hex(tree.stateHashes[0]!),
    terminal_state_hash: hex(tree.stateHashes[1]!),
    verdict: "Accepted",
    rejection_code_hash: "00".repeat(32),
  };
  const descriptors = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    [
      {
        key: Buffer.from(
          Data.to(opening.trace_proof.value.event_key, SDK.EventKey),
          "hex",
        ),
        value: Buffer.from(
          Data.to(descriptor, SDK.ValidationTraceDescriptor),
          "hex",
        ),
      },
    ],
  );
  const claim: SDK.ValidationClaimWitness = {
    version: 1n,
    descriptor_membership: {
      domain: descriptors.domain,
      root: descriptors.root,
      phas_root: descriptors.phasRoot,
      count: descriptors.count,
      proof: [],
      key: opening.trace_proof.value.event_key,
      value: descriptor,
    },
    transition_step_membership: opening.trace_proof,
    event_to_step_membership: opening.event_to_step,
    source_membership: {
      NormalValidationSource: {
        membership: { ...opening.source_membership, value: nativeSource },
      },
    },
    validation_context_cbor: hex(context),
    initial_state: state(initial),
    terminal_state: state(terminal),
    initial_state_proof: {
      state_index: 0n,
      state_hash: hex(tree.stateHashes[0]!),
      siblings: tree.proofs[0]!.siblings.map(hex),
    },
    terminal_state_proof: {
      state_index: 1n,
      state_hash: hex(tree.stateHashes[1]!),
      siblings: tree.proofs[1]!.siblings.map(hex),
    },
  };
  const resultHeader = {
    ...header,
    validationTracesRoot: descriptors.root,
    validationTraceCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(resultHeader));
  const proof: SDK.TransitionFaultProof = {
    challenged_header_hash: headerHash,
    header: resultHeader,
    fault: {
      AcceptedTransactionTransitionMismatch: {
        witness: {
          claim,
          terminal_acceptance_witness_cbor: hex(terminalWitness),
        },
      },
    },
  };
  return { header: resultHeader, headerHash, proof };
};
