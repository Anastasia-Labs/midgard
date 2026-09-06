import { encodeCbor, MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import {
  decodeMidgardValidationMachineState,
  hashMidgardValidationContext,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MidgardValidationPhase,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

import {
  type DaPayloadEntry,
  decodeRetainedValidationWitness,
  decodeRetainedValidationWitnessKey,
} from "./da-payload.js";
import {
  type ValidationMachineState,
  validationTraceDescriptorCoreFromData,
  validationTraceProofCoreFromData,
} from "./fraud-proof/validation-dispute.js";
import {
  type EventKey,
  EventKey as EventKeySchema,
  type ValidationTraceDescriptor,
} from "./ledger-state.js";

const checkedStepCount = (count: bigint): bigint => {
  if (
    count < 0n ||
    count > BigInt(MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount)
  )
    throw new Error(
      "Retained validation step count is outside its exact integer domain",
    );
  return count;
};

export const retainedValidationStateCoordinate = (
  stepCount: bigint,
  stateIndex: bigint,
): bigint => {
  checkedStepCount(stepCount);
  if (stateIndex < 0n || stateIndex > stepCount)
    throw new Error("Retained validation state index is outside the trace");
  return stateIndex - stepCount - 1n;
};

export const retainedValidationEndpointCoordinate = (
  stepCount: bigint,
  endpoint: "initial" | "terminal",
): bigint => -checkedStepCount(stepCount) - (endpoint === "initial" ? 2n : 3n);

const phases = {
  CanonicalDecode: 0,
  CompactBinding: 1,
  StaticLedgerRules: 2,
  InputSets: 3,
  Signatures: 4,
  PhaseANativeScripts: 5,
  PhaseAScriptPreconditions: 6,
  ResolveInputs: 7,
  ScriptSources: 8,
  NativeScripts: 9,
  ScriptIntegrity: 10,
  Cek: 11,
  ValueAndMint: 12,
  LedgerDelta: 13,
  Terminal: 14,
} as const;
const coreState = (state: ValidationMachineState) =>
  decodeMidgardValidationMachineState(
    encodeCbor([
      state.machine_version,
      Buffer.from(state.event_key_hash, "hex"),
      Buffer.from(state.transaction_id, "hex"),
      Buffer.from(state.transaction_commitment, "hex"),
      Buffer.from(state.validation_context_hash, "hex"),
      state.source_kind === "Normal" ? 0 : 1,
      Buffer.from(state.prior_ledger_root, "hex"),
      phases[state.phase],
      state.program_counter,
      Buffer.from(state.work_root, "hex"),
      state.execution_cpu,
      state.execution_memory,
      state.verdict === "Pending" ? 0 : state.verdict === "Accepted" ? 1 : 2,
      Buffer.from(state.rejection_code_hash, "hex"),
      Buffer.from(state.ledger_delta_root, "hex"),
    ]),
  );

/** The caller authenticates the selected descriptor's counted-root membership.
 * Endpoint preimages, proofs, context and terminal work are verified here against
 * that descriptor; retained labels never substitute for trace membership. */
export const readRetainedValidationEndpoints = ({
  entries,
  eventKey,
  descriptor,
}: {
  readonly entries: readonly DaPayloadEntry[];
  readonly eventKey: EventKey;
  readonly descriptor: ValidationTraceDescriptor;
}) => {
  const target = Data.to(eventKey, EventKeySchema);
  const selected = new Map<
    bigint,
    ReturnType<typeof decodeRetainedValidationWitness>
  >();
  for (const [keyBytes, valueBytes] of entries) {
    const key = decodeRetainedValidationWitnessKey(
      Buffer.from(keyBytes, "hex"),
    );
    if (Data.to(key.event_key, EventKeySchema) !== target) continue;
    if (selected.has(key.execution_index))
      throw new Error("Duplicate retained validation coordinate");
    selected.set(
      key.execution_index,
      decodeRetainedValidationWitness(Buffer.from(valueBytes, "hex")),
    );
  }
  const coreDescriptor = validationTraceDescriptorCoreFromData(descriptor);
  const endpoint = (kind: "initial" | "terminal") => {
    const record = selected.get(
      retainedValidationEndpointCoordinate(descriptor.step_count, kind),
    );
    if (record === undefined)
      throw new Error(`Retained validation ${kind} endpoint is absent`);
    const state = coreState(record.machine_state);
    const proof = validationTraceProofCoreFromData(record.trace_proof);
    const expectedIndex = kind === "initial" ? 0n : descriptor.step_count;
    const expectedHash =
      kind === "initial"
        ? descriptor.initial_state_hash
        : descriptor.terminal_state_hash;
    if (
      record.trace_proof.state_index !== expectedIndex ||
      record.trace_proof.state_hash !== expectedHash ||
      !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
      !verifyMidgardValidationTraceProof({ descriptor: coreDescriptor, proof })
    ) {
      throw new Error(
        `Retained validation ${kind} endpoint does not open the selected operator trace`,
      );
    }
    if (kind === "initial") {
      if (
        record.phase !== -1n ||
        record.program_counter !== BigInt(state.programCounter) ||
        !hashMidgardValidationContext(
          Buffer.from(record.witness_cbor, "hex"),
        ).equals(state.validationContextHash)
      )
        throw new Error(
          "Retained validation context differs from the operator state",
        );
    } else if (
      record.phase !== BigInt(MidgardValidationPhase[state.phase]) ||
      record.program_counter !== BigInt(state.programCounter) ||
      !hashMidgardValidationWorkWitness({
        phase: state.phase,
        programCounter: state.programCounter,
        witnessCbor: Buffer.from(record.witness_cbor, "hex"),
      }).equals(state.workRoot)
    ) {
      throw new Error(
        "Retained validation terminal work differs from the operator state",
      );
    }
    return record;
  };
  return Object.freeze({
    initial: endpoint("initial"),
    terminal: endpoint("terminal"),
  });
};

/** Opens one operator state and its exact deterministic work witness. */
export const readRetainedValidationState = ({
  entries,
  eventKey,
  descriptor,
  stateIndex,
}: {
  readonly entries: readonly DaPayloadEntry[];
  readonly eventKey: EventKey;
  readonly descriptor: ValidationTraceDescriptor;
  readonly stateIndex: bigint;
}) => {
  const coordinate = retainedValidationStateCoordinate(
    descriptor.step_count,
    stateIndex,
  );
  const target = Data.to(eventKey, EventKeySchema);
  const matches = entries.filter(([keyBytes]) => {
    const key = decodeRetainedValidationWitnessKey(
      Buffer.from(keyBytes, "hex"),
    );
    return (
      key.execution_index === coordinate &&
      Data.to(key.event_key, EventKeySchema) === target
    );
  });
  if (matches.length !== 1)
    throw new Error(
      "Selected retained validation state is absent or duplicated",
    );
  const record = decodeRetainedValidationWitness(
    Buffer.from(matches[0]![1], "hex"),
  );
  const state = coreState(record.machine_state);
  const proof = validationTraceProofCoreFromData(record.trace_proof);
  if (
    record.trace_proof.state_index !== stateIndex ||
    !hashMidgardValidationMachineState(state).equals(proof.stateHash) ||
    !verifyMidgardValidationTraceProof({
      descriptor: validationTraceDescriptorCoreFromData(descriptor),
      proof,
    })
  ) {
    throw new Error(
      "Retained validation state does not open the selected operator trace",
    );
  }
  if (
    record.phase !== BigInt(MidgardValidationPhase[state.phase]) ||
    record.program_counter !== BigInt(state.programCounter) ||
    !hashMidgardValidationWorkWitness({
      phase: state.phase,
      programCounter: state.programCounter,
      witnessCbor: Buffer.from(record.witness_cbor, "hex"),
    }).equals(state.workRoot)
  ) {
    throw new Error("Retained validation work differs from the operator state");
  }
  return record;
};
