import { MIDGARD_VALIDATION_MACHINE_VERSION } from "@al-ft/midgard-core/consensus-profile";
import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationContext,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
  MidgardValidationPhase,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const retainedEndpointsMatchDescriptor = (member: {
  readonly eventKey: SDK.EventKey;
  readonly value: SDK.ValidationTraceDescriptor;
  readonly witnesses: readonly SDK.DaPayloadEntry[];
}): boolean => {
  const endpoints = SDK.readRetainedValidationEndpoints({
    entries: member.witnesses,
    eventKey: member.eventKey,
    descriptor: member.value,
  });
  return (
    endpoints.initial.trace_proof.state_index === 0n &&
    endpoints.terminal.trace_proof.state_index === member.value.step_count
  );
};

/** A two-state trace for payload structure tests, not semantic replay. */
export const fixtureValidationTrace = (
  eventKey: SDK.EventKey,
  transactionId: string,
  verdict: "accepted" | "rejected" = "accepted",
  rejectionCodeHash = Buffer.alloc(32),
) => {
  const key = Data.to(eventKey, SDK.EventKey);
  const witnessCbor = Buffer.from("80", "hex");
  const state: MidgardValidationMachineState = {
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    eventKeyHash: hashMidgardValidationEventKey(Buffer.from(key, "hex")),
    transactionId: Buffer.from(transactionId, "hex"),
    transactionCommitment: Buffer.alloc(32),
    validationContextHash: hashMidgardValidationContext(witnessCbor),
    sourceKind: "ForcedTransactionEventKey" in eventKey ? "forced" : "normal",
    priorLedgerRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
    phase: "terminal",
    programCounter: 0,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "terminal",
      programCounter: 0,
      witnessCbor,
    }),
    executionCpu: 0n,
    executionMemory: 0n,
    verdict,
    rejectionCodeHash,
    ledgerDeltaRoot: Buffer.alloc(32),
  };
  const initialState: MidgardValidationMachineState = {
    ...state,
    phase: "canonicalDecode",
    verdict: "pending",
    rejectionCodeHash: Buffer.alloc(32),
    workRoot: hashMidgardValidationWorkWitness({
      phase: "canonicalDecode",
      programCounter: 0,
      witnessCbor,
    }),
  };
  const states = [initialState, state];
  const tree = buildMidgardValidationTraceTree(
    states.map(hashMidgardValidationMachineState),
    verdict,
    rejectionCodeHash,
  );
  const descriptor = SDK.validationTraceDescriptorDataFromCore(tree.descriptor);
  const witnesses = (["initial", "terminal"] as const).map(
    (endpoint): SDK.DaPayloadEntry => [
      SDK.encodeRetainedValidationWitnessKey({
        event_key: eventKey,
        execution_index: SDK.retainedValidationEndpointCoordinate(
          descriptor.step_count,
          endpoint,
        ),
      }).toString("hex"),
      SDK.encodeRetainedValidationWitness({
        machine_state: SDK.validationMachineStateDataFromCore(
          endpoint === "initial" ? initialState : state,
        ),
        trace_proof: SDK.validationTraceProofDataFromCore(
          tree.proofs[endpoint === "initial" ? 0 : 1]!,
        ),
        phase:
          endpoint === "initial"
            ? -1n
            : BigInt(MidgardValidationPhase.terminal),
        program_counter: 0n,
        witness_cbor: witnessCbor.toString("hex"),
        auxiliary: "NoAuxiliaryWitness",
      }).toString("hex"),
    ],
  );
  return {
    entry: [
      key,
      Data.to(descriptor, SDK.ValidationTraceDescriptor),
    ] satisfies SDK.DaPayloadEntry,
    witnesses,
  };
};
