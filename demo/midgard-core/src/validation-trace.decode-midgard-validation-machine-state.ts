import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
} from "./consensus-profile.js";
import {
  asBoundedUint,
  exactCode,
  fail,
  hashDomain,
  MIDGARD_VALIDATION_MACHINE_STATE_FIELDS,
  type MidgardValidationMachineState,
  type MidgardValidationMachineStateFieldName,
  type MidgardValidationTraceDescriptor,
  MidgardValidationVerdict,
  phaseNames,
  sourceKindNames,
  validateVerdictRejectionBinding,
  VALIDATION_CONTEXT_DOMAIN,
  verdictNames,
  ZERO_HASH32,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

export const decodeMidgardValidationMachineState = (
  bytes: Uint8Array,
): MidgardValidationMachineState => {
  const fields = asArray(
    decodeSingleCbor(bytes),
    "validation_machine_state_v1",
  );
  const fieldCount = MIDGARD_VALIDATION_MACHINE_STATE_FIELDS.length;
  if (fields.length !== fieldCount) {
    return fail(
      `validation_machine_state_v1 must contain exactly ${fieldCount.toString()} fields`,
      `length=${fields.length.toString()}`,
    );
  }
  const field = (name: MidgardValidationMachineStateFieldName): unknown =>
    fields[MIDGARD_VALIDATION_MACHINE_STATE_FIELDS.indexOf(name)];
  const hash = (
    name: MidgardValidationMachineStateFieldName,
    label: string,
  ): Hash32 => ensureHash32(asBytes(field(name), label), label);
  const machineVersion = asBoundedUint(
    field("machineVersion"),
    "state.machine_version",
    255,
  );
  if (machineVersion !== MIDGARD_VALIDATION_MACHINE_VERSION) {
    return fail(
      "Unsupported validation machine version",
      machineVersion.toString(),
    );
  }
  const executionCpu = asBigInt(field("executionCpu"), "state.execution_cpu");
  const executionMemory = asBigInt(
    field("executionMemory"),
    "state.execution_memory",
  );
  if (executionCpu < 0n || executionMemory < 0n) {
    return fail("Validation execution units must be unsigned");
  }
  const verdict = exactCode(
    field("verdict"),
    "validation verdict",
    verdictNames,
  );
  const rejectionCodeHash = hash(
    "rejectionCodeHash",
    "state.rejection_code_hash",
  );
  validateVerdictRejectionBinding(verdict, rejectionCodeHash, "state");
  return {
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    eventKeyHash: hash("eventKeyHash", "state.event_key_hash"),
    transactionId: hash("transactionId", "state.transaction_id"),
    transactionCommitment: hash(
      "transactionCommitment",
      "state.transaction_commitment",
    ),
    validationContextHash: hash(
      "validationContextHash",
      "state.validation_context_hash",
    ),
    sourceKind: exactCode(
      field("sourceKind"),
      "validation source kind",
      sourceKindNames,
    ),
    priorLedgerRoot: hash("priorLedgerRoot", "state.prior_ledger_root"),
    phase: exactCode(field("phase"), "validation phase", phaseNames),
    programCounter: asBoundedUint(
      field("programCounter"),
      "state.program_counter",
      0xffff_ffff,
    ),
    workRoot: hash("workRoot", "state.work_root"),
    executionCpu,
    executionMemory,
    verdict,
    rejectionCodeHash,
  };
};

export {
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "./validation-trace.hash-midgard-validation-step.js";

export const encodeMidgardValidationTraceDescriptor = (
  descriptor: MidgardValidationTraceDescriptor,
): Buffer => {
  validateVerdictRejectionBinding(
    descriptor.verdict,
    descriptor.rejectionCodeHash,
    "descriptor",
  );
  const stepCount = asBoundedUint(
    descriptor.stepCount,
    "descriptor.step_count",
    MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
  );
  return encodeCbor([
    BigInt(descriptor.schemaVersion),
    BigInt(descriptor.machineVersion),
    ensureHash32(descriptor.traceRoot, "descriptor.trace_root"),
    BigInt(stepCount),
    ensureHash32(descriptor.initialStateHash, "descriptor.initial_state_hash"),
    ensureHash32(
      descriptor.terminalStateHash,
      "descriptor.terminal_state_hash",
    ),
    BigInt(MidgardValidationVerdict[descriptor.verdict]),
    ensureHash32(
      descriptor.rejectionCodeHash,
      "descriptor.rejection_code_hash",
    ),
  ]);
};

export const decodeMidgardValidationTraceDescriptor = (
  bytes: Uint8Array,
): MidgardValidationTraceDescriptor => {
  const fields = asArray(
    decodeSingleCbor(bytes),
    "validation_trace_descriptor_v1",
  );
  if (fields.length !== 8) {
    return fail(
      "validation_trace_descriptor_v1 must contain exactly 8 fields",
      `length=${fields.length.toString()}`,
    );
  }
  const schemaVersion = asBoundedUint(
    fields[0],
    "descriptor.schema_version",
    255,
  );
  if (schemaVersion !== MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION) {
    return fail(
      "Unsupported validation trace descriptor version",
      schemaVersion.toString(),
    );
  }
  const machineVersion = asBoundedUint(
    fields[1],
    "descriptor.machine_version",
    255,
  );
  if (machineVersion !== MIDGARD_VALIDATION_MACHINE_VERSION) {
    return fail(
      "Unsupported validation machine version",
      machineVersion.toString(),
    );
  }
  const verdict = exactCode(fields[6], "validation verdict", verdictNames);
  if (verdict === "pending") {
    return fail("A validation trace descriptor verdict must be terminal");
  }
  const rejectionCodeHash = ensureHash32(
    asBytes(fields[7], "descriptor.rejection_code_hash"),
    "descriptor.rejection_code_hash",
  );
  validateVerdictRejectionBinding(verdict, rejectionCodeHash, "descriptor");
  return {
    schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    traceRoot: ensureHash32(
      asBytes(fields[2], "descriptor.trace_root"),
      "descriptor.trace_root",
    ),
    stepCount: asBoundedUint(
      fields[3],
      "descriptor.step_count",
      MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
    ),
    initialStateHash: ensureHash32(
      asBytes(fields[4], "descriptor.initial_state_hash"),
      "descriptor.initial_state_hash",
    ),
    terminalStateHash: ensureHash32(
      asBytes(fields[5], "descriptor.terminal_state_hash"),
      "descriptor.terminal_state_hash",
    ),
    verdict,
    rejectionCodeHash,
  };
};

export const hashMidgardValidationContext = (
  canonicalContextCbor: Uint8Array,
): Hash32 =>
  hashDomain(VALIDATION_CONTEXT_DOMAIN, Buffer.from(canonicalContextCbor));

export const MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH = ensureHash32(
  ZERO_HASH32,
  "no_rejection_code_hash",
);
