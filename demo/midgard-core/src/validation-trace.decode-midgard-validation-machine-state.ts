import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
  encodeCborArrayRaw,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
} from "./consensus-profile.js";
import {
  encodeMidgardMpfProofDescriptor,
  type MidgardMpfProofDescriptor,
} from "./mpf-proof-fold.js";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "./plutus-data-cbor.js";
import {
  buildMidgardValidationMerkleFrontier,
  commitMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
} from "./validation-merkle.js";
import {
  asBoundedUint,
  encodeMidgardValidationMachineState,
  exactCode,
  fail,
  hashDomain,
  LEDGER_DELTA_DOMAIN,
  type MidgardValidationMachineState,
  MidgardValidationPhase,
  type MidgardValidationPhaseName,
  type MidgardValidationTraceDescriptor,
  MidgardValidationVerdict,
  phaseNames,
  REJECTION_CODE_DOMAIN,
  sourceKindNames,
  STATE_HASH_DOMAIN,
  validateVerdictRejectionBinding,
  VALIDATION_CONTEXT_DOMAIN,
  verdictNames,
  WORK_WITNESS_DOMAIN,
  ZERO_HASH32,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

export const decodeMidgardValidationMachineState = (
  bytes: Uint8Array,
): MidgardValidationMachineState => {
  const fields = asArray(
    decodeSingleCbor(bytes),
    "validation_machine_state_v1",
  );
  if (fields.length !== 15) {
    return fail(
      "validation_machine_state_v1 must contain exactly 15 fields",
      `length=${fields.length.toString()}`,
    );
  }
  const machineVersion = asBoundedUint(fields[0], "state.machine_version", 255);
  if (machineVersion !== MIDGARD_VALIDATION_MACHINE_VERSION) {
    return fail(
      "Unsupported validation machine version",
      machineVersion.toString(),
    );
  }
  const executionCpu = asBigInt(fields[10], "state.execution_cpu");
  const executionMemory = asBigInt(fields[11], "state.execution_memory");
  if (executionCpu < 0n || executionMemory < 0n) {
    return fail("Validation execution units must be unsigned");
  }
  const verdict = exactCode(fields[12], "validation verdict", verdictNames);
  const rejectionCodeHash = ensureHash32(
    asBytes(fields[13], "state.rejection_code_hash"),
    "state.rejection_code_hash",
  );
  validateVerdictRejectionBinding(verdict, rejectionCodeHash, "state");
  return {
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    eventKeyHash: ensureHash32(
      asBytes(fields[1], "state.event_key_hash"),
      "state.event_key_hash",
    ),
    transactionId: ensureHash32(
      asBytes(fields[2], "state.transaction_id"),
      "state.transaction_id",
    ),
    transactionCommitment: ensureHash32(
      asBytes(fields[3], "state.transaction_commitment"),
      "state.transaction_commitment",
    ),
    validationContextHash: ensureHash32(
      asBytes(fields[4], "state.validation_context_hash"),
      "state.validation_context_hash",
    ),
    sourceKind: exactCode(fields[5], "validation source kind", sourceKindNames),
    priorLedgerRoot: ensureHash32(
      asBytes(fields[6], "state.prior_ledger_root"),
      "state.prior_ledger_root",
    ),
    phase: exactCode(fields[7], "validation phase", phaseNames),
    programCounter: asBoundedUint(
      fields[8],
      "state.program_counter",
      0xffff_ffff,
    ),
    workRoot: ensureHash32(
      asBytes(fields[9], "state.work_root"),
      "state.work_root",
    ),
    executionCpu,
    executionMemory,
    verdict,
    rejectionCodeHash,
    ledgerDeltaRoot: ensureHash32(
      asBytes(fields[14], "state.ledger_delta_root"),
      "state.ledger_delta_root",
    ),
  };
};

export const hashMidgardValidationMachineState = (
  state: MidgardValidationMachineState,
): Hash32 =>
  hashDomain(STATE_HASH_DOMAIN, encodeMidgardValidationMachineState(state));

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

export const hashMidgardValidationRejectionCode = (
  rejectCode: string,
): Hash32 => {
  if (!/^E_[A-Z0-9_]+$/u.test(rejectCode)) {
    return fail(
      "Validation rejection code must use the frozen E_[A-Z0-9_]+ form",
      rejectCode,
    );
  }
  return hashDomain(REJECTION_CODE_DOMAIN, Buffer.from(rejectCode, "ascii"));
};

/**
 * Commits the canonical witness consumed by one validation-machine
 * transition. The phase and program counter are inside the commitment so the
 * same bytes cannot be replayed as a different instruction.
 */
export const hashMidgardValidationWorkWitness = ({
  phase,
  programCounter,
  witnessCbor,
}: {
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly witnessCbor: Uint8Array;
}): Hash32 =>
  hashDomain(
    WORK_WITNESS_DOMAIN,
    encodeCborArrayRaw([
      encodeCbor(BigInt(MidgardValidationPhase[phase])),
      encodeCbor(
        BigInt(
          asBoundedUint(
            programCounter,
            "work_witness.program_counter",
            MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
          ),
        ),
      ),
      Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          encodeCbor(Buffer.from(witnessCbor)).toString("hex"),
        ),
        "hex",
      ),
    ]),
  );

export const hashMidgardValidationContext = (
  canonicalContextCbor: Uint8Array,
): Hash32 =>
  hashDomain(VALIDATION_CONTEXT_DOMAIN, Buffer.from(canonicalContextCbor));

export const hashMidgardValidationLedgerDeltaCbor = (
  canonicalLedgerDeltaCbor: Uint8Array,
): Hash32 =>
  hashDomain(LEDGER_DELTA_DOMAIN, Buffer.from(canonicalLedgerDeltaCbor));

const LEDGER_DELTA_OPERATION_DOMAIN = Buffer.from(
  "MidgardValidationLedgerDeltaOperationV1",
  "utf8",
);

export type MidgardValidationLedgerDeltaOperation =
  | {
      readonly type: "delete";
      readonly key: Uint8Array;
    }
  | {
      readonly type: "insert";
      readonly key: Uint8Array;
      readonly value: Uint8Array;
    };

export type MidgardValidationAuthenticatedLedgerDeltaOperation =
  MidgardValidationLedgerDeltaOperation & {
    readonly proofDescriptor: MidgardMpfProofDescriptor;
  };

export const hashMidgardValidationLedgerDeltaOperation = (
  operation: MidgardValidationAuthenticatedLedgerDeltaOperation,
): Hash32 =>
  hashDomain(
    LEDGER_DELTA_OPERATION_DOMAIN,
    Buffer.concat([
      encodeCbor(operation.type === "delete" ? 0n : 1n),
      encodeCbor(Buffer.from(operation.key)),
      encodeCbor(
        operation.type === "delete"
          ? Buffer.alloc(0)
          : Buffer.from(operation.value),
      ),
      encodeMidgardMpfProofDescriptor(operation.proofDescriptor),
    ]),
  );

export const buildMidgardValidationLedgerDeltaFrontier = (
  operations: readonly MidgardValidationAuthenticatedLedgerDeltaOperation[],
): MidgardValidationMerkleFrontier =>
  buildMidgardValidationMerkleFrontier(
    operations.map(hashMidgardValidationLedgerDeltaOperation),
  );

export const hashMidgardValidationLedgerDelta = (
  operations: readonly MidgardValidationAuthenticatedLedgerDeltaOperation[],
): Hash32 =>
  commitMidgardValidationMerkleFrontier(
    buildMidgardValidationLedgerDeltaFrontier(operations),
  );

export const MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH = ensureHash32(
  ZERO_HASH32,
  "no_rejection_code_hash",
);
