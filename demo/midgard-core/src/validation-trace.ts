import { blake2b } from "@noble/hashes/blake2.js";

import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
  encodeCborArrayRaw,
} from "./codec/cbor.js";
import {
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "./codec/errors.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
} from "./consensus-profile-v1.js";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "./plutus-data-cbor.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  commitMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
} from "./validation-merkle.js";

const STATE_HASH_DOMAIN = Buffer.from(
  "MidgardValidationMachineStateV1",
  "utf8",
);
const TRACE_LEAF_DOMAIN = Buffer.from("MidgardValidationTraceLeafV1", "utf8");
const TRACE_BRANCH_DOMAIN = Buffer.from(
  "MidgardValidationTraceBranchV1",
  "utf8",
);
const REJECTION_CODE_DOMAIN = Buffer.from(
  "MidgardValidationRejectCodeV1",
  "utf8",
);
const WORK_WITNESS_DOMAIN = Buffer.from(
  "MidgardValidationWorkWitnessV1",
  "utf8",
);
const VALIDATION_CONTEXT_DOMAIN = Buffer.from(
  "MidgardValidationContextV1",
  "utf8",
);
const LEDGER_DELTA_DOMAIN = Buffer.from(
  "MidgardValidationLedgerDeltaV1",
  "utf8",
);

export const MidgardValidationPhase = {
  canonicalDecode: 0,
  compactBinding: 1,
  staticLedgerRules: 2,
  inputSets: 3,
  signatures: 4,
  phaseANativeScripts: 5,
  phaseAScriptPreconditions: 6,
  resolveInputs: 7,
  scriptSources: 8,
  nativeScripts: 9,
  scriptIntegrity: 10,
  cek: 11,
  valueAndMint: 12,
  ledgerDelta: 13,
  terminal: 14,
} as const;

export type MidgardValidationPhaseName = keyof typeof MidgardValidationPhase;

export const MidgardValidationSourceKind = {
  normal: 0,
  forced: 1,
} as const;

export type MidgardValidationSourceKindName =
  keyof typeof MidgardValidationSourceKind;

export const MidgardValidationVerdict = {
  pending: 0,
  accepted: 1,
  rejected: 2,
} as const;

export type MidgardValidationVerdictName =
  keyof typeof MidgardValidationVerdict;

const phaseNames = new Map<number, MidgardValidationPhaseName>(
  Object.entries(MidgardValidationPhase).map(([name, code]) => [
    code,
    name as MidgardValidationPhaseName,
  ]),
);

const verdictNames = new Map<number, MidgardValidationVerdictName>(
  Object.entries(MidgardValidationVerdict).map(([name, code]) => [
    code,
    name as MidgardValidationVerdictName,
  ]),
);

const sourceKindNames = new Map<number, MidgardValidationSourceKindName>(
  Object.entries(MidgardValidationSourceKind).map(([name, code]) => [
    code,
    name as MidgardValidationSourceKindName,
  ]),
);

export type MidgardValidationMachineStateV1 = {
  readonly machineVersion: typeof MIDGARD_VALIDATION_MACHINE_V1_VERSION;
  readonly eventKeyHash: Hash32;
  readonly transactionId: Hash32;
  /**
   * Hash of the canonical compact transaction plus compact witness set.
   * Every dynamic field is authenticated against the hashes reachable from
   * this commitment; the aggregate full transaction is never an L1 witness.
   */
  readonly transactionCommitment: Hash32;
  readonly validationContextHash: Hash32;
  readonly sourceKind: MidgardValidationSourceKindName;
  readonly priorLedgerRoot: Hash32;
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly workRoot: Hash32;
  readonly executionCpu: bigint;
  readonly executionMemory: bigint;
  readonly verdict: MidgardValidationVerdictName;
  readonly rejectionCodeHash: Hash32;
  readonly ledgerDeltaRoot: Hash32;
};

export type MidgardValidationTraceDescriptorV1 = {
  readonly schemaVersion: typeof MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION;
  readonly machineVersion: typeof MIDGARD_VALIDATION_MACHINE_V1_VERSION;
  readonly traceRoot: Hash32;
  /** Number of transitions. The trace contains stepCount + 1 state hashes. */
  readonly stepCount: number;
  readonly initialStateHash: Hash32;
  readonly terminalStateHash: Hash32;
  readonly verdict: Exclude<MidgardValidationVerdictName, "pending">;
  readonly rejectionCodeHash: Hash32;
};

export type MidgardValidationTraceProofV1 = {
  readonly stateIndex: number;
  readonly stateHash: Hash32;
  readonly siblings: readonly Hash32[];
};

export type MidgardValidationTraceTree = {
  readonly descriptor: MidgardValidationTraceDescriptorV1;
  readonly stateHashes: readonly Hash32[];
  readonly paddedLeafCount: number;
  readonly proofs: readonly MidgardValidationTraceProofV1[];
};

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    message,
    detail,
  );
};

const asBoundedUint = (
  value: unknown,
  fieldName: string,
  maximum: number,
): number => {
  const parsed = asBigInt(value, fieldName);
  if (parsed < 0n || parsed > BigInt(maximum)) {
    return fail(
      `${fieldName} is outside the compiled consensus bound`,
      `${parsed.toString()} > ${maximum.toString()}`,
    );
  }
  return Number(parsed);
};

const exactCode = <T extends string>(
  value: unknown,
  fieldName: string,
  names: ReadonlyMap<number, T>,
): T => {
  const parsed = asBoundedUint(value, fieldName, 255);
  const name = names.get(parsed);
  if (name === undefined) {
    return fail(`Unknown ${fieldName}`, parsed.toString());
  }
  return name;
};

const hashDomain = (domain: Uint8Array, bytes: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(bytes)]), {
      dkLen: 32,
    }),
    "domain_hash",
  );

export const encodeMidgardValidationMachineStateV1 = (
  state: MidgardValidationMachineStateV1,
): Buffer => {
  validateVerdictRejectionBinding(
    state.verdict,
    state.rejectionCodeHash,
    "state",
  );
  return encodeCbor([
    BigInt(state.machineVersion),
    ensureHash32(state.eventKeyHash, "state.event_key_hash"),
    ensureHash32(state.transactionId, "state.transaction_id"),
    ensureHash32(state.transactionCommitment, "state.transaction_commitment"),
    ensureHash32(state.validationContextHash, "state.validation_context_hash"),
    BigInt(MidgardValidationSourceKind[state.sourceKind]),
    ensureHash32(state.priorLedgerRoot, "state.prior_ledger_root"),
    BigInt(MidgardValidationPhase[state.phase]),
    BigInt(
      asBoundedUint(state.programCounter, "state.program_counter", 0xffff_ffff),
    ),
    ensureHash32(state.workRoot, "state.work_root"),
    state.executionCpu,
    state.executionMemory,
    BigInt(MidgardValidationVerdict[state.verdict]),
    ensureHash32(state.rejectionCodeHash, "state.rejection_code_hash"),
    ensureHash32(state.ledgerDeltaRoot, "state.ledger_delta_root"),
  ]);
};

export const decodeMidgardValidationMachineStateV1 = (
  bytes: Uint8Array,
): MidgardValidationMachineStateV1 => {
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
  if (machineVersion !== MIDGARD_VALIDATION_MACHINE_V1_VERSION) {
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
    machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
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

export const hashMidgardValidationMachineStateV1 = (
  state: MidgardValidationMachineStateV1,
): Hash32 =>
  hashDomain(STATE_HASH_DOMAIN, encodeMidgardValidationMachineStateV1(state));

export const encodeMidgardValidationTraceDescriptorV1 = (
  descriptor: MidgardValidationTraceDescriptorV1,
): Buffer => {
  validateVerdictRejectionBinding(
    descriptor.verdict,
    descriptor.rejectionCodeHash,
    "descriptor",
  );
  const stepCount = asBoundedUint(
    descriptor.stepCount,
    "descriptor.step_count",
    MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount,
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

export const decodeMidgardValidationTraceDescriptorV1 = (
  bytes: Uint8Array,
): MidgardValidationTraceDescriptorV1 => {
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
  if (schemaVersion !== MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION) {
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
  if (machineVersion !== MIDGARD_VALIDATION_MACHINE_V1_VERSION) {
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
    schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
    machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
    traceRoot: ensureHash32(
      asBytes(fields[2], "descriptor.trace_root"),
      "descriptor.trace_root",
    ),
    stepCount: asBoundedUint(
      fields[3],
      "descriptor.step_count",
      MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount,
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

const ZERO_HASH32 = Buffer.alloc(32);

const hash32IsZero = (value: Uint8Array): boolean =>
  Buffer.from(ensureHash32(value, "rejection_code_hash")).equals(ZERO_HASH32);

const validateVerdictRejectionBinding = (
  verdict: MidgardValidationVerdictName,
  rejectionCodeHash: Uint8Array,
  context: string,
): void => {
  const isZero = hash32IsZero(rejectionCodeHash);
  if (verdict === "rejected" ? isZero : !isZero) {
    return fail(
      `${context} verdict and rejection_code_hash are inconsistent`,
      `verdict=${verdict}`,
    );
  }
};

export const hashMidgardValidationRejectionCodeV1 = (
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
export const hashMidgardValidationWorkWitnessV1 = ({
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
            MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount,
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

export const hashMidgardValidationContextV1 = (
  canonicalContextCbor: Uint8Array,
): Hash32 =>
  hashDomain(VALIDATION_CONTEXT_DOMAIN, Buffer.from(canonicalContextCbor));

export const hashMidgardValidationLedgerDeltaCborV1 = (
  canonicalLedgerDeltaCbor: Uint8Array,
): Hash32 =>
  hashDomain(LEDGER_DELTA_DOMAIN, Buffer.from(canonicalLedgerDeltaCbor));

const LEDGER_DELTA_OPERATION_DOMAIN = Buffer.from(
  "MidgardValidationLedgerDeltaOperationV1",
  "utf8",
);

export type MidgardValidationLedgerDeltaOperationV1 =
  | {
      readonly type: "delete";
      readonly key: Uint8Array;
    }
  | {
      readonly type: "insert";
      readonly key: Uint8Array;
      readonly value: Uint8Array;
    };

export const hashMidgardValidationLedgerDeltaOperationV1 = (
  operation: MidgardValidationLedgerDeltaOperationV1,
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
    ]),
  );

export const buildMidgardValidationLedgerDeltaFrontierV1 = (
  operations: readonly MidgardValidationLedgerDeltaOperationV1[],
): MidgardValidationMerkleFrontierV1 =>
  buildMidgardValidationMerkleFrontierV1(
    operations.map(hashMidgardValidationLedgerDeltaOperationV1),
  );

export const hashMidgardValidationLedgerDeltaV1 = (
  operations: readonly MidgardValidationLedgerDeltaOperationV1[],
): Hash32 =>
  commitMidgardValidationMerkleFrontierV1(
    buildMidgardValidationLedgerDeltaFrontierV1(operations),
  );

export const MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH = ensureHash32(
  ZERO_HASH32,
  "no_rejection_code_hash",
);

const traceLeafHash = (stateHash: Uint8Array): Hash32 =>
  hashDomain(
    TRACE_LEAF_DOMAIN,
    ensureHash32(stateHash, "validation_trace.state_hash"),
  );

const traceBranchHash = (left: Uint8Array, right: Uint8Array): Hash32 =>
  hashDomain(
    TRACE_BRANCH_DOMAIN,
    Buffer.concat([
      ensureHash32(left, "validation_trace.left"),
      ensureHash32(right, "validation_trace.right"),
    ]),
  );

const nextPowerOfTwo = (value: number): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    return fail("Trace state count must be a positive safe integer");
  }
  let result = 1;
  while (result < value) {
    result *= 2;
    if (!Number.isSafeInteger(result)) {
      return fail("Trace leaf count exceeds the safe implementation bound");
    }
  }
  return result;
};

export const validationTraceDepthForStepCount = (stepCount: number): number => {
  const bounded = asBoundedUint(
    stepCount,
    "trace.step_count",
    MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount,
  );
  return Math.ceil(Math.log2(bounded + 1));
};

export const buildMidgardValidationTraceTree = (
  stateHashesInput: readonly Uint8Array[],
  verdict: Exclude<MidgardValidationVerdictName, "pending">,
  rejectionCodeHash: Uint8Array = MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
): MidgardValidationTraceTree => {
  validateVerdictRejectionBinding(verdict, rejectionCodeHash, "trace");
  if (stateHashesInput.length === 0) {
    return fail("Validation trace must contain at least its initial state");
  }
  const stepCount = stateHashesInput.length - 1;
  asBoundedUint(
    stepCount,
    "trace.step_count",
    MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount,
  );
  const stateHashes = stateHashesInput.map((stateHash, index) =>
    ensureHash32(stateHash, `trace.state_hashes[${index.toString()}]`),
  );
  const paddedLeafCount = nextPowerOfTwo(stateHashes.length);
  const paddedStates = [...stateHashes];
  while (paddedStates.length < paddedLeafCount) {
    paddedStates.push(stateHashes[stateHashes.length - 1]!);
  }

  const levels: Hash32[][] = [paddedStates.map(traceLeafHash)];
  while (levels[levels.length - 1]!.length > 1) {
    const previous = levels[levels.length - 1]!;
    const next: Hash32[] = [];
    for (let index = 0; index < previous.length; index += 2) {
      next.push(traceBranchHash(previous[index]!, previous[index + 1]!));
    }
    levels.push(next);
  }

  const proofs = stateHashes.map((stateHash, stateIndex) => {
    const siblings: Hash32[] = [];
    let index = stateIndex;
    for (let level = 0; level < levels.length - 1; level += 1) {
      siblings.push(levels[level]![index ^ 1]!);
      index = Math.floor(index / 2);
    }
    return { stateIndex, stateHash, siblings };
  });

  const traceRoot = levels[levels.length - 1]![0]!;
  return {
    descriptor: {
      schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_V1_VERSION,
      machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
      traceRoot,
      stepCount,
      initialStateHash: stateHashes[0]!,
      terminalStateHash: stateHashes[stateHashes.length - 1]!,
      verdict,
      rejectionCodeHash: ensureHash32(
        rejectionCodeHash,
        "trace.rejection_code_hash",
      ),
    },
    stateHashes,
    paddedLeafCount,
    proofs,
  };
};

export const verifyMidgardValidationTraceProofV1 = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardValidationTraceDescriptorV1;
  readonly proof: MidgardValidationTraceProofV1;
}): boolean => {
  if (
    !Number.isSafeInteger(proof.stateIndex) ||
    proof.stateIndex < 0 ||
    proof.stateIndex > descriptor.stepCount
  ) {
    return false;
  }
  const expectedDepth = validationTraceDepthForStepCount(descriptor.stepCount);
  if (proof.siblings.length !== expectedDepth) {
    return false;
  }
  let hash = traceLeafHash(proof.stateHash);
  let index = proof.stateIndex;
  for (const sibling of proof.siblings) {
    hash =
      index % 2 === 0
        ? traceBranchHash(hash, sibling)
        : traceBranchHash(sibling, hash);
    index = Math.floor(index / 2);
  }
  return Buffer.from(hash).equals(descriptor.traceRoot);
};
