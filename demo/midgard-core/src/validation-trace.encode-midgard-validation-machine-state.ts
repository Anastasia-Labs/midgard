import { midgardBlake2b } from "./codec/blake2b.js";
import { asBigInt, encodeCbor } from "./codec/cbor.js";
import {
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "./codec/errors.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
} from "./consensus-profile.js";

export const STATE_HASH_DOMAIN = Buffer.from(
  "MidgardValidationMachineStateV1",
  "utf8",
);

export const TRACE_LEAF_DOMAIN = Buffer.from(
  "MidgardValidationTraceLeafV1",
  "utf8",
);

export const TRACE_BRANCH_DOMAIN = Buffer.from(
  "MidgardValidationTraceBranchV1",
  "utf8",
);

export const REJECTION_CODE_DOMAIN = Buffer.from(
  "MidgardValidationRejectCodeV1",
  "utf8",
);

export const WORK_WITNESS_DOMAIN = Buffer.from(
  "MidgardValidationWorkWitnessV1",
  "utf8",
);

export const VALIDATION_CONTEXT_DOMAIN = Buffer.from(
  "MidgardValidationContextV1",
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
  terminal: 13,
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

export const phaseNames = new Map<number, MidgardValidationPhaseName>(
  Object.entries(MidgardValidationPhase).map(([name, code]) => [
    code,
    name as MidgardValidationPhaseName,
  ]),
);

export const verdictNames = new Map<number, MidgardValidationVerdictName>(
  Object.entries(MidgardValidationVerdict).map(([name, code]) => [
    code,
    name as MidgardValidationVerdictName,
  ]),
);

export const sourceKindNames = new Map<number, MidgardValidationSourceKindName>(
  Object.entries(MidgardValidationSourceKind).map(([name, code]) => [
    code,
    name as MidgardValidationSourceKindName,
  ]),
);

export type MidgardValidationMachineState = {
  readonly machineVersion: typeof MIDGARD_VALIDATION_MACHINE_VERSION;
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
};

/**
 * The wire order of `validation_machine_state_v1`, one name per CBOR array
 * slot. The encoder and the decoder both index through this list, so a field
 * added to the state is placed by inserting its name here (and its value in
 * `encodeMidgardValidationMachineState`); no other slot number moves by hand.
 */
export const MIDGARD_VALIDATION_MACHINE_STATE_FIELDS = [
  "machineVersion",
  "eventKeyHash",
  "transactionId",
  "transactionCommitment",
  "validationContextHash",
  "sourceKind",
  "priorLedgerRoot",
  "phase",
  "programCounter",
  "workRoot",
  "executionCpu",
  "executionMemory",
  "verdict",
  "rejectionCodeHash",
] as const satisfies readonly (keyof MidgardValidationMachineState)[];

export type MidgardValidationMachineStateFieldName =
  (typeof MIDGARD_VALIDATION_MACHINE_STATE_FIELDS)[number];

export type MidgardValidationTraceDescriptor = {
  readonly schemaVersion: typeof MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION;
  readonly machineVersion: typeof MIDGARD_VALIDATION_MACHINE_VERSION;
  readonly traceRoot: Hash32;
  /** Number of transitions. The trace contains stepCount + 1 state hashes. */
  readonly stepCount: number;
  readonly initialStateHash: Hash32;
  readonly terminalStateHash: Hash32;
  readonly verdict: Exclude<MidgardValidationVerdictName, "pending">;
  readonly rejectionCodeHash: Hash32;
};

export type MidgardValidationTraceProof = {
  readonly stateIndex: number;
  readonly stateHash: Hash32;
  readonly siblings: readonly Hash32[];
};

export type MidgardValidationTraceTree = {
  readonly descriptor: MidgardValidationTraceDescriptor;
  readonly stateHashes: readonly Hash32[];
  readonly paddedLeafCount: number;
  readonly proofs: readonly MidgardValidationTraceProof[];
};

export const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    message,
    detail,
  );
};

export const asBoundedUint = (
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

export const exactCode = <T extends string>(
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

export const hashDomain = (domain: Uint8Array, bytes: Uint8Array): Hash32 =>
  ensureHash32(
    midgardBlake2b(Buffer.concat([Buffer.from(domain), Buffer.from(bytes)]), {
      dkLen: 32,
    }),
    "domain_hash",
  );

/** Exact event-key hash committed into every validation-machine state. */
export const hashMidgardValidationEventKey = (
  canonicalEventKeyCbor: Uint8Array,
): Hash32 =>
  ensureHash32(
    midgardBlake2b(Buffer.from(canonicalEventKeyCbor), { dkLen: 32 }),
    "validation_event_key_hash",
  );

export const encodeMidgardValidationMachineState = (
  state: MidgardValidationMachineState,
): Buffer => {
  validateVerdictRejectionBinding(
    state.verdict,
    state.rejectionCodeHash,
    "state",
  );
  const encoded: Record<MidgardValidationMachineStateFieldName, unknown> = {
    machineVersion: BigInt(state.machineVersion),
    eventKeyHash: ensureHash32(state.eventKeyHash, "state.event_key_hash"),
    transactionId: ensureHash32(state.transactionId, "state.transaction_id"),
    transactionCommitment: ensureHash32(
      state.transactionCommitment,
      "state.transaction_commitment",
    ),
    validationContextHash: ensureHash32(
      state.validationContextHash,
      "state.validation_context_hash",
    ),
    sourceKind: BigInt(MidgardValidationSourceKind[state.sourceKind]),
    priorLedgerRoot: ensureHash32(
      state.priorLedgerRoot,
      "state.prior_ledger_root",
    ),
    phase: BigInt(MidgardValidationPhase[state.phase]),
    programCounter: BigInt(
      asBoundedUint(state.programCounter, "state.program_counter", 0xffff_ffff),
    ),
    workRoot: ensureHash32(state.workRoot, "state.work_root"),
    executionCpu: state.executionCpu,
    executionMemory: state.executionMemory,
    verdict: BigInt(MidgardValidationVerdict[state.verdict]),
    rejectionCodeHash: ensureHash32(
      state.rejectionCodeHash,
      "state.rejection_code_hash",
    ),
  };
  return encodeCbor(
    MIDGARD_VALIDATION_MACHINE_STATE_FIELDS.map((name) => encoded[name]),
  );
};

export const ZERO_HASH32 = Buffer.alloc(32);

const hash32IsZero = (value: Uint8Array): boolean =>
  Buffer.from(ensureHash32(value, "rejection_code_hash")).equals(ZERO_HASH32);

export const validateVerdictRejectionBinding = (
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
