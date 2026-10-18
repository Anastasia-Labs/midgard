import { midgardBlake2b } from "./codec/blake2b.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import {
  asBoundedUint,
  encodeMidgardValidationMachineState,
  fail,
  type MidgardValidationMachineState,
  MidgardValidationPhase,
  type MidgardValidationPhaseName,
  MidgardValidationSourceKind,
  MidgardValidationVerdict,
  STATE_HASH_DOMAIN,
  validateVerdictRejectionBinding,
  WORK_WITNESS_DOMAIN,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

/*
 * The two hashes a validation trace takes once per machine step, written
 * straight into one buffer each. Both are byte-identical to the generic
 * codec path (`tests/validation-trace.step-hashes.test.ts` holds them to
 * it), which is kept for every value the direct writers do not cover.
 */

/** Byte length of a CBOR head whose argument is `value`. */
const cborHeadLength = (value: number): number =>
  value < 24
    ? 1
    : value <= 0xff
      ? 2
      : value <= 0xffff
        ? 3
        : value <= 0xffff_ffff
          ? 5
          : 9;

/** Writes a CBOR head at `offset`; returns the offset after it. */
const writeCborHead = (
  out: Buffer,
  offset: number,
  major: number,
  value: number,
): number => {
  const prefix = major << 5;
  if (value < 24) {
    out[offset] = prefix | value;
    return offset + 1;
  }
  if (value <= 0xff) {
    out[offset] = prefix | 24;
    out[offset + 1] = value;
    return offset + 2;
  }
  if (value <= 0xffff) {
    out[offset] = prefix | 25;
    out.writeUInt16BE(value, offset + 1);
    return offset + 3;
  }
  if (value <= 0xffff_ffff) {
    out[offset] = prefix | 26;
    out.writeUInt32BE(value, offset + 1);
    return offset + 5;
  }
  out[offset] = prefix | 27;
  out.writeBigUInt64BE(BigInt(value), offset + 1);
  return offset + 9;
};

/** Plutus Data byte strings longer than this are serialised in chunks. */
const DATA_BYTES_CHUNK = 64;

/**
 * Commits the canonical witness consumed by one validation-machine
 * transition. The phase and program counter are inside the commitment so the
 * same bytes cannot be replayed as a different instruction.
 *
 * The preimage is the domain followed by the CBOR array
 * `[phase, program_counter, witness]`, where the witness is serialised as
 * `cbor.serialise(builtin.b_data(witness_cbor))` serialises it on chain: a
 * definite byte string up to 64 bytes, otherwise an indefinite byte string of
 * 64-byte definite chunks. It is written straight into one buffer, since the
 * commit build hashes one witness per machine step.
 */
export const hashMidgardValidationWorkWitness = ({
  phase,
  programCounter,
  witnessCbor,
}: {
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly witnessCbor: Uint8Array;
}): Hash32 => {
  const phaseCode = MidgardValidationPhase[phase];
  const counter = asBoundedUint(
    programCounter,
    "work_witness.program_counter",
    MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
  );
  const length = witnessCbor.length;
  const chunked = length > DATA_BYTES_CHUNK;
  const fullChunks = Math.floor(length / DATA_BYTES_CHUNK);
  const tail = length - fullChunks * DATA_BYTES_CHUNK;
  const bytesLength = chunked
    ? 2 +
      fullChunks * (2 + DATA_BYTES_CHUNK) +
      (tail === 0 ? 0 : cborHeadLength(tail) + tail)
    : cborHeadLength(length) + length;
  const out = Buffer.allocUnsafe(
    WORK_WITNESS_DOMAIN.length +
      1 +
      cborHeadLength(phaseCode) +
      cborHeadLength(counter) +
      bytesLength,
  );
  let offset = WORK_WITNESS_DOMAIN.copy(out, 0);
  out[offset++] = 0x83;
  offset = writeCborHead(out, offset, 0, phaseCode);
  offset = writeCborHead(out, offset, 0, counter);
  if (!chunked) {
    offset = writeCborHead(out, offset, 2, length);
    out.set(witnessCbor, offset);
    offset += length;
  } else {
    out[offset++] = 0x5f;
    for (let start = 0; start < length; start += DATA_BYTES_CHUNK) {
      const chunk = witnessCbor.subarray(start, start + DATA_BYTES_CHUNK);
      offset = writeCborHead(out, offset, 2, chunk.length);
      out.set(chunk, offset);
      offset += chunk.length;
    }
    out[offset++] = 0xff;
  }
  if (offset !== out.length) {
    return fail("work witness preimage length mismatch", offset.toString());
  }
  return ensureHash32(midgardBlake2b(out, { dkLen: 32 }), "domain_hash");
};

const HASH32_HEAD = Buffer.from([0x58, 0x20]);
const MAX_UINT64 = 0xffff_ffff_ffff_ffffn;

/** Writes `value` as a 32-byte CBOR byte string; `fieldName` names a bad one. */
const writeHash32 = (
  out: Buffer,
  offset: number,
  value: Uint8Array,
  fieldName: string,
): number => {
  if (value.length !== 32) ensureHash32(value, fieldName);
  HASH32_HEAD.copy(out, offset);
  out.set(value, offset + 2);
  return offset + 34;
};

/** Byte length of a CBOR unsigned integer head for a 64-bit `value`. */
const cborUint64HeadLength = (value: bigint): number =>
  value < 24n
    ? 1
    : value <= 0xffn
      ? 2
      : value <= 0xffffn
        ? 3
        : value <= 0xffff_ffffn
          ? 5
          : 9;

const writeCborUint64 = (
  out: Buffer,
  offset: number,
  value: bigint,
): number => {
  if (value <= 0xffff_ffffn)
    return writeCborHead(out, offset, 0, Number(value));
  out[offset] = 27;
  out.writeBigUInt64BE(value, offset + 1);
  return offset + 9;
};

/**
 * The state hash: the domain followed by the state's CBOR encoding. An
 * execution budget outside 64 bits, or a machine version that is not a
 * non-negative safe integer, takes the generic encoder, which encodes or
 * refuses it.
 */
export const hashMidgardValidationMachineState = (
  state: MidgardValidationMachineState,
): Hash32 => {
  const { executionCpu, executionMemory, machineVersion } = state;
  const sourceKind = MidgardValidationSourceKind[state.sourceKind] as
    | number
    | undefined;
  const phase = MidgardValidationPhase[state.phase] as number | undefined;
  const verdict = MidgardValidationVerdict[state.verdict] as number | undefined;
  if (
    sourceKind === undefined ||
    phase === undefined ||
    verdict === undefined ||
    !Number.isSafeInteger(machineVersion) ||
    machineVersion < 0 ||
    typeof executionCpu !== "bigint" ||
    typeof executionMemory !== "bigint" ||
    executionCpu < 0n ||
    executionCpu > MAX_UINT64 ||
    executionMemory < 0n ||
    executionMemory > MAX_UINT64
  ) {
    const encoded = encodeMidgardValidationMachineState(state);
    return ensureHash32(
      midgardBlake2b(Buffer.concat([STATE_HASH_DOMAIN, encoded]), {
        dkLen: 32,
      }),
      "domain_hash",
    );
  }
  validateVerdictRejectionBinding(
    state.verdict,
    state.rejectionCodeHash,
    "state",
  );
  // Sized for the widest program counter head; the fields are checked in
  // the generic encoder's order, so a bad state is refused the same way.
  const out = Buffer.allocUnsafe(
    STATE_HASH_DOMAIN.length +
      1 +
      cborHeadLength(machineVersion) +
      7 * 34 +
      cborHeadLength(sourceKind) +
      cborHeadLength(phase) +
      cborHeadLength(0xffff_ffff) +
      cborUint64HeadLength(executionCpu) +
      cborUint64HeadLength(executionMemory) +
      cborHeadLength(verdict),
  );
  let offset = STATE_HASH_DOMAIN.copy(out, 0);
  out[offset++] = 0x8e;
  offset = writeCborHead(out, offset, 0, machineVersion);
  offset = writeHash32(out, offset, state.eventKeyHash, "state.event_key_hash");
  offset = writeHash32(
    out,
    offset,
    state.transactionId,
    "state.transaction_id",
  );
  offset = writeHash32(
    out,
    offset,
    state.transactionCommitment,
    "state.transaction_commitment",
  );
  offset = writeHash32(
    out,
    offset,
    state.validationContextHash,
    "state.validation_context_hash",
  );
  offset = writeCborHead(out, offset, 0, sourceKind);
  offset = writeHash32(
    out,
    offset,
    state.priorLedgerRoot,
    "state.prior_ledger_root",
  );
  offset = writeCborHead(out, offset, 0, phase);
  offset = writeCborHead(
    out,
    offset,
    0,
    asBoundedUint(state.programCounter, "state.program_counter", 0xffff_ffff),
  );
  offset = writeHash32(out, offset, state.workRoot, "state.work_root");
  offset = writeCborUint64(out, offset, executionCpu);
  offset = writeCborUint64(out, offset, executionMemory);
  offset = writeCborHead(out, offset, 0, verdict);
  offset = writeHash32(
    out,
    offset,
    state.rejectionCodeHash,
    "state.rejection_code_hash",
  );
  return ensureHash32(
    midgardBlake2b(out.subarray(0, offset), { dkLen: 32 }),
    "domain_hash",
  );
};
