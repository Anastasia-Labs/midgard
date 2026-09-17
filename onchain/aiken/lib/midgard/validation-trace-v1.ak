use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray

pub const machine_version: Int = 1

pub const descriptor_version: Int = 1

pub const max_machine_step_count: Int = 4_294_967_295

const state_hash_domain =
  #"4d69646761726456616c69646174696f6e4d616368696e6553746174655631"

const trace_leaf_domain =
  #"4d69646761726456616c69646174696f6e54726163654c6561665631"

const trace_branch_domain =
  #"4d69646761726456616c69646174696f6e54726163654272616e63685631"

const work_witness_domain =
  #"4d69646761726456616c69646174696f6e576f726b5769746e6573735631"

const rejection_code_domain =
  #"4d69646761726456616c69646174696f6e52656a656374436f64655631"

const validation_context_domain =
  #"4d69646761726456616c69646174696f6e436f6e746578745631"

const ledger_delta_domain =
  #"4d69646761726456616c69646174696f6e4c656467657244656c74615631"

pub type ValidationPhase {
  CanonicalDecode
  CompactBinding
  StaticLedgerRules
  InputSets
  Signatures
  PhaseANativeScripts
  PhaseAScriptPreconditions
  ResolveInputs
  ScriptSources
  NativeScripts
  ScriptIntegrity
  Cek
  ValueAndMint
  LedgerDelta
  Terminal
}

pub type ValidationVerdict {
  Pending
  Accepted
  Rejected
}

pub type ValidationSourceKind {
  Normal
  Forced
}

pub type ValidationMachineStateV1 {
  machine_version: Int,
  event_key_hash: ByteArray,
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  validation_context_hash: ByteArray,
  source_kind: ValidationSourceKind,
  prior_ledger_root: ByteArray,
  phase: ValidationPhase,
  program_counter: Int,
  work_root: ByteArray,
  execution_cpu: Int,
  execution_memory: Int,
  verdict: ValidationVerdict,
  rejection_code_hash: ByteArray,
  ledger_delta_root: ByteArray,
}

pub type ValidationTraceDescriptorV1 {
  schema_version: Int,
  machine_version: Int,
  trace_root: ByteArray,
  step_count: Int,
  initial_state_hash: ByteArray,
  terminal_state_hash: ByteArray,
  verdict: ValidationVerdict,
  rejection_code_hash: ByteArray,
}

pub type ValidationTraceProof {
  state_index: Int,
  state_hash: ByteArray,
  siblings: List<ByteArray>,
}

fn phase_code(phase: ValidationPhase) -> Int {
  when phase is {
    CanonicalDecode -> 0
    CompactBinding -> 1
    StaticLedgerRules -> 2
    InputSets -> 3
    Signatures -> 4
    PhaseANativeScripts -> 5
    PhaseAScriptPreconditions -> 6
    ResolveInputs -> 7
    ScriptSources -> 8
    NativeScripts -> 9
    ScriptIntegrity -> 10
    Cek -> 11
    ValueAndMint -> 12
    LedgerDelta -> 13
    Terminal -> 14
  }
}

fn verdict_code(verdict: ValidationVerdict) -> Int {
  when verdict is {
    Pending -> 0
    Accepted -> 1
    Rejected -> 2
  }
}

fn source_kind_code(source_kind: ValidationSourceKind) -> Int {
  when source_kind is {
    Normal -> 0
    Forced -> 1
  }
}

const no_rejection_code_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

fn verdict_rejection_binding_is_valid(
  verdict: ValidationVerdict,
  rejection_code_hash: ByteArray,
) -> Bool {
  and {
    bytearray.length(rejection_code_hash) == 32,
    if verdict == Rejected {
      rejection_code_hash != no_rejection_code_hash
    } else {
      rejection_code_hash == no_rejection_code_hash
    },
  }
}

pub fn machine_state_is_well_formed(state: ValidationMachineStateV1) -> Bool {
  and {
    state.machine_version == machine_version,
    bytearray.length(state.event_key_hash) == 32,
    bytearray.length(state.transaction_id) == 32,
    bytearray.length(state.transaction_commitment) == 32,
    bytearray.length(state.validation_context_hash) == 32,
    bytearray.length(state.prior_ledger_root) == 32,
    state.program_counter >= 0,
    state.program_counter <= max_machine_step_count,
    bytearray.length(state.work_root) == 32,
    state.execution_cpu >= 0,
    state.execution_memory >= 0,
    verdict_rejection_binding_is_valid(state.verdict, state.rejection_code_hash),
    bytearray.length(state.ledger_delta_root) == 32,
  }
}

pub fn descriptor_is_well_formed(
  descriptor: ValidationTraceDescriptorV1,
) -> Bool {
  and {
    descriptor.schema_version == descriptor_version,
    descriptor.machine_version == machine_version,
    descriptor.step_count >= 0,
    descriptor.step_count <= max_machine_step_count,
    descriptor.verdict != Pending,
    verdict_rejection_binding_is_valid(
      descriptor.verdict,
      descriptor.rejection_code_hash,
    ),
    bytearray.length(descriptor.trace_root) == 32,
    bytearray.length(descriptor.initial_state_hash) == 32,
    bytearray.length(descriptor.terminal_state_hash) == 32,
  }
}

fn encode_h32(bytes: ByteArray) -> ByteArray {
  expect bytearray.length(bytes) == 32
  #"5820" |> bytearray.concat(bytes)
}

pub fn encode_machine_state(state: ValidationMachineStateV1) -> ByteArray {
  expect state.machine_version == machine_version
  expect state.program_counter >= 0
  expect state.program_counter <= max_machine_step_count
  expect state.execution_cpu >= 0
  expect state.execution_memory >= 0
  expect
    verdict_rejection_binding_is_valid(state.verdict, state.rejection_code_hash)

  #"8f"
    |> bytearray.concat(cbor.serialise(state.machine_version))
    |> bytearray.concat(encode_h32(state.event_key_hash))
    |> bytearray.concat(encode_h32(state.transaction_id))
    |> bytearray.concat(encode_h32(state.transaction_commitment))
    |> bytearray.concat(encode_h32(state.validation_context_hash))
    |> bytearray.concat(cbor.serialise(source_kind_code(state.source_kind)))
    |> bytearray.concat(encode_h32(state.prior_ledger_root))
    |> bytearray.concat(cbor.serialise(phase_code(state.phase)))
    |> bytearray.concat(cbor.serialise(state.program_counter))
    |> bytearray.concat(encode_h32(state.work_root))
    |> bytearray.concat(cbor.serialise(state.execution_cpu))
    |> bytearray.concat(cbor.serialise(state.execution_memory))
    |> bytearray.concat(cbor.serialise(verdict_code(state.verdict)))
    |> bytearray.concat(encode_h32(state.rejection_code_hash))
    |> bytearray.concat(encode_h32(state.ledger_delta_root))
}

pub fn hash_machine_state(state: ValidationMachineStateV1) -> ByteArray {
  state_hash_domain
    |> bytearray.concat(encode_machine_state(state))
    |> blake2b_256
}

pub fn hash_work_witness(
  phase: ValidationPhase,
  program_counter: Int,
  witness_cbor: ByteArray,
) -> ByteArray {
  expect program_counter >= 0
  expect program_counter <= max_machine_step_count
  work_witness_domain
    |> bytearray.concat(#"83")
    |> bytearray.concat(cbor.serialise(phase_code(phase)))
    |> bytearray.concat(cbor.serialise(program_counter))
    |> bytearray.concat(cbor.serialise(witness_cbor))
    |> blake2b_256
}

pub fn hash_rejection_code(rejection_code: ByteArray) -> ByteArray {
  rejection_code_domain
    |> bytearray.concat(rejection_code)
    |> blake2b_256
}

pub fn hash_validation_context(context_cbor: ByteArray) -> ByteArray {
  validation_context_domain
    |> bytearray.concat(context_cbor)
    |> blake2b_256
}

pub fn hash_ledger_delta(delta_cbor: ByteArray) -> ByteArray {
  ledger_delta_domain
    |> bytearray.concat(delta_cbor)
    |> blake2b_256
}

pub fn encode_descriptor(descriptor: ValidationTraceDescriptorV1) -> ByteArray {
  expect descriptor.schema_version == descriptor_version
  expect descriptor.machine_version == machine_version
  expect descriptor.step_count >= 0
  expect descriptor.step_count <= max_machine_step_count
  expect descriptor.verdict != Pending
  expect
    verdict_rejection_binding_is_valid(
      descriptor.verdict,
      descriptor.rejection_code_hash,
    )

  #"88"
    |> bytearray.concat(cbor.serialise(descriptor.schema_version))
    |> bytearray.concat(cbor.serialise(descriptor.machine_version))
    |> bytearray.concat(encode_h32(descriptor.trace_root))
    |> bytearray.concat(cbor.serialise(descriptor.step_count))
    |> bytearray.concat(encode_h32(descriptor.initial_state_hash))
    |> bytearray.concat(encode_h32(descriptor.terminal_state_hash))
    |> bytearray.concat(cbor.serialise(verdict_code(descriptor.verdict)))
    |> bytearray.concat(encode_h32(descriptor.rejection_code_hash))
}

pub fn trace_leaf_hash(state_hash: ByteArray) -> ByteArray {
  expect bytearray.length(state_hash) == 32
  trace_leaf_domain
    |> bytearray.concat(state_hash)
    |> blake2b_256
}

pub fn trace_branch_hash(left: ByteArray, right: ByteArray) -> ByteArray {
  expect bytearray.length(left) == 32
  expect bytearray.length(right) == 32
  trace_branch_domain
    |> bytearray.concat(left)
    |> bytearray.concat(right)
    |> blake2b_256
}

fn trace_depth_loop(state_count: Int, capacity: Int, depth: Int) -> Int {
  if capacity >= state_count {
    depth
  } else {
    trace_depth_loop(state_count, capacity * 2, depth + 1)
  }
}

pub fn trace_depth(step_count: Int) -> Int {
  expect step_count >= 0
  expect step_count <= max_machine_step_count
  trace_depth_loop(step_count + 1, 1, 0)
}

fn fold_trace_path(
  siblings: List<ByteArray>,
  index: Int,
  hash: ByteArray,
) -> ByteArray {
  when siblings is {
    [] -> hash
    [sibling, ..rest] -> {
      expect bytearray.length(sibling) == 32
      let parent = if index % 2 == 0 {
        trace_branch_hash(hash, sibling)
      } else {
        trace_branch_hash(sibling, hash)
      }
      fold_trace_path(rest, index / 2, parent)
    }
  }
}

pub fn verify_trace_proof(
  descriptor: ValidationTraceDescriptorV1,
  proof: ValidationTraceProof,
) -> Bool {
  and {
    descriptor_is_well_formed(descriptor),
    proof.state_index >= 0,
    proof.state_index <= descriptor.step_count,
    bytearray.length(proof.state_hash) == 32,
    list.length(proof.siblings) == trace_depth(descriptor.step_count),
    fold_trace_path(
      proof.siblings,
      proof.state_index,
      trace_leaf_hash(proof.state_hash),
    ) == descriptor.trace_root,
  }
}
