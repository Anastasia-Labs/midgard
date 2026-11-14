import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
} from "./consensus-profile.js";
import { MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH } from "./validation-trace.decode-midgard-validation-machine-state.js";
import {
  asBoundedUint,
  fail,
  hashDomain,
  type MidgardValidationTraceDescriptor,
  type MidgardValidationTraceProof,
  type MidgardValidationTraceTree,
  type MidgardValidationVerdictName,
  TRACE_BRANCH_DOMAIN,
  TRACE_LEAF_DOMAIN,
  validateVerdictRejectionBinding,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

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
    MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
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
    MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
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
      schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
      machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
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

export const verifyMidgardValidationTraceProof = ({
  descriptor,
  proof,
}: {
  readonly descriptor: MidgardValidationTraceDescriptor;
  readonly proof: MidgardValidationTraceProof;
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
