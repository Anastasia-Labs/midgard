import { Effect, Either } from "effect";
import { expect } from "vitest";

import {
  buildDeterministicValidationMachineTrace as buildProofTrace,
  commitValidationTraceV1,
  type DeterministicValidationMachineTrace,
  openValidationTraceV1,
  type ValidationMachineReplayInput,
} from "../../src/index.js";

const failureKind = (error: Error) => ({
  type: error.constructor,
  reason: (error as { readonly reason?: unknown }).reason,
});

/**
 * The proof-mode trace builder, with every call also replayed in streaming
 * commit mode: the commit-mode descriptor must be bit-identical to the
 * proof-mode tree's descriptor, the opened trace must hold exactly the
 * proof trace's state hashes and terminal state, and an input the proof
 * build refuses must be refused the same way. Corpus tests import the
 * builder from here, so every trace they build is a commit-mode golden.
 */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    const proof = yield* Effect.either(buildProofTrace(input));
    const commit = yield* Effect.either(commitValidationTraceV1(input));
    const opened = yield* Effect.either(openValidationTraceV1(input));
    if (Either.isLeft(proof)) {
      expect(Either.isLeft(commit)).toBe(true);
      expect(Either.isLeft(opened)).toBe(true);
      if (Either.isLeft(commit) && Either.isLeft(opened)) {
        expect(failureKind(commit.left)).toEqual(failureKind(proof.left));
        expect(failureKind(opened.left)).toEqual(failureKind(proof.left));
      }
      return yield* Effect.fail(proof.left);
    }
    const trace = proof.right;
    if (Either.isLeft(commit)) throw commit.left;
    if (Either.isLeft(opened)) throw opened.left;
    expect(commit.right.descriptor).toEqual(trace.tree.descriptor);
    expect(commit.right.rejectionCode).toEqual(trace.rejectionCode);
    expect(commit.right.ledgerOps).toEqual(trace.ledgerOps);
    expect(opened.right.descriptor).toEqual(trace.tree.descriptor);
    expect(opened.right.stepCount).toBe(trace.tree.descriptor.stepCount);
    expect(opened.right.stateHashes).toEqual(
      Buffer.concat(trace.tree.stateHashes),
    );
    expect(opened.right.states.get(opened.right.stepCount)).toEqual(
      trace.states.at(-1),
    );
    return trace;
  });
