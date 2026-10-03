import { Effect } from "effect";
import { expect } from "vitest";

import { runPhaseBValidationWithPatch } from "../src/index.js";
import type { PhaseBResultWithPatch } from "../src/phase-b.js";
import type { PhaseBConfig, RejectCode, RejectedTx } from "../src/types.js";

export const phaseBConfig: PhaseBConfig = {
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
};

export const runPhaseB = (
  candidates: Parameters<typeof runPhaseBValidationWithPatch>[0],
  preState: Parameters<typeof runPhaseBValidationWithPatch>[1],
  config = phaseBConfig,
) =>
  Effect.runPromise(runPhaseBValidationWithPatch(candidates, preState, config));

export const preState = (
  entries: readonly (readonly [outRef: Buffer, output: Buffer])[],
) =>
  new Map(entries.map(([outRef, output]) => [outRef.toString("hex"), output]));

export const expectSinglePhaseBRejection = (
  result: PhaseBResultWithPatch,
  expectedCode: RejectCode,
): RejectedTx => {
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0].code).toBe(expectedCode);
  return result.rejected[0];
};
