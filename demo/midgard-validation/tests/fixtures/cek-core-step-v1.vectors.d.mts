/**
 * Types for `cek-core-step-v1.vectors.mjs`.
 *
 * The program definitions are authored as plain ESM so the generator can load
 * them on bare `node` without a build step; this file is what lets the vitest
 * suite consume them with the same types the CEK exposes, rather than through
 * `any`. See `demo/midgard-core/tests/fixtures/native-tx-field-items-v1.vectors.d.mts`
 * for why a `.d.mts` beside a `.mjs` rather than a `.ts`, and for the
 * `skipLibCheck` hazard: a name here that no longer exists in `src/` degrades
 * silently to `any`, so the export list is checked against the real module by
 * `tests/cek-core-step-goldens.test.ts` and every declared value is driven
 * through the real trace builder and compared byte-for-byte against the
 * checked-in fixture.
 */

import type { Data } from "@harmoniclabs/plutus-data";
import type { UPLCTerm } from "@harmoniclabs/uplc";

import type { MidgardCekExecutionStep } from "../../src/cek-executor.js";
import type { MidgardCekCoreStepWitness } from "../../src/cek-machine.js";

export declare const compileMidgardCekGoldenProgram: (term: UPLCTerm) => Buffer;

export declare const EMPTY_CONTEXT: () => Data;

/** One curated program: a UPLC term, the script context it is applied to. */
export type CekCoreStepProgramV1 = {
  readonly label: string;
  readonly note: string;
  readonly term: () => UPLCTerm;
  readonly context: () => Data;
  readonly maxSteps: number;
  readonly executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  };
};

export declare const CEK_CORE_STEP_PROGRAMS: readonly CekCoreStepProgramV1[];

export declare const INITIAL_MODE: "compute";

/** One negative twist: which step of a trace it perturbs, and how. */
export type CekCoreStepTwistV1 = {
  readonly label: string;
  readonly select: (steps: readonly MidgardCekExecutionStep[]) => number;
  readonly apply: (step: MidgardCekExecutionStep) => MidgardCekExecutionStep;
};

export declare const CEK_CORE_STEP_TWISTS: readonly CekCoreStepTwistV1[];

export declare const CEK_CORE_STEP_WITNESS_KINDS: readonly MidgardCekCoreStepWitness["kind"][];

export declare const CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED: readonly MidgardCekCoreStepWitness["kind"][];
