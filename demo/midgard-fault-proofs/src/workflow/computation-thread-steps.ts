/**
 * The computation-thread step range one family chain may declare. The bound
 * is a workflow-framework size, not a protocol limit: it covers the longest
 * installed chain (missingRedeemer, 39 roles).
 */
export const MAX_COMPUTATION_THREAD_STEPS = 39;

type Ordinals<
  Limit extends number,
  Seen extends readonly unknown[] = [unknown],
> = Seen["length"] extends Limit
  ? Seen["length"]
  : Seen["length"] | Ordinals<Limit, [...Seen, unknown]>;

/** One-based step ordinal, 1 to {@link MAX_COMPUTATION_THREAD_STEPS}. */
export type ComputationThreadStep = Ordinals<
  typeof MAX_COMPUTATION_THREAD_STEPS
>;

type TwoDigits<Step extends number> = Step extends
  | 1
  | 2
  | 3
  | 4
  | 5
  | 6
  | 7
  | 8
  | 9
  ? `0${Step}`
  : `${Step}`;

export type ComputationThreadStepRole =
  `computation_thread_step_${TwoDigits<ComputationThreadStep>}`;

const twoDigits = (step: number): string => step.toString().padStart(2, "0");

/** Every step role in ordinal order (index `i` holds step `i + 1`). */
export const COMPUTATION_THREAD_STEP_ROLES: readonly ComputationThreadStepRole[] =
  Object.freeze(
    Array.from(
      { length: MAX_COMPUTATION_THREAD_STEPS },
      (_, index) =>
        `computation_thread_step_${twoDigits(index + 1)}` as ComputationThreadStepRole,
    ),
  );

export const isComputationThreadStep = (
  value: number,
): value is ComputationThreadStep =>
  Number.isSafeInteger(value) &&
  value >= 1 &&
  value <= MAX_COMPUTATION_THREAD_STEPS;

/** The workflow stage name of a step, `step_01` onwards. */
export const computationThreadStage = (step: ComputationThreadStep): string =>
  `step_${twoDigits(step)}`;

/** The step a canonical stage name denotes, or `undefined`. */
export const parseComputationThreadStage = (
  stage: string,
): ComputationThreadStep | undefined => {
  const match = /^step_([0-9]{2})$/u.exec(stage);
  if (match === null) return undefined;
  const step = Number(match[1]);
  return isComputationThreadStep(step) ? step : undefined;
};
