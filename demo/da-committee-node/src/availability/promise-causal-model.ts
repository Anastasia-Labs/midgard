/** Conditional inclusion model, not a deterministic ledger service guarantee.
 * The caller must separately adopt and enforce the software/resource domain. */
export type CommitteePromiseCausalModel = Readonly<{
  activeSlotProbability: number;
  eligibleFutureSlots: number;
  initialReadySlots: number;
  includedToNextReadySlots: number;
  expiryToReplacementReadySlots: number;
  aggregateAllowedFailedAttempts: number;
}>;

export type CommitteePromiseCausalResult = Readonly<{
  successProbability: number;
  deadlineMissProbability: number;
  retryExhaustionProbability: number;
}>;

const natural = (value: number): boolean =>
  Number.isSafeInteger(value) && value >= 0;

/** Every inclusion must precede the exclusive deadline. Failed attempts are
 * charged across the entire remaining prefix, rather than renewed per action.
 * This deliberately supports only the measured small controlled profile. */
export const committeePromiseCausalProbability = (
  input: Readonly<{
    model: CommitteePromiseCausalModel;
    remainingActions: number;
    remainingHorizonSlots: number;
    usedFailedAttempts: number;
  }>,
): CommitteePromiseCausalResult => {
  const {
    model,
    remainingActions: actions,
    remainingHorizonSlots: horizon,
  } = input;
  if (
    !Number.isFinite(model.activeSlotProbability) ||
    model.activeSlotProbability <= 0 ||
    model.activeSlotProbability > 1 ||
    !Object.entries(model).every(
      ([key, value]) => key === "activeSlotProbability" || natural(value),
    ) ||
    model.eligibleFutureSlots === 0 ||
    !natural(actions) ||
    actions === 0 ||
    actions > 8 ||
    !natural(horizon) ||
    horizon > 1024 ||
    !natural(input.usedFailedAttempts) ||
    model.aggregateAllowedFailedAttempts > 8 ||
    input.usedFailedAttempts > model.aggregateAllowedFailedAttempts
  )
    throw new Error(
      "Conditional promise model is outside its supported domain",
    );
  const retries =
    model.aggregateAllowedFailedAttempts - input.usedFailedAttempts;
  const states = Array.from({ length: horizon + 1 }, () =>
    Array.from({ length: actions }, () => new Float64Array(retries + 1)),
  );
  if (model.initialReadySlots >= horizon)
    return {
      successProbability: 0,
      deadlineMissProbability: 1,
      retryExhaustionProbability: 0,
    };
  states[model.initialReadySlots]![0]![0] = 1;
  const p = model.activeSlotProbability,
    q = 1 - p;
  let success = 0,
    late = 0,
    exhausted = 0;
  for (let time = 0; time < horizon; time++) {
    const waits = horizon - time - 1;
    for (let action = 0; action < actions; action++) {
      for (let failures = 0; failures <= retries; failures++) {
        const mass = states[time]![action]![failures]!;
        if (mass === 0) continue;
        late += mass * q ** waits;
        let probability = mass * p;
        for (let wait = 1; wait <= waits; wait++, probability *= q) {
          if (wait <= model.eligibleFutureSlots) {
            if (action + 1 === actions) success += probability;
            else {
              const next = time + wait + model.includedToNextReadySlots;
              if (next < horizon)
                states[next]![action + 1]![failures]! += probability;
              else late += probability;
            }
          } else if (failures === retries) exhausted += probability;
          else {
            const next = time + wait + model.expiryToReplacementReadySlots;
            if (next < horizon)
              states[next]![action]![failures + 1]! += probability;
            else late += probability;
          }
        }
      }
    }
  }
  if (Math.abs(success + late + exhausted - 1) > 1e-10)
    throw new Error("Conditional promise probability mass is inconsistent");
  return {
    successProbability: success,
    deadlineMissProbability: late,
    retryExhaustionProbability: exhausted,
  };
};
