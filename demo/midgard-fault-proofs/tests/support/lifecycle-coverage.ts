import {
  type CompleteLifecycleBaseScenario,
  type CompleteLifecycleCoverage,
} from "../../src/testing/complete-lifecycle.js";

export type LifecycleDirection =
  CompleteLifecycleCoverage["successfulDirectionByReason"][string][number];

/**
 * Accumulates what a lifecycle suite actually exercised while it runs, so the
 * closing `assertCompleteLifecycleCoverage` call reports observed coverage
 * rather than a pre-filled record.
 */
export const createLifecycleCoverageRecorder = () => {
  const reasonArms = new Set<string>();
  const directions = new Map<string, Set<LifecycleDirection>>();
  const scenarios = new Set<CompleteLifecycleBaseScenario>();
  const seams = new Set<string>();
  const cancelled = new Set<string>();
  let resumedAfterCheckpoint = false;
  let adjacentOverBoundRefused = false;
  return {
    /** A reason arm was exercised; with a direction, it completed to a proof. */
    reason(reason: string, direction?: LifecycleDirection): void {
      reasonArms.add(reason);
      if (direction !== undefined) {
        const observed =
          directions.get(reason) ?? new Set<LifecycleDirection>();
        observed.add(direction);
        directions.set(reason, observed);
      }
    },
    scenario(scenario: CompleteLifecycleBaseScenario): void {
      scenarios.add(scenario);
    },
    seamMutated(seam: string): void {
      seams.add(seam);
    },
    cancelled(step: string): void {
      cancelled.add(step);
    },
    resumed(): void {
      resumedAfterCheckpoint = true;
    },
    adjacentOverBoundRefused(): void {
      adjacentOverBoundRefused = true;
    },
    snapshot(): CompleteLifecycleCoverage {
      return {
        reasonArms: [...reasonArms],
        successfulDirectionByReason: Object.fromEntries(
          [...directions.entries()].map(([reason, observed]) => [
            reason,
            [...observed],
          ]),
        ),
        scenarios: [...scenarios],
        authenticatedSeamsMutated: [...seams],
        cancelledPhysicalSteps: [...cancelled],
        resumedAfterCheckpoint,
        adjacentOverBoundRefused,
      };
    },
  };
};

export type LifecycleCoverageRecorder = ReturnType<
  typeof createLifecycleCoverageRecorder
>;
