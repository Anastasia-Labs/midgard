/**
 * A settlement attempt's L1 outcome, derived from the intent journal (plan
 * §8.2; §15 N6, D-N4), never stored before it is final:
 *
 * - landed more than k blocks deep: `final`, terminal. The follower's prune
 *   step stores it (`settlement.final-hook.ts`) in the step that prunes the
 *   attempt's journal entry, since the journal derives nothing once pruned.
 * - landed at depth >= cd: `safe`: its job takes the next phase. A rollback
 *   that takes it below cd reverts that, and the attempt blocks new work
 *   again until it relands or is proven expired.
 * - anything else (live, landed short of cd, dead, or not journaled):
 *   `open`: not confirmed.
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DepthParameters,
  type IntentStatus,
  levelAtDepth,
} from "@al-ft/midgard-l1-follower";
import { Effect } from "effect";

import * as Journal from "../database/settlement.js";
import { readIntentStatus } from "./intent-journal.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";
import { settlementNextPhase } from "./settlement.reconcile-attempt.js";

export type SettlementLevel = "final" | "safe" | "open";

/** The level of an attempt whose derived status is `status` (null: not journaled, or pruned). */
export const settlementLevel = (
  status: IntentStatus | null,
  parameters: DepthParameters,
): SettlementLevel => {
  if (status?.kind !== "landed") return "open";
  const level = levelAtDepth(status.depth, parameters);
  return level === "final" ? "final" : level === "safe" ? "safe" : "open";
};

/**
 * The deployment's cd and k exactly as the node's follower reads them
 * (`l1-follower.ts`): the follower's prune step marks `final` at its k, so
 * the derived levels use the same pair.
 */
export const settlementDepthParameters = Effect.gen(function* () {
  const identity = yield* ContractDeploymentIdentity;
  const finality =
    identity.manifest?.l1Finality ?? DEPLOYMENT_MANIFEST_L1_FINALITY;
  return {
    confirmationDepth: finality.confirmationDepth,
    securityParameter: finality.automaticRecoveryMaxDepth,
  } satisfies DepthParameters;
});

export type DerivedAttempt = Readonly<{
  attempt: Journal.OpenSettlementAttempt;
  status: IntentStatus | null;
  level: SettlementLevel;
}>;

/** One open attempt with its level; a stored `final` is final whatever the journal reads. */
const derive = (
  attempt: Journal.OpenSettlementAttempt,
  parameters: DepthParameters,
) =>
  Effect.gen(function* () {
    if (attempt.status === "final")
      return { attempt, status: null, level: "final" } as DerivedAttempt;
    const status = yield* readIntentStatus(attempt.tx_hash);
    return {
      attempt,
      status,
      level: settlementLevel(status, parameters),
    } as DerivedAttempt;
  });

/**
 * Derives every open attempt (`Journal.openAttempts`), oldest first, and
 * keeps each job's phase at what its latest attempt reads: the next phase
 * once that attempt is safe or final, the attempt's own phase while it is
 * open (a rollback below cd reverts an advance). Returns the oldest open
 * attempt, which blocks new work until it settles or expires.
 */
export const settleAttempts = (
  owner: Journal.SettlementOwner,
  parameters: DepthParameters,
) =>
  Effect.gen(function* () {
    let blocker: DerivedAttempt | undefined;
    for (const attempt of yield* Journal.openAttempts(owner.deploymentId)) {
      const derived = yield* derive(attempt, parameters);
      const settled = derived.level !== "open";
      if (!settled) blocker ??= derived;
      if (!attempt.latest) continue;
      const phase = settled
        ? settlementNextPhase(attempt.phase)
        : attempt.phase;
      if (attempt.job_phase !== phase)
        yield* Journal.updateJob(owner, attempt, phase);
    }
    return blocker;
  });

/**
 * The check `Journal.saveAttempt` runs under the owner-row lock: no new
 * attempt is journaled while a pending one reads open. A rollback that
 * takes a settled attempt below cd restores its fee coin; a body built on
 * that coin meanwhile would conflict with the attempt S6 is resending.
 */
export const noOpenAttempt = (
  owner: Journal.SettlementOwner,
  parameters: DepthParameters,
) =>
  Effect.gen(function* () {
    for (const attempt of yield* Journal.openAttempts(owner.deploymentId)) {
      if ((yield* derive(attempt, parameters)).level === "open")
        return yield* Effect.fail(
          new Error(
            `Settlement attempt ${attempt.tx_hash} is not confirmed (below confirmation depth); it is reconciled before new work`,
          ),
        );
    }
  });
