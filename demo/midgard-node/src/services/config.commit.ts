import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Config, Option } from "effect";

import { positiveSafeInteger } from "../artifact-schema.js";
import { blockExUnitsBudgetConfig } from "./config.block-ex-units-budget.js";
import {
  cappedPositiveSafeIntegerConfig,
  integerConfig,
  nonNegativeSafeIntegerConfig,
  positiveFiniteNumberConfig,
  positiveSafeIntegerConfig,
} from "./config.parse.js";

/**
 * How the node builds, commits and merges blocks: block limits and the build
 * cost model, the commit and merge cadence, the state-queue mutation lease
 * and the operator watchdog.
 */
export const commitConfig = Config.all({
  WAIT_BETWEEN_BLOCK_COMMITMENT: integerConfig(
    "WAIT_BETWEEN_BLOCK_COMMITMENT",
    1000,
  ),
  UNCONFIRMED_BLOCK_MAX_AGE_MS: integerConfig(
    "UNCONFIRMED_BLOCK_MAX_AGE_MS",
    180_000,
  ),
  WAIT_BETWEEN_MERGE_TXS: integerConfig("WAIT_BETWEEN_MERGE_TXS", 10000),
  MIN_QUEUE_LENGTH_FOR_MERGING: integerConfig(
    "MIN_QUEUE_LENGTH_FOR_MERGING",
    8,
  ),
  RUN_GENESIS_ON_STARTUP: Config.string("RUN_GENESIS_ON_STARTUP").pipe(
    Config.withDefault("false"),
    Config.map((value) => value.trim().toLowerCase() === "true"),
  ),
  OPERATOR_WATCHDOG_ENABLED: Config.boolean("OPERATOR_WATCHDOG_ENABLED").pipe(
    Config.withDefault(true),
  ),
  OPERATOR_WATCHDOG_PATIENCE_MS: nonNegativeSafeIntegerConfig(
    "OPERATOR_WATCHDOG_PATIENCE_MS",
    120_000,
  ),
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: positiveSafeIntegerConfig(
    "STATE_QUEUE_MUTATION_LEASE_TTL_MS",
    10 * 60 * 1000,
  ),
  /** Unset is a third of the TTL, at most a minute. */
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Config.option(
    Config.integer("STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS").pipe(
      Config.mapAttempt((value) =>
        positiveSafeInteger(
          value,
          "STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS",
        ),
      ),
    ),
  ),
  /** The longest a state-queue mutation lease may be held before `/readyz`
   * names its holder (`state_queue_lease_held_long`): the
   * `commit_worker_failed` escalation bound, until real hold times are
   * measured. */
  STATE_QUEUE_MUTATION_LEASE_MAX_HOLD_MS: positiveSafeIntegerConfig(
    "STATE_QUEUE_MUTATION_LEASE_MAX_HOLD_MS",
    10 * 60 * 1000,
  ),
  /** The commit-event depth d (plan §8.1): the deployment profile's
   * `l1_finality.commit_event_depth`. A commit's anchor is the follower
   * block d below the view it is planned at, and its header end is at most
   * the anchor's time + event_wait - 1 (`database/commit-anchor.ts`). */
  COMMIT_EVENT_DEPTH: Config.succeed(
    DEPLOYMENT_MANIFEST_L1_FINALITY.commitEventDepth,
  ),
  /** The per-block declared ex-units budget for normal transactions
   * (`config.block-ex-units-budget.ts`). */
  BLOCK_EX_UNITS_BUDGET: blockExUnitsBudgetConfig,
  COMMIT_MAX_L2_TX_COUNT: cappedPositiveSafeIntegerConfig(
    "COMMIT_MAX_L2_TX_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
    MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
  ),
  COMMIT_MAX_LEDGER_OP_COUNT: cappedPositiveSafeIntegerConfig(
    "COMMIT_MAX_LEDGER_OP_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount,
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOperationCount,
  ),
  COMMIT_MAX_TRANSITION_STEP_COUNT: cappedPositiveSafeIntegerConfig(
    "COMMIT_MAX_TRANSITION_STEP_COUNT",
    MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount,
    MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount,
  ),
  COMMIT_BUILD_COST_MODEL: Config.literal(
    "static",
    "ewma",
  )("COMMIT_BUILD_COST_MODEL").pipe(Config.withDefault("static")),
  COMMIT_BUILD_EWMA_ALPHA: positiveFiniteNumberConfig(
    "COMMIT_BUILD_EWMA_ALPHA",
    0.2,
  ).pipe(
    Config.mapAttempt((value) => {
      if (value > 1) {
        throw new Error("COMMIT_BUILD_EWMA_ALPHA must be <= 1");
      }
      return value;
    }),
  ),
  COMMIT_BUILD_EWMA_SAFETY_FACTOR: positiveFiniteNumberConfig(
    "COMMIT_BUILD_EWMA_SAFETY_FACTOR",
    1.5,
  ),
}).pipe(
  Config.mapAttempt(
    ({ STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: renew, ...settings }) => {
      const ttl = settings.STATE_QUEUE_MUTATION_LEASE_TTL_MS;
      const renewIntervalMs = Option.getOrElse(renew, () =>
        Math.min(60 * 1000, Math.max(1, Math.floor(ttl / 3))),
      );
      if (renewIntervalMs >= ttl) {
        throw new Error(
          "STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS must be less than STATE_QUEUE_MUTATION_LEASE_TTL_MS",
        );
      }
      return {
        ...settings,
        STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: renewIntervalMs,
      };
    },
  ),
);
