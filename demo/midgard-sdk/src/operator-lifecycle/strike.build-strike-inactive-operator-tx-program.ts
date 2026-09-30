import { type BuildTxWithRedeemer, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { SCHEDULER_ASSET_NAME, SchedulerError } from "../scheduler.js";
import { completeOptionsWithLocalEval } from "../tx-completion.js";
import { requireOwnSpendPurpose } from "../tx-context-redeemer.js";
import {
  buildStrikeInactiveOperatorTx,
  deriveStrikeLayout,
  encodeActiveOperatorStrikeRedeemer,
  encodeSchedulerStrikeRedeemer,
} from "./strike.derive-strike-layout.js";
import {
  type BuildStrikeInactiveOperatorTxConfig,
  encodeStrikeDatums,
  failScheduler,
  schedulerError,
  type StrikeInactiveOperatorLayout,
  type StrikeInactiveOperatorTxResult,
} from "./strike.plan-inactivity-takeover.js";

/**
 * Builds the unsigned strike-and-takeover transaction: it spends the scheduler
 * and the inactive operator's active node, reproduces both (the node with one
 * more strike, its bond and lovelace untouched), and appoints the next
 * operator for a shift starting at the validity range's inclusive upper bound.
 */
export const buildStrikeInactiveOperatorTxProgram = (
  config: BuildStrikeInactiveOperatorTxConfig,
): Effect.Effect<StrikeInactiveOperatorTxResult, SchedulerError> =>
  Effect.gen(function* () {
    const encoded = yield* Effect.try({
      try: () => encodeStrikeDatums(config),
      catch: (cause) =>
        schedulerError("Failed to encode inactivity strike datums", cause),
    });
    const schedulerWitnessUnit = toUnit(
      config.scheduler.policyId,
      SCHEDULER_ASSET_NAME,
    );
    let layout: StrikeInactiveOperatorLayout | undefined;
    let schedulerRedeemerCbor: string | undefined;
    let activeOperatorRedeemerCbor: string | undefined;
    let callbackCount = 0;

    const resolveLayout = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): StrikeInactiveOperatorLayout => {
      callbackCount += 1;
      const resolved = deriveStrikeLayout({
        config,
        ctx,
        refreshedSchedulerDatumCbor: encoded.refreshedSchedulerDatumCbor,
        struckNodeDatumCbor: encoded.struckNodeDatumCbor,
        schedulerWitnessUnit,
      });
      layout = resolved;
      return resolved;
    };
    const requireStable = (
      previous: string | undefined,
      next: string,
      label: string,
    ): string => {
      if (previous !== undefined && previous !== next) {
        throw schedulerError(
          `BuildTxWithRedeemer resolved inconsistent ${label} redeemers`,
          {
            callback_count: callbackCount.toString(),
            previous_redeemer_cbor: previous,
            next_redeemer_cbor: next,
          },
        );
      }
      return next;
    };
    const schedulerRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.schedulerInput,
        "inactivity strike scheduler",
      );
      const next = encodeSchedulerStrikeRedeemer(config, resolveLayout(ctx));
      schedulerRedeemerCbor = requireStable(
        schedulerRedeemerCbor,
        next,
        "inactivity strike scheduler",
      );
      return next;
    }) satisfies BuildTxWithRedeemer;
    const activeOperatorRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.skippedOperatorNode.utxo,
        "inactivity strike active node",
      );
      const next = encodeActiveOperatorStrikeRedeemer(
        config,
        resolveLayout(ctx),
      );
      activeOperatorRedeemerCbor = requireStable(
        activeOperatorRedeemerCbor,
        next,
        "inactivity strike active node",
      );
      return next;
    }) satisfies BuildTxWithRedeemer;

    yield* Effect.tryPromise({
      try: () =>
        buildStrikeInactiveOperatorTx(config, encoded, {
          scheduler: schedulerRedeemer,
          activeOperators: activeOperatorRedeemer,
        }).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(
          `Failed to build inactivity strike tx: ${String(cause)}`,
          cause,
        ),
    });
    if (
      layout === undefined ||
      schedulerRedeemerCbor === undefined ||
      activeOperatorRedeemerCbor === undefined
    ) {
      return yield* failScheduler(
        "BuildTxWithRedeemer did not resolve both inactivity strike redeemers",
        `callback_count=${callbackCount.toString()}`,
      );
    }
    const resolvedLayout = layout;
    const resolvedSchedulerRedeemerCbor = schedulerRedeemerCbor;
    const resolvedActiveOperatorRedeemerCbor = activeOperatorRedeemerCbor;
    const tx = yield* Effect.tryPromise({
      try: () =>
        buildStrikeInactiveOperatorTx(config, encoded, {
          scheduler: resolvedSchedulerRedeemerCbor,
          activeOperators: resolvedActiveOperatorRedeemerCbor,
        }).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(
          `Failed to rebuild inactivity strike tx: ${String(cause)}`,
          cause,
        ),
    });
    return {
      tx,
      layout: resolvedLayout,
      struckInactivityStrikes: encoded.struckInactivityStrikes,
    };
  });
