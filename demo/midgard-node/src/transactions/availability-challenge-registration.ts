import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

import { journaledIntent, openPlan } from "../services/intent-journal.js";
import { handleSignSubmit } from "./utils.js";
import { readSelectedWalletViewInputs } from "./utils.wallet-view.js";

/** An availability-challenge yield's reward account is not registered: a
 * deployment verdict, which reading again does not change. */
export class AvailabilityRewardAccountUnregisteredError extends Data.TaggedError(
  "AvailabilityRewardAccountUnregisteredError",
)<SDK.GenericErrorFields> {}

/** Check ledger readiness without changing deployment or wallet state. */
export const assertAvailabilityChallengeRewardAccountsRegisteredProgram = (
  lucid: Pick<LucidEvolution, "config" | "rewardAccountAt">,
  contracts: Pick<SDK.MidgardValidators, "availabilityChallenge">,
  actions: readonly (keyof SDK.AvailabilityChallengeYieldValidators)[] = [
    "open",
    "settle",
    "close",
    "timeout",
  ],
): Effect.Effect<
  void,
  SDK.StateQueueError | AvailabilityRewardAccountUnregisteredError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Availability challenge reward-account readiness requires a configured network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    for (const action of actions) {
      const validator = contracts.availabilityChallenge.yields[action];
      const rewardAddress = validatorToRewardAddress(
        network,
        validator.withdrawalScript,
      );
      const account = yield* Effect.tryPromise({
        try: () => lucid.rewardAccountAt(rewardAddress),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: `Failed to inspect availability challenge ${action} reward-account readiness`,
            cause,
          }),
      });
      if (!account.registered) {
        return yield* Effect.fail(
          new AvailabilityRewardAccountUnregisteredError({
            message: `Availability challenge ${action} reward account is not registered; complete protocol initialization before using this deployment`,
            cause: `rewardAddress=${rewardAddress},scriptHash=${validator.withdrawalScriptHash}`,
          }),
        );
      }
    }
  });

/** Register each deployed availability yield before any challenge can invoke it. */
export const ensureAvailabilityChallengeRewardAccountsRegisteredProgram = (
  lucid: LucidEvolution,
  contracts: Pick<SDK.MidgardValidators, "availabilityChallenge">,
) =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.UnspecifiedNetworkError({
          message:
            "Cannot register availability challenge yields without a configured network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const registrations: {
      action: string;
      rewardAddress: string;
      scriptHash: string;
      txHash: string | null;
    }[] = [];
    for (const [action, validator] of Object.entries(
      contracts.availabilityChallenge.yields,
    )) {
      const rewardAddress = validatorToRewardAddress(
        network,
        validator.withdrawalScript,
      );
      const query = () =>
        Effect.tryPromise({
          try: () => lucid.rewardAccountAt(rewardAddress),
          catch: (cause) =>
            new SDK.LucidError({
              message: `Failed to query availability challenge ${action} reward account`,
              cause,
            }),
        });
      let txHash: string | null = null;
      // S5: the plan opens before the registration read it is built on.
      const plan = yield* openPlan;
      if (!(yield* query()).registered) {
        const presetWalletInputs = yield* readSelectedWalletViewInputs(
          lucid,
          `availability challenge ${action} reward registration`,
        ).pipe(
          Effect.mapError(
            (cause) =>
              new SDK.LucidError({
                message: `Failed to fund availability challenge ${action} reward registration`,
                cause,
              }),
          ),
        );
        const tx = yield* Effect.tryPromise({
          try: () =>
            lucid
              .newTx()
              .register.Stake(rewardAddress)
              .complete({ localUPLCEval: true, presetWalletInputs }),
          catch: (cause) =>
            new SDK.LucidError({
              message: `Failed to build availability challenge ${action} reward registration`,
              cause,
            }),
        });
        // Requery on submission failure too: another initializer may register this
        // credential concurrently. Only authoritative ledger registration recovers it.
        const submitted = yield* Effect.either(
          handleSignSubmit(
            lucid,
            tx,
            journaledIntent(
              "script_reward_registration",
              `script_reward_registration:availability_challenge:${action}`,
              plan,
            ),
          ),
        );
        if (submitted._tag === "Left") {
          if (!(yield* query()).registered)
            return yield* Effect.fail(submitted.left);
        } else {
          txHash = submitted.right;
          if (!(yield* query()).registered) {
            return yield* Effect.fail(
              new SDK.LucidError({
                message: `Availability challenge ${action} reward registration is not visible after confirmation`,
                cause: txHash,
              }),
            );
          }
        }
      }
      registrations.push({
        action,
        rewardAddress,
        scriptHash: validator.withdrawalScriptHash,
        txHash,
      });
    }
    return registrations;
  });
