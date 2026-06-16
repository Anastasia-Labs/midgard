import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type TxSignBuilder,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  loadPexcludesExclusionWithdrawalScript,
  pexcludesExclusionRewardAddress,
} from "@/pexcludes-exclusion.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

export type PexcludesExclusionRewardRegistrationResult = {
  readonly rewardAddress: string;
  readonly scriptHash: string;
} & (
  | {
      readonly status: "registration_submitted";
      readonly txHash: string;
    }
  | {
      readonly status: "already_registered";
      readonly txHash: null;
    }
);

const isAlreadyRegisteredError = (
  error: TxSubmitError,
  scriptHash: string,
): boolean => {
  const message = formatUnknownError(error, { includeCause: true });
  return (
    message.includes("StakeKeyRegisteredDELEG") && message.includes(scriptHash)
  );
};

/**
 * Registers the stake credential of the `pexcludes.exclusion.withdraw`
 * validator so the no-input / non-existent-input fault-proof steps can perform
 * their withdraw-zero non-membership checks. Mirrors the PHAS membership
 * registration used by the double-spend (membership) proof path.
 */
export const ensurePexcludesExclusionRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
): Effect.Effect<
  PexcludesExclusionRewardRegistrationResult,
  SDK.LucidError | TxConfirmError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to resolve network for pexcludes reward registration",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const script = loadPexcludesExclusionWithdrawalScript();
    const rewardAddress = pexcludesExclusionRewardAddress(network, script);
    const scriptHash = validatorToScriptHash(script);
    const unsignedTx = yield* Effect.tryPromise({
      try: () =>
        lucid.newTx().register.Stake(rewardAddress).complete({
          localUPLCEval: true,
        }),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to build pexcludes exclusion reward-account registration transaction",
          cause,
        }),
    });
    const submitted = yield* Effect.either(
      handleSignSubmit(lucid, unsignedTx as TxSignBuilder),
    );
    if (submitted._tag === "Left") {
      if (
        submitted.left instanceof TxSubmitError &&
        isAlreadyRegisteredError(submitted.left, scriptHash)
      ) {
        yield* Effect.logInfo(
          `pexcludes exclusion reward account is already registered: scriptHash=${scriptHash},rewardAddress=${rewardAddress}`,
        );
        return {
          status: "already_registered",
          rewardAddress,
          scriptHash,
          txHash: null,
        } satisfies PexcludesExclusionRewardRegistrationResult;
      }
      return yield* Effect.fail(submitted.left);
    }
    return {
      status: "registration_submitted",
      rewardAddress,
      scriptHash,
      txHash: submitted.right,
    } satisfies PexcludesExclusionRewardRegistrationResult;
  });
