import * as SDK from "@al-ft/midgard-sdk";
import {
  validatorToScriptHash,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  loadPhasMembershipWithdrawalScript,
  phasMembershipRewardAddress,
} from "@/phas-membership.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

export type PhasMembershipRewardRegistrationResult = {
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

const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    const cause = (error as Error & { cause?: unknown }).cause;
    return cause === undefined
      ? `${error.name}: ${error.message}`
      : `${error.name}: ${error.message}; cause=${formatUnknownError(cause)}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

const isAlreadyRegisteredError = (
  error: TxSubmitError,
  scriptHash: string,
): boolean => {
  const message = formatUnknownError(error);
  return (
    message.includes("StakeKeyRegisteredDELEG") && message.includes(scriptHash)
  );
};

export const ensurePhasMembershipRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
): Effect.Effect<
  PhasMembershipRewardRegistrationResult,
  SDK.LucidError | TxConfirmError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to resolve network for PHAS reward registration",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const script = loadPhasMembershipWithdrawalScript();
    const rewardAddress = phasMembershipRewardAddress(network, script);
    const scriptHash = validatorToScriptHash(script);
    const unsignedTx = yield* Effect.tryPromise({
      try: () =>
        lucid.newTx().register.Stake(rewardAddress).complete({
          localUPLCEval: true,
        }),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to build PHAS membership reward-account registration transaction",
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
          `PHAS membership reward account is already registered: scriptHash=${scriptHash},rewardAddress=${rewardAddress}`,
        );
        return {
          status: "already_registered",
          rewardAddress,
          scriptHash,
          txHash: null,
        } satisfies PhasMembershipRewardRegistrationResult;
      }
      return yield* Effect.fail(submitted.left);
    }
    return {
      status: "registration_submitted",
      rewardAddress,
      scriptHash,
      txHash: submitted.right,
    } satisfies PhasMembershipRewardRegistrationResult;
  });
