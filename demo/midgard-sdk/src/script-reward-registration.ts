import {
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import { LucidError, UnspecifiedNetworkError } from "./errors.js";
import {
  completeTxWithLocalUPLCEvalProgram,
  EMPTY_WALLET_INPUTS_CAUSE,
  emptyWalletInputsRefusal,
} from "./tx-completion.js";

/**
 * Register the credential only; semantic yield execution occurs on withdrawal.
 *
 * With `walletInputs` (the submitting wallet's coins as the caller's view holds
 * them), coin selection and collateral use exactly those and the provider is
 * never read.
 */
export const buildScriptRewardRegistrationTxProgram = (
  lucid: LucidEvolution,
  script: Script,
  options: { readonly walletInputs?: readonly UTxO[] } = {},
) =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new UnspecifiedNetworkError({
          message:
            "Cannot register a script reward account without a configured network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const emptyInputs = emptyWalletInputsRefusal(
      options.walletInputs,
      "the script reward-account registration",
    );
    if (emptyInputs !== null) {
      return yield* Effect.fail(
        new LucidError({
          message: emptyInputs,
          cause: EMPTY_WALLET_INPUTS_CAUSE,
        }),
      );
    }
    const rewardAddress = scriptRewardAddress(network, script);
    const tx = yield* completeTxWithLocalUPLCEvalProgram(
      lucid.newTx().register.Stake(rewardAddress),
      (cause) =>
        new LucidError({
          message: "Failed to complete script reward-account registration",
          cause,
        }),
      options.walletInputs,
    );
    return { tx, rewardAddress, scriptHash: validatorToScriptHash(script) };
  });
