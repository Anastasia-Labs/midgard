import {
  type LucidEvolution,
  type Script,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import { LucidError, UnspecifiedNetworkError } from "./errors.js";
import { completeTxWithLocalUPLCEvalProgram } from "./tx-completion.js";

/** Register the credential only; semantic yield execution occurs on withdrawal. */
export const buildScriptRewardRegistrationTxProgram = (
  lucid: LucidEvolution,
  script: Script,
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
    const rewardAddress = scriptRewardAddress(network, script);
    const tx = yield* completeTxWithLocalUPLCEvalProgram(
      lucid.newTx().register.Stake(rewardAddress),
      (cause) =>
        new LucidError({
          message: "Failed to complete script reward-account registration",
          cause,
        }),
    );
    return { tx, rewardAddress, scriptHash: validatorToScriptHash(script) };
  });
