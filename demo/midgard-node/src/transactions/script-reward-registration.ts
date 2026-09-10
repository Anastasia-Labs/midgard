import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { handleSignSubmit } from "./utils.js";

/** Register runtime rewarding roles after availability and PHAS initialization. */
export const ensureRuntimeRewardAccountsRegisteredProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.gen(function* () {
    const validators = [
      ...Object.values(contracts.stateQueue.yields),
      ...Object.values(contracts.fraudProofContracts.transitionTrace.yields),
      ...Object.values(
        contracts.fraudProofContracts.validationTraceDispute.yields,
      ),
      ...Object.values(contracts.fraudProofContracts.minAda.yields),
      contracts.chunkedVerify,
      contracts.pexcludes,
      contracts.reserve,
    ];
    const scripts = new Map<string, Script>();
    for (const { withdrawalScript: script } of validators) {
      scripts.set(validatorToScriptHash(script), script);
    }
    return yield* Effect.forEach(
      [...scripts.values()],
      (script) => ensureScriptRewardAccountRegisteredProgram(lucid, script),
      { concurrency: 1 },
    );
  });

export const queryScriptRewardRegistrationProgram = (
  lucid: LucidEvolution,
  script: Script,
) =>
  Effect.tryPromise({
    try: async () => {
      const network = lucid.config().network;
      if (network === undefined)
        throw new Error("Lucid network is required for script registration");
      const rewardAddress = SDK.scriptRewardAddress(network, script);
      return {
        rewardAddress,
        scriptHash: validatorToScriptHash(script),
        registered: (await lucid.rewardAccountAt(rewardAddress)).registered,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query script reward-account registration",
        cause,
      }),
  });

/** Query before submission and confirm visibility before reporting readiness. */
export const ensureScriptRewardAccountRegisteredProgram = (
  lucid: LucidEvolution,
  script: Script,
) =>
  Effect.gen(function* () {
    const before = yield* queryScriptRewardRegistrationProgram(lucid, script);
    if (before.registered) return { ...before, txHash: null };
    const built = yield* SDK.buildScriptRewardRegistrationTxProgram(
      lucid,
      script,
    );
    const txHash = yield* handleSignSubmit(lucid, built.tx);
    const after = yield* queryScriptRewardRegistrationProgram(lucid, script);
    if (!after.registered) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Confirmed script registration is not visible; initialization must be retried",
          cause: `scriptHash=${after.scriptHash},txHash=${txHash}`,
        }),
      );
    }
    return { ...after, txHash };
  });
