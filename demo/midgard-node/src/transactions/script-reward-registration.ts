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
    const before = yield* Effect.forEach(
      [...scripts.values()],
      (script) => queryScriptRewardRegistrationProgram(lucid, script),
      { concurrency: 1 },
    );
    const results = new Map(
      before
        .filter(({ registered }) => registered)
        .map((record) => [
          record.scriptHash,
          { ...record, txHash: null as string | null },
        ]),
    );
    const missing = before.filter(({ registered }) => !registered);
    // Registration certificates carry no Plutus witnesses. Bounded batches
    // amortize confirmation and wallet reconciliation across independent roles;
    // Lucid still enforces the live transaction-size and funding limits.
    for (let offset = 0; offset < missing.length; offset += 32) {
      const batch = missing.slice(offset, offset + 32);
      const tx = yield* Effect.tryPromise({
        try: () =>
          batch
            .reduce(
              (builder, { rewardAddress }) =>
                builder.register.Stake(rewardAddress),
              lucid.newTx(),
            )
            .complete({ localUPLCEval: true }),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to build runtime reward-account registrations",
            cause,
          }),
      });
      const txHash = yield* handleSignSubmit(lucid, tx);
      for (const record of batch) {
        const after = yield* queryScriptRewardRegistrationProgram(
          lucid,
          scripts.get(record.scriptHash)!,
        );
        if (!after.registered)
          return yield* Effect.fail(
            new SDK.LucidError({
              message: "Confirmed runtime reward registration is not visible",
              cause: `scriptHash=${record.scriptHash},txHash=${txHash}`,
            }),
          );
        results.set(record.scriptHash, { ...after, txHash });
      }
    }
    return before.map(({ scriptHash }) => results.get(scriptHash)!);
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
