import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { journaledIntent, openPlan } from "../services/intent-journal.js";
import type { WalletViewUnavailable } from "../services/intent-journal.wallet-view.js";
import { deploymentInitializationNonceOutRefKeys } from "./reference-scripts.deployment-initialization-nonces.js";
import { utxoOutRefKey } from "./reference-scripts.fetch-reference-script-utxos-program.js";
import { handleSignSubmit } from "./utils.js";
import { buildRewardRegistrationsTx } from "./utils.reward-registration-funding.js";
import {
  readSelectedWalletView,
  requireWalletViewInputs,
} from "./utils.wallet-view.js";

const fundingFailure = (cause: WalletViewUnavailable) =>
  new SDK.LucidError({
    message:
      "Failed to read the wallet view to fund reward-account registrations",
    cause,
  });

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
      contracts.fraudProofContracts.validationTraceDispute.threadExpiry,
      ...Object.values(contracts.fraudProofContracts.minAda.yields),
      contracts.chunkedVerify,
      contracts.pexcludes,
      contracts.reserve,
      ...Object.values(SDK.requireEventHistoryContracts(contracts)).flatMap(
        ({ list, retirement }) => [list, retirement],
      ),
    ];
    const scripts = new Map<string, Script>();
    for (const { withdrawalScript: script } of validators) {
      scripts.set(validatorToScriptHash(script), script);
    }
    // S5: the plan opens before the registration reads the batches rest on.
    const plan = yield* openPlan;
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
    // amortize confirmation across independent roles; each batch selects its
    // own inputs for its deposits (`buildRewardRegistrationsTx`). These
    // registrations follow protocol initialization, which has consumed the
    // deployment's initialization nonces, so none is reserved here.
    for (let offset = 0; offset < missing.length; offset += 32) {
      const batch = missing.slice(offset, offset + 32);
      const view = yield* readSelectedWalletView(lucid).pipe(
        Effect.mapError(fundingFailure),
      );
      const tx = yield* Effect.tryPromise({
        try: () =>
          buildRewardRegistrationsTx(
            lucid,
            view,
            batch.map(({ rewardAddress }) => rewardAddress),
            "the runtime reward-account registrations",
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to build runtime reward-account registrations",
            cause,
          }),
      });
      const txHash = yield* handleSignSubmit(
        lucid,
        tx,
        journaledIntent(
          "script_reward_registration",
          `script_reward_registration:runtime:${batch[0]!.scriptHash}+${batch.length.toString()}`,
          plan,
        ),
      );
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

/**
 * Registration precedes root initialization, funded from the reference-script
 * wallet (`initialization.fetch-protocol-deployment-status.ts`). Its view is
 * the funding, tokens included (their change keeps its minimum ADA), less
 * three kinds of output: the deployment's initialization nonces
 * (`deploymentInitializationNonceOutRefKeys`), which the init consumes; and,
 * through `buildRewardRegistrationsTx`, the reference-script outputs, which
 * may be the deployment's publications at that same address, and
 * datum-bearing outputs, which are not plain funds.
 */
export const ensureEventHistoryRewardAccountsRegisteredProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.gen(function* () {
    const history = Object.values(SDK.requireEventHistoryContracts(contracts));
    // S5: the plan opens before the registration and wallet reads.
    const plan = yield* openPlan;
    const registrations = yield* Effect.forEach(
      history.flatMap(({ list, retirement }) => [list, retirement]),
      ({ withdrawalScript }) =>
        queryScriptRewardRegistrationProgram(lucid, withdrawalScript),
    );
    const missing = registrations.filter(({ registered }) => !registered);
    if (missing.length === 0) return registrations;
    const view = yield* readSelectedWalletView(lucid).pipe(
      Effect.mapError(fundingFailure),
    );
    const reservedNonces = deploymentInitializationNonceOutRefKeys(contracts);
    const tx = yield* Effect.tryPromise({
      try: () =>
        buildRewardRegistrationsTx(
          lucid,
          {
            address: view.address,
            utxos: view.utxos.filter(
              (utxo) => !reservedNonces.has(utxoOutRefKey(utxo)),
            ),
          },
          missing.map(({ rewardAddress }) => rewardAddress),
          "the history observer registrations",
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to build event history observer registrations",
          cause,
        }),
    });
    yield* handleSignSubmit(
      lucid,
      tx,
      journaledIntent(
        "script_reward_registration",
        `script_reward_registration:event_history:${missing.length.toString()}`,
        plan,
      ),
    );
    return yield* Effect.tryPromise({
      try: async () => {
        for (const { rewardAddress } of missing)
          if (!(await lucid.rewardAccountAt(rewardAddress)).registered)
            throw new Error(
              `Confirmed history registration is not visible: ${rewardAddress}`,
            );
        return registrations.map((registration) => ({
          ...registration,
          registered: true,
        }));
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to confirm history observer registrations",
          cause,
        }),
    });
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
    // S5: the plan opens before the registration read it is built on.
    const plan = yield* openPlan;
    const before = yield* queryScriptRewardRegistrationProgram(lucid, script);
    if (before.registered) return { ...before, txHash: null };
    // The registration selects its own inputs for its deposit
    // (`buildRewardRegistrationsTx`); an empty view is refused by name first.
    const funding = yield* readSelectedWalletView(lucid).pipe(
      Effect.flatMap((view) =>
        requireWalletViewInputs(
          view,
          "the script reward-account registration",
        ).pipe(Effect.map((utxos) => ({ address: view.address, utxos }))),
      ),
      Effect.mapError(fundingFailure),
    );
    const tx = yield* Effect.tryPromise({
      try: () =>
        buildRewardRegistrationsTx(
          lucid,
          funding,
          [before.rewardAddress],
          "the script reward-account registration",
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to complete script reward-account registration",
          cause,
        }),
    });
    const txHash = yield* handleSignSubmit(
      lucid,
      tx,
      journaledIntent(
        "script_reward_registration",
        `script_reward_registration:${before.scriptHash}`,
        plan,
      ),
    );
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
