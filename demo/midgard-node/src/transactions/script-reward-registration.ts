import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
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
      ...Object.values(SDK.requireEventHistoryContracts(contracts)).flatMap(
        ({ list, retirement }) => [list, retirement],
      ),
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

/** Registration precedes root initialization. Explicit funding preserves both
 * declared initialization nonces even when the operator also publishes scripts. */
export const ensureEventHistoryRewardAccountsRegisteredProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
) =>
  Effect.gen(function* () {
    const history = Object.values(SDK.requireEventHistoryContracts(contracts));
    const registrations = yield* Effect.forEach(
      history.flatMap(({ list, retirement }) => [list, retirement]),
      ({ withdrawalScript }) =>
        queryScriptRewardRegistrationProgram(lucid, withdrawalScript),
    );
    const missing = registrations.filter(({ registered }) => !registered);
    if (missing.length === 0) return registrations;
    const tx = yield* Effect.tryPromise({
      try: async () => {
        const inputs = (await lucid.wallet().getUtxos()).filter(
          (utxo) =>
            utxo.scriptRef == null &&
            utxo.datum == null &&
            Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
            !history.some(
              ({ recipe }) =>
                recipe.initializationNonce.transactionId === utxo.txHash &&
                recipe.initializationNonce.outputIndex ===
                  BigInt(utxo.outputIndex),
            ),
        );
        if (inputs.length === 0)
          throw new Error(
            "No plain funding remains after reserving history initialization nonces",
          );
        // Lucid's automatic selection does not fund a batch's certificate
        // deposits. Select enough explicit inputs, without collecting every
        // publication change output in a fragmented wallet.
        const parameters = lucid.config().protocolParameters;
        if (parameters === undefined)
          throw new Error("History registration requires protocol parameters");
        const minimumChange = calculateMinLovelaceFromUTxO(
          parameters.coinsPerUtxoByte,
          {
            txHash: "00".repeat(32),
            outputIndex: 0,
            address: await lucid.wallet().address(),
            assets: { lovelace: 0xffffffffffffffffn },
          },
        );
        const budget =
          BigInt(missing.length) * parameters.keyDeposit +
          BigInt(parameters.minFeeA) * BigInt(parameters.maxTxSize) +
          BigInt(parameters.minFeeB) +
          minimumChange;
        const selected = [];
        let total = 0n;
        for (const input of inputs.sort((left, right) => {
          const delta =
            (right.assets.lovelace ?? 0n) - (left.assets.lovelace ?? 0n);
          return delta === 0n
            ? left.txHash.localeCompare(right.txHash) ||
                left.outputIndex - right.outputIndex
            : delta > 0n
              ? 1
              : -1;
        })) {
          selected.push(input);
          total += input.assets.lovelace ?? 0n;
          if (total >= budget) break;
        }
        if (total < budget)
          throw new Error(
            "Insufficient unreserved funding for history observer deposits and fees",
          );
        const completed = await missing
          .reduce(
            (builder, { rewardAddress }) =>
              builder.register.Stake(rewardAddress),
            lucid.newTx().collectFrom(selected),
          )
          .complete({
            coinSelection: false,
            presetWalletInputs: selected,
            localUPLCEval: true,
          });
        const allowed = new Set(
          inputs.map((input) => `${input.txHash}#${input.outputIndex}`),
        );
        const consumed = completed.toTransaction().body().inputs();
        for (let index = 0; index < consumed.len(); index += 1) {
          const input = consumed.get(index);
          if (
            !allowed.has(`${input.transaction_id().to_hex()}#${input.index()}`)
          )
            throw new Error(
              "History registration selected a reserved initialization input",
            );
        }
        return completed;
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to build event history observer registrations",
          cause,
        }),
    });
    yield* handleSignSubmit(lucid, tx);
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
