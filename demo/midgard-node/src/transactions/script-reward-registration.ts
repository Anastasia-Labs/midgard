import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { journaledIntent, openPlan } from "../services/intent-journal.js";
import type { WalletViewUnavailable } from "../services/intent-journal.wallet-view.js";
import { tokenChangeMinLovelace } from "./operators/funding-preflight.js";
import { deploymentInitializationNonceOutRefKeys } from "./reference-scripts.deployment-initialization-nonces.js";
import { utxoOutRefKey } from "./reference-scripts.fetch-reference-script-utxos-program.js";
import { handleSignSubmit } from "./utils.js";
import {
  readSelectedWalletView,
  readSelectedWalletViewInputs,
} from "./utils.wallet-view.js";

const fundingFailure = (cause: WalletViewUnavailable) =>
  new SDK.LucidError({
    message:
      "Failed to read the wallet view to fund reward-account registrations",
    cause,
  });

/**
 * The history observer registration found too little to fund it once the
 * deployment's initialization nonces, its reference-script outputs and any
 * datum-bearing output are left out of the funding wallet's view.
 */
export class EventHistoryRegistrationUnfunded extends Error {
  override readonly name = "EventHistoryRegistrationUnfunded";
}

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
    // amortize confirmation across independent roles;
    // Lucid still enforces the live transaction-size and funding limits.
    for (let offset = 0; offset < missing.length; offset += 32) {
      const batch = missing.slice(offset, offset + 32);
      const presetWalletInputs = yield* readSelectedWalletViewInputs(
        lucid,
        "runtime reward-account registrations",
      ).pipe(Effect.mapError(fundingFailure));
      const tx = yield* Effect.tryPromise({
        try: () =>
          batch
            .reduce(
              (builder, { rewardAddress }) =>
                builder.register.Stake(rewardAddress),
              lucid.newTx(),
            )
            .complete({ localUPLCEval: true, presetWalletInputs }),
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
 * (`deploymentInitializationNonceOutRefKeys`), which the init consumes; the
 * reference-script outputs, which may be the deployment's publications at
 * that same address; and datum-bearing outputs, which are not plain funds.
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
      try: async () => {
        const inputs = view.utxos.filter(
          (utxo) =>
            utxo.scriptRef == null &&
            utxo.datum == null &&
            utxo.datumHash == null &&
            !reservedNonces.has(utxoOutRefKey(utxo)),
        );
        if (inputs.length === 0)
          throw new EventHistoryRegistrationUnfunded(
            "No funding remains after reserving the initialization nonces and the reference-script outputs",
          );
        // Lucid's coin selection (0.8.3) leaves certificate deposits out of
        // what it must cover, so this path selects its inputs itself: enough
        // for the deposits, the fee and the change, without collecting every
        // output of a fragmented wallet. Revisit this if a later Lucid counts
        // the deposits.
        const parameters = lucid.config().protocolParameters;
        if (parameters === undefined)
          throw new Error("History registration requires protocol parameters");
        const plainChange = calculateMinLovelaceFromUTxO(
          parameters.coinsPerUtxoByte,
          {
            txHash: "00".repeat(32),
            outputIndex: 0,
            address: view.address,
            assets: { lovelace: 0xffffffffffffffffn },
          },
        );
        const depositsAndFee =
          BigInt(missing.length) * parameters.keyDeposit +
          BigInt(parameters.minFeeA) * BigInt(parameters.maxTxSize) +
          BigInt(parameters.minFeeB);
        // The change returns every selected token, so it needs their
        // minimum ADA rather than a plain output's.
        const budgetFor = (selection: readonly UTxO[]) => {
          const tokenChange = tokenChangeMinLovelace(
            lucid,
            view.address,
            selection,
          );
          return (
            depositsAndFee +
            (tokenChange > plainChange ? tokenChange : plainChange)
          );
        };
        const selected: UTxO[] = [];
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
          if (total >= budgetFor(selected)) break;
        }
        if (total < budgetFor(selected))
          throw new EventHistoryRegistrationUnfunded(
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
        const allowed = new Set(selected.map(utxoOutRefKey));
        const consumed = completed.toTransaction().body().inputs();
        for (let index = 0; index < consumed.len(); index += 1) {
          const input = consumed.get(index);
          if (
            !allowed.has(`${input.transaction_id().to_hex()}#${input.index()}`)
          )
            throw new Error(
              "History registration spent an input it did not select",
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
    const built = yield* SDK.buildScriptRewardRegistrationTxProgram(
      lucid,
      script,
      {
        walletInputs: yield* readSelectedWalletViewInputs(
          lucid,
          "the script reward-account registration",
        ).pipe(Effect.mapError(fundingFailure)),
      },
    );
    const txHash = yield* handleSignSubmit(
      lucid,
      built.tx,
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
