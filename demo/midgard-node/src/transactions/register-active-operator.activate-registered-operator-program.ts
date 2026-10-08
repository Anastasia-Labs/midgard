import "./register-active-operator.to-lifecycle-result.js";

import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { Lucid, MidgardContracts } from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { configuredOperatorEconomicsProgram } from "./operators/exit.js";
import { OperatorFundingShortfall } from "./operators/funding-preflight.js";
import {
  type ActivationTxHashes,
  type DeregistrationTxHashes,
  type RegistrationTxHashes,
  toActivationResult,
} from "./register-active-operator.fetch-hub-oracle-ref-input.js";
import { operatorLifecycleProgram } from "./register-active-operator.operator-lifecycle-program.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

export const registerAndActivateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-and-activate",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.andThen(({ registerTxHash, activateTxHash }) =>
      toActivationResult({
        registerTxHash,
        activateTxHash,
      }),
    ),
  );

export const registerOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  RegistrationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "register-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ registerTxHash }) => ({
      registerTxHash,
    })),
  );

export const activateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "activate-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ registerTxHash, activateTxHash }) => ({
      registerTxHash,
      activateTxHash,
    })),
  );

/**
 * Activate an eligible registered operator from any funded wallet. The
 * registered node's activation time must already have elapsed unless the
 * active set is empty; the caller's wallet pays only the fee.
 */
export const activateRegisteredOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  operatorKeyHash: string,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "activate-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
    { operatorKeyHash },
  ).pipe(
    Effect.map(({ registerTxHash, activateTxHash }) => ({
      registerTxHash,
      activateTxHash,
    })),
  );

export const deregisterOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  DeregistrationTxHashes,
  | SDK.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
> =>
  operatorLifecycleProgram(
    lucid,
    contracts,
    requiredBondLovelace,
    "deregister-only",
    referenceScriptsLucid,
    referenceScriptsAddress,
  ).pipe(
    Effect.map(({ deregisterTxHash }) => ({
      deregisterTxHash,
    })),
  );

const configuredReleaseRequiredBondProgram = Effect.map(
  configuredOperatorEconomicsProgram,
  (economics) => economics.requiredBondLovelace,
);

export const program = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const requiredBondLovelace = yield* configuredReleaseRequiredBondProgram;
  yield* lucidService.switchToOperatorsMainWallet;
  return yield* registerAndActivateOperatorProgram(
    lucidService.api,
    contracts,
    requiredBondLovelace,
    lucidService.referenceScriptsApi,
    lucidService.referenceScriptsAddress,
  );
});

export const activateProgram = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const requiredBondLovelace = yield* configuredReleaseRequiredBondProgram;
  yield* lucidService.switchToOperatorsMainWallet;
  return yield* activateOperatorProgram(
    lucidService.api,
    contracts,
    requiredBondLovelace,
    lucidService.referenceScriptsApi,
    lucidService.referenceScriptsAddress,
  );
});
