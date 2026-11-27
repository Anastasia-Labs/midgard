import "./register-active-operator.to-lifecycle-result.js";

import type * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { IntentJournal } from "../services/intent-journal.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import { configuredOperatorEconomicsProgram } from "./operators/exit.js";
import {
  OperatorFundingShortfall,
  OperatorWalletInputsEmpty,
} from "./operators/funding-preflight.js";
import {
  type ActivationTxHashes,
  type DeregistrationTxHashes,
  type RegistrationTxHashes,
  toActivationResult,
} from "./register-active-operator.fetch-hub-oracle-ref-input.js";
import { operatorLifecycleProgram } from "./register-active-operator.operator-lifecycle-program.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.await-required-output-visibility.js";
import { TxExpiredError } from "./utils.tx-expiry.js";

export const registerAndActivateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  requiredBondLovelace: bigint,
  referenceScriptsLucid?: LucidEvolution,
  referenceScriptsAddress?: string,
): Effect.Effect<
  ActivationTxHashes,
  | SDKStateQueue.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | OperatorWalletInputsEmpty
  | TxConfirmError
  | TxExpiredError
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
  | SDKStateQueue.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | OperatorWalletInputsEmpty
  | TxConfirmError
  | TxExpiredError
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
  | SDKStateQueue.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | OperatorWalletInputsEmpty
  | TxConfirmError
  | TxExpiredError
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
  | SDKStateQueue.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | OperatorWalletInputsEmpty
  | TxConfirmError
  | TxExpiredError
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
  | SDKStateQueue.StateQueueError
  | SDK.LucidError
  | OperatorFundingShortfall
  | OperatorWalletInputsEmpty
  | TxConfirmError
  | TxExpiredError
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
