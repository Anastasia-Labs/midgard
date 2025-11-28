import { Effect } from "effect";
import {
  Data,
  LucidEvolution,
  TxBuilder,
  makeReturn,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { LucidError } from "./common.js";
import {
  HubOracleInitParams,
  incompleteHubOracleInitTxProgram,
} from "./hub-oracle.js";
import {
  incompleteSchedulerInitTxProgram,
  SchedulerInitParams,
} from "./scheduler.js";
import {
  FraudProofCatalogueInitParams,
  incompleteFraudProofInitTxProgram,
} from "./fraud-proof/catalogue.js";
import { ConfirmedState } from "./ledger-state.js";
import {
  EmptyRootData,
  incompleteInitLinkedListTxProgram,
} from "./linked-list.js";
import { StateQueueInitParams } from "./state-queue.js";
import { RegisteredOperatorInitParams } from "./registered-operators.js";
import { ActiveOperatorInitParams } from "./active-operators.js";
import { RetiredOperatorInitParams } from "./retired-operators.js";

export type InitializationParams = {
  genesisTime: bigint;
  initialOperator: string;
  hubOracle: HubOracleInitParams;
  scheduler: SchedulerInitParams;
  fraudProofCatalogue: FraudProofCatalogueInitParams;
  stateQueue: StateQueueInitParams;
  registeredOperators: RegisteredOperatorInitParams;
  activeOperators: ActiveOperatorInitParams;
  retiredOperators: RetiredOperatorInitParams;
};

export const incompleteInitializationTxProgram = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<TxBuilder, LucidError> =>
  Effect.gen(function* () {
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (e) =>
        new LucidError({
          message: "Failed to fetch UTxOs for nonce",
          cause: e,
        }),
    });

    if (utxos.length === 0) {
      return yield* Effect.fail(
        new LucidError({
          message: "No UTxOs available for nonce",
          cause: "Wallet has no UTxOs",
        }),
      );
    }

    const nonceUtxo = utxos[0];
    let tx = lucid.newTx().collectFrom([nonceUtxo]);

    const hubOracleTx = incompleteHubOracleInitTxProgram(
      lucid,
      params.hubOracle,
    );

    const stateQueueTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.stateQueue.validator,
      data: Data.castTo(params.stateQueue.data, ConfirmedState),
    });

    const registeredOperatorsTx = yield* incompleteInitLinkedListTxProgram(
      lucid,
      {
        validator: params.registeredOperators.validator,
        data: Data.castTo(params.registeredOperators.data, EmptyRootData),
      },
    );

    const activeOperatorsTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.activeOperators.validator,
      data: Data.castTo(params.activeOperators.data, EmptyRootData),
    });

    const retiredOperatorsTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.retiredOperators.validator,
      data: Data.castTo(params.retiredOperators.data, EmptyRootData),
    });

    const schedulerTx = incompleteSchedulerInitTxProgram(lucid, {
      validator: params.scheduler.validator,
      operator: params.initialOperator,
      startTime: params.genesisTime,
    });

    const fraudProofCatalogueTx = incompleteFraudProofInitTxProgram(
      lucid,
      params.fraudProofCatalogue,
    );

    return tx
      .compose(hubOracleTx)
      .compose(stateQueueTx)
      .compose(registeredOperatorsTx)
      .compose(activeOperatorsTx)
      .compose(retiredOperatorsTx)
      .compose(schedulerTx)
      .compose(fraudProofCatalogueTx);
  });

export const unsignedInitializationTxProgram = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteInitializationTxProgram(
      lucid,
      initParams,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the init transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for initializing all Midgard contracts.
 * This includes: Hub Oracle, State Queue,
 * Registered/Active/Retired Operators, Scheduler, Escape Hatch,
 * and Fraud Proof Catalogue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedInitializationTx = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedInitializationTxProgram(lucid, initParams)).unsafeRun();
