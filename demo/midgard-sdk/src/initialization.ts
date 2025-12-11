import { Effect } from "effect";
import {
  Data,
  LucidEvolution,
  TxBuilder,
  makeReturn,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import {
  AuthenticatedValidator,
  Bech32DeserializationError,
  LucidError,
  MintingValidatorInfo,
} from "./common.js";
import { incompleteHubOracleInitTxProgram } from "./hub-oracle.js";
import {
  incompleteSchedulerInitTxProgram,
  SchedulerDatum,
} from "./scheduler.js";
import { incompleteFraudProofInitTxProgram } from "./fraud-proof/catalogue.js";
import { ConfirmedState } from "./ledger-state.js";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";
import {
  GENESIS_HASH_28,
  GENESIS_HASH_32,
  INITIAL_PROTOCOL_VERSION,
} from "./constants.js";
import { StateQueueRedeemer } from "./state-queue.js";
import { ActiveOperatorMintRedeemer } from "./active-operators.js";
import { RegisteredOperatorMintRedeemer } from "./registered-operators.js";
import { RetiredOperatorMintRedeemer } from "./retired-operators.js";

export type InitializationParams = {
  genesisTime: bigint;
  initialOperator: string;
  hubOracle: MintingValidatorInfo;
  stateQueue: AuthenticatedValidator;
  registeredOperators: AuthenticatedValidator;
  activeOperators: AuthenticatedValidator;
  retiredOperators: AuthenticatedValidator;
  scheduler: AuthenticatedValidator;
  fraudProofCatalogue: AuthenticatedValidator;
  fraudProof: AuthenticatedValidator;
};

export const incompleteInitializationTxProgram = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<TxBuilder, LucidError | Bech32DeserializationError> =>
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

    const hubOracleTx = yield* incompleteHubOracleInitTxProgram(
      lucid,
      params.hubOracle,
      {
        registeredOperators: params.registeredOperators,
        activeOperators: params.activeOperators,
        retiredOperators: params.retiredOperators,
        scheduler: params.scheduler,
        stateQueue: params.stateQueue,
        fraudProofCatalogue: params.fraudProofCatalogue,
        fraudProof: params.fraudProof,
      },
    );

    const stateQueueData: ConfirmedState = {
      headerHash: GENESIS_HASH_28,
      prevHeaderHash: GENESIS_HASH_28,
      utxoRoot: GENESIS_HASH_32,
      startTime: params.genesisTime,
      endTime: params.genesisTime,
      protocolVersion: INITIAL_PROTOCOL_VERSION,
    };

    const stateQueueTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.stateQueue,
      data: Data.castTo(stateQueueData, ConfirmedState),
      redeemer: Data.to("Init", StateQueueRedeemer),
    });

    const registeredOperatorsTx = yield* incompleteInitLinkedListTxProgram(
      lucid,
      {
        validator: params.registeredOperators,
        redeemer: Data.to("Init", RegisteredOperatorMintRedeemer),
      },
    );

    const activeOperatorsTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.activeOperators,
      redeemer: Data.to("Init", ActiveOperatorMintRedeemer),
    });

    const retiredOperatorsTx = yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.retiredOperators,
      redeemer: Data.to("Init", RetiredOperatorMintRedeemer),
    });

    const schedulerGenesisData: SchedulerDatum = {
      operator: params.initialOperator,
      shiftStart: params.genesisTime,
    };

    const schedulerTx = incompleteSchedulerInitTxProgram(lucid, {
      validator: params.scheduler,
      datum: schedulerGenesisData,
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
): Effect.Effect<TxSignBuilder, LucidError | Bech32DeserializationError> =>
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
