import { Effect } from "effect";
import {
  LucidEvolution,
  TxBuilder,
  UTxO,
  Script,
  makeReturn,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { LucidError } from "./common.js";
import {
  HubOracleDatum,
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
import { EmptyRootData, initLinkedListProgram } from "./linked-list.js";

export type InitializationParams = {
  nonceUtxo: UTxO;
  genesisTime: bigint;
  initialOperator: string;
  hubOracle: HubOracleInitParams & {
    policyId: string;
    address: string;
    mintScript: Script;
    datum: HubOracleDatum;
  };
  scheduler: Omit<SchedulerInitParams, "operator" | "startTime"> & {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: EmptyRootData;
    data: EmptyRootData;
  };
  fraudProofCatalogue: FraudProofCatalogueInitParams;
  // All the linked lists
  stateQueue: {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: ConfirmedState;
    data: ConfirmedState;
  };
  settlementQueue: {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: EmptyRootData;
    data: EmptyRootData;
  };
  registeredOperators: {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: EmptyRootData;
    data: EmptyRootData;
  };
  activeOperators: {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: EmptyRootData;
    data: EmptyRootData;
  };
  retiredOperators: {
    policyId: string;
    address: string;
    mintScript: Script;
    dataSchema: EmptyRootData;
    data: EmptyRootData;
  };
};

export const initializationTxProgram = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    // 1. Start with nonce UTxO
    let tx = lucid.newTx().collectFrom([params.nonceUtxo]);

    // 2. Initialize hub oracle
    const hubOracleTx = incompleteHubOracleInitTxProgram(
      lucid,
      params.hubOracle,
    );

    // 3. Initialize state queue (with ConfirmedState data)
    const stateQueueTx = yield* initLinkedListProgram(lucid, {
      policyId: params.stateQueue.policyId,
      address: params.stateQueue.address,
      mintScript: params.stateQueue.mintScript,
      dataSchema: params.stateQueue.dataSchema,
      data: params.stateQueue.data,
    });

    // 4. Initialize settlement queue
    const settlementQueueTx = yield* initLinkedListProgram(lucid, {
      policyId: params.settlementQueue.policyId,
      address: params.settlementQueue.address,
      mintScript: params.settlementQueue.mintScript,
      dataSchema: params.settlementQueue.dataSchema, // ← Add schema
      data: [],
    });

    // 5. Initialize registered operators
    const registeredOperatorsTx = yield* initLinkedListProgram(lucid, {
      policyId: params.registeredOperators.policyId,
      address: params.registeredOperators.address,
      mintScript: params.registeredOperators.mintScript,
      dataSchema: params.registeredOperators.dataSchema,
      data: params.registeredOperators.data,
    });

    // 6. Initialize active operators
    const activeOperatorsTx = yield* initLinkedListProgram(lucid, {
      policyId: params.activeOperators.policyId,
      address: params.activeOperators.address,
      mintScript: params.activeOperators.mintScript,
      dataSchema: params.activeOperators.dataSchema,
      data: params.activeOperators.data,
    });

    // 7. Initialize retired operators
    const retiredOperatorsTx = yield* initLinkedListProgram(lucid, {
      policyId: params.retiredOperators.policyId,
      address: params.retiredOperators.address,
      mintScript: params.retiredOperators.mintScript,
      dataSchema: params.retiredOperators.dataSchema,
      data: params.retiredOperators.data,
    });

    // 8. Initialize scheduler
    const schedulerTx = incompleteSchedulerInitTxProgram(lucid, {
      policyId: params.scheduler.policyId,
      address: params.scheduler.address,
      mintScript: params.scheduler.mintScript,
      operator: params.initialOperator,
      startTime: params.genesisTime,
    });

    // 9. Initialize fraud proof catalogue
    const fraudProofCatalogueTx = incompleteFraudProofInitTxProgram(
      lucid,
      params.fraudProofCatalogue,
    );

    // 10. Compose everything into ONE transaction
    return tx
      .compose(hubOracleTx)
      .compose(stateQueueTx)
      .compose(settlementQueueTx)
      .compose(registeredOperatorsTx)
      .compose(activeOperatorsTx)
      .compose(retiredOperatorsTx)
      .compose(schedulerTx)
      .compose(fraudProofCatalogueTx);
  });

export const unsignedInitializationProgram = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const commitTx = yield* initializationTxProgram(lucid, initParams);
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
 * This includes: Hub Oracle, State Queue, Settlement Queue,
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
  makeReturn(unsignedInitializationProgram(lucid, initParams)).unsafeRun();
