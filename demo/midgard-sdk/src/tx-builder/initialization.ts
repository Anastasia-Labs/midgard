import { Effect } from "effect";
import {
  LucidEvolution,
  TxBuilder,
  UTxO,
  Script,
} from "@lucid-evolution/lucid";
import { HubOracle, FraudProofCatalogue, Scheduler } from "./index.js";
import { initLinkedListTxBuilder } from "./linked-list/init.js";

export type InitializationParams = {
  nonceUtxo: UTxO;
  genesisTime: bigint;
  initialOperator: string;
  hubOracle: HubOracle.InitParams;
  scheduler: Omit<Scheduler.InitParams, "operator" | "startTime"> & {
    policyId: string;
    address: string;
    mintScript: Script;
  };
  fraudProofCatalogue: FraudProofCatalogue.InitParams;
  // All the linked lists
  stateQueue: { policyId: string; address: string; mintScript: Script };
  settlementQueue: { policyId: string; address: string; mintScript: Script };
  registeredOperators: {
    policyId: string;
    address: string;
    mintScript: Script;
  };
  activeOperators: { policyId: string; address: string; mintScript: Script };
  retiredOperators: { policyId: string; address: string; mintScript: Script };
};

export const initTxBuilder = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    // 1. Start with nonce UTxO
    let tx = lucid.newTx().collectFrom([params.nonceUtxo]);

    // 2. Initialize hub oracle
    const hubOracleTx = HubOracle.initTxBuilder(lucid, params.hubOracle);

    // 3. Initialize state queue (with ConfirmedState data) ← USE IT HERE
    const stateQueueTx = yield* initLinkedListTxBuilder(lucid, {
      policyId: params.stateQueue.policyId,
      address: params.stateQueue.address,
      mintScript: params.stateQueue.mintScript,
      data: {
        header_hash: "00".repeat(28),
        prev_header_hash: "00".repeat(28),
        utxo_root: "00".repeat(32),
        start_time: params.genesisTime,
        end_time: params.genesisTime,
        protocol_version: 0n,
      },
    });

    // 5. Initialize registered operators (empty data)
    const registeredOperatorsTx = yield* initLinkedListTxBuilder(lucid, {
      policyId: params.registeredOperators.policyId,
      address: params.registeredOperators.address,
      mintScript: params.registeredOperators.mintScript,
      data: null,
    });

    // 6. Initialize active operators (empty data)
    const activeOperatorsTx = yield* initLinkedListTxBuilder(lucid, {
      policyId: params.activeOperators.policyId,
      address: params.activeOperators.address,
      mintScript: params.activeOperators.mintScript,
      data: null,
    });

    // 7. Initialize retired operators (empty data)
    const retiredOperatorsTx = yield* initLinkedListTxBuilder(lucid, {
      policyId: params.retiredOperators.policyId,
      address: params.retiredOperators.address,
      mintScript: params.retiredOperators.mintScript,
      data: null,
    });

    // 8. Initialize scheduler
    const schedulerTx = Scheduler.initTxBuilder(lucid, {
      policyId: params.scheduler.policyId,
      address: params.scheduler.address,
      mintScript: params.scheduler.mintScript,
      operator: params.initialOperator,
      startTime: params.genesisTime,
    });

    // 10. Initialize fraud proof catalogue
    const fraudProofCatalogueTx = FraudProofCatalogue.initTxBuilder(
      lucid,
      params.fraudProofCatalogue,
    );

    // 11. Compose everything into ONE transaction
    return tx
      .compose(hubOracleTx)
      .compose(stateQueueTx) 
      .compose(registeredOperatorsTx) // ←
      .compose(activeOperatorsTx) // ←
      .compose(retiredOperatorsTx) // ←
      .compose(schedulerTx)
      .compose(fraudProofCatalogueTx);
  });
