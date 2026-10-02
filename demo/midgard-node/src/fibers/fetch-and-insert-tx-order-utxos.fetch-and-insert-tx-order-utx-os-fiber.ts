import { Effect, Schedule } from "effect";

import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { fetchAndInsertTxOrderUTxOs } from "./fetch-and-insert-tx-order-utxos.tx-order-utx-oto-entry.js";
import { repeatVisibleUserEventIngestionFiber } from "./user-event-ingestion.js";

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | MidgardContracts
  | ContractDeploymentIdentity
  | Lucid
  | Database
  | NodeConfig
  | Globals
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "Fetch and insert TxOrderUTxOs.",
    spanName: "fetch-and-insert-tx-order-utxos-fiber",
    action: fetchAndInsertTxOrderUTxOs,
  });
