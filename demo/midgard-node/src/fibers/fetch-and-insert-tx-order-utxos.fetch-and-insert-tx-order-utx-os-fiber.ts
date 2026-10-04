import { type MidgardForcedTxAdmissionStopped } from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule } from "effect";

import { DatabaseError } from "../database/utils/common.js";
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
  SDK.LucidError | DatabaseError | MidgardForcedTxAdmissionStopped,
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
