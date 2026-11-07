import {
  HttpRouter,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { type HttpBodyError } from "@effect/platform/HttpBody";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect } from "effect";

import { requestTxQueueProcessorWakeup } from "../../fibers/index.js";
import { PEER_INVENTORY_PATH, PEER_TXS_PATH } from "../../l2-peer/protocol.js";
import {
  AdmissionWriter,
  BatchSql,
  ContractDeploymentIdentity,
  Database,
  Globals,
  HttpReadSql,
  Lucid,
  MempoolLedgerCache,
  MidgardContracts,
  NodeConfig,
  ValidationPool,
  WriteBehind,
} from "../../services/index.js";
import { type IntentJournal } from "../../services/intent-journal.js";
import {
  getAdminSessionHandler,
  withAdminAccess,
} from "../listen-admin-access.js";
import { withHttpIngress } from "../listen-http-ingress.js";
import { withHttpReadSql } from "../listen-http-read.js";
import { failWith500 } from "../listen-response.js";
import { READINESS_ENDPOINT } from "../readiness.js";
import { getDepositStatusHandler } from "./deposits.js";
import { postTxEvaluateHandler } from "./evaluate.js";
import {
  getCommitEndpoint,
  getInitHandler,
  getMergeHandler,
} from "./operator-actions.js";
import {
  getLogBlocksDBHandler,
  getLogGlobalsHandler,
  getOperatorStatusHandler,
  getStateQueueHandler,
} from "./operator-inspection.js";
import { postPeerInventoryHandler, postPeerTxsHandler } from "./peer-routes.js";
import { getPipelineStatusHandler } from "./pipeline-status.js";
import {
  getBlockHandler,
  getHealthHandler,
  getProtocolInfoHandler,
} from "./public-info.js";
import { getReadinessHandler } from "./readiness.js";
import {
  ADDRESS_HISTORY_ENDPOINT,
  BLOCK_ENDPOINT,
  COMMIT_ENDPOINT,
  DEPOSIT_STATUS_ENDPOINT,
  HEALTH_ENDPOINT,
  INIT_ENDPOINT,
  MERGE_ENDPOINT,
  OPERATOR_STATUS_ENDPOINT,
  PIPELINE_STATUS_ENDPOINT,
  PROTOCOL_INFO_ENDPOINT,
  STATE_QUEUE_ENDPOINT,
  TX_ENDPOINT,
  TX_EVALUATE_ENDPOINT,
  TX_STATUS_ENDPOINT,
  UTXO_ENDPOINT,
  UTXOS_ENDPOINT,
} from "./routes.js";
import {
  getTxHandler,
  getTxStatusHandler,
  postTxStatusBatchHandler,
} from "./transaction-status.js";
import {
  getTxsOfAddressHandler,
  getUtxoHandler,
  getUtxosHandler,
  postUtxosByTxOutRefsHandler,
} from "./utxo-reads.js";

/**
 * Builds the HTTP router for the node command server: every route except
 * `POST /submit`, which the raw handler in `submit.ts` answers.
 */
export const buildListenRouter = (): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | HttpReadSql
  | AdmissionWriter
  | BatchSql
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
  | Lucid
  | NodeConfig
  | MidgardContracts
  | ContractDeploymentIdentity
  | SqlClient
  | HttpServerRequest.HttpServerRequest
  | Globals
  | IntentJournal
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${HEALTH_ENDPOINT}`, getHealthHandler),
      HttpRouter.get(
        `/${READINESS_ENDPOINT}`,
        withHttpReadSql(getReadinessHandler, 2_000, "operator"),
      ),
      HttpRouter.get(
        `/${PIPELINE_STATUS_ENDPOINT}`,
        withHttpReadSql(getPipelineStatusHandler),
      ),
      HttpRouter.get(`/${PROTOCOL_INFO_ENDPOINT}`, getProtocolInfoHandler),
      HttpRouter.get(`/${TX_ENDPOINT}`, withHttpReadSql(getTxHandler)),
      HttpRouter.get(
        `/${TX_STATUS_ENDPOINT}`,
        withHttpReadSql(getTxStatusHandler),
      ),
      HttpRouter.post(
        `/${TX_STATUS_ENDPOINT}`,
        withHttpReadSql(postTxStatusBatchHandler),
      ),
      HttpRouter.get(
        `/${DEPOSIT_STATUS_ENDPOINT}`,
        withHttpReadSql(getDepositStatusHandler),
      ),
      HttpRouter.get(
        `/${ADDRESS_HISTORY_ENDPOINT}`,
        withHttpReadSql(getTxsOfAddressHandler),
      ),
      HttpRouter.get(`/${UTXO_ENDPOINT}`, withHttpReadSql(getUtxoHandler)),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, withHttpReadSql(getUtxosHandler)),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, withHttpReadSql(getBlockHandler)),
    )
    .pipe(
      HttpRouter.post(
        `/${INIT_ENDPOINT}`,
        withAdminAccess(INIT_ENDPOINT, getInitHandler),
      ),
      HttpRouter.post(
        `/${COMMIT_ENDPOINT}`,
        withAdminAccess(COMMIT_ENDPOINT, getCommitEndpoint),
      ),
      HttpRouter.post(
        `/${MERGE_ENDPOINT}`,
        withAdminAccess(MERGE_ENDPOINT, getMergeHandler),
      ),
      HttpRouter.get(
        "/admin/session",
        withAdminAccess("admin/session", getAdminSessionHandler),
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_ENDPOINT}`,
        withAdminAccess(
          STATE_QUEUE_ENDPOINT,
          withHttpReadSql(getStateQueueHandler, 2_000, "operator"),
        ),
      ),
      HttpRouter.post(
        `/logBlocksDB`,
        withAdminAccess(
          "logBlocksDB",
          withHttpReadSql(getLogBlocksDBHandler, 2_000, "operator"),
        ),
      ),
      HttpRouter.post(
        `/logGlobals`,
        withAdminAccess("logGlobals", getLogGlobalsHandler),
      ),
      HttpRouter.get(
        `/${OPERATOR_STATUS_ENDPOINT}`,
        withAdminAccess(
          OPERATOR_STATUS_ENDPOINT,
          withHttpReadSql(getOperatorStatusHandler, 10_000, "operator"),
        ),
      ),
      HttpRouter.post(
        `/${UTXOS_ENDPOINT}`,
        withHttpReadSql(postUtxosByTxOutRefsHandler),
      ),
      HttpRouter.post(`/${TX_EVALUATE_ENDPOINT}`, postTxEvaluateHandler),
      HttpRouter.post(
        PEER_TXS_PATH,
        postPeerTxsHandler(requestTxQueueProcessorWakeup),
      ),
      HttpRouter.post(PEER_INVENTORY_PATH, postPeerInventoryHandler),
    )
    .pipe(
      Effect.catchAllCause((cause) => failWith500("GET", "router", cause)),
      withHttpIngress,
    );
