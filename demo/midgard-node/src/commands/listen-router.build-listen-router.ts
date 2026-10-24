import {
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import {
  HttpRouter,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect } from "effect";

import { requestTxQueueProcessorWakeup } from "../fibers/index.js";
import {
  AdmissionWriter,
  BatchSql,
  Database,
  HttpReadSql,
  MempoolLedgerCache,
  ValidationPool,
  WriteBehind,
} from "../services/index.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { getAdminSessionHandler } from "./listen-admin-access.js";
import { withHttpIngress } from "./listen-http-ingress.js";
import { withHttpReadSql } from "./listen-http-read.js";
import { failWith500 } from "./listen-response.js";
import {
  getBlockHandler,
  getCommitEndpoint,
  getInitHandler,
  getPipelineStatusHandler,
  getProtocolInfoHandler,
} from "./listen-router.get-pipeline-status-handler.js";
import { getReadinessHandler } from "./listen-router.get-readiness-handler.js";
import {
  getLogBlocksDBHandler,
  getMergeHandler,
  getStateQueueHandler,
  getTxsOfAddressHandler,
  OPERATOR_STATUS_ENDPOINT,
} from "./listen-router.get-state-queue-handler.js";
import {
  getTxHandler,
  withAdminAccess,
} from "./listen-router.get-tx-handler.js";
import {
  getTxStatusHandler,
  getUtxoHandler,
  getUtxosHandler,
  postUtxosByTxOutRefsHandler,
} from "./listen-router.get-tx-status-handler.js";
import {
  ADDRESS_HISTORY_ENDPOINT,
  BLOCK_ENDPOINT,
  COMMIT_ENDPOINT,
  DEPOSIT_BUILD_ENDPOINT,
  INIT_ENDPOINT,
  MERGE_ENDPOINT,
  SUBMIT_ENDPOINT,
  TX_ENDPOINT,
  UTXO_ENDPOINT,
  UTXOS_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import {
  getLogGlobalsHandler,
  getOperatorStatusHandler,
  postDepositBuildHandler,
} from "./listen-router.post-deposit-build-handler.js";
import { postSubmitHandler } from "./listen-router.post-submit-handler.js";
import {
  postTxEvaluateHandler,
  TX_EVALUATE_ENDPOINT,
} from "./listen-router.post-tx-evaluate-handler.js";
import {
  getDepositStatusHandler,
  getHealthHandler,
  postTxStatusBatchHandler,
} from "./listen-router.post-tx-status-batch-handler.js";
import {
  DEPOSIT_STATUS_ENDPOINT,
  HEALTH_ENDPOINT,
  PIPELINE_STATUS_ENDPOINT,
  PROTOCOL_INFO_ENDPOINT,
  STATE_QUEUE_ENDPOINT,
  TX_STATUS_ENDPOINT,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import { READINESS_ENDPOINT } from "./readiness.js";

/**
 * Focused router used by the admission integration harness. Keeping this as a
 * real HttpRouter (rather than calling the handler as a function) makes route,
 * request-body, status-code, and response-body contract executable while
 * allowing the harness to hold validation wakeups at a deterministic boundary.
 */
export const buildSubmitRouter = <R>(
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  withMonitoring?: boolean,
  consensusProfile: MidgardConsensusProfile = MIDGARD_CONSENSUS_PROFILE,
) =>
  HttpRouter.empty.pipe(
    HttpRouter.post(
      `/${SUBMIT_ENDPOINT}`,
      postSubmitHandler(withMonitoring, wakeTxQueueProcessor).pipe(
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile,
          }),
        ),
      ),
    ),
    withHttpIngress,
  );

/**
 * Builds the full HTTP router for the node command server.
 */
export const buildListenRouter = (
  withMonitoring?: boolean,
): Effect.Effect<
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
        withHttpReadSql(getReadinessHandler),
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
          withHttpReadSql(getStateQueueHandler),
        ),
      ),
      HttpRouter.post(
        `/logBlocksDB`,
        withAdminAccess("logBlocksDB", withHttpReadSql(getLogBlocksDBHandler)),
      ),
      HttpRouter.post(
        `/logGlobals`,
        withAdminAccess("logGlobals", getLogGlobalsHandler),
      ),
      HttpRouter.get(
        `/${OPERATOR_STATUS_ENDPOINT}`,
        withAdminAccess(
          OPERATOR_STATUS_ENDPOINT,
          withHttpReadSql(getOperatorStatusHandler, 10_000),
        ),
      ),
      HttpRouter.post(
        `/${UTXOS_ENDPOINT}`,
        withHttpReadSql(postUtxosByTxOutRefsHandler),
      ),
      HttpRouter.post(
        `/${DEPOSIT_BUILD_ENDPOINT}`,
        withHttpReadSql(postDepositBuildHandler, 10_000),
      ),
      HttpRouter.post(
        `/${SUBMIT_ENDPOINT}`,
        postSubmitHandler(withMonitoring, requestTxQueueProcessorWakeup),
      ),
      HttpRouter.post(`/${TX_EVALUATE_ENDPOINT}`, postTxEvaluateHandler),
    )
    .pipe(
      Effect.catchAllCause((cause) => failWith500("GET", "router", cause)),
      withHttpIngress,
    );
