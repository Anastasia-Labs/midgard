/** The installed HTTP surface and endpoint names. This module performs no provider reads or operator actions. */

export const OPERATOR_STATUS_ENDPOINT = "operator/status";

export const TX_ENDPOINT: string = "tx";

export const ADDRESS_HISTORY_ENDPOINT: string = "txs";

export const MERGE_ENDPOINT: string = "merge";

export const UTXO_ENDPOINT: string = "utxo";

export const UTXOS_ENDPOINT: string = "utxos";

export const BLOCK_ENDPOINT: string = "block";

export const INIT_ENDPOINT: string = "init";

export const COMMIT_ENDPOINT: string = "commit";

export const SUBMIT_ENDPOINT: string = "submit";

export const TX_EVALUATE_ENDPOINT = "tx/evaluate";

export const STATE_QUEUE_ENDPOINT: string = "stateQueue";

export const TX_STATUS_ENDPOINT: string = "tx-status";

export const PIPELINE_STATUS_ENDPOINT: string = "pipeline-status";

export const DEPOSIT_STATUS_ENDPOINT: string = "deposit-status";

export const PROTOCOL_INFO_ENDPOINT: string = "protocol-info";

export const HEALTH_ENDPOINT: string = "healthz";

/**
 * Installed HTTP surface. CLI-only operator commands are never dispatched over
 * HTTP. `peer` routes take requests signed by an active operator's key.
 */
export const HTTP_ROUTE_ACTIONS = [
  ["GET", "/healthz", "public"],
  ["GET", "/readyz", "public"],
  ["GET", "/pipeline-status", "public"],
  ["GET", "/protocol-info", "public"],
  ["GET", "/tx", "public"],
  ["GET", "/tx-status", "public"],
  ["POST", "/tx-status", "public"],
  ["GET", "/deposit-status", "public"],
  ["GET", "/txs", "public"],
  ["GET", "/utxo", "public"],
  ["GET", "/utxos", "public"],
  ["POST", "/utxos", "public"],
  ["GET", "/block", "public"],
  ["POST", "/submit", "public"],
  ["POST", "/tx/evaluate", "public"],
  ["POST", "/peer/txs", "peer"],
  ["POST", "/peer/inventory", "peer"],
  ["POST", "/init", "admin"],
  ["POST", "/commit", "admin"],
  ["POST", "/merge", "admin"],
  ["GET", "/stateQueue", "admin"],
  ["POST", "/logBlocksDB", "admin"],
  ["POST", "/logGlobals", "admin"],
  ["GET", "/operator/status", "admin"],
  ["GET", "/admin/session", "admin"],
] as const;
