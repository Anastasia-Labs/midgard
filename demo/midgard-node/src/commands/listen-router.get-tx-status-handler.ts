import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect, Ref } from "effect";

import {
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../database/index.js";
import { Globals } from "../services/index.js";
import { parseAddressArgument, parseTxOutRefCborHex } from "./command-utils.js";
import { failWith500 } from "./listen-response.js";
import { parseFixedHexParam } from "./listen-router.get-tx-handler.js";
import {
  UTXO_ENDPOINT,
  UTXOS_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import {
  errorMessage,
  TX_STATUS_ENDPOINT,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import { resolveTxStatus } from "./tx-status.js";
import { readTxStatusMergeEvidence } from "./tx-status-merge-evidence.js";
import * as UtxosCommand from "./utxos.js";

/**
 * `GET /utxos`: returns spendable mempool-ledger UTxOs for an address, less
 * the outputs a pending withdrawal names (admission refuses those as inputs).
 */
export const getUtxosHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${UTXOS_ENDPOINT} - Invalid address type: ${String(addr)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${String(addr)}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);

    const utxosWithAddress =
      yield* MempoolLedgerDB.retrieveSpendableByAddress(address);
    const response = UtxosCommand.encodeStoredUtxos(utxosWithAddress);

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /utxo`: returns one spendable mempool-ledger UTxO by raw TxOutRef CBOR
 * hex.
 */
export const getUtxoHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const rawTxOutRef = params["txOutRef"];

  let txOutRef: Buffer;
  try {
    txOutRef = parseTxOutRefCborHex(rawTxOutRef, "txOutRef");
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `GET /${UTXO_ENDPOINT} - invalid txOutRef: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram([txOutRef]);
  if (matched.length === 0) {
    return yield* HttpServerResponse.json(
      { error: `UTxO not found for txOutRef ${txOutRef.toString("hex")}` },
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json({
    utxo: UtxosCommand.encodeStoredUtxo(matched[0]),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", UTXO_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXO_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `POST /utxos?by-outrefs`: returns spendable mempool-ledger UTxOs for a
 * requested list of `txHash#outputIndex` identifiers.
 */
export const postUtxosByTxOutRefsHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const params = yield* ParsedSearchParams;

  try {
    UtxosCommand.requireByOutRefsSelector(params);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - missing selector: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  let txOutRefs: readonly Buffer[];
  try {
    txOutRefs = UtxosCommand.parseTxOutRefsRequest(parsedBody.right);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram(txOutRefs);
  return yield* HttpServerResponse.json({
    utxos: UtxosCommand.encodeStoredUtxos(matched),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /tx-status`: resolves the node's canonical status for a tx hash.
 */
export const getTxStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${String(txHashParam)}` },
      { status: 400 },
    );
  }

  const globals = yield* Globals;
  const rejected = yield* TxRejectionsDB.retrieveByTxId(txHashBytes);
  const admission = yield* TxAdmissionsDB.getByTxId(txHashBytes);
  const inImmutable = yield* ImmutableDB.retrieveTxCborsByHashes([txHashBytes]);
  const inMempool = yield* MempoolDB.retrieveTxCborsByHashes([txHashBytes]);
  const inProcessedMempool = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
    txHashBytes,
  ]);

  const headerEvidence = yield* readTxStatusMergeEvidence([txHashBytes]);
  const resolved = resolveTxStatus({
    txIdHex: txHashParam as string,
    ...headerEvidence.get(txHashBytes.toString("hex")),
    rejection:
      rejected.length > 0
        ? {
            rejectCode: rejected[0].reject_code,
            rejectDetail: rejected[0].reject_detail,
            createdAtIso: rejected[0].created_at.toISOString(),
          }
        : null,
    admissionStatus: admission?.status ?? null,
    inImmutable: inImmutable.length > 0,
    inMempool: inMempool.length > 0,
    inProcessedMempool: inProcessedMempool.length > 0,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
  });

  if (resolved.status === "not_found") {
    return yield* HttpServerResponse.json(resolved, { status: 404 });
  }

  return yield* HttpServerResponse.json(resolved);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "GET",
      TX_STATUS_ENDPOINT,
      e.cause,
      "merge evidence query failed",
    ),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_STATUS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

export type TxStatusBatchRejectionRow = {
  readonly tx_hash: string;
  readonly reject_code: string;
  readonly reject_detail: string | null;
  readonly created_at: Date;
};

export type TxStatusBatchAdmissionRow = {
  readonly tx_hash: string;
  readonly status: TxAdmissionsDB.Status;
};

export type TxStatusBatchMembershipRow = {
  readonly tx_hash: string;
};

export type TxStatusBatchHeaderRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
};
