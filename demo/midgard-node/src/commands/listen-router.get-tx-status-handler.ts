import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect, Ref } from "effect";

import {
  PublicHttpReadLimitError,
  requirePublicReadPayload,
} from "../database/http-read-byte-budget.js";
import { publicUtxosByOutRefs } from "../database/http-read-lookups.js";
import { spendableUtxoPage } from "../database/http-read-pages.js";
import { readHttpTxStatusFacts } from "../database/http-tx-status-facts.js";
import { TxAdmissionsDB } from "../database/index.js";
import { Globals } from "../services/index.js";
import { parseAddressArgument, parseTxOutRefCborHex } from "./command-utils.js";
import { publicReadDataLimitResponse } from "./listen-http-read.js";
import {
  includePrivateHttpDiagnosticDetails,
  publicTxStatusPresentation,
} from "./listen-public-error-presentation.js";
import { encodeReadPage, parseReadPage } from "./listen-read-page.js";
import { failWith500 } from "./listen-response.js";
import { parseFixedHexParam } from "./listen-router.get-tx-handler.js";
import {
  UTXO_ENDPOINT,
  UTXOS_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import { TX_STATUS_ENDPOINT } from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
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
      `GET /${UTXOS_ENDPOINT} - Invalid address type: [redacted]`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: [redacted]` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);

    const utxosWithAddress = yield* spendableUtxoPage(
      address,
      parseReadPage(params, 38),
    );
    const page = encodeReadPage(
      utxosWithAddress,
      parseReadPage(params, 38),
      (row) => row.outref,
      (row) =>
        UtxosCommand.encodeStoredUtxo({
          outref: row.outref,
          output: requirePublicReadPayload(row.output),
        }),
    );
    const response = page.entries;

    yield* Effect.logInfo(`Found ${response.length} UTxOs for [redacted]`);
    return yield* HttpServerResponse.json({
      utxos: response,
      nextCursor: page.nextCursor,
    });
  } catch (_error) {
    if (_error instanceof PublicHttpReadLimitError)
      return yield* publicReadDataLimitResponse();
    yield* Effect.logInfo(`Invalid address: [redacted]`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: [redacted]` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    e.message === "E_PUBLIC_READ_MIGRATING"
      ? HttpServerResponse.json(
          {
            error: "Pending withdrawal read authority is being populated",
            error_code: "E_PUBLIC_READ_MIGRATING",
          },
          { status: 503, headers: { "retry-after": "5" } },
        )
      : failWith500("GET", UTXOS_ENDPOINT, e),
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
  } catch {
    const message = "Invalid request";
    yield* Effect.logInfo(
      `GET /${UTXO_ENDPOINT} - invalid txOutRef: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* publicUtxosByOutRefs([txOutRef]);
  if (matched.length === 0) {
    return yield* HttpServerResponse.json(
      { error: `UTxO not found for txOutRef [redacted]` },
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json({
    utxo: UtxosCommand.encodeStoredUtxo(matched[0]),
  });
}).pipe(
  Effect.catchTag("PublicHttpReadLimitError", publicReadDataLimitResponse),
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
  } catch {
    const message = "Invalid request";
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
  } catch {
    const message = "Invalid request";
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* publicUtxosByOutRefs(txOutRefs);
  return yield* HttpServerResponse.json({
    utxos: UtxosCommand.encodeStoredUtxos(matched),
  });
}).pipe(
  Effect.catchTag("PublicHttpReadLimitError", publicReadDataLimitResponse),
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
      { error: `Invalid transaction hash: [redacted]` },
      { status: 400 },
    );
  }

  const globals = yield* Globals;
  const facts = yield* readHttpTxStatusFacts(
    txHashBytes,
    includePrivateHttpDiagnosticDetails(),
  );

  const headerEvidence = yield* readTxStatusMergeEvidence([txHashBytes]);
  const resolved = resolveTxStatus({
    txIdHex: txHashParam as string,
    ...headerEvidence.get(txHashBytes.toString("hex")),
    rejection:
      facts.reject_code !== null && facts.created_at !== null
        ? {
            rejectCode: facts.reject_code,
            rejectDetail: facts.reject_detail,
            createdAtIso: facts.created_at.toISOString(),
          }
        : null,
    admissionStatus: facts.admission_status,
    inImmutable: facts.in_immutable,
    inMempool: facts.in_mempool,
    inProcessedMempool: facts.in_processed_mempool,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
  });

  if (resolved.status === "not_found") {
    return yield* HttpServerResponse.json(
      publicTxStatusPresentation(resolved),
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json(publicTxStatusPresentation(resolved));
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
