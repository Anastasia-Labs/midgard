/**
 * `POST /peer/txs` and `POST /peer/inventory`: other active operators share
 * the L2 transactions they accepted. Each request must be signed by an
 * operator key in this node's on-chain active operator set. A peer's
 * transactions pass the same checks as `/submit` and are admitted with
 * source `peer`; this node's own validation decides their fate.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { Effect, Option, Ref } from "effect";

import { L2PeerShareDB } from "../../database/index.js";
import {
  decodePeerList,
  encodePeerList,
  PEER_INVENTORY_MAX_BODY_BYTES,
  PEER_INVENTORY_MAX_IDS,
  PEER_INVENTORY_PATH,
  PEER_MAX_BODY_BYTES,
  PEER_MEDIA_TYPE,
  PEER_TXS_MAX_BODY_BYTES,
  PEER_TXS_PATH,
  verifyPeerRequest,
} from "../../l2-peer/protocol.js";
import {
  AdmissionWriter,
  Globals,
  MidgardContracts,
  NodeConfig,
} from "../../services/index.js";
import { failWith500 } from "../listen-response.js";
import {
  requestMediaType,
  resolveSubmitIngressReservation,
  withSubmitIngressPermit,
} from "./submission-capacity.js";
import { SUBMIT_RETRY_AFTER_SECONDS } from "./submit.js";
import {
  admitCheckedTransaction,
  checkSubmittedTransaction,
} from "./transaction-admission.js";

const UNAVAILABLE = {
  status: 503,
  headers: { "retry-after": SUBMIT_RETRY_AFTER_SECONDS.toString() },
};

const refuse = (status: number, error: string) =>
  HttpServerResponse.unsafeJson({ error }, { status });

/**
 * Reads a peer body of at most `maxBytes` while holding a permit of the
 * peer ingress pool, then checks its signature and the signer's place in the
 * active set. The pool is apart from `/submit`'s and as many requests deep,
 * so peers and users never hold each other's permits. Gives the body and the
 * signer's key hash; left is the refusal.
 */
const readAuthorizedBody = (path: string, maxBytes: number) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const request = yield* HttpServerRequest.HttpServerRequest;
    if (requestMediaType(request.headers["content-type"]) !== PEER_MEDIA_TYPE)
      return yield* Effect.fail(
        refuse(415, `Content-Type must be ${PEER_MEDIA_TYPE}`),
      );
    const reservation = resolveSubmitIngressReservation(
      request.headers["content-length"],
      maxBytes,
    );
    if (reservation.kind !== "ready")
      return yield* Effect.fail(
        reservation.kind === "too_large"
          ? refuse(413, `Peer body exceeds ${maxBytes.toString()} bytes`)
          : refuse(400, "Invalid Content-Length"),
      );
    const body = yield* withSubmitIngressPermit({
      pool: "peer",
      maxConcurrency: nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY,
      maxInFlightBytes:
        nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY * PEER_MAX_BODY_BYTES,
      permitBytes: reservation.permitBytes,
      effect: HttpIncomingMessage.withMaxBodySize(
        request.arrayBuffer,
        Option.some(maxBytes),
      ).pipe(
        Effect.mapError(() =>
          refuse(413, `Peer body exceeds ${maxBytes.toString()} bytes`),
        ),
      ),
    });
    if (Option.isNone(body))
      return yield* Effect.fail(
        HttpServerResponse.unsafeJson(
          { error: "Peer ingress capacity is full; retry later" },
          UNAVAILABLE,
        ),
      );
    const bytes = Buffer.from(body.value);
    const contracts = yield* MidgardContracts;
    const keyHash = verifyPeerRequest(
      request.headers,
      path,
      contracts.hubOracle.policyId,
      bytes,
    );
    if (keyHash === null)
      return yield* Effect.fail(
        refuse(401, "Missing or invalid peer signature"),
      );
    const published = yield* Ref.get((yield* Globals).OPERATOR_SET);
    const active =
      published?.set.active.some(
        (node) => SDK.nodeKeyHex(node.datum.key)?.toLowerCase() === keyHash,
      ) ?? false;
    if (!active)
      return yield* Effect.fail(
        refuse(403, "Signer is not an active operator here"),
      );
    return { bytes, keyHash };
  });

const decodeOr400 = (bytes: Uint8Array, maxItems: number) =>
  Effect.try({
    try: () => decodePeerList(bytes, maxItems),
    catch: () => refuse(400, "Body must be a canonical CBOR list of bytes"),
  });

/** Admits a signed batch of transactions, in body order. */
export const postPeerTxsHandler = <R>(
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const { bytes, keyHash } = yield* readAuthorizedBody(
      PEER_TXS_PATH,
      PEER_TXS_MAX_BODY_BYTES,
    );
    const txs = yield* decodeOr400(bytes, PEER_INVENTORY_MAX_IDS);
    const admittedIds: string[] = [];
    let admitted = 0;
    let duplicates = 0;
    let refused = 0;
    for (const tx of txs) {
      const checked = checkSubmittedTransaction(
        tx,
        nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
      );
      if (!checked.ok) {
        refused++;
        continue;
      }
      const outcome = yield* admitCheckedTransaction({
        normalized: checked.normalized,
        submitSource: "peer",
        wakeTxQueueProcessor,
      }).pipe(
        Effect.map((result) => result.kind),
        // A peer tx is never refused for its inputs; the input arm is here
        // only because admission's refusal type carries it.
        Effect.catchTags({
          TxAdmissionConflictError: () => Effect.succeed("conflict" as const),
          AdmissionInputSpentError: () => Effect.succeed("conflict" as const),
        }),
      );
      if (outcome === "new") {
        admitted++;
        admittedIds.push(checked.normalized.txIdHex);
      } else if (outcome === "duplicate") duplicates++;
      else refused++;
    }
    // The signer is not stored with the rows: this line is what ties a row
    // the tx-queue processor later forgets to the operator who sent it.
    if (admitted > 0)
      yield* Effect.logInfo(
        `Admitted ${admitted.toString()} L2 txs from peer operator ${keyHash}`,
      ).pipe(
        Effect.annotateLogs({ peerOperator: keyHash, txIds: admittedIds }),
      );
    return yield* HttpServerResponse.json({ admitted, duplicates, refused });
  }).pipe(
    Effect.catchIf(HttpServerResponse.isServerResponse, Effect.succeed),
    Effect.catchTags({
      TxAdmissionBacklogFullError: () =>
        HttpServerResponse.json(
          { error: "Durable admission backlog is full" },
          UNAVAILABLE,
        ),
      AdmissionWriterFullError: () =>
        HttpServerResponse.json(
          { error: "Durable admission writer is full" },
          UNAVAILABLE,
        ),
      AdmissionWriterStalledError: () =>
        HttpServerResponse.json(
          { error: "Durable admission writes are stalled" },
          UNAVAILABLE,
        ),
      AdmissionWriterShutdownError: () =>
        HttpServerResponse.json(
          { error: "Durable admission writer is unavailable" },
          UNAVAILABLE,
        ),
    }),
  );

/** Answers which of the offered transaction ids this node has never seen. */
export const postPeerInventoryHandler = Effect.gen(function* () {
  const { bytes } = yield* readAuthorizedBody(
    PEER_INVENTORY_PATH,
    PEER_INVENTORY_MAX_BODY_BYTES,
  );
  const ids = yield* decodeOr400(bytes, PEER_INVENTORY_MAX_IDS);
  if (ids.some((id) => id.length !== 32))
    return refuse(400, "Every id must be 32 bytes");
  // A tx still pending in the admission writer has no row yet but is not
  // missing: leave it out so the peer does not push it again.
  const writer = yield* Effect.serviceOption(AdmissionWriter);
  const missing = yield* L2PeerShareDB.missingAdmissionIds(
    Option.isSome(writer)
      ? ids.filter((id) => writer.value.pendingSince(id) === undefined)
      : ids,
  );
  return HttpServerResponse.uint8Array(encodePeerList(missing), {
    contentType: PEER_MEDIA_TYPE,
  });
}).pipe(
  Effect.catchIf(HttpServerResponse.isServerResponse, Effect.succeed),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500("POST", "peer/inventory", e.cause, "inventory read failed"),
  ),
);
