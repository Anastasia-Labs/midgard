/**
 * `POST /peer/txs` and `POST /peer/inventory`: other active operators share
 * the L2 transactions they accepted. Each request must be signed by an
 * operator key in this node's on-chain active operator set. A peer's
 * transactions pass the same checks as `/submit`, on the submit check
 * workers, and are admitted with source `peer`; this node's own validation
 * decides their fate.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  HttpIncomingMessage,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { Effect, Either, Option, Ref } from "effect";

import { daLocalSigners } from "../../da/local-signers.js";
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
  peerAnswerHeaders,
  type PeerSigner,
  verifyPeerRequest,
} from "../../l2-peer/protocol.js";
import {
  AdmissionWriter,
  Globals,
  MidgardContracts,
  NodeConfig,
} from "../../services/index.js";
import type { SubmitCheckPool } from "../../services/submit-check-pool.js";
import { failWith500 } from "../listen-response.js";
import { admitCheckedTransactions } from "./checked-transaction-admission.js";
import {
  requestMediaType,
  resolveSubmitIngressReservation,
  withSubmitIngressPermit,
} from "./submission-capacity.js";
import { SUBMIT_RETRY_AFTER_SECONDS } from "./submit.js";
import { type CheckedSubmittedTx } from "./transaction-admission.js";

const UNAVAILABLE = {
  status: 503,
  headers: { "retry-after": SUBMIT_RETRY_AFTER_SECONDS.toString() },
};

const refuse = (status: number, error: string) =>
  HttpServerResponse.unsafeJson({ error }, { status });

/**
 * Reads a peer body of at most `maxBytes` while holding a permit of the
 * peer ingress pool, checks its signature and the signer's place in the
 * active set, and runs `use` on the body and the signer's key hash, still
 * holding the permit. The pool is apart from `/submit`'s and as many
 * requests deep, so peers and users never hold each other's permits. The
 * failure is the refusal.
 */
const withAuthorizedBody = <A, R>(
  path: string,
  maxBytes: number,
  use: (body: {
    readonly bytes: Buffer;
    readonly keyHash: string;
  }) => Effect.Effect<A, HttpServerResponse.HttpServerResponse, R>,
) =>
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
    const contracts = yield* MidgardContracts;
    const globals = yield* Globals;
    const result = yield* withSubmitIngressPermit({
      pool: "peer",
      maxConcurrency: nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY,
      maxInFlightBytes:
        nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY * PEER_MAX_BODY_BYTES,
      permitBytes: reservation.permitBytes,
      effect: Effect.gen(function* () {
        const body = yield* HttpIncomingMessage.withMaxBodySize(
          request.arrayBuffer,
          Option.some(maxBytes),
        ).pipe(
          Effect.mapError(() =>
            refuse(413, `Peer body exceeds ${maxBytes.toString()} bytes`),
          ),
        );
        const bytes = Buffer.from(body);
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
        const published = yield* Ref.get(globals.OPERATOR_SET);
        const active =
          published?.set.active.some(
            (node) => SDK.nodeKeyHex(node.datum.key)?.toLowerCase() === keyHash,
          ) ?? false;
        if (!active)
          return yield* Effect.fail(
            refuse(403, "Signer is not an active operator here"),
          );
        return yield* use({ bytes, keyHash });
      }),
    });
    if (Option.isNone(result))
      return yield* Effect.fail(
        HttpServerResponse.unsafeJson(
          { error: "Peer ingress capacity is full; retry later" },
          UNAVAILABLE,
        ),
      );
    return result.value;
  });

const decodeOr400 = (bytes: Uint8Array, maxItems: number) =>
  Effect.try({
    try: () => decodePeerList(bytes, maxItems),
    catch: () => refuse(400, "Body must be a canonical CBOR list of bytes"),
  });

const CAPACITY_REFUSALS = {
  TxAdmissionBacklogFullError: "Durable admission backlog is full",
  AdmissionWriterFullError: "Durable admission writer is full",
  AdmissionWriterStalledError: "Durable admission writes are stalled",
  AdmissionWriterShutdownError: "Durable admission writer is unavailable",
} as const;

/**
 * Admits a signed batch of transactions, in body order: every tx is checked
 * on the submit check workers (`checkPool`; the main thread only waits),
 * then all that pass go to the admission writer in one submission, and the
 * route answers once they are in its pending set (design D3, D9). The
 * checks run under the request's peer permit, not `/submit`'s queue slots.
 *
 * A tx refused for capacity (backlog or writer full, writes stalled, writer
 * stopping), or left unchecked because no check worker answered, makes the
 * answer a 503: the sender backs off and its catch-up offers the txs again,
 * and the inventory leaves out the ones admitted here.
 */
export const postPeerTxsHandler = <R>(
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  checkPool: SubmitCheckPool,
) =>
  Effect.gen(function* () {
    const { txs, outcomes, keyHash } = yield* withAuthorizedBody(
      PEER_TXS_PATH,
      PEER_TXS_MAX_BODY_BYTES,
      ({ bytes, keyHash }) =>
        Effect.gen(function* () {
          const txs = yield* decodeOr400(bytes, PEER_INVENTORY_MAX_IDS);
          // A peer's tx passes the same stateless checks as a user's (design
          // D6): they read no ledger, so a tx the sender admitted passes them
          // here too. The permit stays held until every verdict is in.
          const outcomes = yield* Effect.promise(() =>
            checkPool.checkTransactions(txs),
          ).pipe(Effect.uninterruptible);
          return { txs, outcomes, keyHash };
        }),
    );
    let refused = 0;
    let verifierUnavailable = false;
    let checkUnavailable = false;
    const candidates: CheckedSubmittedTx[] = [];
    for (const [index, outcome] of outcomes.entries()) {
      if (outcome.kind === "unavailable") checkUnavailable = true;
      else if (outcome.kind === "failed") {
        // The check threw on this tx: it is not admitted here, and the
        // rest of the body still is.
        refused++;
        yield* Effect.logError(
          `peer tx ${index.toString()} of ${txs.length.toString()} from ${keyHash}: check failed: ${outcome.detail}`,
        );
      } else if (outcome.check.ok) candidates.push(outcome.check.normalized);
      else {
        refused++;
        if (outcome.check.status === 503) verifierUnavailable = true;
      }
    }
    const results = yield* admitCheckedTransactions({
      candidates,
      submitSource: "peer",
      wakeTxQueueProcessor,
    });
    const admittedIds: string[] = [];
    let duplicates = 0;
    let capacityRefusal: keyof typeof CAPACITY_REFUSALS | undefined;
    for (const [index, result] of results.entries()) {
      if (Either.isRight(result)) {
        if (result.right.kind === "new")
          admittedIds.push(candidates[index]!.txIdHex);
        else duplicates++;
      } else {
        refused++;
        // A peer tx is never refused for its inputs; the input arm is here
        // only because admission's refusal type carries it.
        if (
          result.left._tag !== "TxAdmissionConflictError" &&
          result.left._tag !== "AdmissionInputSpentError"
        )
          capacityRefusal ??= result.left._tag;
      }
    }
    const admitted = admittedIds.length;
    // The signer is not stored with the rows: this line is what ties a row
    // the tx-queue processor later forgets to the operator who sent it.
    if (admitted > 0)
      yield* Effect.logInfo(
        `Admitted ${admitted.toString()} L2 txs from peer operator ${keyHash}`,
      ).pipe(
        Effect.annotateLogs({ peerOperator: keyHash, txIds: admittedIds }),
      );
    if (capacityRefusal !== undefined)
      return yield* HttpServerResponse.json(
        { error: CAPACITY_REFUSALS[capacityRefusal] },
        UNAVAILABLE,
      );
    if (checkUnavailable)
      return yield* HttpServerResponse.json(
        { error: "Peer tx check is unavailable; retry later" },
        UNAVAILABLE,
      );
    if (verifierUnavailable)
      return yield* HttpServerResponse.json(
        { error: "Signature verification is unavailable" },
        UNAVAILABLE,
      );
    return yield* HttpServerResponse.json({ admitted, duplicates, refused });
  }).pipe(Effect.catchIf(HttpServerResponse.isServerResponse, Effect.succeed));

/** This node's operator key, derived once per node config. */
const answerSigners = new WeakMap<object, PeerSigner>();
const answerSigner = (nodeConfig: NodeConfig["Type"]): PeerSigner => {
  let signer = answerSigners.get(nodeConfig);
  if (signer === undefined) {
    signer = daLocalSigners(nodeConfig).find(
      (local) => local.role === "operator",
    )!;
    answerSigners.set(nodeConfig, signer);
  }
  return signer;
};

/**
 * Answers which of the offered transaction ids this node has never seen,
 * signed by this node's operator key so the sender knows which operator
 * holds the rest (`fibers/l2-tx-peer-forwarder.ts`).
 */
export const postPeerInventoryHandler = Effect.gen(function* () {
  const { bytes } = yield* withAuthorizedBody(
    PEER_INVENTORY_PATH,
    PEER_INVENTORY_MAX_BODY_BYTES,
    Effect.succeed,
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
  const answer = encodePeerList(missing);
  return HttpServerResponse.uint8Array(answer, {
    contentType: PEER_MEDIA_TYPE,
    headers: peerAnswerHeaders(
      answerSigner(yield* NodeConfig),
      PEER_INVENTORY_PATH,
      (yield* MidgardContracts).hubOracle.policyId,
      bytes,
      answer,
    ),
  });
}).pipe(
  Effect.catchIf(HttpServerResponse.isServerResponse, Effect.succeed),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500("POST", "peer/inventory", e.cause, "inventory read failed"),
  ),
);
