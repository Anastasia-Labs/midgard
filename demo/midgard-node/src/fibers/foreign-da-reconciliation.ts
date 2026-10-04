import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SqlClient } from "@effect/sql";
import { Cause, Duration, Effect, Option, Schedule } from "effect";
import {
  WatcherPublicDaClient,
  WatcherPublicDaLibp2pTransport,
} from "midgard-watcher/public-da-client";

import { ForeignPayloadRetriever } from "../da/foreign-payload-retriever.js";
import { downloadedForeignPayloadRow } from "../da/foreign-payload-store.js";
import { loadDaProducerPublicationManifestFromEnv } from "../da/libp2p-producer.js";
import {
  DaPayloadsDB,
  ForeignTipReconciliationsDB,
} from "../database/index.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
} from "../services/index.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { decodeRetainedHeader } from "../workers/t2-foreign-event-reconciliation.decode-retained-header.js";
import { entryVerdict } from "../workers/t2-foreign-event-reconciliation.foreign-window-gate.js";
import { STORED_INVALID_PREFIX } from "../workers/t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js";
import { eventCommitmentsAreConsistent } from "../workers/t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";
import { consumeDownloadedForeignDa } from "./foreign-da-reconciliation.consume-payload.js";

/** The source the retrieval raises its unavailability under. It holds no
 * fiber: an unretrieved payload keeps its own row awaiting, and the commit
 * gate decides what that row blocks. */
export const FOREIGN_DA_RETRIEVAL_SOURCE = "foreign_da_retrieval";
export const FOREIGN_DA_RETRIEVAL_UNAVAILABLE =
  "foreign_da_retrieval_unavailable";

/** Between failed starts: doubling from the first delay, capped. */
export const FOREIGN_DA_RETRIEVAL_RESTART_POLICY = Object.freeze({
  firstDelayMs: 30_000,
  maxDelayMs: 300_000,
});

/** Escalated once a start has kept failing this long. */
const FOREIGN_DA_RETRIEVAL_ESCALATE_AFTER_MS = 900_000;

/**
 * What one retrieval pass does with an awaiting row, given whether this node
 * already stores a payload for its header. A header whose event roots and
 * counts disagree is malformed on its face: no payload can repair it, so it
 * is never fetched. A row holding a stored `invalid` verdict already failed
 * against the payload this node holds; consuming it again replays the same
 * verdict, and a download cannot replace it (the payload store refuses
 * different bytes), so the row is left alone. Only a row with no stored
 * payload is fetched.
 */
export const foreignDaRowAction = (
  entry: ForeignTipReconciliationsDB.Entry,
  payloadStored: boolean,
): "skip" | "fetch" | "consume_stored" => {
  const verdict = entryVerdict(entry);
  if (!eventCommitmentsAreConsistent(verdict.commitments)) return "skip";
  if (!payloadStored) return "fetch";
  return verdict.blockingReason?.startsWith(STORED_INVALID_PREFIX) === true
    ? "skip"
    : "consume_stored";
};

/**
 * Visits `hashes` in order until `retrieveRow` consumes a payload, and
 * returns the hash it stopped at (the scan's next cursor). A row that fails,
 * a refused payload overwrite among them, is logged and passed over: it never
 * ends the pass for the rows after it, and a later pass retries it.
 */
export const retrieveFromPage = <R>(
  hashes: readonly Buffer[],
  retrieveRow: (hash: Buffer) => Effect.Effect<boolean, unknown, R>,
): Effect.Effect<Buffer, never, R> =>
  Effect.gen(function* () {
    let cursor = hashes[0]!;
    for (const hash of hashes) {
      cursor = hash;
      const consumed = yield* retrieveRow(hash).pipe(
        Effect.catchAllCause((cause) =>
          Cause.isInterrupted(cause)
            ? Effect.interrupt
            : Effect.logWarning(
                `Foreign DA retrieval for ${hash.toString("hex")} failed; a later pass retries it: ${String(cause)}`,
              ).pipe(Effect.as(false)),
        ),
      );
      if (consumed) break;
    }
    return cursor;
  });

/**
 * Runs `session` (one start of the retrieval and its tick loop), starting it
 * again after a failure with a growing, capped delay. While it cannot start,
 * `foreign_da_retrieval_unavailable` is raised; the first start that
 * completes clears it. Never fails.
 */
export const superviseForeignDaRetrieval = <R>(
  globals: Pick<Globals, "LIVENESS_REASONS">,
  session: (started: Effect.Effect<void>) => Effect.Effect<void, unknown, R>,
  policy: {
    readonly firstDelayMs: number;
    readonly maxDelayMs: number;
  } = FOREIGN_DA_RETRIEVAL_RESTART_POLICY,
): Effect.Effect<void, never, R> =>
  session(clearLivenessIncident(globals, FOREIGN_DA_RETRIEVAL_SOURCE)).pipe(
    Effect.catchAllDefect((defect) => Effect.fail(defect)),
    Effect.tapError((cause) =>
      raiseLivenessIncident(
        globals,
        FOREIGN_DA_RETRIEVAL_SOURCE,
        FOREIGN_DA_RETRIEVAL_UNAVAILABLE,
        `foreign DA retrieval could not start; retrying: ${String(cause)}`,
        { escalateAfterMs: FOREIGN_DA_RETRIEVAL_ESCALATE_AFTER_MS },
      ),
    ),
    Effect.retry(
      Schedule.union(
        Schedule.exponential(Duration.millis(policy.firstDelayMs)),
        Schedule.spaced(Duration.millis(policy.maxDelayMs)),
      ),
    ),
    Effect.catchAll(() => Effect.void),
  );

/** One bounded read-only download per tick; ordinary T2 recovery consumes
 * authenticated stored bytes. Never fails: a start that fails (the DA
 * manifest, or the transport) is retried, and raises a readiness reason
 * until one succeeds. */
export const foreignDaReconciliationFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Database | ContractDeploymentIdentity | Globals
> =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    if (
      identity.kind !== "manifest" ||
      identity.manifest === undefined ||
      identity.deploymentMarker === undefined
    ) {
      yield* Effect.logInfo(
        "Foreign DA retrieval requires a finalized deployment manifest; derived deployment has no remote authority.",
      );
      return;
    }
    const marker = identity.deploymentMarker;
    const deploymentManifest = identity.manifest;
    const globals = yield* Globals;
    yield* superviseForeignDaRetrieval(globals, (started) =>
      Effect.scoped(
        Effect.gen(function* () {
          const manifest = yield* Effect.tryPromise(() =>
            loadDaProducerPublicationManifestFromEnv(),
          );
          if (
            manifest.deploymentFingerprint !== deploymentManifest.manifestId ||
            manifest.contractDeploymentManifestId !==
              deploymentManifest.manifestId
          )
            return yield* Effect.fail(
              new Error(
                "Foreign DA runtime manifest does not match active contract deployment",
              ),
            );
          const transport = yield* Effect.acquireRelease(
            Effect.sync(() => new WatcherPublicDaLibp2pTransport()),
            (transport) =>
              Effect.tryPromise(() => transport.stop()).pipe(
                Effect.timeout("5 seconds"),
                Effect.catchAllCause((cause) =>
                  Effect.logWarning(
                    `Foreign DA transport cleanup failed: ${String(cause)}`,
                  ),
                ),
              ),
          );
          const client = yield* Effect.try(
            () =>
              new WatcherPublicDaClient({
                deploymentManifest,
                peers: manifest.committeePeers.map((peer) => ({
                  identity: `committee-${peer.signerIndex}`,
                  multiaddr: peer.multiaddrs[0]!,
                })),
                requestTimeoutMs: Math.min(manifest.requestTimeoutMs, 10_000),
                fetchTimeoutMs: 60_000,
                maxConcurrency: 1,
                transport,
              }),
          );
          yield* Effect.addFinalizer(() => Effect.sync(() => client.close()));
          yield* Effect.tryPromise({
            try: (signal) => transport.start(signal),
            catch: (cause) => cause,
          }).pipe(Effect.timeout("10 seconds"));
          yield* started;
          const retriever = new ForeignPayloadRetriever(client);
          /** Whether a payload for `hash` was consumed. */
          const retrieveRow = (hash: Buffer) =>
            Effect.gen(function* () {
              const entry =
                yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                  hash.toString("hex"),
                );
              if (Option.isNone(entry)) return false;
              const stored = yield* DaPayloadsDB.retrieveByHeaderHash(hash);
              const action = foreignDaRowAction(
                entry.value,
                Option.isSome(stored),
              );
              if (action === "skip") return false;
              if (Option.isSome(stored)) {
                yield* consumeDownloadedForeignDa(entry.value, stored.value);
                return true;
              }
              const retained =
                ForeignTipReconciliationsDB.decodeForeignTipReconciliation(
                  entry.value,
                );
              yield* Effect.try(() =>
                assertDeploymentMarkerMatches(
                  retained.deploymentMarker,
                  marker,
                  "foreign DA fetch",
                ),
              );
              const header = yield* decodeRetainedHeader(entry.value);
              const fetched = yield* Effect.tryPromise({
                try: (signal) => {
                  const onAbort = () => client.close();
                  signal.addEventListener("abort", onAbort, { once: true });
                  return retriever
                    .fetch(hash.toString("hex"), header)
                    .finally(() =>
                      signal.removeEventListener("abort", onAbort),
                    );
                },
                catch: (cause) => cause,
              });
              if (fetched === undefined) return false;
              const row = yield* Effect.tryPromise(() =>
                downloadedForeignPayloadRow(
                  entry.value,
                  header,
                  fetched,
                  marker,
                ),
              );
              yield* consumeDownloadedForeignDa(entry.value, row);
              return true;
            });
          let cursor = Buffer.alloc(0);
          const tick = Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            // Keyset paging avoids an unreachable first page starving later headers.
            const hashes = yield* sql<{ foreign_header_hash: Buffer }>`
            SELECT f.foreign_header_hash FROM foreign_tip_reconciliations f
            WHERE f.deployment_manifest_id = ${marker.manifestId}
              AND f.status = 'awaiting' AND f.verified_da_payload_cbor IS NULL
              AND f.foreign_header_hash > ${cursor}
            ORDER BY f.foreign_header_hash LIMIT 16
          `;
            if (hashes.length === 0) {
              cursor = Buffer.alloc(0);
              return;
            }
            cursor = yield* retrieveFromPage(
              hashes.map(({ foreign_header_hash }) => foreign_header_hash),
              retrieveRow,
            );
          });
          yield* Effect.logInfo("Foreign DA payload retrieval fiber started.");
          yield* Effect.repeat(
            tick.pipe(
              Effect.catchAllCause((cause) =>
                Effect.logWarning(
                  `Foreign DA retrieval remains held; retry policy applies: ${String(cause)}`,
                ),
              ),
            ),
            schedule,
          );
        }),
      ),
    );
  });
