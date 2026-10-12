import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_TRANSPORT_LIMITS,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Clock, Effect } from "effect";

import { DaPayloadsDB } from "../database/index.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { ContractDeploymentIdentity } from "../services/index.js";
import { sha256 } from "../sha256.js";
import {
  computeDaPayloadRoots,
  headerCounts,
  headerRoots,
  rootMismatches,
} from "../workers/commit-block-header/da-payload.compute-da-payload-roots.js";
import { loadDaProducerPublicationManifestFromEnv } from "./libp2p-producer.parse-da-producer-publication-manifest.js";
import { getPublicationTransport } from "./libp2p-producer.publish-da-payload-insert-from-env.js";

/** Decodes a stored DA payload envelope; only the canonical V1 schema is accepted. */
export const decodeStoredPayload = ({
  payloadCbor,
  schemaVersion,
}: {
  readonly payloadCbor: Buffer;
  readonly schemaVersion: number;
}): Promise<SDK.DaPayload> =>
  schemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)
    ? Promise.reject(
        new Error("Stored DA payload schema version must equal canonical V1"),
      )
    : unwrapDaPayload(payloadCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      }).then((unwrapped) => SDK.decodeDaPayload(unwrapped.innerBytes));

/** Construct a retained row only from the canonical header and acquired bytes.
 * The caller persists it once the block's replay reached its header's root. */
export const foreignRetainedDaInsert = (
  headerHash: string,
  header: SDK.Header,
  payloadBytes: Buffer,
): DaPayloadsDB.InsertInput => ({
  header_hash: Buffer.from(headerHash, "hex"),
  consensus_profile_id: MIDGARD_CONSENSUS_PROFILE_ID,
  version: 1,
  payload_cbor: payloadBytes,
  payload_sha256: sha256(payloadBytes),
  utxos_root: header.utxosRoot,
  forced_transactions_root: header.forcedTransactionsRoot,
  transactions_root: header.transactionsRoot,
  deposits_root: header.depositsRoot,
  withdrawals_root: header.withdrawalsRoot,
  transition_trace_root: header.transitionTraceRoot,
  event_to_step_root: header.eventToStepRoot,
  validation_traces_root: header.validationTracesRoot,
  withdrawal_count: header.withdrawalCount,
  forced_transaction_count: header.forcedTransactionCount,
  l2_transaction_count: header.l2TransactionCount,
  deposit_count: header.depositCount,
  total_event_count: header.totalEventCount,
  transition_step_count: header.transitionStepCount,
  validation_trace_count: header.validationTraceCount,
  block_start_time: new Date(Number(header.startTime)),
  block_end_time: new Date(Number(header.endTime)),
});

/** Acquire untrusted public bytes through the existing authenticated libp2p
 * transport. Peer metadata and signatures confer no block validity. A peer's
 * substituted body is skipped; only canonical-header-bound content leaves
 * this boundary, and the caller still must perform ordinary full replay. */
export const fetchForeignRetainedDa = (
  headerHash: string,
  header: SDK.Header,
) =>
  Effect.gen(function* () {
    const missing = (detail: string) =>
      new ForeignBlockVerificationError({
        foreignHeaderHash: headerHash,
        reason: "missing",
        detail,
      });
    const identity = yield* ContractDeploymentIdentity;
    const manifest = yield* Effect.tryPromise({
      try: () => loadDaProducerPublicationManifestFromEnv(),
      catch: (cause) =>
        missing(
          `foreign public DA configuration unavailable: ${String(cause)}`,
        ),
    });
    if (
      identity.manifestId === undefined ||
      manifest.contractDeploymentManifestId !== identity.manifestId
    )
      return yield* Effect.fail(
        missing(
          "foreign public DA deployment binding differs from the active manifest",
        ),
      );
    const transport = yield* Effect.tryPromise({
      try: () => getPublicationTransport(manifest),
      catch: (cause) =>
        missing(`foreign public DA transport unavailable: ${String(cause)}`),
    });
    for (const peer of manifest.committeePeers) {
      // Committee nodes retain and serve the same public by-header protocol;
      // the optional retrieval role does not determine content authority.
      const source = new DaLibp2pRetainedDaSource({
        sourceId: "node-foreign-import",
        deploymentFingerprint: manifest.deploymentFingerprint,
        peers: [peer],
        timeoutMs: manifest.requestTimeoutMs,
        maxInlineResponseBytes: manifest.maxInlineResponseBytes,
        maxChunkBytes: manifest.maxChunkBytes,
        transport: {
          request: ({ protocol, payload, timeoutMs }) =>
            transport.request(
              peer,
              daRequestResponseProtocolId(
                manifest.deploymentFingerprint,
                protocol,
              ),
              payload,
              timeoutMs,
            ),
        },
      });
      const candidate = yield* Effect.either(
        Effect.tryPromise(() =>
          fetchRetainedDaPayloadByHeaderHash({
            headerHash,
            sources: [source],
            retries: 0,
          }),
        ).pipe(
          Effect.flatMap((fetched) =>
            Effect.gen(function* () {
              const payloadBytes = fetched.payloadEnvelopeCbor;
              if (payloadBytes.length > manifest.maxPayloadBytes)
                return yield* Effect.fail(
                  missing("foreign public DA exceeds its admitted byte bound"),
                );
              const payload = yield* Effect.tryPromise(() =>
                decodeStoredPayload({
                  payloadCbor: payloadBytes,
                  schemaVersion: Number(SDK.DA_PAYLOAD_VERSION),
                }),
              );
              if (
                payload.block_body.header_hash !== headerHash ||
                (yield* SDK.hashBlockHeader(payload.block_body.header)) !==
                  headerHash
              )
                return yield* Effect.fail(
                  missing("foreign peer substituted the canonical header"),
                );
              const roots = yield* computeDaPayloadRoots(payload);
              if (rootMismatches(headerRoots(header), roots).length !== 0)
                return yield* Effect.fail(
                  missing(
                    "foreign peer body is not bound to canonical commitments",
                  ),
                );
              const body = payload.block_body;
              const counts: SDK.DaPayloadCounts = {
                depositCount: BigInt(body.deposits.length),
                withdrawalCount: BigInt(body.withdrawals.length),
                forcedTransactionCount: BigInt(body.forced_transactions.length),
                l2TransactionCount: BigInt(body.transactions.length),
                totalEventCount: BigInt(
                  body.deposits.length +
                    body.withdrawals.length +
                    body.forced_transactions.length +
                    body.transactions.length,
                ),
                transitionStepCount: BigInt(body.transition_trace.length),
                validationTraceCount: BigInt(body.validation_traces.length),
              };
              for (const key of Object.keys(
                counts,
              ) as (keyof SDK.DaPayloadCounts)[]) {
                if (
                  counts[key] !== headerCounts(header)[key] ||
                  body.counts[key] !== counts[key]
                )
                  return yield* Effect.fail(
                    missing(
                      "foreign peer counts are not bound to canonical commitments",
                    ),
                  );
              }
              return { payloadBytes, payload };
            }),
          ),
        ),
      );
      if (candidate._tag === "Right") return candidate.right;
    }
    return yield* Effect.fail(
      missing(
        `foreign public DA unavailable or unauthenticated: ${headerHash}`,
      ),
    );
  });

/** The first wait after a failed foreign DA fetch; it doubles per failure. */
export const FOREIGN_DA_RETRY_BASE_MS = 1_000;
/** The longest wait between foreign DA fetches of one header. */
export const FOREIGN_DA_RETRY_MAX_MS = 60_000;
/** Headers the memo remembers; the oldest is forgotten past this. */
const FOREIGN_DA_MEMO_LIMIT = 1_024;

type Attempt = Readonly<{ atMs: number; failures: number }>;

/**
 * A per-header next-attempt memo for foreign DA fetches: after a failed
 * fetch, the header is not fetched again (no peer is asked) before its next
 * attempt time, which backs off exponentially from
 * `FOREIGN_DA_RETRY_BASE_MS` to `FOREIGN_DA_RETRY_MAX_MS`; a fetch that
 * succeeds forgets the header. Time is read from the Effect clock; nothing
 * sleeps: a fetch asked for early fails `missing` at once.
 */
export const foreignDaFetchMemo = () => {
  const attempts = new Map<string, Attempt>();
  return (headerHash: string, header: SDK.Header) =>
    Effect.gen(function* () {
      const now = yield* Clock.currentTimeMillis;
      const last = attempts.get(headerHash);
      if (last !== undefined && now < last.atMs)
        return yield* Effect.fail(
          new ForeignBlockVerificationError({
            foreignHeaderHash: headerHash,
            reason: "missing",
            detail: `foreign public DA fetch of ${headerHash} backs off ${(last.atMs - now).toString()} ms after ${last.failures.toString()} failed attempts`,
          }),
        );
      const fetched = yield* Effect.either(
        fetchForeignRetainedDa(headerHash, header),
      );
      if (fetched._tag === "Right") {
        attempts.delete(headerHash);
        return fetched.right;
      }
      const failures = (last?.failures ?? 0) + 1;
      attempts.delete(headerHash);
      attempts.set(headerHash, {
        atMs:
          (yield* Clock.currentTimeMillis) +
          Math.min(
            FOREIGN_DA_RETRY_BASE_MS * 2 ** Math.min(failures - 1, 30),
            FOREIGN_DA_RETRY_MAX_MS,
          ),
        failures,
      });
      for (const oldest of attempts.keys()) {
        if (attempts.size <= FOREIGN_DA_MEMO_LIMIT) break;
        attempts.delete(oldest);
      }
      return yield* Effect.fail(fetched.left);
    });
};
