/**
 * A foreign block's DA payload for the replayer: the node's retained copy,
 * or a fetch over the public DA transport (through the replayer's backoff
 * memo). A retained copy that no longer verifies is deleted and fetched
 * again, and the block waits as `da_refetch_pending` until a refetch is
 * served.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  decodeStoredPayload,
  type foreignDaFetchMemo,
  foreignRetainedDaInsert,
} from "../da/foreign-retained-da.js";
import { DaPayloadsDB } from "../database/index.js";
import {
  runHistoryProducer,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { sha256 } from "../sha256.js";
import { Verdict } from "./replay-foreign.verdict.js";

export type FetchDa = ReturnType<typeof foreignDaFetchMemo>;

/**
 * Headers whose retained payload failed verification and was deleted,
 * with why, until a refetch is served. Bounded like the fetch memo.
 */
export type Refetching = Map<string, string>;
const REFETCHING_LIMIT = 1_024;

/**
 * A retained row that no longer verifies is local corruption, not the
 * block's fault: it is deleted and the payload fetched again. Until a
 * refetch is served the block waits as `da_refetch_pending`.
 */
const discardCorrupt = (
  fetchDa: FetchDa,
  refetching: Refetching,
  headerHash: string,
  header: SDK.Header,
  why: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logWarning(
      `deleting the retained DA payload of ${headerHash} to refetch it: ${why}`,
    );
    yield* runHistoryProducer(
      withHistoryWrite(
        DaPayloadsDB.removeByHeaderHash(Buffer.from(headerHash, "hex")),
      ),
    );
    refetching.delete(headerHash);
    refetching.set(headerHash, why);
    for (const oldest of refetching.keys()) {
      if (refetching.size <= REFETCHING_LIMIT) break;
      refetching.delete(oldest);
    }
    return yield* fetched(fetchDa, refetching, headerHash, header);
  });

/** Fetches the payload; while a refetch is owed, a failure says so. */
const fetched = (
  fetchDa: FetchDa,
  refetching: Refetching,
  headerHash: string,
  header: SDK.Header,
) =>
  fetchDa(headerHash, header).pipe(
    Effect.mapError((error) => {
      const why = refetching.get(headerHash);
      return why === undefined
        ? error
        : new Verdict(
            "da_refetch_pending",
            `the retained DA payload was deleted (${why}); refetch: ${error.detail}`,
          );
    }),
    Effect.map((acquired) => {
      refetching.delete(headerHash);
      return {
        payload: acquired.payload,
        acquired: foreignRetainedDaInsert(
          headerHash,
          header,
          acquired.payloadBytes,
        ),
      };
    }),
  );

/** The block's DA payload, and the row to retain if it was fetched. */
export const payloadOf = (
  fetchDa: FetchDa,
  refetching: Refetching,
  headerHash: string,
  header: SDK.Header,
) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(headerHash, "hex"),
    );
    if (Option.isNone(stored))
      return yield* fetched(fetchDa, refetching, headerHash, header);
    const corrupt = (why: string) =>
      discardCorrupt(fetchDa, refetching, headerHash, header, why);
    const row = stored.value;
    if (
      !row[DaPayloadsDB.Columns.HEADER_HASH].equals(
        Buffer.from(headerHash, "hex"),
      ) ||
      row[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID] !==
        identity.consensusProfile.profileId ||
      !sha256(row[DaPayloadsDB.Columns.PAYLOAD_CBOR]).equals(
        row[DaPayloadsDB.Columns.PAYLOAD_SHA256],
      )
    )
      return yield* corrupt("its stored digest or identity does not verify");
    const payload = yield* Effect.either(
      Effect.tryPromise(() =>
        decodeStoredPayload({
          payloadCbor: row[DaPayloadsDB.Columns.PAYLOAD_CBOR],
          schemaVersion: row[DaPayloadsDB.Columns.VERSION],
        }),
      ),
    );
    if (payload._tag === "Left")
      return yield* corrupt(`it does not decode: ${String(payload.left)}`);
    // The body is the block's before its event lists are read, as a fetched
    // payload's is.
    if (
      payload.right.block_body.header_hash !== headerHash ||
      (yield* SDK.hashBlockHeader(payload.right.block_body.header).pipe(
        Effect.orElseSucceed(() => undefined),
      )) !== headerHash
    )
      return yield* corrupt("its body is not the block's");
    return { payload: payload.right, acquired: undefined };
  });
