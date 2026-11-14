/**
 * A foreign block's DA payload for the replayer: the node's retained copy,
 * or a fetch over the public DA transport (through the replayer's backoff
 * memo). A retained copy that no longer verifies is deleted and fetched
 * again, and the block waits as `da_refetch_pending` until a refetch is
 * served. Once the block's queue node carries an availability commitment, a
 * retained copy that does not rebuild it is such a copy, and a fetched one
 * must rebuild it.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  type ForeignDaBinding,
  type foreignDaFetchMemo,
  foreignRetainedDaInsert,
} from "../da/foreign-retained-da.js";
import { decodeStoredPayload } from "../da/stored-payload.js";
import { DaPayloadsDB } from "../database/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
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
  blockTime: bigint,
  binding: ForeignDaBinding | undefined,
  why: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logWarning(
      `deleting the retained DA payload of ${headerHash} to refetch it: ${why}`,
    );
    yield* withFollowerWrite(
      DaPayloadsDB.removeByHeaderHash(Buffer.from(headerHash, "hex")),
    );
    refetching.delete(headerHash);
    refetching.set(headerHash, why);
    for (const oldest of refetching.keys()) {
      if (refetching.size <= REFETCHING_LIMIT) break;
      refetching.delete(oldest);
    }
    return yield* fetched(
      fetchDa,
      refetching,
      headerHash,
      header,
      blockTime,
      binding,
    );
  });

/** Fetches the payload; while a refetch is owed, a failure says so. */
const fetched = (
  fetchDa: FetchDa,
  refetching: Refetching,
  headerHash: string,
  header: SDK.Header,
  blockTime: bigint,
  binding: ForeignDaBinding | undefined,
) =>
  fetchDa(headerHash, header, binding).pipe(
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
        bound: acquired.bound,
        served: { peer: acquired.peer } as Served,
        acquired: foreignRetainedDaInsert(
          headerHash,
          header,
          blockTime,
          acquired.payloadBytes,
          acquired.roots,
          acquired.counts,
        ),
      };
    }),
  );

/** Where the payload came from: the peer that served it, or the node's row. */
export type Served = Readonly<{ peer: number } | { stored: true }>;

/**
 * The block's DA payload, the row to retain if it was fetched, whether its
 * queue node's availability commitment binds it, and where it came from.
 */
export const payloadOf = (
  fetchDa: FetchDa,
  refetching: Refetching,
  headerHash: string,
  header: SDK.Header,
  blockTime: bigint,
  binding?: ForeignDaBinding,
) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(headerHash, "hex"),
    );
    if (Option.isNone(stored))
      return yield* fetched(
        fetchDa,
        refetching,
        headerHash,
        header,
        blockTime,
        binding,
      );
    const corrupt = (why: string) =>
      discardCorrupt(
        fetchDa,
        refetching,
        headerHash,
        header,
        blockTime,
        binding,
        why,
      );
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
    const bound =
      binding !== undefined && SDK.daAvailabilityStatusBinds(binding.status);
    if (
      bound &&
      SDK.daPayloadAvailabilityBinding({
        status: binding.status,
        authority: yield* Effect.try({
          try: () => binding.authority(),
          catch: (cause) =>
            new Verdict(
              "missing",
              `the deployment's availability parameters are unavailable: ${String(cause)}`,
            ),
        }),
        headerHash,
        payload: row[DaPayloadsDB.Columns.PAYLOAD_CBOR],
      }) !== "bound"
    )
      return yield* corrupt("it differs from the attested payload commitment");
    return {
      payload: payload.right,
      bound,
      served: { stored: true } as Served,
      acquired: undefined,
    };
  });
