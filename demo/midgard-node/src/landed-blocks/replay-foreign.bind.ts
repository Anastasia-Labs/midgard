/**
 * Binding the copies of foreign blocks imported while Unattested. Such a
 * copy replayed, but no committee commitment fixed the sections its header
 * does not commit, so the replayer holds it apart (`UnboundCopies`) and never
 * retains or serves it. Once the block's queue node carries a commitment,
 * this pass checks the node's retained copy against it: a matching one is
 * kept (retained, if it was held apart); another is dropped and a bound body
 * fetched and retained in its place. The block's verdict and landed row are
 * not touched: the commitment covers the bytes, not the replay.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  type ForeignDaBinding,
  foreignRetainedDaInsert,
} from "../da/foreign-retained-da.js";
import { DaPayloadsDB } from "../database/index.js";
import type { LandedStateQueue } from "../l1-state-queue/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import type { FetchDa } from "./replay-foreign.payload.js";
import { retrieveRows } from "./store.js";

/** Copies imported unbound, by header hash, until their node binds them. */
export type UnboundCopies = Map<string, DaPayloadsDB.InsertInput>;
const UNBOUND_LIMIT = 1_024;

/** Holds `row` apart; the oldest copy is forgotten past the limit. */
export const keepUnbound = (
  copies: UnboundCopies,
  headerHash: string,
  row: DaPayloadsDB.InsertInput,
) => {
  copies.delete(headerHash);
  copies.set(headerHash, row);
  for (const oldest of copies.keys()) {
    if (copies.size <= UNBOUND_LIMIT) break;
    copies.delete(oldest);
  }
};

/**
 * The pass over the landed queue's nodes. Each foreign node is checked once
 * per availability status per process; a pass that cannot finish a node
 * (no bound body is served yet) logs and leaves it for the next one. It
 * never fails.
 */
export const bindRetainedForeignPayloads = (deps: {
  readonly fetchDa: FetchDa;
  readonly unbound: UnboundCopies;
  readonly authority: ForeignDaBinding["authority"];
}) => {
  const checked = new Set<string>();
  const remember = (key: string) => {
    checked.add(key);
    for (const oldest of checked) {
      if (checked.size <= UNBOUND_LIMIT) break;
      checked.delete(oldest);
    }
  };
  const bindOne = (
    headerHash: string,
    node: SDK.StateQueueNode,
    authority: SDK.DaPayloadAvailabilityAuthority,
  ) =>
    Effect.gen(function* () {
      const status = node.da_attestation;
      const binds = (payload: Uint8Array) =>
        SDK.daPayloadAvailabilityBinding({
          status,
          authority,
          headerHash,
          payload,
        }) === "bound";
      const key = Buffer.from(headerHash, "hex");
      const stored = yield* DaPayloadsDB.retrieveByHeaderHash(key);
      if (
        Option.isSome(stored) &&
        binds(stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR])
      ) {
        deps.unbound.delete(headerHash);
        return true;
      }
      if (Option.isSome(stored)) {
        yield* Effect.logWarning(
          `dropping the retained DA payload of ${headerHash}: it differs from the attested payload commitment`,
        );
        yield* withFollowerWrite(DaPayloadsDB.removeByHeaderHash(key));
      }
      const held = deps.unbound.get(headerHash);
      deps.unbound.delete(headerHash);
      if (held !== undefined && binds(held.payload_cbor)) {
        yield* withFollowerWrite(DaPayloadsDB.upsertAvailable(held));
        return true;
      }
      const fetched = yield* Effect.either(
        deps.fetchDa(headerHash, node.header, {
          status,
          authority: () => authority,
        }),
      );
      if (fetched._tag === "Left") {
        yield* Effect.logInfo(
          `no bound DA payload of ${headerHash} yet: ${fetched.left.detail}`,
        );
        return false;
      }
      yield* withFollowerWrite(
        DaPayloadsDB.upsertAvailable(
          foreignRetainedDaInsert(
            headerHash,
            node.header,
            node.block_time,
            fetched.right.payloadBytes,
            fetched.right.roots,
            fetched.right.counts,
          ),
        ),
      );
      return true;
    });
  return (queue: LandedStateQueue) =>
    Effect.gen(function* () {
      const foreign = new Set(
        (yield* retrieveRows)
          .filter((row) => row.kind === "foreign" && row.state === "processed")
          .map((row) => row.headerHash),
      );
      for (const { headerHash, element, daStatus } of queue.nodes) {
        const key = `${headerHash}/${daStatus ?? ""}`;
        if (checked.has(key) || !foreign.has(headerHash)) continue;
        const node = yield* Effect.either(
          SDK.getStateQueueNodeFromStateQueueDatum(element.datum),
        );
        if (
          node._tag === "Left" ||
          !SDK.daAvailabilityStatusBinds(node.right.da_attestation)
        )
          continue;
        const done = yield* bindOne(
          headerHash,
          node.right,
          deps.authority(),
        ).pipe(
          Effect.catchAllCause((cause) =>
            Effect.as(
              Effect.logWarning(
                `binding the retained DA payload of ${headerHash} failed`,
                cause,
              ),
              false,
            ),
          ),
        );
        if (done) remember(key);
      }
    }).pipe(
      Effect.catchAllCause((cause) =>
        Effect.logWarning("the retained DA binding pass failed", cause),
      ),
    );
};
