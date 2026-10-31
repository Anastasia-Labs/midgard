import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Option } from "effect";

import { publishDaPayloadInsertFromEnv } from "../da/libp2p-producer.js";
import { DaPayloadsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import type { Database } from "../services/database.js";

export const publishFinalizedDaPayloadBestEffort = (
  finalizedHeaderHash: string,
): Effect.Effect<void, never, Database> =>
  Effect.gen(function* () {
    const publication = yield* Effect.either(
      DaPayloadsDB.retrieveByHeaderHash(
        Buffer.from(finalizedHeaderHash, "hex"),
      ).pipe(
        Effect.flatMap(
          Option.match({
            onNone: () =>
              Effect.fail(
                new DatabaseError({
                  table: DaPayloadsDB.tableName,
                  message:
                    "Finalized DA payload is missing before parent publication trigger",
                  cause: `header_hash=${finalizedHeaderHash}`,
                }),
              ),
            onSome: publishDaPayloadInsertFromEnv,
          }),
        ),
      ),
    );
    if (publication._tag === "Left") {
      yield* Effect.logWarning(
        `🔹 DA payload publication deferred to durable reconciler (header=${finalizedHeaderHash},error=${formatUnknownError(publication.left)})`,
      );
    } else if (publication.right.configured) {
      yield* Effect.logInfo(
        `🔹 Published DA payload over libp2p (header=${publication.right.headerHash},accepted_peers=${publication.right.acceptedPeers.toString()},threshold=${publication.right.threshold?.toString() ?? "unknown"})`,
      );
    } else {
      yield* Effect.logInfo(
        `🔹 Skipped DA payload libp2p publication for ${publication.right.headerHash}: ${publication.right.reason ?? "not configured"}`,
      );
    }
  }).pipe(Effect.catchAllCause(Effect.logWarning));

/** Runs the publication trigger only after the caller's L1 effect releases. */
export const runAfterL1ControlPlaneRelease = <A, E, R, E2, R2>(
  l1Effect: Effect.Effect<A, E, R>,
  finalizedHeaderHash: (result: A) => string | undefined,
  publish: (headerHash: string) => Effect.Effect<void, E2, R2>,
): Effect.Effect<A, E | E2, R | R2> =>
  Effect.gen(function* () {
    const result = yield* l1Effect;
    const headerHash = finalizedHeaderHash(result);
    if (headerHash !== undefined) {
      yield* publish(headerHash);
    }
    return result;
  });
