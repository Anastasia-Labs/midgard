import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
} from "../workers/utils/commit-block-header.js";
import { withHistoryWrite } from "./event-history-producer.js";
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "./mpf-native-owner/index.js";

/** The parent must restore native state before a recovery worker can finalize
 * SQL. A lost submission response has no promotion handle; its authenticated
 * journal supplies the replay after canonical observation instead. */
export const recoverNativeMpfForLocalFinalization = (
  owner: NativeMpfOwnerService,
  block: SerializedStateQueueUTxO,
) =>
  Effect.gen(function* () {
    const confirmed = yield* deserializeStateQueueUTxO(block);
    const header = yield* SDK.getHeaderFromStateQueueDatum(confirmed.datum);
    const headerHash = yield* SDK.hashBlockHeader(header);
    const fail = (message: string) =>
      Effect.fail(
        new DatabaseError({
          table: Pending.tableName,
          message,
          cause: headerHash,
        }),
      );
    const readReplay = withHistoryWrite(
      Effect.gen(function* () {
        const pending = yield* Pending.retrieveByHeaderHash(
          Buffer.from(headerHash, "hex"),
          true,
        );
        if (Option.isNone(pending))
          return yield* fail(
            "Native finalization recovery requires its journal",
          );
        const record = pending.value;
        const status = record[Pending.Columns.STATUS];
        const observed =
          status === Pending.Status.ObservedWaitingStability ||
          status === Pending.Status.Finalized;
        const acknowledged =
          record[Pending.Columns.SUBMITTED_TX_HASH] !== null &&
          (status === Pending.Status.SubmittedUnconfirmed ||
            status === Pending.Status.SubmittedLocalFinalizationPending);
        if (!observed && !acknowledged)
          return yield* fail(
            "Signed intent alone cannot authorize native finalization recovery",
          );
        const replay = record.nativeMpfReplay;
        if (
          replay === undefined ||
          record[Pending.Columns.EXPECTED_UTXOS_ROOT] !== header.utxosRoot ||
          replay.candidateRoot.toString("hex") !== header.utxosRoot
        )
          return yield* fail(
            "Native finalization replay does not match the confirmed header",
          );
        yield* Pending.assertCanonicalEventMembers(record);
        return {
          schema: 1,
          ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
          baseRoot: replay.baseRoot.toString("hex"),
          candidateRoot: replay.candidateRoot.toString("hex"),
          eventLog: replay.eventLog,
          eventLogDigest: replay.eventLogDigest.toString("hex"),
          eventRoots: replay.eventRoots,
          eventCount: replay.eventCount,
        } satisfies PersistedNativeMpfReplay;
      }),
    );
    const replay = yield* readReplay;
    // No SQL transaction spans native IO. Finish promotion and recheck current
    // authority before allowing SQL finalization, including on cancellation.
    yield* Effect.tryPromise(() => owner.recover(replay)).pipe(
      Effect.zipRight(readReplay),
      Effect.uninterruptible,
    );
  });
