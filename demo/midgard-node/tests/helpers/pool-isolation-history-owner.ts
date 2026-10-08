import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { vi } from "vitest";

import * as Authority from "../../src/database/eventHistoryAuthority.js";
import { repairUnpublishedHistoryLedger } from "../../src/database/eventHistoryLedgerRepair.js";
import { BatchSql } from "../../src/services/database.js";
import {
  type EventHistoryOwner,
  makeEventHistoryOwner,
} from "../../src/services/event-history-owner.js";
import { Globals } from "../../src/services/globals.js";
import { ingestAtFollowerView } from "../../src/services/l1-follower.recovery.js";
import type { MempoolLedgerCacheService } from "../../src/services/mempool-ledger-cache.js";
import { testDatabaseName } from "../test-env.js";
import {
  makeRecordedHistoryTransport,
  openHistorySourceOwnerLifecycle,
} from "./history-source-owner-emulator.js";

/** Real initialized paired history supplies the production source gate; only
 * admission/validation work is the pool model. The owner's SQL uses the existing
 * batch pool, with exact-binding cleanup and no alternate producer path. */
export const makePoolIsolationHistoryOwner = (input: {
  readonly globals: Globals;
  readonly cache: MempoolLedgerCacheService;
}) =>
  Effect.gen(function* () {
    const batchSql = yield* BatchSql;
    const scoped = Effect.gen(function* () {
      const [database] = yield* batchSql<{
        name: string;
      }>`SELECT current_database() AS name`;
      if (database?.name !== testDatabaseName())
        throw new Error(
          "Pool history fixture requires its isolated worker database",
        );
      if (Option.isSome(yield* Authority.retrieve))
        throw new Error("Pool history fixture cannot adopt an existing owner");
      if ((yield* Ref.get(input.globals.EVENT_HISTORY_OWNER)) !== undefined)
        throw new Error(
          "Pool history fixture cannot replace another runtime owner",
        );

      const recorded = yield* Effect.tryPromise(() =>
        openHistorySourceOwnerLifecycle(),
      );
      const { binding } = recorded;
      const transport = makeRecordedHistoryTransport(recorded);
      const ownerToken = randomUUID();
      const lifecycle: { owner: EventHistoryOwner | undefined } = {
        owner: undefined,
      };
      let cleaned = false;
      const close = Effect.uninterruptible(
        Effect.gen(function* () {
          if (cleaned) return;
          const owner = lifecycle.owner;
          if (owner !== undefined) yield* owner.close;
          yield* Ref.update(input.globals.EVENT_HISTORY_OWNER, (current) =>
            current === owner ? undefined : current,
          );
          transport.close();
          recorded.observer.restore();
          vi.useRealTimers();
          yield* batchSql.withTransaction(
            Effect.gen(function* () {
              const [current] = yield* batchSql<{
                generation: string;
                state: string;
              }>`SELECT generation::text, state FROM event_history_authority WHERE singleton = true AND deployment_identity = ${Buffer.from(binding.manifestId, "hex")} AND owner_token = ${ownerToken}::uuid FOR UPDATE`;
              if (current === undefined || current.state !== "suspended")
                throw new Error(
                  "Pool history cleanup requires its own released authority",
                );
              const digest = Buffer.from(binding.digest, "hex");
              // Exact helper binding only; no CASCADE and no unrelated event rows.
              yield* batchSql`DELETE FROM event_history_l2_ledger_receipts WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_replay_receipts WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_block_applications WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_incarnations WHERE binding_digest = ${digest}`;
              yield* batchSql`DELETE FROM event_history_cursor WHERE binding_digest = ${digest}`;
              const deleted =
                yield* batchSql`DELETE FROM event_history_authority WHERE singleton = true AND deployment_identity = ${Buffer.from(binding.manifestId, "hex")} AND owner_token = ${ownerToken}::uuid AND generation = ${current.generation}::bigint AND state = 'suspended' RETURNING singleton`;
              if (deleted.length !== 1)
                throw new Error("Pool history cleanup authority changed");
            }),
          );
          cleaned = true;
        }),
      );
      yield* Effect.addFinalizer(() => close.pipe(Effect.orDie));
      const owner = yield* makeEventHistoryOwner({
        binding,
        histories: SDK.requireEventHistoryContracts(recorded.fixture.contracts),
        ownerToken,
        cache: input.cache,
        slotToUnixTime: recorded.fixture.operatorLucid.slotToUnixTime,
        transport: transport.options,
        heartbeatIntervalMs: 1000,
        leaseDurationMs: 60_000,
        rollbackHorizon: 2160,
        retainedPointLimit: 16,
        maximumReceiptBytes: 16 * 1024 * 1024,
        reconcile: (change) =>
          ingestAtFollowerView({
            change,
            repair: repairUnpublishedHistoryLedger(change),
            network: "Preprod",
            slotToUnixTime: recorded.fixture.operatorLucid.slotToUnixTime,
          }).pipe(Effect.provideService(Globals, input.globals)),
      });
      lifecycle.owner = owner;
      yield* owner.awaitReady;
      yield* Ref.set(input.globals.EVENT_HISTORY_OWNER, owner);
      return {
        owner,
        close: close.pipe(
          Effect.provideService(SqlClient.SqlClient, batchSql),
          Effect.orDie,
        ),
      };
    });
    return yield* scoped.pipe(
      Effect.provideService(SqlClient.SqlClient, batchSql),
    );
  });
