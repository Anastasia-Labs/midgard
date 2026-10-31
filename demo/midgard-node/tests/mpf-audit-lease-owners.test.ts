import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect, Ref, Runtime } from "effect";
import { describe, expect } from "vitest";

import { runMpfAudit } from "../src/commands/mpf-audit.js";
import * as MpfEngineStateDB from "../src/database/mpfEngineState.js";
import * as StateQueueLeases from "../src/database/stateQueueMutationLeases.js";
import { runLedgerPayloadAudit } from "../src/fibers/mpf-payload-audit.js";
import type { Database } from "../src/services/database.js";
import { Globals } from "../src/services/index.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// Node startup retires the running node's own audit leases after a kill and
// keeps the offline command's, so each audit must take both leases under its
// own names. The persisted-root read runs under both leases, so it records
// the holder and owner they were taken as.

const DURABLE_ROOT = "ab".repeat(32);

type Held = {
  readonly stateQueueHolder: string | undefined;
  readonly ledgerOwner: string | null;
};

const auditDb = <A, E>(effect: Effect.Effect<A, E, any>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }).pipe(Effect.provide(Globals.Default)),
  ) as Effect.Effect<A, E, never>;

/** Reads the leases held at this moment, then answers the persisted root. */
const readHeldLeases = (seen: Held[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const active = yield* StateQueueLeases.retrieveActive();
    const rows = yield* sql<{ readonly lease_owner: string | null }>`SELECT
      lease_owner FROM mpf_engine_state WHERE store_name = 'ledger'`;
    seen.push({
      stateQueueHolder: active?.[StateQueueLeases.Columns.HOLDER],
      ledgerOwner: rows[0]?.lease_owner ?? null,
    });
    return DURABLE_ROOT;
  });

/** Both leases are released once the audit returns. */
const expectReleased = Effect.gen(function* () {
  expect(yield* StateQueueLeases.retrieveActive()).toBeUndefined();
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly lease_owner: string | null }>`SELECT
    lease_owner FROM mpf_engine_state WHERE store_name = 'ledger'`;
  expect(rows[0]?.lease_owner ?? null).toBeNull();
});

describe("MPF audit lease names", () => {
  it.effect(
    "the running node's payload audit holds both leases under node-process names",
    () =>
      auditDb(
        Effect.gen(function* () {
          const seen: Held[] = [];
          const runtime = yield* Effect.runtime<
            Database | SqlClient.SqlClient
          >();
          const globals = yield* Globals;
          yield* Ref.set(globals.NATIVE_MPF_OWNER, {
            diagnostics: () =>
              Runtime.runPromise(runtime)(
                readHeldLeases(seen).pipe(
                  Effect.map((durableRoot) => ({
                    durableRoot,
                    activeGenerations: 1,
                  })),
                ),
              ),
          } as never);
          const result = yield* runLedgerPayloadAudit;
          expect(result.persistedRoot).toBe(DURABLE_ROOT);
          expect(seen).toHaveLength(1);
          expect(seen[0]!.stateQueueHolder).toBe("node-mpf-payload-audit");
          expect(
            seen[0]!.ledgerOwner?.startsWith(
              MpfEngineStateDB.NODE_PROCESS_AUDIT_LEASE_OWNER_PREFIX,
            ),
          ).toBe(true);
          yield* expectReleased;
        }),
      ),
  );

  it.effect(
    "the offline mpf-audit command keeps the names startup leaves alone",
    () =>
      auditDb(
        Effect.gen(function* () {
          const seen: Held[] = [];
          const runtime = yield* Effect.runtime<
            Database | SqlClient.SqlClient
          >();
          const result = yield* runMpfAudit({
            readNativeDurableRoot: Effect.promise(() =>
              Runtime.runPromise(runtime)(readHeldLeases(seen)),
            ),
          });
          expect(result.persistedRoot).toBe(DURABLE_ROOT);
          expect(seen).toHaveLength(1);
          expect(seen[0]!.stateQueueHolder).toBe("mpf-payload-audit");
          expect(seen[0]!.ledgerOwner).toMatch(/^audit:/);
          yield* expectReleased;
        }),
      ),
  );
});
