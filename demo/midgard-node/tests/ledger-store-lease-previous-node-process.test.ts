import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Exit } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { releaseLedgerStoreLeaseOfPreviousNodeProcess } from "../src/commands/listen-startup.release-ledger-store-lease-of-previous-node-process.js";
import {
  NODE_PROCESS_MPF_AUDIT_LEASES,
  OFFLINE_MPF_AUDIT_LEASES,
} from "../src/commands/mpf-audit-leases.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as MpfEngineStateDB from "../src/database/mpfEngineState.js";
import { HistoryPreparation } from "../src/services/event-history-recovery.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// A node killed while its commit worker or its own payload audit held the
// ledger MPF lease leaves that lease live for its whole TTL; nothing else
// clears it. Startup retires it so the restarted node can commit at once, and
// keeps every lease a live process outside the node may hold.

const run = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(program));
const TEN_MINUTES_MS = 10 * 60 * 1000;

/** A lease a killed process took and never released. */
const leftBehind = (owner: string) =>
  Effect.gen(function* () {
    const acquired = yield* MpfEngineStateDB.acquireLedgerStoreLease({
      owner,
      ttlMs: TEN_MINUTES_MS,
    });
    if (!acquired) throw new Error("ledger MPF lease busy");
  });
const nextCommitAcquires = Effect.suspend(() =>
  MpfEngineStateDB.acquireLedgerStoreLease({
    owner: MpfEngineStateDB.nodeProcessCommitLeaseOwner(),
    ttlMs: TEN_MINUTES_MS,
  }),
);
const leaseOwner = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly lease_owner: string | null }>`SELECT
    lease_owner FROM mpf_engine_state WHERE store_name = 'ledger'`;
  return rows[0]?.lease_owner ?? null;
});

// The lease upsert creates the ledger row an empty table lacks.
beforeEach(async () => run(resetApplicationTables));

describe("startup retires the ledger MPF lease of a killed node process", () => {
  for (const [site, killed, prefix] of [
    [
      "commit",
      MpfEngineStateDB.nodeProcessCommitLeaseOwner(),
      MpfEngineStateDB.NODE_PROCESS_COMMIT_LEASE_OWNER_PREFIX,
    ],
    [
      "payload audit",
      NODE_PROCESS_MPF_AUDIT_LEASES.ledgerStoreOwner(),
      MpfEngineStateDB.NODE_PROCESS_AUDIT_LEASE_OWNER_PREFIX,
    ],
  ] as const)
    it(`retires a live node ${site} lease, so the next commit acquires the store`, async () => {
      expect(killed.startsWith(prefix)).toBe(true);
      await run(
        Effect.gen(function* () {
          yield* leftBehind(killed);
          expect(yield* nextCommitAcquires).toBe(false);
          const retired = yield* releaseLedgerStoreLeaseOfPreviousNodeProcess;
          expect(retired?.owner).toBe(killed);
          expect(retired?.expiresAt).toBeInstanceOf(Date);
          expect(yield* leaseOwner).toBeNull();
          const stale = yield* Effect.exit(
            MpfEngineStateDB.revalidateLedgerStoreLease(killed),
          );
          expect(Exit.isFailure(stale)).toBe(true);
          expect(yield* nextCommitAcquires).toBe(true);
        }),
      );
    });

  for (const owner of [
    `commit:${randomUUID()}`,
    OFFLINE_MPF_AUDIT_LEASES.ledgerStoreOwner(),
  ])
    it(`keeps ${owner.split(":")[0]}: leases, which a live offline command may hold`, async () => {
      await run(
        Effect.gen(function* () {
          yield* leftBehind(owner);
          expect(
            yield* releaseLedgerStoreLeaseOfPreviousNodeProcess,
          ).toBeUndefined();
          expect(yield* leaseOwner).toBe(owner);
          yield* MpfEngineStateDB.revalidateLedgerStoreLease(owner);
          expect(yield* nextCommitAcquires).toBe(false);
        }),
      );
    });

  it("retires only under the current history authority, never under a superseded or absent one", async () => {
    const killed = MpfEngineStateDB.nodeProcessCommitLeaseOwner();
    await run(
      Effect.gen(function* () {
        yield* leftBehind(killed);
        const acquire = Authority.acquire({
          deploymentIdentity: "ab".repeat(32),
          ownerToken: randomUUID(),
          leaseDurationMs: 60_000,
        });
        const stale = yield* acquire;
        const current = yield* acquire;
        const releaseAs = (authority: Authority.Token | undefined) =>
          Effect.exit(
            authority === undefined
              ? releaseLedgerStoreLeaseOfPreviousNodeProcess
              : releaseLedgerStoreLeaseOfPreviousNodeProcess.pipe(
                  Effect.provideService(HistoryPreparation, {
                    token: authority,
                    assertCurrent: Effect.void,
                  }),
                ),
          );
        expect(Exit.isFailure(yield* releaseAs(undefined))).toBe(true);
        expect(Exit.isFailure(yield* releaseAs(stale))).toBe(true);
        expect(yield* leaseOwner).toBe(killed);
        yield* MpfEngineStateDB.revalidateLedgerStoreLease(killed);
        const released = yield* releaseAs(current);
        expect(Exit.isSuccess(released)).toBe(true);
        expect(yield* leaseOwner).toBeNull();
      }),
    );
  });
});
