import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Fiber } from "effect";
import { expect, it, vi } from "vitest";

import {
  applications,
  authorityRow,
  causeText,
  eventually,
  produces,
  scenario as ownerScenario,
} from "./helpers/history-source-owner-reconnect.js";

// The real authority SQL runs throughout. Only acquisition is counted, and a
// renewal may be replaced by the error a dropped database connection raises.
const authority = vi.hoisted(() => ({
  acquired: 0,
  renewals: 0,
  failRenewals: 0,
}));
vi.mock("../src/database/eventHistoryAuthority.js", async (importOriginal) => {
  const { Effect } = await import("effect");
  const { SqlError } = await import("@effect/sql");
  const { DatabaseError } = await import("../src/database/utils/common.js");
  const actual =
    await importOriginal<
      typeof import("../src/database/eventHistoryAuthority.js")
    >();
  return {
    ...actual,
    acquire: (...args: Parameters<typeof actual.acquire>) =>
      Effect.suspend(() => {
        authority.acquired += 1;
        return actual.acquire(...args);
      }),
    renew: (...args: Parameters<typeof actual.renew>) =>
      Effect.suspend(() => {
        authority.renewals += 1;
        if (authority.failRenewals === 0) return actual.renew(...args);
        authority.failRenewals -= 1;
        return Effect.fail(
          new DatabaseError({
            table: "event_history_authority",
            message: "Failed to renew history authority",
            cause: new SqlError.SqlError({
              message: "write CONNECTION_CLOSED",
              cause: Object.assign(new Error("write CONNECTION_CLOSED"), {
                code: "CONNECTION_CLOSED",
              }),
            }),
          }),
        );
      }),
  };
});

const scenario = (test: Parameters<typeof ownerScenario>[0]) =>
  ownerScenario(test, {
    reset: () =>
      Object.assign(authority, { acquired: 0, renewals: 0, failRenewals: 0 }),
  });

it(
  "reconnects after a dropped source socket, renewing its lease and closing its gate while down, then appends the next block exactly once",
  scenario(({ owner, link, advance, forwards }) =>
    Effect.gen(function* () {
      const head = yield* advance;
      yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
      const before = yield* authorityRow;
      const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));

      link.state.down = true;
      link.drop();
      yield* eventually(
        owner.sourceStatus.pipe(
          Effect.filterOrFail(
            (status) => status.state === "reconnecting" && status.attempts >= 2,
          ),
        ),
      );
      expect(yield* owner.sourceStatus).toMatchObject({
        reason: "history_owner_reconnecting",
        lastError: expect.stringMatching(/Ogmios chain-sync socket/u),
      });
      expect((yield* owner.frontier).ready).toBe(false);
      expect(yield* produces(owner)).toBe("Left");
      // The keeper renews while nothing source-authenticated can.
      const renewalsDown = authority.renewals;
      const leaseDown = (yield* authorityRow).lease_until.getTime();
      yield* Effect.sleep("350 millis");
      expect(authority.renewals).toBeGreaterThan(renewalsDown);
      expect((yield* authorityRow).lease_until.getTime()).toBeGreaterThan(
        leaseDown,
      );
      expect((yield* authorityRow).state).toBe("recovering");

      link.state.down = false;
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
      );
      expect(yield* owner.sourceStatus).toMatchObject({
        state: "following",
        reason: null,
        since: null,
      });
      expect(yield* produces(owner)).toBe("Right");
      const resumed = yield* applications(head.id);
      expect(resumed.block).toBe(1);

      const next = yield* advance;
      yield* owner.awaitReadyAt(next).pipe(Effect.timeout("15 seconds"));
      expect(yield* applications(next.id)).toEqual({
        total: resumed.total + 1,
        block: 1,
      });
      expect(yield* applications(head.id)).toMatchObject({ block: 1 });
      expect(forwards().filter((id) => id === next.id)).toHaveLength(1);
      expect(forwards().filter((id) => id === head.id)).toHaveLength(1);
      // One lease, one owner: re-validation moved only the generation.
      const after = yield* authorityRow;
      expect(authority.acquired).toBe(1);
      expect(after.owner_token).toBe(before.owner_token);
      expect(BigInt(after.generation)).toBeGreaterThan(
        BigInt(before.generation),
      );
      expect(after.state).toBe("ready");
      expect(after.point_hash?.toString("hex")).toBe(next.id);
      expect(stopped.unsafePoll()).toBeNull();
      yield* Fiber.interrupt(stopped);
    }),
  ),
  180_000,
);

it(
  "stops when no retained point intersects the source after a reconnect",
  scenario(({ owner, link }) =>
    Effect.gen(function* () {
      link.state.noIntersection = true;
      link.drop();
      const stopped = yield* owner.awaitStopped.pipe(
        Effect.flip,
        Effect.timeout("20 seconds"),
      );
      expect(causeText(stopped.cause)).toMatch(
        /Ogmios chain-sync error: .*No intersection found/u,
      );
      expect((yield* owner.frontier).ready).toBe(false);
      expect(yield* produces(owner)).toBe("Left");
    }),
  ),
  180_000,
);

it(
  "keeps its lease through connection-class renewal failures and reopens its gate",
  scenario(({ owner, advance }) =>
    Effect.gen(function* () {
      const before = yield* authorityRow;
      const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));
      authority.failRenewals = 3;
      // The gate closes while renewal is in doubt.
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => !ready)),
      );
      yield* eventually(
        Effect.suspend(() =>
          authority.failRenewals === 0
            ? Effect.void
            : Effect.fail(new Error("Not yet")),
        ),
      );
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
      );
      expect(yield* produces(owner)).toBe("Right");
      const next = yield* advance;
      yield* owner.awaitReadyAt(next).pipe(Effect.timeout("15 seconds"));
      const after = yield* authorityRow;
      expect(authority.acquired).toBe(1);
      expect(after.owner_token).toBe(before.owner_token);
      expect(after.state).toBe("ready");
      expect(stopped.unsafePoll()).toBeNull();
      yield* Fiber.interrupt(stopped);
    }),
  ),
  180_000,
);

it(
  "stops when its renewal is refused because another owner holds the authority",
  scenario(({ owner }) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE event_history_authority SET owner_token = ${randomUUID()}`;
      const stopped = yield* owner.awaitStopped.pipe(
        Effect.flip,
        Effect.timeout("20 seconds"),
      );
      expect(causeText(stopped.cause)).toMatch(
        /History authority generation or owner changed/u,
      );
      expect((yield* owner.frontier).ready).toBe(false);
    }),
  ),
  180_000,
);
