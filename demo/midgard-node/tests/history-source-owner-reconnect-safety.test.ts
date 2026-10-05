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

// The real authority SQL runs throughout; acquisition and recovery
// generations are only recorded, in the order the owner requests them.
const authority = vi.hoisted(() => ({
  acquired: 0,
  events: [] as string[],
}));
vi.mock("../src/database/eventHistoryAuthority.js", async (importOriginal) => {
  const { Effect } = await import("effect");
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
    beginRecovery: (...args: Parameters<typeof actual.beginRecovery>) =>
      Effect.suspend(() => {
        authority.events.push(`recovery:${args[1]}`);
        return actual.beginRecovery(...args);
      }),
  };
});

const scenario = (test: Parameters<typeof ownerScenario>[0]) =>
  ownerScenario(test, {
    reset: () => Object.assign(authority, { acquired: 0, events: [] }),
  });

// Samples the gate until the owner stops; true if it was ever open.
const everReadyUntilStopped = (
  owner: Parameters<Parameters<typeof ownerScenario>[0]>[0]["owner"],
) =>
  Effect.gen(function* () {
    let opened = false;
    const sampler = yield* Effect.fork(
      Effect.forever(
        owner.frontier.pipe(
          Effect.tap(({ ready }) =>
            Effect.sync(() => {
              opened ||= ready;
            }),
          ),
          Effect.zipRight(Effect.sleep("5 millis")),
        ),
      ),
    );
    const stopped = yield* owner.awaitStopped.pipe(
      Effect.flip,
      Effect.timeout("20 seconds"),
    );
    yield* Fiber.interrupt(sampler);
    return { stopped, opened };
  });

it(
  "re-validates its lease before every source session it opens after a drop",
  scenario(({ owner, link, advance }) =>
    Effect.gen(function* () {
      const head = yield* advance;
      yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
      authority.events.length = 0;
      link.state.onOpen = () => authority.events.push("open");
      link.state.down = true;
      link.drop();
      yield* eventually(
        owner.sourceStatus.pipe(
          Effect.filterOrFail(({ attempts }) => attempts >= 3),
        ),
      );
      link.state.down = false;
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
      );
      const opens = authority.events.flatMap((event, index) =>
        event === "open" ? [index] : [],
      );
      expect(opens.length).toBeGreaterThan(2);
      // Each new session's first socket follows a reconnect re-validation
      // made after the previous session's sockets.
      const revalidated = authority.events.flatMap((event, index) =>
        event === "recovery:history source reconnecting" ? [index] : [],
      );
      expect(revalidated.length).toBeGreaterThanOrEqual(3);
      expect(revalidated[0]).toBeLessThan(opens[0]!);
      expect(authority.acquired).toBe(1);
    }),
  ),
  180_000,
);

it(
  "stops and never reopens when another owner takes the authority while its source is down",
  scenario(({ owner, link, advance, forwards }) =>
    Effect.gen(function* () {
      const head = yield* advance;
      yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
      link.state.down = true;
      link.drop();
      yield* eventually(
        owner.sourceStatus.pipe(
          Effect.filterOrFail(({ state }) => state === "reconnecting"),
        ),
      );
      const journaled = forwards().length;
      const foreign = randomUUID();
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE event_history_authority SET owner_token = ${foreign}::uuid`;
      link.state.down = false;
      yield* advance;
      const { stopped, opened } = yield* everReadyUntilStopped(owner);
      expect(causeText(stopped.cause)).toMatch(
        /History authority generation or owner changed/u,
      );
      expect(opened).toBe(false);
      expect(yield* produces(owner)).toBe("Left");
      expect(forwards().length).toBe(journaled);
      // Never reclaimed: the other owner's token still holds the row.
      expect((yield* authorityRow).owner_token).toBe(foreign);
      expect(authority.acquired).toBe(1);
    }),
  ),
  180_000,
);

it(
  "rewinds once and resumes when a reconnected session's source rolls back behind its intersection",
  scenario(({ owner, link, source, advance, extend, forwards, changes }) =>
    Effect.gen(function* () {
      const parent = yield* advance;
      const head = yield* advance;
      yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
      // A reconnected session intersects at the journal head, so its own
      // path starts there and the parent is behind it.
      link.drop();
      // The drop reaches the owner asynchronously: wait for the gate to
      // close before waiting for the reconnected session to reopen it.
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => !ready)),
      );
      yield* eventually(
        owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
      );
      const before = yield* applications(head.id);
      const rollbacksBefore = changes.filter(
        ({ kind }) => kind === "rollback",
      ).length;
      const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));

      source.rollbackTo(parent.id);
      const fork = yield* extend;
      yield* owner.awaitReadyAt(fork).pipe(Effect.timeout("15 seconds"));

      const rollbacks = changes
        .filter(({ kind }) => kind === "rollback")
        .slice(rollbacksBefore)
        .map(({ after }) => after.head.id);
      expect(rollbacks).toEqual([parent.id]);
      expect(forwards().filter((id) => id === fork.id)).toHaveLength(1);
      expect(forwards().filter((id) => id === head.id)).toHaveLength(1);
      expect(yield* applications(fork.id)).toEqual({
        total: before.total,
        block: 1,
      });
      expect((yield* authorityRow).point_hash?.toString("hex")).toBe(fork.id);
      expect(authority.acquired).toBe(1);
      expect(stopped.unsafePoll()).toBeNull();
      yield* Fiber.interrupt(stopped);
    }),
  ),
  180_000,
);
