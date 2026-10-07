import { Effect, Fiber } from "effect";
import { expect, it } from "vitest";

import { L1SourceUnavailable } from "../src/l1-source-unavailable.js";
import {
  applications,
  authorityRow,
  eventually,
  produces,
  scenario,
} from "./helpers/history-source-owner-reconnect.js";

const OUTAGE_LIMIT_MS = 1_500;
const MAX_BACKOFF_MS = 200;
const bounds = {
  sourceReconnect: {
    initialMs: 50,
    maxMs: MAX_BACKOFF_MS,
    outageLimitMs: OUTAGE_LIMIT_MS,
  },
};

it(
  "keeps its gated owner and lease beyond outage escalation, then resumes the same checkpoint and appends exactly once",
  scenario(
    ({ owner, link, advance, forwards }) =>
      Effect.gen(function* () {
        const head = yield* advance;
        yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
        const before = yield* authorityRow;
        const beforeApplications = yield* applications(head.id);
        const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));
        link.state.down = true;
        link.drop();
        yield* eventually(
          owner.sourceStatus.pipe(
            Effect.filterOrFail(({ state }) => state === "reconnecting"),
          ),
        );
        yield* Effect.sleep(OUTAGE_LIMIT_MS + MAX_BACKOFF_MS);
        expect(stopped.unsafePoll()).toBeNull();
        expect(yield* owner.sourceStatus).toMatchObject({
          state: "reconnecting",
          escalated: true,
        });
        expect((yield* owner.frontier).ready).toBe(false);
        expect(yield* produces(owner)).toBe("Left");
        const down = yield* authorityRow;
        const attempts = (yield* owner.sourceStatus).attempts;
        yield* Effect.sleep("350 millis");
        const stillDown = yield* authorityRow;
        expect(stillDown.lease_until.getTime()).toBeGreaterThan(
          down.lease_until.getTime(),
        );
        expect(stillDown.owner_token).toBe(before.owner_token);
        expect(stillDown.state).toBe("recovering");
        expect(stillDown.point_hash).toEqual(before.point_hash);
        expect((yield* owner.sourceStatus).attempts).toBeGreaterThan(attempts);
        expect(yield* applications(head.id)).toEqual(beforeApplications);
        expect(yield* produces(owner)).toBe("Left");

        link.state.down = false;
        yield* owner.awaitReadyAt(head).pipe(Effect.timeout("15 seconds"));
        expect(yield* owner.sourceStatus).toMatchObject({
          state: "following",
          escalated: false,
          since: null,
        });
        const next = yield* advance;
        yield* owner.awaitReadyAt(next).pipe(Effect.timeout("15 seconds"));
        expect(yield* applications(next.id)).toEqual({
          total: beforeApplications.total + 1,
          block: 1,
        });
        expect(forwards().filter((id) => id === next.id)).toHaveLength(1);
        expect((yield* authorityRow).owner_token).toBe(before.owner_token);
        expect(yield* produces(owner)).toBe("Right");
        expect(stopped.unsafePoll()).toBeNull();
        yield* Fiber.interrupt(stopped);
      }),
    bounds,
  ),
  180_000,
);

// Each new session journals the blocks that arrived meanwhile, then fails the
// same way before its gate can reopen. Those appends are not progress.
const completion = {
  failing: false,
  failures: 0,
  failedHead: undefined as string | undefined,
};
it(
  "keeps the gate closed through recurrent recoverable completion failures beyond escalation until completion succeeds",
  scenario(
    ({ owner, link, advance, forwards }) =>
      Effect.gen(function* () {
        const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));
        const journaled = forwards().length;
        const awaitFailedCompletion = (head: string) =>
          eventually(
            Effect.suspend(() =>
              forwards().includes(head) && completion.failedHead === head
                ? Effect.void
                : Effect.fail("waiting for journaled head to fail completion"),
            ),
          ).pipe(
            Effect.catchAll(() =>
              Effect.gen(function* () {
                return yield* Effect.fail(
                  new Error(
                    `Completion did not fail at journaled head ${head}: ${JSON.stringify(
                      {
                        completion,
                        forwards: forwards(),
                        frontier: yield* owner.frontier,
                        source: yield* owner.sourceStatus,
                        stopped: stopped.unsafePoll(),
                      },
                    )}`,
                  ),
                );
              }),
            ),
          );
        // An open gate appends without completing; a reconnect completes.
        completion.failing = true;
        link.drop();
        // Hold each source tip stable until the real journal and preparation
        // reach it; producing faster than replay can keep completion out of reach.
        const first = yield* advance;
        yield* awaitFailedCompletion(first.id);
        yield* Effect.sleep(OUTAGE_LIMIT_MS + 500);
        const second = yield* advance;
        yield* awaitFailedCompletion(second.id);
        expect(stopped.unsafePoll()).toBeNull();
        expect(completion.failures).toBeGreaterThan(1);
        expect((yield* owner.sourceStatus).escalated).toBe(true);
        // Reconnected sessions kept journaling new blocks behind the gate.
        expect(forwards().length - journaled).toBeGreaterThan(1);
        expect((yield* owner.frontier).ready).toBe(false);
        expect(yield* produces(owner)).toBe("Left");
        completion.failing = false;
        yield* eventually(
          owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
        );
        expect(yield* produces(owner)).toBe("Right");
        expect(stopped.unsafePoll()).toBeNull();
        yield* Fiber.interrupt(stopped);
      }),
    {
      ...bounds,
      reset: () =>
        Object.assign(completion, {
          failing: false,
          failures: 0,
          failedHead: undefined,
        }),
      prepareCompletion: (checkpoint) =>
        Effect.suspend(() => {
          if (!completion.failing) return Effect.void;
          completion.failures += 1;
          completion.failedHead = checkpoint.head.id;
          return Effect.die(
            new L1SourceUnavailable("Kupo has not indexed the released body"),
          );
        }),
    },
  ),
  180_000,
);

it(
  "closes its gate on a tolerated heartbeat miss, keeps renewing its lease, and reopens on the next answer",
  scenario(
    ({ owner, link }) =>
      Effect.gen(function* () {
        const before = yield* authorityRow;
        const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));
        link.state.stallTips = true;
        yield* eventually(
          owner.sourceStatus.pipe(
            Effect.filterOrFail((status) => status.state === "reconnecting"),
          ),
        );
        expect(yield* owner.sourceStatus).toMatchObject({
          reason: "history_owner_reconnecting",
          lastError: expect.stringMatching(/queryNetwork\/tip/u),
        });
        expect((yield* owner.frontier).ready).toBe(false);
        expect(yield* produces(owner)).toBe("Left");
        // The socket is open but silent: the keeper renews meanwhile.
        const leaseDown = (yield* authorityRow).lease_until.getTime();
        yield* Effect.sleep("350 millis");
        expect((yield* authorityRow).lease_until.getTime()).toBeGreaterThan(
          leaseDown,
        );
        expect((yield* owner.frontier).ready).toBe(false);

        link.state.stallTips = false;
        yield* eventually(
          owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
        );
        expect(yield* produces(owner)).toBe("Right");
        expect(yield* owner.sourceStatus).toMatchObject({
          state: "following",
          since: null,
        });
        const after = yield* authorityRow;
        expect(after.owner_token).toBe(before.owner_token);
        expect(after.state).toBe("ready");
        expect(stopped.unsafePoll()).toBeNull();
        yield* Fiber.interrupt(stopped);
      }),
    { timeoutMs: 1_500 },
  ),
  180_000,
);

// A pending reconciliation holds the gate across two recoverable failures
// further apart than the outage limit. Each session the source answers
// re-prepares the hold, which is the owner waiting on evidence, not on the
// source: the clock restarts, the owner never stops, and the gate reopens only
// once the evidence clears the hold.
const hold = { reason: undefined as string | undefined, prepared: 0 };
it(
  "keeps a pending reconciliation held over an answering source across failures further apart than the outage limit",
  scenario(
    ({ owner, link }) =>
      Effect.gen(function* () {
        const stopped = yield* Effect.fork(Effect.either(owner.awaitStopped));
        hold.reason = "Awaiting evidence the hold is gone";
        link.drop();
        yield* eventually(
          Effect.suspend(() =>
            hold.prepared > 0 ? Effect.void : Effect.fail("not held"),
          ),
        );
        expect((yield* owner.frontier).ready).toBe(false);
        yield* Effect.sleep(OUTAGE_LIMIT_MS + 500);
        // The second failure, past the limit since the first.
        const prepared = hold.prepared;
        link.drop();
        yield* eventually(
          Effect.suspend(() =>
            hold.prepared > prepared ? Effect.void : Effect.fail("not held"),
          ),
        );
        yield* Effect.sleep(MAX_BACKOFF_MS);
        expect(stopped.unsafePoll()).toBeNull();
        expect(yield* owner.reconciliationStatus).toEqual({
          status: "pending",
          reason: hold.reason,
        });
        expect((yield* owner.frontier).ready).toBe(false);
        expect(yield* produces(owner)).toBe("Left");
        // Only the evidence clears it.
        hold.reason = undefined;
        yield* eventually(
          owner.frontier.pipe(Effect.filterOrFail(({ ready }) => ready)),
        );
        expect(yield* produces(owner)).toBe("Right");
        expect(stopped.unsafePoll()).toBeNull();
        yield* Fiber.interrupt(stopped);
      }),
    {
      ...bounds,
      reset: () => Object.assign(hold, { reason: undefined, prepared: 0 }),
      pending: () => hold.reason,
      preparePendingReconciliation: () =>
        Effect.sync(() => {
          hold.prepared += 1;
        }),
    },
  ),
  180_000,
);
