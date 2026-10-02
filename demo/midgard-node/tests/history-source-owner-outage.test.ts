import { Effect, Fiber } from "effect";
import { expect, it } from "vitest";

import { L1SourceUnavailable } from "../src/l1-source-unavailable.js";
import { HistoryOwnerUnavailable } from "../src/services/event-history-owner.history-owner-change.js";
import {
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

/** The owner's own stop: the outage limit, not any other refusal. */
const outageStop = (stopped: unknown) => {
  expect(stopped).toBeInstanceOf(HistoryOwnerUnavailable);
  const outer = (stopped as HistoryOwnerUnavailable).cause;
  expect(outer).toBeInstanceOf(HistoryOwnerUnavailable);
  return String((outer as HistoryOwnerUnavailable).cause);
};

it(
  "stops once the source stays unreachable past the outage limit",
  scenario(
    ({ owner, link }) =>
      Effect.gen(function* () {
        link.state.down = true;
        const from = performance.now();
        link.drop();
        const stopped = yield* owner.awaitStopped.pipe(
          Effect.flip,
          Effect.timeout("20 seconds"),
        );
        const elapsed = performance.now() - from;
        expect(outageStop(stopped)).toMatch(
          /^History source unavailable for \d+ s: .*Ogmios chain-sync socket/u,
        );
        expect(elapsed).toBeGreaterThanOrEqual(OUTAGE_LIMIT_MS);
        // One more backoff, one failed open, and the stop itself.
        expect(elapsed).toBeLessThan(OUTAGE_LIMIT_MS + MAX_BACKOFF_MS + 3_000);
        expect((yield* owner.frontier).ready).toBe(false);
        expect(yield* produces(owner)).toBe("Left");
      }),
    bounds,
  ),
  180_000,
);

// Each new session journals the blocks that arrived meanwhile, then fails the
// same way before its gate can reopen. Those appends are not progress.
const completion = { failing: false, failures: 0 };
it(
  "stops when a recoverable failure recurs before its gate reopens, however many blocks it journals",
  scenario(
    ({ owner, link, advance, forwards }) =>
      Effect.gen(function* () {
        const journaled = forwards().length;
        // An open gate appends without completing; a reconnect completes.
        completion.failing = true;
        link.drop();
        const advancing = yield* Effect.fork(
          Effect.forever(
            advance.pipe(Effect.zipRight(Effect.sleep("150 millis"))),
          ),
        );
        const stopped = yield* owner.awaitStopped.pipe(
          Effect.flip,
          Effect.timeout("20 seconds"),
        );
        yield* Fiber.interrupt(advancing);
        expect(outageStop(stopped)).toMatch(
          /^History source unavailable for \d+ s: Kupo has not indexed/u,
        );
        expect(completion.failures).toBeGreaterThan(1);
        // Reconnected sessions kept journaling new blocks behind the gate.
        expect(forwards().length - journaled).toBeGreaterThan(1);
        expect((yield* owner.frontier).ready).toBe(false);
      }),
    {
      ...bounds,
      reset: () => Object.assign(completion, { failing: false, failures: 0 }),
      prepareCompletion: () =>
        Effect.suspend(() => {
          if (!completion.failing) return Effect.void;
          completion.failures += 1;
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
