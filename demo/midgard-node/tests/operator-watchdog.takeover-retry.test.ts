import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { citableNeglectedEventProgram } from "../src/fibers/operator-watchdog.takeover-retry.js";
import {
  type CitationFailures,
  emptyCitationFailures,
  MAX_STRIKE_ATTEMPTS_PER_CITATION,
  recordCitationFailure,
} from "../src/fibers/operator-watchdog-policy.js";
import type { PublishedOperatorSet } from "../src/l1-operator-set/index.js";

const claim = (txHash: string): SDK.NeglectedUserEventClaim => ({
  kind: "Deposit",
  utxo: {
    txHash: txHash.repeat(64),
    outputIndex: 0,
    address: "addr_test1",
    assets: { lovelace: 2_000_000n },
  },
  inclusionTimeMs: 1_000n,
});

/** A published set whose follower store holds `events`, earliest first. */
const setHolding = (
  events: readonly SDK.NeglectedUserEventClaim[],
): PublishedOperatorSet =>
  ({
    neglectedUserEvent: (_tailEndMs: bigint, excluded?: ReadonlySet<string>) =>
      Promise.resolve(
        events.find(
          (event) => !excluded?.has(SDK.neglectedUserEventCitationId(event)),
        ) ?? null,
      ),
  }) as unknown as PublishedOperatorSet;

/** The failures after `event`'s strikes failed: refused once, or run out of
 * attempts. */
const failed = (
  failures: CitationFailures,
  event: SDK.NeglectedUserEventClaim,
  how: "refused" | "attempts",
): CitationFailures => {
  let current = failures;
  const times = how === "refused" ? 1 : MAX_STRIKE_ATTEMPTS_PER_CITATION;
  for (let attempt = 0; attempt < times; attempt += 1)
    current = recordCitationFailure(current, {
      schedulerRef: "sched#0",
      citationId: SDK.neglectedUserEventCitationId(event),
      refused: how === "refused",
    }).failures;
  return current;
};

const cited = (
  events: readonly SDK.NeglectedUserEventClaim[],
  failures: CitationFailures,
) =>
  Effect.runPromise(
    citableNeglectedEventProgram(setHolding(events), 0n, failures),
  );

describe("the event the watchdog's next strike cites", () => {
  const first = claim("a");
  const second = claim("b");

  it("cites the next event past one whose attempts ran out", async () => {
    const failures = failed(emptyCitationFailures, first, "attempts");
    expect(await cited([first, second], failures)).toEqual(second);
  });

  it("cites again an event whose attempts ran out when no other is citable", async () => {
    const failures = failed(emptyCitationFailures, first, "attempts");
    expect(await cited([first], failures)).toEqual(first);
  });

  it("never cites again an event a script refused", async () => {
    const failures = failed(emptyCitationFailures, first, "refused");
    expect(await cited([first], failures)).toBeNull();
  });

  it("cites again the event whose attempts ran out, never the refused one", async () => {
    const failures = failed(
      failed(emptyCitationFailures, first, "refused"),
      second,
      "attempts",
    );
    expect(await cited([first, second], failures)).toEqual(second);
  });
});
