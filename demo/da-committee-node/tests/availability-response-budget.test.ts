import {
  DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS,
  DA_AVAILABILITY_VALIDITY_BACKDATE_MS,
  DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS,
} from "@al-ft/midgard-sdk/da";
import { describe, expect, it } from "vitest";

import {
  AVAILABILITY_RESPONDER_RETRY_POLICY,
  availabilityResponderRetryBoundMs,
} from "../src/availability-response-loop.js";

/**
 * The full availability response budget: a
 * committee answering a small-class challenge spends
 *
 *   (confirmation depth + chained publications + one poll block)
 *     x mean L1 block time x 2
 *
 * plus every other bounded wait on the way to its answer: one bounded
 * rebuild, the bounded retries at the owning operation and the
 * rebroadcast-before-replace wait. All of it must fit the
 * profile's small response window, for both live testing profiles and both
 * public profiles. Each wait is read from the code that enforces it. A wait
 * with no time bound in this tree is listed as unbounded and fails the budget;
 * it is never given an assumed number. When the budget does not fit, the
 * failure message carries every term, for the owner to rule on; the depth
 * is not changed to make it fit.
 *
 * Profiles are read from config/deployments through the profile generator
 * (demo/scripts/deployment-profiles.mjs), which is also the source of the
 * block-time model and the publication count; `deployment:check` keeps the
 * compiled profiles equal to those files.
 */

type Profile = Readonly<{
  name: string;
  l1_finality: Readonly<{ confirmation_depth: number }>;
  timing: Readonly<{
    block_maturity_ms: number;
    da_small_response_window_ms: number;
    da_full_response_window_ms: number;
  }>;
}>;

type ProfileScript = Readonly<{
  readProfiles: () => Readonly<Record<string, Profile>>;
  minimumDaResponseBudgetMs: (profile: Profile) => number;
  l1BlocksBudgetMs: (blocks: number) => number;
  L1_MEAN_BLOCK_MS: number;
  L1_BLOCK_SAFETY_FACTOR: number;
  DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS: number;
  DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS: number;
}>;

const script = (await import(
  new URL("../../scripts/deployment-profiles.mjs", import.meta.url).href
)) as ProfileScript;

/** Both live testing profiles and both public profiles. */
const BUDGETED_PROFILES = [
  "local-devnet-testing",
  "preprod-testing",
  "preprod-public",
  "mainnet",
] as const;

type BoundedWait = Readonly<{
  /** A short label for the wait. */
  item: "rebroadcast" | "rebuild" | "retries";
  name: string;
}> &
  (
    | Readonly<{ ms: number; derivation: string; unbounded?: undefined }>
    | Readonly<{ unbounded: string; ms?: undefined }>
  );

/**
 * A signed responder transaction that has not landed is rebroadcast with
 * its exact bytes while it can still land, and is replaced only once the
 * journal marks it expired. Reconciliation (midgard-sdk
 * availability-challenge-operation.reconcile.ts) marks it expired when the
 * canonical boundary's slot, the slot of the tip block, reaches the intent's
 * validity upper bound. The responder takes its interval from
 * `SDK.daAvailabilityValidityWindow` on the committee's L1 clock: the upper
 * bound runs at most the lookahead past the L1 clock, so the transaction was
 * built at most the lookahead before that bound (the lower bound is backdated
 * from the tip, and the whole interval stays within the validator's longest
 * range). The tip passes the bound only with the next L1 block, which is
 * counted like every other block: at the mean block time times the safety
 * factor.
 */
const REBROADCAST_VALIDITY_REMAINING_MS = DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS;
const REBROADCAST_EXPIRY_BLOCK_MS = script.l1BlocksBudgetMs(1);

const RETRY_BOUND_MS = availabilityResponderRetryBoundMs();

const BOUNDED_WAITS: readonly BoundedWait[] = [
  // The committee responder has no protocol-parameter refresh yet; adding it
  // is separate follow-up work, which must count its own bound here if it
  // adds a wait.
  {
    item: "rebuild",
    name: "one bounded rebuild after a protocol-parameter refresh",
    ms: 0,
    derivation:
      "no separate wait: the responder builds each action once per drain, so a " +
      "rebuild is the next drain, a bounded retry, already counted there",
  },
  {
    item: "retries",
    name: "bounded retries at the owning operation",
    ms: RETRY_BOUND_MS,
    derivation:
      `${AVAILABILITY_RESPONDER_RETRY_POLICY.retries.toString()} retries x backoff ceiling ` +
      `${AVAILABILITY_RESPONDER_RETRY_POLICY.backoffCeilingMs.toString()} ms ` +
      `(AVAILABILITY_RESPONDER_RETRY_POLICY, availability-response-loop.ts)`,
  },
  {
    item: "rebroadcast",
    name: "rebroadcast before replace",
    ms: REBROADCAST_VALIDITY_REMAINING_MS + REBROADCAST_EXPIRY_BLOCK_MS,
    derivation:
      `validity lookahead ${DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS.toString()} ms past the L1 clock ` +
      `(DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS, midgard-sdk ` +
      `availability-challenge-transactions.validity-window.ts) + one expiry block ` +
      `${REBROADCAST_EXPIRY_BLOCK_MS.toString()} ms`,
  },
];

const profiles = script.readProfiles();
const profile = (name: string): Profile => {
  const found = profiles[name];
  if (found === undefined)
    throw new Error(`Unknown deployment profile ${name}`);
  return found;
};

const baseTermMs = (selected: Profile): number =>
  script.l1BlocksBudgetMs(
    selected.l1_finality.confirmation_depth +
      script.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS +
      script.DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS,
  );

const countedWaitsMs = BOUNDED_WAITS.reduce(
  (sum, wait) => sum + (wait.ms ?? 0),
  0,
);
const unboundedWaits = BOUNDED_WAITS.filter(
  (wait) => wait.unbounded !== undefined,
);

/** What the small response window leaves after the counted budget. The
 * small window is the binding one: the profile generator keeps it at most
 * the full window. */
const responseMarginMs = (selected: Profile): number =>
  selected.timing.da_small_response_window_ms -
  (baseTermMs(selected) + countedWaitsMs);

/** Every term of a profile's budget, for the failure message. */
const budgetTable = (selected: Profile): string => {
  const base = baseTermMs(selected);
  const counted = base + countedWaitsMs;
  const window = selected.timing.da_small_response_window_ms;
  return [
    `${selected.name} availability response budget:`,
    `  L1 path: (depth ${selected.l1_finality.confirmation_depth.toString()} + ` +
      `${script.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS.toString()} publications + ` +
      `${script.DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS.toString()} poll) x ` +
      `${script.L1_MEAN_BLOCK_MS.toString()} ms x ${script.L1_BLOCK_SAFETY_FACTOR.toString()} = ` +
      `${base.toString()} ms`,
    ...BOUNDED_WAITS.map((wait) =>
      wait.ms === undefined
        ? `  + ${wait.name}: UNBOUNDED (${wait.unbounded})`
        : `  + ${wait.name}: ${wait.ms.toString()} ms (${wait.derivation})`,
    ),
    `  = ${counted.toString()} ms counted` +
      (unboundedWaits.length === 0
        ? ""
        : ` plus ${unboundedWaits.length.toString()} unbounded wait(s)`) +
      `; small response window ${window.toString()} ms ` +
      (counted <= window
        ? `(${(window - counted).toString()} ms left)`
        : `(OVER by ${(counted - window).toString()} ms)`) +
      `; full response window ${selected.timing.da_full_response_window_ms.toString()} ms; ` +
      `block maturity ${selected.timing.block_maturity_ms.toString()} ms`,
  ].join("\n");
};

describe("availability response budget", () => {
  it("runs the live testing profiles at 10 confirmations and the public profiles at 30", () => {
    expect(
      Object.fromEntries(
        BUDGETED_PROFILES.map((name) => [
          name,
          profile(name).l1_finality.confirmation_depth,
        ]),
      ),
    ).toEqual({
      "local-devnet-testing": 10,
      "preprod-testing": 10,
      "preprod-public": 30,
      mainnet: 30,
    });
  });

  it.each(BUDGETED_PROFILES)(
    "%s: the confirmation, publication and poll term is the profile generator's minimum response budget",
    (name) => {
      const selected = profile(name);
      expect(baseTermMs(selected)).toBe(
        script.minimumDaResponseBudgetMs(selected),
      );
      expect(baseTermMs(selected)).toBe(
        (selected.l1_finality.confirmation_depth +
          script.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS +
          script.DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS) *
          script.L1_MEAN_BLOCK_MS *
          script.L1_BLOCK_SAFETY_FACTOR,
      );
    },
  );

  it("counts each named wait exactly once", () => {
    expect(BOUNDED_WAITS.map(({ item }) => item).sort()).toEqual([
      "rebroadcast",
      "rebuild",
      "retries",
    ]);
    for (const wait of BOUNDED_WAITS) {
      if (wait.ms === undefined) continue;
      expect(Number.isSafeInteger(wait.ms) && wait.ms >= 0).toBe(true);
    }
    expect(RETRY_BOUND_MS).toBeGreaterThan(0);
  });

  it("rebroadcast before replace waits out the 60 s validity lookahead, which fits the validator's range with the backdate", () => {
    expect(REBROADCAST_VALIDITY_REMAINING_MS).toBe(60_000);
    expect(
      DA_AVAILABILITY_VALIDITY_BACKDATE_MS +
        DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS,
    ).toBeLessThanOrEqual(Number(DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS));
  });

  it("every named wait has a time bound in the code", () => {
    expect(
      unboundedWaits.map((wait) => `${wait.name}: ${wait.unbounded}`),
      "a wait with no bound cannot be counted, so the budget cannot be shown to fit",
    ).toEqual([]);
  });

  it.each(BUDGETED_PROFILES)(
    "%s: the response budget fits the response windows and block maturity",
    (name) => {
      const selected = profile(name);
      // With unbounded waits this is a lower bound: if it already overruns a
      // window, the full budget does too.
      const counted = baseTermMs(selected) + countedWaitsMs;
      expect(
        counted <= selected.timing.da_small_response_window_ms &&
          counted <= selected.timing.da_full_response_window_ms &&
          counted < selected.timing.block_maturity_ms,
        budgetTable(selected),
      ).toBe(true);
    },
  );

  it.each(BUDGETED_PROFILES)(
    "%s: the small response window leaves a positive margin over the counted budget",
    (name) => {
      const selected = profile(name);
      expect(responseMarginMs(selected), budgetTable(selected)).toBeGreaterThan(
        0,
      );
    },
  );

  it("the margins are the ruled ones: 110 s at depth 10, 2,030 s at depth 30", () => {
    expect(
      Object.fromEntries(
        BUDGETED_PROFILES.map((name) => [
          name,
          responseMarginMs(profile(name)),
        ]),
      ),
    ).toEqual({
      "local-devnet-testing": 110_000,
      "preprod-testing": 110_000,
      "preprod-public": 2_030_000,
      mainnet: 2_030_000,
    });
  });
});
