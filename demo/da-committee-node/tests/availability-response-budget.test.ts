import { describe, expect, it } from "vitest";

import {
  AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS,
  AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS,
} from "../src/availability/factory.discover-availability-responder-challenges.js";

/**
 * The full availability response budget (owner ruling B1, 2026-10-01): a
 * committee answering a small-class challenge spends
 *
 *   (confirmation depth + chained publications + one poll block)
 *     x mean L1 block time x 2
 *
 * plus every other bounded wait on the way to its answer: the A1 retrieval
 * cooldown, the A7 one bounded rebuild, the B6 bounded retries at the owning
 * operation and the A5 rebroadcast-before-replace wait. All of it must fit the
 * profile's small response window, for both live testing profiles and both
 * public profiles. Each wait is read from the code that enforces it. A wait
 * with no time bound in this tree is listed as unbounded and fails the budget;
 * it is never given an assumed number. When the budget does not fit, the
 * failure message carries every term, for the owner to rule on (B1); the depth
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
  /** The public-testnet decision item that introduces the wait. */
  item: "A1" | "A5" | "A7" | "B6";
  name: string;
}> &
  (
    | Readonly<{ ms: number; derivation: string; unbounded?: undefined }>
    | Readonly<{ unbounded: string; ms?: undefined }>
  );

/**
 * A5: a signed responder transaction that has not landed is rebroadcast with
 * its exact bytes while it can still land, and is replaced only once the
 * journal marks it expired. Reconciliation (midgard-sdk
 * availability-challenge-operation.reconcile.ts) marks it expired when the
 * canonical boundary's slot, the slot of the tip block, reaches the intent's
 * validity upper bound. The transaction was built at most span minus backdate
 * before that bound, and the tip passes the bound only with the next L1 block,
 * which is counted like every other block: at the mean block time times the
 * safety factor.
 */
const A5_VALIDITY_REMAINING_MS =
  AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS -
  AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS;
const A5_EXPIRY_BLOCK_MS = script.l1BlocksBudgetMs(1);

const BOUNDED_WAITS: readonly BoundedWait[] = [
  {
    item: "A1",
    name: "foreign payload retrieval cooldown",
    unbounded:
      "no bounded foreign payload retriever or retrieval cooldown exists in this tree",
  },
  {
    item: "A7",
    name: "one bounded rebuild after a protocol-parameter refresh",
    unbounded:
      "no refresh-and-rebuild-once wrapper exists in this tree; the responder neither refreshes protocol parameters nor rebuilds",
  },
  {
    item: "B6",
    name: "bounded retries at the owning operation",
    unbounded:
      "the responder has no retry budget: a failed drain is retried on the next poll, with no limit (availability-response-loop.ts)",
  },
  {
    item: "A5",
    name: "rebroadcast before replace",
    ms: A5_VALIDITY_REMAINING_MS + A5_EXPIRY_BLOCK_MS,
    derivation:
      `validity span ${AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS.toString()} ms - backdate ` +
      `${AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS.toString()} ms ` +
      `(factory.discover-availability-responder-challenges.ts) + one expiry block ` +
      `${A5_EXPIRY_BLOCK_MS.toString()} ms`,
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
        ? `  + ${wait.item} ${wait.name}: UNBOUNDED (${wait.unbounded})`
        : `  + ${wait.item} ${wait.name}: ${wait.ms.toString()} ms (${wait.derivation})`,
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

describe("availability response budget (B1)", () => {
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
      "A1",
      "A5",
      "A7",
      "B6",
    ]);
    for (const wait of BOUNDED_WAITS) {
      if (wait.ms === undefined) continue;
      expect(Number.isSafeInteger(wait.ms) && wait.ms > 0).toBe(true);
    }
  });

  it("every named wait has a time bound in the code", () => {
    expect(
      unboundedWaits.map(
        (wait) => `${wait.item} ${wait.name}: ${wait.unbounded}`,
      ),
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
});
