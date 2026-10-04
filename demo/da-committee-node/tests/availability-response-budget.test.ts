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
 * public profiles. A wait that cannot be read from the code that enforces it
 * keeps the analysis incomplete, never an assumed number.
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
  L1_MEAN_BLOCK_MS: number;
  L1_BLOCK_SAFETY_FACTOR: number;
  DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS: number;
  DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS: number;
}>;

const script = (await import(
  new URL("../../scripts/deployment-profiles.mjs", import.meta.url).href
)) as ProfileScript;

type BudgetAnalysis = Readonly<{
  accepted: boolean;
  totalMs: number | undefined;
  lowerBoundMs: number;
  missing: readonly string[];
  exceeded: readonly Readonly<{ name: string }>[];
}>;
const { analyzeAvailabilityResponseBudget: analyzeBudget } = (await import(
  new URL("../../scripts/availability-response-budget.mjs", import.meta.url)
    .href
)) as Readonly<{
  analyzeAvailabilityResponseBudget: (
    profile: Profile,
    waits: readonly BoundedWait[],
  ) => BudgetAnalysis;
}>;

// Synthetic complete inputs test the analyzer, never stand in for runtime bounds.
const completeWaits = (totalMs: number): readonly BoundedWait[] =>
  (["A1", "A5", "A7", "B6"] as const).map((item, index) => ({
    item,
    name: "analyzer fixture",
    ms: index === 0 ? totalMs : 0,
    source: "synthetic analyzer input",
  }));

/** Both live testing profiles and both public profiles. */
const BUDGETED_PROFILES = [
  "local-devnet-testing",
  "preprod-testing",
  "preprod-public",
  "mainnet",
] as const;

type BoundedWait = Readonly<{
  /** The REPORT.md item that introduces the wait. */
  item: "A1" | "A5" | "A7" | "B6";
  name: string;
}> &
  (
    | Readonly<{ ms: number; source: string }>
    | Readonly<{ unresolved: string; ms?: undefined }>
  );

const BOUNDED_WAITS: readonly BoundedWait[] = [
  {
    item: "A1",
    name: "foreign payload retrieval cooldown",
    unresolved:
      "B1-A1: the bounded payload-by-header client and its retry cooldown are not on this branch (no demo/midgard-node/src/da/foreign-payload-retriever.ts); import its cooldown here when it lands",
  },
  {
    item: "A7",
    name: "one bounded rebuild after a protocol-parameter refresh",
    unresolved:
      "B1-A7: the refresh-and-rebuild-once wrapper is not on this branch (no demo/midgard-watcher/src/funding/protocol-parameter-retry.ts); import its time bound here when it lands",
  },
  {
    item: "B6",
    name: "bounded retries at the owning operation",
    unresolved:
      "B1-B6: the responder has no bounded retry budget on this branch (a failed drain is retried on the next poll, without a limit); import the owning operation's retry budget here when it lands",
  },
  {
    item: "A5",
    name: "rebroadcast before replace",
    // A signed responder transaction that has not landed is rebroadcast and
    // replaced only once it can no longer land, that is once its validity
    // has passed; it was built at most this long before then.
    ms:
      AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS -
      AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS,
    source:
      "demo/da-committee-node/src/availability/factory.discover-availability-responder-challenges.ts",
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
  (selected.l1_finality.confirmation_depth +
    script.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS +
    script.DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS) *
  script.L1_MEAN_BLOCK_MS *
  script.L1_BLOCK_SAFETY_FACTOR;

const budgetTable = (selected: Profile): string => {
  const base = baseTermMs(selected);
  const known = BOUNDED_WAITS.reduce((sum, wait) => sum + (wait.ms ?? 0), 0);
  return [
    `${selected.name}: depth ${selected.l1_finality.confirmation_depth.toString()}, ` +
      `${script.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS.toString()} publications, ` +
      `${script.DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS.toString()} poll, ` +
      `${script.L1_MEAN_BLOCK_MS.toString()} ms x ${script.L1_BLOCK_SAFETY_FACTOR.toString()} = ${base.toString()} ms`,
    ...BOUNDED_WAITS.map((wait) =>
      wait.ms === undefined
        ? `  + ${wait.item} ${wait.name}: unknown (${wait.unresolved})`
        : `  + ${wait.item} ${wait.name}: ${wait.ms.toString()} ms (${wait.source})`,
    ),
    `  = ${(base + known).toString()} ms counted, against a small response window of ` +
      `${selected.timing.da_small_response_window_ms.toString()} ms and block maturity of ` +
      `${selected.timing.block_maturity_ms.toString()} ms`,
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

  it("derives every counted wait from the code that enforces it", () => {
    expect(new Set(BOUNDED_WAITS.map(({ item }) => item))).toEqual(
      new Set(["A1", "A5", "A7", "B6"]),
    );
    for (const wait of BOUNDED_WAITS) {
      if (wait.ms === undefined) {
        expect(wait.unresolved.length).toBeGreaterThan(0);
        continue;
      }
      expect(Number.isSafeInteger(wait.ms) && wait.ms > 0).toBe(true);
    }
  });

  it.each(BUDGETED_PROFILES)(
    "%s: the confirmation, publication and poll term is the profile generator's minimum response budget",
    (name) => {
      const selected = profile(name);
      expect(baseTermMs(selected)).toBe(
        script.minimumDaResponseBudgetMs(selected),
      );
    },
  );

  it.each(BUDGETED_PROFILES)(
    "%s: unresolved waits prevent full-budget acceptance",
    (name) => {
      const selected = profile(name);
      const result = analyzeBudget(selected, BOUNDED_WAITS);
      // This is an analyzer regression, not a claim that the profile is safe.
      // #704 remains open until every wait is enforced and the total fits.
      expect(result.accepted, budgetTable(selected)).toBe(false);
      expect(result.totalMs).toBeUndefined();
      expect(result.missing).toEqual(["A1", "A7", "B6"]);
      expect(result.lowerBoundMs).toBe(baseTermMs(selected) + 60_000);
    },
  );

  it("rejects an omitted wait even when all counted waits fit", () => {
    const result = analyzeBudget(profile("preprod-testing"), [
      BOUNDED_WAITS[3]!,
    ]);
    expect(result.accepted).toBe(false);
    expect(result.missing).toEqual(["A1", "A7", "B6"]);
  });

  it("detects a full budget overrun instead of accepting its base term", () => {
    const selected = profile("preprod-testing");
    const headroom =
      selected.timing.da_small_response_window_ms - baseTermMs(selected);
    const waits = completeWaits(headroom + 1);
    const result = analyzeBudget(selected, waits);
    expect(result.missing).toEqual([]);
    expect(result.totalMs).toBe(
      selected.timing.da_small_response_window_ms + 1,
    );
    expect(result.accepted).toBe(false);
    expect(result.exceeded.map(({ name }) => name)).toContain("small response");
  });

  it("accepts an exactly fitting complete response budget before maturity", () => {
    const selected = profile("preprod-testing");
    const headroom =
      selected.timing.da_small_response_window_ms - baseTermMs(selected);
    const result = analyzeBudget(selected, completeWaits(headroom));
    expect(result.totalMs).toBe(selected.timing.da_small_response_window_ms);
    expect(result.accepted).toBe(true);
  });

  it("refuses equality with block maturity even if response windows fit", () => {
    const selected = profile("preprod-testing");
    const result = analyzeBudget(
      {
        ...selected,
        timing: {
          ...selected.timing,
          block_maturity_ms: baseTermMs(selected),
        },
      },
      completeWaits(0),
    );
    expect(result.accepted).toBe(false);
    expect(result.exceeded.map(({ name }) => name)).toContain("block maturity");
  });

  it("rejects guessed, duplicate, negative or nonfinite wait durations", () => {
    for (const ms of [-1, Infinity, NaN, 0.5])
      expect(() =>
        analyzeBudget(profile("preprod-testing"), [
          { item: "A1", name: "fixture", ms, source: "fixture" },
        ]),
      ).toThrow(/enforced duration/u);
    expect(() =>
      analyzeBudget(profile("preprod-testing"), [
        { item: "A1", name: "fixture", ms: 1, source: "" },
      ]),
    ).toThrow(/enforced duration/u);
    expect(() =>
      analyzeBudget(profile("preprod-testing"), [
        BOUNDED_WAITS[3]!,
        BOUNDED_WAITS[3]!,
      ]),
    ).toThrow(/duplicate/u);
  });
});
