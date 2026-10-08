import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE,
  SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
} from "../src/services/liveness-halt.js";
import {
  BASE_AGGREGATE,
  CHILD_ROOT,
  compensationScenario,
  PREFIX_ROOT,
} from "./helpers/history-displacement-compensation-scenario.js";
import { UTXOS_ROOT } from "./helpers/history-expired-intent-release-before-ttl.js";
import type { Fixture } from "./helpers/history-expired-intent-release-preparation.js";

const fixture = vi.hoisted(
  (): Fixture => ({ queue: undefined, coverage: "unavailable" }),
);

vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.ledgerSnapshot(original),
  ),
);
vi.mock(
  "../src/services/history-expired-intent-release.signed-commit-node.js",
  (original) =>
    import(
      "./helpers/history-expired-intent-release-preparation.mocks.js"
    ).then((mocks) => mocks.queueAuthentication(original, fixture)),
);
vi.mock("../src/database/eventHistoryCanonicalCoverage.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.canonicalCoverage(original, fixture),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.nodeSerialization(original),
  ),
);

const domain = (plan: { intent: string }) =>
  (JSON.parse(plan.intent) as { domain: string }).domain;

describe("durable returned-prefix displacement compensation", () => {
  it("uses an admitted suffix removal despite future TTL and retains the correction rollback guard", async () => {
    const result = await compensationScenario(fixture, {
      bad: "future TTL",
      removedSuffix: true,
    });
    expect(result.native).toBe(PREFIX_ROOT);
    expect(result.ledger).toBe(PREFIX_ROOT);
    expect(result.child?.status).toBe(Pending.Status.Abandoned);
    const intent = JSON.parse(result.final[0]!.intent) as {
      suffixMembers: {
        headerHash: string;
        kind: string;
        transitionDigest: string;
      }[];
    };
    expect(intent.suffixMembers[0]?.kind).toBe("removed");
    expect(result.removed.get(intent.suffixMembers[0]!.headerHash)).toBe(
      intent.suffixMembers[0]?.transitionDigest,
    );
    expect(result.again.failure).toBeUndefined();
    expect(result.replays).toBe(0);
  });
  it.each([false, true])(
    "restores only the landed prefix from original native CAS performed=%s",
    async (beforeInitialCas) => {
      const result = await compensationScenario(fixture, { beforeInitialCas });
      expect(result.ledger).toBe(PREFIX_ROOT);
      expect(result.native).toBe(PREFIX_ROOT);
      expect(result.child?.status).toBe(Pending.Status.Abandoned);
      expect(result.s?.status).toBe(Pending.Status.LocallyApplied);
      expect(result.w?.status).toBe(Pending.Status.Abandoned);
      expect(result.replays).toBe(0);
      expect(result.final.map(({ state }) => state)).toEqual(["applied"]);
      const intent = JSON.parse(result.final[0]!.intent) as {
        originalRecoveryId: string;
        originalIntent: unknown;
      };
      expect(intent.originalRecoveryId).toBe(result.original[0]?.recovery_id);
      expect(typeof intent.originalIntent).toBe("object");
      expect(result.again.failure).toBeUndefined();
    },
  );
  it.each([
    "before replacement",
    "before compensation CAS",
    "after compensation CAS",
    "before SQL receipt",
  ] as const)(
    "resumes across %s with original closure and exact durable native identity",
    async (stop) => {
      const result = await compensationScenario(fixture, { stop });
      expect(result.interrupted.failure).toContain(`stop ${stop}`);
      expect(result.intermediate.map(({ state }) => state)).toEqual([
        "prepared",
      ]);
      expect(result.intermediateLedger).toBe(CHILD_ROOT);
      expect(domain(result.intermediate[0]!)).toContain(
        stop === "before replacement"
          ? "displaced-block-revival"
          : "displacement-compensation",
      );
      if (stop !== "before replacement")
        expect(result.nativeBoundaryPlans?.[0]?.recovery_id).toBe(
          result.intermediate[0]?.recovery_id,
        );
      expect(result.native).toBe(PREFIX_ROOT);
      expect(result.ledger).toBe(PREFIX_ROOT);
      expect(result.child?.status).toBe(Pending.Status.Abandoned);
      expect(result.replays).toBe(0);
      if (stop === "after compensation CAS" || stop === "before SQL receipt")
        expect(result.operations.at(-1)).toBe(
          result.intermediate[0]?.recovery_id,
        );
      expect(result.again.failure).toBeUndefined();
    },
  );
  it.each(["original winner", "full chain"] as const)(
    "rederives %s branch after compensation CAS before SQL",
    async (branch) => {
      const result = await compensationScenario(fixture, {
        stop: "after compensation CAS",
        branch,
      });
      expect(result.intermediateNative).toBe(PREFIX_ROOT);
      expect(result.again.failure).toBeUndefined();
      expect(result.final.map(({ state }) => state)).toEqual(["applied"]);
      expect(result.final[0]?.recovery_id).not.toBe(
        result.intermediate[0]?.recovery_id,
      );
      expect(result.native).toBe(
        branch === "full chain" ? CHILD_ROOT : UTXOS_ROOT,
      );
      expect(result.ledger).toBe(result.native);
      expect(result.child?.status).toBe(
        branch === "full chain"
          ? Pending.Status.LocallyApplied
          : Pending.Status.Abandoned,
      );
      expect(result.replays).toBe(0);
    },
  );
  it.each([
    "future TTL",
    "missing TTL",
    "coverage",
    "input",
    "signed input",
    "canonical suffix",
    "original root",
    "unknown root",
  ] as const)(
    "preserves the prepared obligation and signed ambiguity for %s",
    async (bad) => {
      const result = await compensationScenario(fixture, { bad });
      expect(result.final.map(({ state }) => state)).toEqual(["prepared"]);
      expect(result.final[0]?.recovery_id).toBe(
        result.original[0]?.recovery_id,
      );
      expect(result.ledger).toBe(CHILD_ROOT);
      expect(result.child?.status).toBe(Pending.Status.LocallyApplied);
      expect(result.operations).toHaveLength(1);
      expect(result.replays).toBe(0);
    },
  );
});

describe("the SQL ledger marker a compensation with no returned prefix writes", () => {
  // The original winner holds its base's slot again, so the compensation's
  // target root is the winner's base root: the marker takes the UTxO payload
  // aggregate of the journal the winner's base tail header hash names only
  // when that journal's expected root equals the target root.
  it.each([
    ["target root", BASE_AGGREGATE],
    ["other root", { entryCount: null, encodedTupleBytes: null }],
  ] as const)(
    "takes the base journal's aggregate only when its expected root is the target root (%s)",
    async (baseJournal, aggregate) => {
      const result = await compensationScenario(fixture, {
        stop: "after compensation CAS",
        branch: "original winner",
        baseJournal,
      });
      expect(result.again.failure).toBeUndefined();
      expect(result.final.map(({ state }) => state)).toEqual(["applied"]);
      expect(result.native).toBe(UTXOS_ROOT);
      expect(result.ledger).toBe(UTXOS_ROOT);
      expect(result.aggregate).toEqual(aggregate);
    },
  );
});

describe("a displacement compensation whose restore is refused as not retained", () => {
  it("holds under the revival source with its plan prepared, then completes once the root is retained", async () => {
    const result = await compensationScenario(fixture, {
      compensationRootNotRetained: true,
    });
    for (const attempt of [result.interrupted, result.interruptedAgain!]) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE)).toBe(
        SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
      );
    }
    // Held: the compensation plan prepared, native MPF at the original
    // displacement's target, the SQL marker and the journals as they were.
    expect(result.intermediate.map(({ state }) => state)).toEqual(["prepared"]);
    expect(domain(result.intermediate[0]!)).toContain(
      "displacement-compensation",
    );
    expect(result.intermediateNative).toBe(UTXOS_ROOT);
    expect(result.intermediateLedger).toBe(CHILD_ROOT);
    // Only the original displacement's restore ran; the refused one did not.
    expect(result.intermediateOperations).toHaveLength(1);
    // Retained again: the next evaluation completes it and clears the reason.
    expect(result.again.failure).toBeUndefined();
    expect(
      result.again.raised.get(HISTORY_REPLACED_BLOCK_REVIVAL_SOURCE),
    ).toBeUndefined();
    expect(result.final.map(({ state }) => state)).toEqual(["applied"]);
    expect(result.native).toBe(PREFIX_ROOT);
    expect(result.ledger).toBe(PREFIX_ROOT);
    expect(result.child?.status).toBe(Pending.Status.Abandoned);
    expect(result.operations).toEqual([
      ...result.intermediateOperations,
      result.intermediate[0]?.recovery_id,
    ]);
  });
});
