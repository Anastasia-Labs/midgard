import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Globals } from "../src/services/globals.js";
import { prepareStateQueueCorrectionRewind } from "../src/services/state-queue-correction-rewind.js";
import {
  activeE,
  BINDING,
  E_HEADER,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import { authority } from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  attempt,
  checkpoint,
  type Node,
  onNode,
  ownerModel,
  plans,
  statusOf,
  UTXOS_ROOT,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";
import type { OwnerModel } from "./helpers/history-expired-intent-release-preparation.owner.js";

/**
 * The state-queue correction rewind while its retained native owner cannot
 * open yet. Its obligation and its re-proof are modelled (an admitted
 * correction removed E, a block whose signed commit was never acknowledged);
 * the rest is the production preparation over seeded SQL. While the store's LevelDB lock is
 * held, the open is converted to a held gate: nothing written, no owner
 * installed. Once it is released the rewind runs exactly once. A binary
 * digest mismatch stays a failure.
 */

const fixture = vi.hoisted(
  (): { open: "locked" | "mismatch" | OwnerModel; opens: number } => ({
    open: "locked",
    opens: 0,
  }),
);

const REMOVAL = "aa".repeat(32);

/** The modelled obligation: E, removed by an admitted correction, owed
 * until E is reopened and abandoned under it. */
const obligation = Effect.map(
  Pending.retrieveByHeaderHash(E_HEADER, true),
  (found) =>
    Option.isNone(found) ||
    found.value[Pending.Columns.STATUS] === Pending.Status.Abandoned
      ? { kind: "none" as const }
      : {
          kind: "ready" as const,
          chain: [
            {
              record: found.value,
              transitionDigest: REMOVAL,
              kind: "removed" as const,
            },
          ],
          parentAggregate: undefined,
        },
);

vi.mock(
  "../src/services/state-queue-correction-rewind.prove-unlanded.js",
  async (original) => ({
    ...(await original<
      typeof import("../src/services/state-queue-correction-rewind.prove-unlanded.js")
    >()),
    loadObligation: () => obligation,
  }),
);
vi.mock(
  "../src/services/state-queue-correction-rewind.load-retained-chain.js",
  async (original) => ({
    ...(await original<
      typeof import("../src/services/state-queue-correction-rewind.load-retained-chain.js")
    >()),
    loadRetainedChain: () => obligation,
  }),
);
vi.mock("../src/services/mpf-native-owner/service.js", async (original) => {
  const actual =
    await original<
      typeof import("../src/services/mpf-native-owner/service.js")
    >();
  const { fakeOwner } = await import(
    "./helpers/history-expired-intent-release-preparation.owner.js"
  );
  return {
    ...actual,
    ProductionNativeMpfOwnerService: {
      create: async () => {
        fixture.opens += 1;
        if (fixture.open === "locked")
          throw new Error("Native MPF owner failed to open its store", {
            cause: Object.assign(new Error("Database is locked"), {
              code: "LEVEL_LOCKED",
            }),
          });
        if (fixture.open === "mismatch")
          throw new Error(
            "binarySha256 does not match the pinned owner binary",
          );
        return fakeOwner(fixture.open);
      },
    },
  };
});

const rewind = (node: Node) =>
  attempt(
    prepareStateQueueCorrectionRewind({
      bindingDigest: BINDING,
      checkpoint,
      preparation: { token: node.token, assertCurrent: Effect.void },
      config: {} as never,
      authority,
    }),
  );

/** E journaled and promoted (the native root and the SQL marker at its
 * candidate), its signed commit never acknowledged. */
const removedE = Effect.gen(function* () {
  yield* activeE();
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE pending_block_finalizations
    SET block_end_time = block_start_time + INTERVAL '1 second'
    WHERE header_hash = ${E_HEADER}`;
  yield* withNativeReplay(E_HEADER);
});

const state = Effect.gen(function* () {
  const globals = yield* Globals;
  const e = yield* statusOf(E_HEADER);
  return {
    e: e?.status,
    digest: e?.digest,
    plans: (yield* plans).map(({ state }) => state),
    ownerOpen: (yield* Ref.get(globals.NATIVE_MPF_OWNER)) !== undefined,
  };
});

describe("the correction rewind while its native owner cannot open", () => {
  it("holds while the store's lock is held, writing nothing, then rewinds exactly once after it is released", async () => {
    const owner = ownerModel(ZERO_ROOT);
    fixture.opens = 0;
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* removedE;
        fixture.open = "locked";
        const locked = [yield* rewind(node), yield* rewind(node)];
        const held = yield* state;
        fixture.open = owner;
        const opened = yield* rewind(node);
        const after = yield* state;
        const again = yield* rewind(node);
        return { locked, held, opened, after, again, final: yield* state };
      }),
    );
    for (const attempt of result.locked)
      expect(attempt.failure).toBeUndefined();
    expect(result.held).toEqual({
      e: Pending.Status.PendingSubmission,
      digest: undefined,
      plans: [],
      ownerOpen: false,
    });
    expect(result.opened.failure).toBeUndefined();
    expect(result.after).toEqual({
      e: Pending.Status.Abandoned,
      digest: REMOVAL,
      plans: ["applied"],
      ownerOpen: true,
    });
    expect(owner.durableRoot).toBe(UTXOS_ROOT);
    expect(owner.restores).toBe(1);
    // Nothing is owed any more: the next evaluation writes nothing.
    expect(result.again.failure).toBeUndefined();
    expect(result.final.plans).toEqual(["applied"]);
    expect(fixture.opens).toBe(3);
  });

  it("still fails on a binary digest mismatch, writing nothing", async () => {
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* removedE;
        fixture.open = "mismatch";
        return { attempt: yield* rewind(node), after: yield* state };
      }),
    );
    expect(result.attempt.failure).toContain(
      "Retained native rewind owner could not open",
    );
    expect(result.after).toEqual({
      e: Pending.Status.PendingSubmission,
      digest: undefined,
      plans: [],
      ownerOpen: false,
    });
  });
});
