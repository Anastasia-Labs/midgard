import { SqlClient } from "@effect/sql";
import { Effect, Logger, Option, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { Globals } from "../src/services/globals.js";
import {
  activeLivenessReasons,
  CORRECTION_REWIND_TARGET_ROOT_NOT_RETAINED,
  HISTORY_CORRECTION_REWIND_SOURCE,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
} from "../src/services/liveness-halt.js";
import {
  NativeMpfFullIndexCapExceeded,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import {
  CORRECTION_REWIND_HELD_ON_NATIVE_STATE,
  prepareStateQueueCorrectionRewind,
} from "../src/services/state-queue-correction-rewind.js";
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
 * digest mismatch stays a failure. A native owner that refuses the restore
 * because it does not retain the target root in full holds the rewind on the
 * native state, its plan retained, until it retains the root again.
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

const prepare = (node: Node) =>
  prepareStateQueueCorrectionRewind({
    bindingDigest: BINDING,
    checkpoint,
    preparation: { token: node.token, assertCurrent: Effect.void },
    config: {} as never,
    authority,
  });

const rewind = (node: Node) => attempt(prepare(node));

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

describe("the correction rewind while its native owner does not retain the target root", () => {
  it("holds on the native state with its plan retained, writing nothing else, then completes once the root is retained", async () => {
    const owner = ownerModel(ZERO_ROOT);
    let retained = false;
    owner.beforeRestore = async ({ targetRoot }) => {
      if (!retained) throw new NativeMpfRootNotRetained(targetRoot);
    };
    const logs: string[] = [];
    const captured = Logger.add(
      Logger.make(({ message }) => {
        logs.push([message].flat().map(String).join(" "));
      }),
    );
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* removedE;
        fixture.open = owner;
        const returned = yield* prepare(node).pipe(Effect.provide(captured));
        const attempts = [yield* rewind(node), yield* rewind(node)];
        const held = { ...(yield* state), root: owner.durableRoot };
        retained = true;
        const completed = yield* rewind(node);
        return { returned, attempts, held, completed, after: yield* state };
      }),
    );
    expect(result.returned).toBe(CORRECTION_REWIND_HELD_ON_NATIVE_STATE);
    for (const attempt of result.attempts) {
      expect(attempt.failure).toBeUndefined();
      expect(attempt.raised.get(HISTORY_CORRECTION_REWIND_SOURCE)).toBe(
        CORRECTION_REWIND_TARGET_ROOT_NOT_RETAINED,
      );
    }
    expect(
      result.attempts[1]!.reasons.filter(
        (reason) => reason === CORRECTION_REWIND_TARGET_ROOT_NOT_RETAINED,
      ),
    ).toHaveLength(1);
    const raised = logs.find((line) =>
      line.startsWith(`${CORRECTION_REWIND_TARGET_ROOT_NOT_RETAINED}:`),
    );
    expect(raised).toContain(
      `Native MPF canonical recovery target root ${UTXOS_ROOT} is not retained in full; refusing to restore`,
    );
    expect(raised).toContain(
      "Operator action is needed: stop the node, install at LEDGER_MPF_DB_PATH a native MPF store that retains this root in full",
    );
    expect(result.held).toEqual({
      e: Pending.Status.PendingSubmission,
      digest: undefined,
      plans: ["prepared"],
      ownerOpen: true,
      root: ZERO_ROOT,
    });
    expect(owner.restores).toBe(1);
    expect(result.completed.failure).toBeUndefined();
    expect(
      result.completed.raised.get(HISTORY_CORRECTION_REWIND_SOURCE),
    ).toBeUndefined();
    expect(result.after).toEqual({
      e: Pending.Status.Abandoned,
      digest: REMOVAL,
      plans: ["applied"],
      ownerOpen: true,
    });
    expect(owner.durableRoot).toBe(UTXOS_ROOT);
  });
});

describe("the correction rewind whose native restore is refused over a full-index cap or on a failed read", () => {
  it.each([
    {
      name: "its target root's full index is over the byte cap",
      refuse: (targetRoot: string) =>
        new NativeMpfFullIndexCapExceeded(
          targetRoot,
          "FULL_INDEX_MAX_BYTES",
          536_870_912,
          536_870_990,
        ),
      reason: NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
      escalateAfterMs: 0,
      text: "over the full-index byte cap FULL_INDEX_MAX_BYTES = 536870912",
    },
    {
      name: "reading its target root's closure failed",
      refuse: (targetRoot: string) =>
        new NativeMpfRestoreReadFailed(targetRoot, {
          cause: Object.assign(new Error("IO error: read failed"), {
            code: "LEVEL_IO_ERROR",
          }),
        }),
      reason: NATIVE_MPF_RESTORE_READ_TRANSIENT,
      escalateAfterMs: NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
      text: "Operator action is needed only if the read keeps failing",
    },
  ])(
    "holds on the native state under its own reason when $name, then completes once the restore succeeds",
    async ({ refuse, reason, escalateAfterMs, text }) => {
      const owner = ownerModel(ZERO_ROOT);
      let refusing = true;
      owner.beforeRestore = async ({ targetRoot }) => {
        if (refusing) throw refuse(targetRoot);
      };
      const logs: string[] = [];
      const captured = Logger.add(
        Logger.make(({ message }) => {
          logs.push([message].flat().map(String).join(" "));
        }),
      );
      const result = await onNode(undefined, (node) =>
        Effect.gen(function* () {
          yield* removedE;
          fixture.open = owner;
          const returned = yield* prepare(node).pipe(Effect.provide(captured));
          const attempts = [yield* rewind(node), yield* rewind(node)];
          const escalation = (yield* activeLivenessReasons(
            yield* Globals,
          )).find(
            ({ source }) => source === HISTORY_CORRECTION_REWIND_SOURCE,
          )?.escalateAfterMs;
          const held = { ...(yield* state), root: owner.durableRoot };
          refusing = false;
          const completed = yield* rewind(node);
          return {
            returned,
            attempts,
            escalation,
            held,
            completed,
            after: yield* state,
          };
        }),
      );
      expect(result.returned).toBe(CORRECTION_REWIND_HELD_ON_NATIVE_STATE);
      for (const attempt of result.attempts) {
        expect(attempt.failure).toBeUndefined();
        expect(attempt.raised.get(HISTORY_CORRECTION_REWIND_SOURCE)).toBe(
          reason,
        );
      }
      expect(result.escalation).toBe(escalateAfterMs);
      expect(logs.find((line) => line.startsWith(`${reason}:`))).toContain(
        text,
      );
      expect(result.held).toEqual({
        e: Pending.Status.PendingSubmission,
        digest: undefined,
        plans: ["prepared"],
        ownerOpen: true,
        root: ZERO_ROOT,
      });
      expect(result.completed.failure).toBeUndefined();
      expect(
        result.completed.raised.get(HISTORY_CORRECTION_REWIND_SOURCE),
      ).toBeUndefined();
      expect(result.after).toEqual({
        e: Pending.Status.Abandoned,
        digest: REMOVAL,
        plans: ["applied"],
        ownerOpen: true,
      });
      expect(owner.durableRoot).toBe(UTXOS_ROOT);
    },
  );
});
