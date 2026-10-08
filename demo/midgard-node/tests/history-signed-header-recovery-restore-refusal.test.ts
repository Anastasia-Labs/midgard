import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/index.js";
import { isRecoverableHistorySourceFailure } from "../src/services/event-history-owner.source-failure.js";
import { Globals } from "../src/services/globals.js";
import { NATIVE_RESTORE_HELD } from "../src/services/history-dependent-recovery.js";
import { prepareSignedHeaderRecovery } from "../src/services/history-signed-header-recovery.js";
import {
  activeLivenessReasons,
  HISTORY_SIGNED_HEADER_RECOVERY_SOURCE,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_ESCALATION_MS,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
} from "../src/services/liveness-halt.js";
import {
  NativeMpfFullIndexCapExceeded,
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import {
  BASE_HEADER,
  BASE_OUT,
  binding,
  bytes,
  fixtureHeader,
  hex,
  insertJournal,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import {
  attempt,
  checkpoint,
  ledgerRoot,
  type Node,
  onNode,
  type OwnerModel,
  ownerModel,
  plans,
  statusOf,
  withNativeReplay,
  ZERO_ROOT,
} from "./helpers/history-expired-intent-release-preparation.js";

/**
 * Signed-header recovery restores the native MPF root through
 * `executeHistoryDependentRecovery` once its plan is prepared. The native
 * owner refuses that restore, before it changes its marker, when the target
 * root's node closure is not in its store (`NativeMpfRootNotRetained`), when
 * the closure's full index is over a full-index cap
 * (`NativeMpfFullIndexCapExceeded`), or when reading the closure failed
 * (`NativeMpfRestoreReadFailed`). Each refusal holds under
 * `history_signed_header_recovery` with its own reason, instead of failing
 * the history owner: the plan stays prepared, native MPF, the SQL root and
 * the journal stay as they are, and every evaluation retries the restore.
 * Once the restore succeeds, the next evaluation completes the recovery and
 * clears the reason. Only the L1 reads (the exact-point capture and its
 * queue validation, the canonical coverage and its evaluation) are modelled;
 * the rest is the production preparation over seeded SQL.
 */

vi.mock("../src/l1-event-history-source.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.ledgerSnapshot(original),
  ),
);
vi.mock("../src/workers/utils/commit-block-header.js", (original) =>
  import("./helpers/history-expired-intent-release-preparation.mocks.js").then(
    (mocks) => mocks.nodeSerialization(original),
  ),
);
vi.mock(
  "../src/database/eventHistoryCanonicalCoverage.js",
  async (original) => {
    const { Effect } = await import("effect");
    return {
      ...(await original<
        typeof import("../src/database/eventHistoryCanonicalCoverage.js")
      >()),
      loadCanonicalHistoryCoverage: () => Effect.succeed({} as never),
    };
  },
);
vi.mock(
  "../src/services/signed-intent-canonical-coverage.js",
  async (original) => ({
    ...(await original<
      typeof import("../src/services/signed-intent-canonical-coverage.js")
    >()),
    // The signed commit is proved absent from canonical L1 history.
    evaluateSignedIntentCoverage: () => ({
      kind: "covered_absent",
      evidenceDigest: "c0".repeat(32),
    }),
  }),
);
vi.mock("../src/services/history-recovery-state-queue.js", async () => {
  const { Effect } = await import("effect");
  return {
    // The canonical queue authorizes the journal's base.
    validateRecoveryStateQueue: () =>
      Effect.succeed({ queueUTxO: { datum: "queue-datum" } }),
  };
});
vi.mock("@al-ft/midgard-sdk", async (original) => {
  const { Effect } = await import("effect");
  return {
    ...(await original<typeof import("@al-ft/midgard-sdk")>()),
    getConfirmedStateFromStateQueueDatum: () =>
      Effect.succeed({ data: { endTime: 5_000n } }),
  };
});

/** The root of the empty confirmed ledger: H's base root, so the confirmed
 * SQL baseline is H's base. */
const EMPTY_LEDGER_ROOT = Effect.runSync(
  computeLedgerMpfRootFromLedgerEntries([]),
);

const H = signedCommit(`${hex("signed-header-base-tx")}#0`, TTL);
const H_HEADER = fixtureHeader(
  "signed-header",
  BASE_HEADER,
  EMPTY_LEDGER_ROOT,
  ZERO_ROOT,
);
const MEMBER = bytes("signed-header-deposit");
const EVENT_KEY = bytes("signed-header-event-key");
const ORIGIN_OUTREF = Buffer.concat([
  bytes("signed-header-deposit-tx"),
  Buffer.from([0, 0]),
]);

/** H, this node's deposit-only block, locally applied (the native root and
 * the SQL marker at its candidate root) while its signed commit never
 * landed; the follower then rolled back past its deposit's admission, which
 * orphans it, so H is a signed-header recovery candidate. */
const seedH = Effect.gen(function* () {
  yield* insertJournal({
    header: H_HEADER,
    status: Pending.Status.LocallyApplied,
    commit: H,
    baseOut: BASE_OUT,
    baseHeader: BASE_HEADER,
    createdAt: new Date(2_000_000),
  });
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE pending_block_finalizations
    SET base_utxos_root = ${EMPTY_LEDGER_ROOT},
      block_end_time = block_start_time + INTERVAL '1 second'
    WHERE header_hash = ${H_HEADER}`;
  yield* withNativeReplay(H_HEADER);
  const payload = Buffer.from("d87980", "hex");
  const at = new Date(1_500_000);
  yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert({
    header_hash: H_HEADER,
    member_id: MEMBER,
    ordinal: 0,
    payload_cbor: payload,
    payload_sha256: createHash("sha256").update(payload).digest(),
    source_table: "deposits_utxos",
    source_id: MEMBER,
    source_time_stamp_tz: at,
    l1_event_key: EVENT_KEY,
    l1_origin_outref: ORIGIN_OUTREF,
  } as never)}`;
  yield* sql`INSERT INTO deposits_utxos ${sql.insert({
    event_id: MEMBER,
    event_info: payload,
    inclusion_time: at,
    deposit_l1_tx_hash: ORIGIN_OUTREF.subarray(0, 32),
    ledger_tx_id: bytes("signed-header-ledger-tx"),
    ledger_output: payload,
    ledger_address: "addr_test_deposit",
    projected_header_hash: H_HEADER,
    status: "projected",
    l1_event_key: EVENT_KEY,
    l1_origin_outref: ORIGIN_OUTREF,
  } as never)}`;
});

const prepare = (node: Node) =>
  prepareSignedHeaderRecovery({
    binding: { ...binding, manifestId: hex("manifest") },
    checkpoint,
    preparation: { token: node.token, assertCurrent: Effect.void },
    transport: {} as never,
    contracts: {
      stateQueue: { spendingScriptAddress: "addr_test_state_queue" },
    } as never,
    config: {} as never,
    confirmationDepth: 3,
    slotToUnixTime: (slot) => slot * 1_000,
  });

const recovery = (node: Node) => attempt(prepare(node));

const state = (owner: OwnerModel) =>
  Effect.gen(function* () {
    return {
      status: (yield* statusOf(H_HEADER))?.status,
      plans: (yield* plans).map(({ state }) => state),
      ledger: yield* ledgerRoot,
      native: owner.durableRoot,
      restores: owner.restores,
    };
  });

const escalationOf = Effect.gen(function* () {
  const globals = yield* Globals;
  return (yield* activeLivenessReasons(globals)).find(
    ({ source }) => source === HISTORY_SIGNED_HEADER_RECOVERY_SOURCE,
  )?.escalateAfterMs;
});

const capturing = () => {
  const logs: string[] = [];
  const layer = Logger.add(
    Logger.make(({ message }) => {
      logs.push([message].flat().map(String).join(" "));
    }),
  );
  return { logs, layer };
};

/** The three refusals, each with the reason it holds under, the escalation
 * readiness reports for it and the operator text it carries. */
const refusals = [
  {
    name: "its target root's closure is not in the store",
    refuse: (targetRoot: string) => new NativeMpfRootNotRetained(targetRoot),
    reason: SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
    escalateAfterMs: 0,
    text: [
      `Native MPF canonical recovery target root ${EMPTY_LEDGER_ROOT} is not retained in full; refusing to restore`,
      "Operator action is needed: stop the node, install at LEDGER_MPF_DB_PATH a native MPF store that retains this root in full",
    ],
  },
  {
    name: "its target root's full index is over the record cap",
    refuse: (targetRoot: string) =>
      new NativeMpfFullIndexCapExceeded(
        targetRoot,
        "FULL_INDEX_MAX_RECORDS",
        2_000_000,
        2_000_001,
      ),
    reason: NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
    escalateAfterMs: 0,
    text: [
      `Native MPF root ${EMPTY_LEDGER_ROOT} reaches 2000001 records, over the full-index record cap FULL_INDEX_MAX_RECORDS = 2000000`,
      "Operator action is needed: run a node build whose full-index caps",
    ],
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
    text: [
      `could not read target root ${EMPTY_LEDGER_ROOT}'s node closure from the native MPF store: IO error: read failed`,
      "Operator action is needed only if the read keeps failing",
    ],
  },
] as const;

describe("a native restore refusal as the history owner classifies it", () => {
  // Why the hold is needed: unheld, the preparation fails with the refusal
  // wrapped as below, which the history owner does not reconnect after; it
  // fails (before its first ready, the ready is rejected).
  it.each(refusals)(
    "is not a failure the owner reconnects after when $name",
    ({ refuse }) => {
      expect(
        isRecoverableHistorySourceFailure(
          new DatabaseError({
            table: "event_history_recovery_plans",
            message: "Native dependent rollback requires resumable recovery",
            cause: refuse(EMPTY_LEDGER_ROOT),
          }),
        ),
      ).toBe(false);
    },
  );
});

describe("signed-header recovery whose native restore is refused", () => {
  it.each(refusals)(
    "holds under its own source when $name, then completes once the restore succeeds",
    async ({ refuse, reason, escalateAfterMs, text }) => {
      const owner = ownerModel(ZERO_ROOT);
      let refusing = true;
      owner.beforeRestore = async ({ targetRoot }) => {
        if (refusing) throw refuse(targetRoot);
      };
      const { logs, layer } = capturing();
      const result = await onNode(owner, (node) =>
        Effect.gen(function* () {
          yield* seedH;
          const before = yield* state(owner);
          const returned = yield* prepare(node).pipe(Effect.provide(layer));
          const attempts = [yield* recovery(node), yield* recovery(node)];
          const escalation = yield* escalationOf;
          const held = yield* state(owner);
          refusing = false;
          const completed = yield* recovery(node);
          return {
            before,
            returned,
            attempts,
            escalation,
            held,
            completed,
            after: yield* state(owner),
          };
        }),
      );
      expect(result.returned).toBe(NATIVE_RESTORE_HELD);
      for (const attempt of result.attempts) {
        expect(attempt.failure).toBeUndefined();
        expect(attempt.raised.get(HISTORY_SIGNED_HEADER_RECOVERY_SOURCE)).toBe(
          reason,
        );
      }
      // Raised once: one reason under the source, not one per evaluation.
      expect(
        result.attempts[1]!.reasons.filter((value) => value === reason),
      ).toHaveLength(1);
      expect(result.escalation).toBe(escalateAfterMs);
      const raised = logs.find((line) => line.startsWith(`${reason}:`));
      for (const fragment of text) expect(raised).toContain(fragment);
      // Held: the plan prepared, and native MPF, the SQL root and the
      // journal unchanged.
      expect(result.before).toEqual({
        status: Pending.Status.LocallyApplied,
        plans: [],
        ledger: ZERO_ROOT,
        native: ZERO_ROOT,
        restores: 0,
      });
      expect(result.held).toEqual({ ...result.before, plans: ["prepared"] });
      expect(result.completed.failure).toBeUndefined();
      expect(
        result.completed.raised.get(HISTORY_SIGNED_HEADER_RECOVERY_SOURCE),
      ).toBeUndefined();
      expect(result.after).toEqual({
        status: Pending.Status.Abandoned,
        plans: ["applied"],
        ledger: EMPTY_LEDGER_ROOT,
        native: EMPTY_LEDGER_ROOT,
        restores: 1,
      });
    },
  );

  it("still fails on any other restore failure, raising nothing", async () => {
    const owner = ownerModel(ZERO_ROOT);
    owner.beforeRestore = async () => {
      throw new Error("Native MPF canonical recovery base changed");
    };
    const result = await onNode(owner, (node) =>
      Effect.gen(function* () {
        yield* seedH;
        return { attempt: yield* recovery(node), held: yield* state(owner) };
      }),
    );
    expect(result.attempt.failure).toContain(
      "Native dependent rollback requires resumable recovery <- Native MPF canonical recovery base changed",
    );
    expect(
      result.attempt.raised.get(HISTORY_SIGNED_HEADER_RECOVERY_SOURCE),
    ).toBeUndefined();
    expect(result.held.plans).toEqual(["prepared"]);
    expect(result.held.native).toBe(ZERO_ROOT);
  });
});
