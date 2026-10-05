import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Cause, Effect, Exit, Option, Ref } from "effect";

import * as Authority from "../../src/database/eventHistoryAuthority.js";
import type { Checkpoint } from "../../src/database/eventHistoryJournal.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { Globals } from "../../src/services/globals.js";
import { prepareExpiredIntentRelease } from "../../src/services/history-expired-intent-release.prepare-expired-intent-release.js";
import { prepareReplacedBlockRevival } from "../../src/services/history-expired-intent-release.prepare-replaced-block-revival.js";
import type { QueueView } from "../../src/services/history-expired-intent-release.signed-commit-node.js";
import { makeSignedIntentDeferral } from "../../src/services/history-expired-intent-release.table.js";
import { activeLivenessReasons } from "../../src/services/liveness-halt.js";
import {
  makeState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
} from "../../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import { prepareStateQueueCorrectionRewind } from "../../src/services/state-queue-correction-rewind.js";
import { provideDatabaseLayers } from "../utils.js";
import {
  BINDING,
  binding,
  bytes,
  hex,
  seed,
  UTXOS_ROOT,
  ZERO_ROOT,
} from "./history-expired-intent-release-before-ttl.js";
import { authority } from "./history-expired-intent-release-displaced-sibling.js";
import type { Coverage } from "./history-expired-intent-release-preparation.coverage.js";
import {
  fakeOwner,
  type OwnerModel,
} from "./history-expired-intent-release-preparation.owner.js";

export {
  type Coverage,
  coverageOf,
} from "./history-expired-intent-release-preparation.coverage.js";
export {
  fakeOwner,
  type OwnerModel,
  ownerModel,
} from "./history-expired-intent-release-preparation.owner.js";

/**
 * Drives the production signed-intent release and replaced-block revival
 * preparations over seeded SQL. Only their L1 reads are modelled: the test
 * file mocks the exact-point capture, its authentication (to `fixture.queue`)
 * and the canonical coverage loader (to `fixture.coverage`), and the native
 * owner is a model whose durable root follows the plan's CAS.
 */

export type Fixture = {
  queue: QueueView | undefined;
  coverage: Coverage;
};

/** The checkpoint of the cursor `seed` writes, at a head past every TTL. */
export const checkpoint = {
  bindingDigest: BINDING,
  manifestId: hex("manifest"),
  revision: "0",
  head: { id: hex("anchor"), slot: 2_000, height: 10 },
  capture: { snapshotDigest: hex("anchor-snapshot") },
} as unknown as Checkpoint;

/** Marks `header`'s journal as carrying a native replay from its base root
 * to its candidate root, as every replaceable journal does. */
export const withNativeReplay = (header: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`UPDATE pending_block_finalizations SET
      replay_kind = 'ledger_delta_native_mpf_v1',
      mpf_owner_schema = 1,
      mpf_owner_binary_sha256 = ${bytes("owner-binary")},
      mpf_replay_base_root = decode(base_utxos_root, 'hex'),
      mpf_replay_candidate_root = decode(expected_utxos_root, 'hex'),
      mpf_replay_event_log = ${Buffer.alloc(92)},
      mpf_replay_event_log_digest = ${bytes("event-log")},
      mpf_replay_event_roots = ${Buffer.alloc(0)},
      mpf_replay_event_count = 0
      WHERE header_hash = ${header}`,
  );

/** The cursor, the SQL ledger marker at `ledgerRoot` and a recovering
 * history authority; returns its token. */
export const seedNode = (ledgerRoot: string = ZERO_ROOT) =>
  Effect.gen(function* () {
    yield* seed;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO mpf_engine_state (store_name, migration_version, root_hex)
      VALUES ('ledger', 0, ${ledgerRoot})
      ON CONFLICT (store_name) DO UPDATE SET root_hex = EXCLUDED.root_hex`;
    const claimed = yield* Authority.acquire({
      deploymentIdentity: hex("manifest"),
      ownerToken: randomUUID(),
      leaseDurationMs: 600_000,
    });
    return yield* Authority.beginRecovery(claimed, "preparation test");
  });

export type Node = Readonly<{
  token: Authority.Token;
  owner: OwnerModel | undefined;
  deferral: ReturnType<typeof makeSignedIntentDeferral>;
}>;

export const inputs = (node: Node) => ({
  binding: { ...binding, manifestId: hex("manifest") },
  checkpoint,
  preparation: { token: node.token, assertCurrent: Effect.void },
  config: { SPECULATIVE_COMMIT_BUILD: false } as never,
  rewindAuthority: authority,
  transport: {} as never,
  contracts: {
    stateQueue: { spendingScriptAddress: "addr_test_state_queue" },
  } as never,
  deferral: node.deferral,
});

export type Attempt = Readonly<{
  /** The failure's messages, outermost first, or undefined on success. */
  failure: string | undefined;
  failureTag: string | undefined;
  reasons: readonly string[];
  raised: ReadonlyMap<string, string>;
}>;

/** Runs one preparation on the node's globals. */
export const attempt = <E>(
  work: Effect.Effect<void, E, SqlClient.SqlClient | Globals>,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const exit = yield* Effect.exit(work);
    const error = Exit.isFailure(exit)
      ? Option.getOrUndefined(Cause.failureOption(exit.cause))
      : undefined;
    if (Exit.isFailure(exit) && error === undefined)
      throw new Error(`preparation died: ${Cause.pretty(exit.cause)}`);
    const messages: string[] = [];
    for (let value: unknown = error; value instanceof Error; )
      value = (messages.push(value.message), value.cause);
    if (
      error !== undefined &&
      messages.length === 0 &&
      typeof (error as { message?: unknown }).message === "string"
    )
      messages.push((error as { message: string }).message);
    return {
      failure: error === undefined ? undefined : messages.join(" <- "),
      failureTag: (error as { _tag?: string } | undefined)?._tag,
      // What readiness reports (see the readiness handler).
      reasons: (yield* activeLivenessReasons(globals)).map(
        ({ reason }) => reason,
      ),
      raised: yield* Ref.get(globals.LIVENESS_REASONS),
    } satisfies Attempt;
  });

/** One node: `rounds` runs with one set of globals, the native owner model
 * installed (when given) and one deferral, as one history owner would. */
export const onNode = <A>(
  owner: OwnerModel | undefined,
  rounds: (
    node: Node,
  ) => Effect.Effect<A, unknown, SqlClient.SqlClient | Globals>,
  ledgerRoot?: string,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const token = yield* seedNode(ledgerRoot);
        const globals = yield* Globals;
        if (owner !== undefined)
          yield* Ref.set(globals.NATIVE_MPF_OWNER, fakeOwner(owner) as never);
        return yield* rounds({
          token,
          owner,
          deferral: makeSignedIntentDeferral(),
        });
      }).pipe(Effect.provide(Globals.Default)),
    ) as Effect.Effect<A, unknown, never>,
  );

export const release = (node: Node) =>
  attempt(prepareExpiredIntentRelease(inputs(node)));

export const revival = (node: Node) =>
  attempt(prepareReplacedBlockRevival(inputs(node)));

export const rewind = (node: Node) => {
  const input = inputs(node);
  return attempt(
    prepareStateQueueCorrectionRewind({
      bindingDigest: input.binding.digest,
      checkpoint,
      preparation: input.preparation,
      config: input.config,
      authority,
    }),
  );
};

/** A correction observer view whose cursor queue holds the root and then
 * `nodes`, and that recorded no transition: the hint that selects revival
 * candidates. */
export const observerSees = (
  nodes: readonly { readonly headerHash: string; readonly outRef: string }[],
) =>
  Effect.gen(function* () {
    const state = makeState({
      schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
      deploymentIdentityDigest: authority.manifestId,
      stateQueuePolicyId: authority.stateQueuePolicyId,
      cursorQueue: [
        { headerHash: null, outRef: `${hex("root-tx")}#0` },
        ...nodes.map(({ headerHash, outRef }) => ({ headerHash, outRef })),
      ],
      pending: [],
      admitted: [],
      retractedTransactionHashes: [],
      postFinalityRollbackIncidents: [],
    });
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO state_queue_terminal_observer_states (
        deployment_identity_digest, state_queue_policy_id, state_digest,
        state_record, updated_at
      ) VALUES (${Buffer.from(authority.manifestId, "hex")},
        ${Buffer.from(authority.stateQueuePolicyId, "hex")},
        ${Buffer.from(state.stateDigest, "hex")}, ${JSON.stringify(state)}, NOW())`;
  });

export const statusOf = (header: Buffer) =>
  Effect.map(Pending.retrieveByHeaderHash(header, true), (found) => {
    const record = Option.getOrUndefined(found);
    return record === undefined
      ? undefined
      : {
          status: record[Pending.Columns.STATUS],
          digest:
            record[Pending.Columns.CORRECTION_TRANSITION_DIGEST] ?? undefined,
        };
  });

export const plans = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) =>
    sql<{
      state: string;
      intent: string;
      recovery_id: string;
    }>`SELECT state, intent, encode(recovery_id, 'hex') AS recovery_id FROM event_history_recovery_plans ORDER BY created_at`,
);

export const ledgerRoot = Effect.map(
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql<{
        root_hex: string;
      }>`SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger'`,
  ),
  (rows) => rows[0]?.root_hex,
);

export { UTXOS_ROOT, ZERO_ROOT };
