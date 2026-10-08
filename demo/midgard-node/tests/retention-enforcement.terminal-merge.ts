import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DaPayloadsDB } from "../src/database/index.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import {
  ContractDeploymentIdentity,
  Globals,
  NodeConfig,
} from "../src/services/index.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";
import {
  daPayloadFixture,
  deploymentManifest,
  h32,
  NOW,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import { deterministicFixtureBytes } from "./utils.js";

/**
 * The greatest final height the fixtures' L1 views carry: a terminal row at
 * or below it is final, one above it can still roll back.
 */
export const FINAL_THROUGH = 1_000;

/** A height above `FINAL_THROUGH`: a terminal row that is not final yet. */
export const NOT_FINAL = FINAL_THROUGH + 1;

/**
 * Records that a landed tx took `headerHash` out of the state queue, as the
 * queue-terminal projection derives it: one `node_l1_queue_terminals` row at
 * `height` (final by default).
 */
export const seedQueueTerminal = (
  headerHash: Buffer,
  outcome: "merged" | "removed",
  sequence: number,
  height = sequence,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  insertQueueTerminal({
    headerHash,
    outcome,
    height,
    transactionHash: Buffer.from(h32(sequence.toString(16)), "hex"),
  });

const publishedFixture = (endTime: Date, sequence = 1) => {
  const header: SDK.Header = {
    ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(endTime.getTime() - 1000),
    endTime: BigInt(endTime.getTime()),
    blockSlot: BigInt(sequence),
    expectedNetworkId: 0n,
    minFeeA: 44n,
    minFeeB: 155381n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  const headerHash = Buffer.from(
    Effect.runSync(SDK.hashBlockHeader(header)),
    "hex",
  );
  return { headerHash };
};

/**
 * A published payload whose header a landed tx merged at `height` (final by
 * default).
 */
export const seedPublished = (endTime: Date, sequence = 1, height = sequence) =>
  Effect.gen(function* () {
    const fixture = publishedFixture(endTime, sequence);
    const row = {
      ...daPayloadFixture(`published-${sequence}`, endTime),
      [DaPayloadsDB.Columns.HEADER_HASH]: fixture.headerHash,
    };
    yield* DaPayloadsDB.upsertAvailable(row);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE da_payloads SET created_at = ${new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY)} WHERE header_hash = ${fixture.headerHash}`;
    yield* seedQueueTerminal(fixture.headerHash, "merged", sequence, height);
    return fixture;
  });

export const manifestDigest = (): Buffer =>
  Buffer.from(deploymentManifest.manifestId, "hex");

/** An L1 view that references none of the seeded payloads. */
const unrelatedView: DaPayloadsDB.RetentionL1View = {
  confirmedHeadHash: deterministicFixtureBytes("unrelated-head", 28),
  liveQueueHeaderHashes: [deterministicFixtureBytes("unrelated-live", 28)],
};

/**
 * Prunes at `NOW` under this deployment (or `digest`), at `view` as given
 * (a view without `finalThroughHeight` has no final height), or by default
 * at a view of none of the seeded payloads final through `FINAL_THROUGH`.
 */
export const prune = (
  options: {
    readonly view?: DaPayloadsDB.RetentionL1View;
    readonly digest?: Buffer | undefined;
  } = {},
) =>
  DaPayloadsDB.pruneBeyondRetention({
    challengeableCutoff: computeChallengeableCutoff(NOW),
    view: options.view ?? {
      ...unrelatedView,
      finalThroughHeight: FINAL_THROUGH,
    },
    deploymentIdentityDigest:
      "digest" in options ? options.digest : manifestDigest(),
  });

export const remainingHashes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly header_hash: Buffer;
  }>`SELECT header_hash FROM da_payloads`;
  return rows.map((row) => row.header_hash.toString("hex")).sort();
});

/** Runs a sweep with RETENTION_DAYS overridden (undefined: unset) under this
 * deployment's verified manifest, on fresh node globals (no history owner, so
 * the history prunes run under the database fixture capability). */
export const withSweepServices = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  retentionDays: number | undefined,
) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    return yield* effect.pipe(
      Effect.provideService(NodeConfig, {
        ...nodeConfig,
        RETENTION_DAYS: retentionDays,
      }),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "manifest",
          manifestId: deploymentManifest.manifestId,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          manifest: deploymentManifest,
        }),
      ),
      // No history owner.
      Effect.provide(Globals.Default),
    );
  });

export const countRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly count: string;
  }>`SELECT COUNT(*)::text AS count FROM da_payloads`;
  return Number(rows[0]?.count ?? "0");
});
