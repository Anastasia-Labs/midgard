import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { deploymentManifestHubOraclePolicyId } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DaPayloadsDB } from "../src/database/index.js";
import {
  computeChallengeableCutoff,
  computeRetentionCutoff,
} from "../src/database/retention-policy.js";
import { NodeConfig } from "../src/services/config.make-config.js";
import { Globals } from "../src/services/globals.globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";
import {
  daPayloadFixture,
  deploymentManifest,
  h32,
  NOW,
} from "./retention-enforcement.executable-retention-deadline-alert.js";
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

const deploymentPolicyId = (): string =>
  deploymentManifestHubOraclePolicyId(deploymentManifest);

/** An L1 view that references none of the seeded payloads. */
const unrelatedView: DaPayloadsDB.RetentionL1View = {
  confirmedHeadHash: deterministicFixtureBytes("unrelated-head", 28),
  liveQueueHeaderHashes: [deterministicFixtureBytes("unrelated-live", 28)],
};

/**
 * Prunes at `NOW` under this deployment (or `hubOraclePolicyId`), at `view` as given
 * (a view without `finalThroughHeight` has no final height), or by default
 * at a view of none of the seeded payloads final through `FINAL_THROUGH`.
 */
export const prune = (
  options: {
    readonly view?: DaPayloadsDB.RetentionL1View;
    readonly hubOraclePolicyId?: string | undefined;
  } = {},
) =>
  DaPayloadsDB.pruneBeyondRetention({
    challengeableCutoff: computeChallengeableCutoff(NOW),
    retentionCutoff: computeRetentionCutoff(NOW, 15),
    view: options.view ?? {
      ...unrelatedView,
      finalThroughHeight: FINAL_THROUGH,
    },
    hubOraclePolicyId:
      "hubOraclePolicyId" in options
        ? options.hubOraclePolicyId
        : deploymentPolicyId(),
  });

export const remainingHashes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly header_hash: Buffer;
  }>`SELECT header_hash FROM da_payloads`;
  return rows.map((row) => row.header_hash.toString("hex")).sort();
});

/** Runs a sweep with RETENTION_DAYS overridden (undefined: unset) under this
 * deployment's verified manifest, on fresh node globals (the history prunes
 * run under the database fixture capability). */
export const withSweepServices = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  retentionDays: number | undefined,
  manifest = deploymentManifest,
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
          hubOraclePolicyId: deploymentManifestHubOraclePolicyId(manifest),
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          manifest,
        }),
      ),
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
