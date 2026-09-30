import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  DaPayloadTerminalOutcomesDB,
} from "../src/database/index.js";
import { computeChallengeableCutoff } from "../src/database/retention-policy.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../src/services/index.js";
import {
  daPayloadFixture,
  deploymentManifest,
  h32,
  NOW,
} from "./retention-enforcement.q54-executable-retention-deadline-alert.js";
import { deterministicFixtureBytes } from "./utils.js";

export const terminalMerge = (
  headerHash: Buffer,
  sequence: number,
  finalityDepth = BigInt(deploymentManifest.l1Finality.confirmationDepth),
): SDK.StateQueueAuthenticatedTransition => {
  const policyId = deploymentManifest.contracts.stateQueueMint.scriptHash;
  const transactionHash = h32(sequence.toString(16));
  const rootOutRef = `${h32("0")}#0`;
  const headerOutRef = `${h32((sequence + 4).toString(16))}#0`;
  const redeemer = {
    MergeToConfirmedStateV1: {
      yield_to_ref_input_index: 0n,
      header_node_key: headerHash.toString("hex"),
      confirmed_state_input_outref: {
        transactionId: h32("0"),
        outputIndex: 0n,
      },
      confirmed_state_output_index: 0n,
      m_settlement_redeemer_index: null,
      merged_block_withdrawals_root: h32("1"),
      merged_block_forced_transactions_root: h32("2"),
      merged_block_transactions_root: h32("3"),
      merged_block_deposits_root: h32("4"),
      merged_block_transition_trace_root: h32("5"),
      merged_block_event_to_step_root: h32("6"),
      merged_block_validation_traces_root: h32("7"),
      merged_block_withdrawal_count: 0n,
      merged_block_forced_transaction_count: 0n,
      merged_block_l2_transaction_count: 0n,
      merged_block_deposit_count: 0n,
      merged_block_total_event_count: 0n,
      merged_block_transition_step_count: 0n,
      merged_block_validation_trace_count: 0n,
    },
  } as const;
  const transition = SDK.deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: deploymentManifest.manifestId,
    stateQueuePolicyId: policyId,
    transactionHash,
    blockHash: h32((sequence + 8).toString(16)),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32((sequence + 12).toString(16)),
    finalityDepth: finalityDepth.toString(),
    mintPolicyIds: [policyId],
    referenceInputOutRefs: [`${h32("f")}#0`],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: `${h32("f")}#0`,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(redeemer, SDK.StateQueueRedeemer),
      },
    ],
    spentInputOutRefs: [rootOutRef, headerOutRef],
    previousQueue: [
      { headerHash: null, outRef: rootOutRef },
      { headerHash: headerHash.toString("hex"), outRef: headerOutRef },
    ],
    nextQueue: [{ headerHash: null, outRef: `${transactionHash}#0` }],
  });
  if (transition === null) throw new Error("invalid terminal merge fixture");
  return transition;
};

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
  const transition = terminalMerge(headerHash, sequence);
  return { headerHash, transition };
};

export const seedPublished = (endTime: Date, sequence = 1) =>
  Effect.gen(function* () {
    const fixture = publishedFixture(endTime, sequence);
    const row = {
      ...daPayloadFixture(`published-${sequence}`, endTime),
      [DaPayloadsDB.Columns.HEADER_HASH]: fixture.headerHash,
    };
    yield* DaPayloadsDB.upsertAvailable(row);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE da_payloads SET created_at = ${new Date(NOW.getTime() - 40 * RETENTION_MS_PER_DAY)} WHERE header_hash = ${fixture.headerHash}`;
    yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
      fixture.transition,
      deploymentManifest,
    );
    return fixture;
  });

export const seedTerminal = (
  headerHash: Buffer,
  sequence: number,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
    terminalMerge(headerHash, sequence),
    deploymentManifest,
  );

export const manifestDigest = (): Buffer =>
  Buffer.from(deploymentManifest.manifestId, "hex");

/** Records an authenticated-looking `removed` outcome under `digest`. */
export const seedRemoved = (
  headerHash: Buffer,
  sequence: number,
  digest: Buffer,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const transition = terminalMerge(headerHash, sequence);
    yield* sql`
      INSERT INTO da_payload_terminal_outcomes (
        header_hash, terminal_outcome, transition_kind,
        deployment_identity_digest, state_queue_policy_id,
        transaction_hash, block_hash, slot, block_no,
        transaction_index, chain_point_id, finality_depth,
        transition_digest, transition_record
      ) VALUES (
        ${headerHash}, 'removed', 'fraud_removal', ${digest},
        ${Buffer.from(deploymentManifest.contracts.stateQueueMint.scriptHash, "hex")},
        ${Buffer.from(transition.transactionHash, "hex")},
        ${Buffer.from(transition.blockHash, "hex")}, ${transition.slot},
        ${transition.blockNo}, ${Number(transition.transactionIndex)},
        ${Buffer.from(transition.chainPointId, "hex")},
        ${transition.finalityDepth},
        ${Buffer.from(transition.transitionDigest, "hex")},
        ${JSON.stringify(transition)}
      )`;
  });

/** An L1 view that references none of the seeded payloads. */
const unrelatedView: DaPayloadsDB.RetentionL1View = {
  confirmedHeadHash: deterministicFixtureBytes("unrelated-head", 28),
  liveQueueHeaderHashes: [deterministicFixtureBytes("unrelated-live", 28)],
};

export const prune = (
  options: {
    readonly view?: DaPayloadsDB.RetentionL1View;
    readonly digest?: Buffer | undefined;
  } = {},
) =>
  DaPayloadsDB.pruneBeyondRetention({
    challengeableCutoff: computeChallengeableCutoff(NOW),
    view: options.view ?? unrelatedView,
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

/** Runs a sweep with RETENTION_DAYS overridden and this deployment's identity. */
export const withSweepServices = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  retentionDays: number,
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
        }),
      ),
    );
  });

export const countRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly count: string;
  }>`SELECT COUNT(*)::text AS count FROM da_payloads`;
  return Number(rows[0]?.count ?? "0");
});
