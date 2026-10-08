import type {
  DerivationContext,
  DerivationHook,
  DialectName,
  MigrationSet,
  RetentionPins,
  SqlTx,
  TemporalTableSpec,
  TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { checkpointDerivation } from "./checkpoints.js";
import { recordDaAttestations } from "./projection.da-attestations.js";
import {
  createdCmlOutput,
  type DecodedRow,
  decodeOutput,
} from "./projection.decode-output.js";
import {
  closeUnreferencedDepartures,
  departureKind,
  recordDeparture,
} from "./projection.departed-headers.js";
import { recordTrackedUnits } from "./projection.followed-units.js";
import { recordProtocolInitFaults } from "./projection.protocol-init.js";
import {
  WATCHER_TEMPORAL_TABLES,
  watcherMigrations,
} from "./projection.schema.js";
import {
  WATCHER_DA_ATTESTATIONS_TABLE,
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROTOCOL_INIT_FAULTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_CARRIERS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

/**
 * The watcher's state-queue projection over the L1 follower's facts
 * (l1-architecture-plan §7.3, ticket W1). One versioned D-t table holds
 * every state-queue and CorrectionLock output the canonical chain created,
 * open while it is unspent. A view at a point is a pure read of the rows
 * live there, so a rollback is the follower's generated rewind and nothing
 * here knows about forks.
 *
 * Outputs are decoded with the same functions the authenticated observation
 * uses (`queueOutput`, `lockOutput`). An output those refuse is kept as a
 * `malformed` row instead of throwing, so one bad output never stops the
 * writer; the view reports it as unhealthy while it is live.
 */

export {
  WATCHER_DA_ATTESTATIONS_TABLE,
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROTOCOL_INIT_FAULTS_TABLE,
  WATCHER_QUEUE_CHECKPOINTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_CARRIERS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

/** The deployment script hashes the projection reads (lowercase hex). */
export type WatcherProjectionDeployment = Readonly<{
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  stateQueueSpend: string;
  stateQueueMint: string;
  correctionLockSpend: string;
  /** The hub-oracle policy: it names the CorrectionLock unit and pays the hub-oracle output. */
  hubOracleMint: string;
  /** The permanent fraud-proof outputs a fraud correction references. */
  fraudProofSpend: string;
  fraudProofMint: string;
  /** The availability-challenge records and the policy an availability timeout burns. */
  availabilityChallengeSpend: string;
  availabilityChallengeMint: string;
  /** The pooled DA bond the availability snapshot reads. */
  daBondPoolSpend: string;
  /** The DA attestation (DAAT) policy: Apply burns the header's DAAT the commitment is read from. */
  daAttestationMint: string;
  /**
   * Every other script the watcher follows (l1-architecture-plan §4.4):
   * the deployment's manifest contracts and the computation-thread policy.
   * Each is tracked as a payment credential and as a policy, and every
   * unit it mints has its history recorded, so the fault-proof families'
   * snapshot scopes and unit histories are served from the store.
   */
  followedScripts?: readonly string[];
}>;

/**
 * The policies whose units' histories the projection records: the
 * hub-oracle, fraud-proof, availability-challenge and DAAT policies and
 * every followed script. The state-queue policy is not one: its node units
 * have their own history (`WATCHER_QUEUE_UNIT_HISTORY_TABLE`).
 */
export const watcherUnitHistoryPolicies = (
  deployment: WatcherProjectionDeployment,
): ReadonlySet<string> =>
  new Set(
    [
      deployment.hubOracleMint,
      deployment.fraudProofMint,
      deployment.availabilityChallengeMint,
      deployment.daAttestationMint,
      ...(deployment.followedScripts ?? []),
    ].filter((policy) => policy !== deployment.stateQueueMint),
  );

/**
 * Whether the projection records `unit`'s history in
 * `WATCHER_UNIT_HISTORY_TABLE`: its policy is one of `policies`
 * (`watcherUnitHistoryPolicies`). A state-queue node unit is not; its
 * history is the header's `WATCHER_QUEUE_UNIT_HISTORY_TABLE` rows.
 */
export const recordsUnitHistory = (
  policies: ReadonlySet<string>,
  unit: string,
): boolean =>
  /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(unit) &&
  policies.has(unit.slice(0, 56));

export type WatcherProjection = Readonly<{
  name: string;
  trackedSet: TrackedSet;
  temporalTables: readonly TemporalTableSpec[];
  migrations: (dialect: DialectName) => MigrationSet;
  derivations: readonly DerivationHook[];
  retentionPins: RetentionPins;
}>;

type Anchor = Readonly<{
  txHash: Buffer;
  slot: number;
  blockHash: Buffer;
  height: number;
}>;

const hexBytes = (hex: string): Buffer => Buffer.from(hex, "hex");

const anchorOf = (row: Record<string, unknown>): Anchor => ({
  txHash: Buffer.from(row.anchor_tx_hash as Uint8Array),
  slot: Number(row.anchor_slot as number | string),
  blockHash: Buffer.from(row.anchor_block_hash as Uint8Array),
  height: Number(row.anchor_height as number | string),
});

/**
 * Closes the queue rows a tx spends and returns, per spent node header hash,
 * the anchor that the tx's re-output of the same header inherits: a header
 * stays anchored at the tx that minted it, as the authenticated observation
 * anchors it (`anchoredHeaderObservation`). Also returns the spent queue
 * roots (`txHash#index`), which a merge must consume.
 */
const closeSpent = async (
  tx: SqlTx,
  context: DerivationContext,
  spent: DerivationContext["qualified"][number]["spent"],
): Promise<
  Readonly<{ inherited: Map<string, Anchor>; roots: Set<string> }>
> => {
  const inherited = new Map<string, Anchor>();
  const roots = new Set<string>();
  const slot = context.block.point.slot;
  for (const outRef of spent) {
    const rows = await tx.query(
      `SELECT kind, header_hash, anchor_tx_hash, anchor_slot, anchor_block_hash, anchor_height FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL`,
      [outRef.txHash, outRef.index],
    );
    const row = rows[0];
    if (row === undefined) continue;
    if (row.kind === "root")
      roots.add(`${outRef.txHash.toString("hex")}#${outRef.index.toString()}`);
    if (row.kind === "node" && row.header_hash !== null)
      inherited.set(
        Buffer.from(row.header_hash as Uint8Array).toString("hex"),
        anchorOf(row),
      );
    await tx.query(
      `UPDATE ${WATCHER_QUEUE_OUTPUTS_TABLE} SET to_slot = ? WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL`,
      [slot, outRef.txHash, outRef.index],
    );
  }
  return { inherited, roots };
};

/**
 * Appends this tx to the unit history of every node header it spent or
 * created, and closes the history of a header whose node it spent without
 * re-outputting it (the header left the queue), recording its departure.
 * Returns whether a header left.
 */
const recordUnitHistory = async (
  context: DerivationContext,
  entry: DerivationContext["qualified"][number],
  spent: ReadonlyMap<string, Anchor>,
  spentRoots: ReadonlySet<string>,
  created: ReadonlySet<string>,
  stateQueuePolicyId: string,
): Promise<boolean> => {
  const { tx, block } = context;
  const txHash = entry.tx.hash;
  let departed = false;
  for (const header of new Set([...spent.keys(), ...created])) {
    await tx.query(
      `INSERT INTO ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (header_hash, tx_hash, block_hash, block_height, from_slot, to_slot) VALUES (?, ?, ?, ?, ?, NULL)`,
      [
        hexBytes(header),
        txHash,
        block.point.hash,
        block.height,
        block.point.slot,
      ],
    );
    if (created.has(header)) continue;
    // Recorded before the history closes: it reads the header's versions.
    await recordDeparture(
      context,
      entry,
      header,
      departureKind(entry, header, spentRoots, stateQueuePolicyId),
    );
    departed = true;
    for (const table of [
      WATCHER_QUEUE_UNIT_HISTORY_TABLE,
      WATCHER_DA_ATTESTATIONS_TABLE,
    ])
      await tx.query(
        `UPDATE ${table} SET to_slot = ? WHERE header_hash = ? AND to_slot IS NULL`,
        [block.point.slot, hexBytes(header)],
      );
  }
  return departed;
};

const derivation = (
  deployment: WatcherProjectionDeployment,
  credentials: ReadonlySet<string>,
): DerivationHook => {
  const unitPolicies = watcherUnitHistoryPolicies(deployment);
  const addresses = {
    stateQueue: credentialToAddress(
      deployment.network,
      scriptHashToCredential(deployment.stateQueueSpend),
    ),
    correctionLock: credentialToAddress(
      deployment.network,
      scriptHashToCredential(deployment.correctionLockSpend),
    ),
  };
  const queueAddress = CML.Address.from_bech32(addresses.stateQueue);
  const stateQueueAddressBytes = Buffer.from(queueAddress.to_raw_bytes());
  queueAddress.free();
  return {
    name: "watcher_state_queue",
    writes: [
      WATCHER_QUEUE_OUTPUTS_TABLE,
      WATCHER_QUEUE_UNIT_HISTORY_TABLE,
      WATCHER_DA_ATTESTATIONS_TABLE,
      WATCHER_PROTOCOL_INIT_FAULTS_TABLE,
      WATCHER_DEPARTED_HEADERS_TABLE,
      WATCHER_UNIT_CARRIERS_TABLE,
      WATCHER_UNIT_HISTORY_TABLE,
    ],
    apply: async (context) => {
      const { tx, block } = context;
      let departed = false;
      for (const entry of context.qualified) {
        await recordDaAttestations(
          context,
          entry,
          deployment.daAttestationMint,
        );
        await recordTrackedUnits(context, entry, unitPolicies);
        await recordProtocolInitFaults(
          context,
          entry,
          deployment.stateQueueMint,
          stateQueueAddressBytes,
        );
        const { inherited, roots } = await closeSpent(tx, context, entry.spent);
        const created = new Set<string>();
        const candidates = entry.created.filter(
          ({ output }) =>
            output.paymentCredential !== null &&
            credentials.has(output.paymentCredential.hash.toString("hex")),
        );
        const body =
          candidates.length === 0
            ? null
            : CML.TransactionBody.from_cbor_bytes(entry.tx.bodyCbor);
        try {
          for (const { outRef } of candidates) {
            const output = createdCmlOutput(
              body!,
              entry.tx.isValid,
              outRef.index,
            );
            const label = `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;
            const decoded: DecodedRow | null =
              output === undefined
                ? { kind: "malformed", reason: "created output is missing" }
                : decodeOutput(output, label, addresses, deployment);
            if (decoded === null) continue;
            const own: Anchor = {
              txHash: entry.tx.hash,
              slot: block.point.slot,
              blockHash: block.point.hash,
              height: block.height,
            };
            const anchor =
              decoded.kind === "node" && decoded.headerHash !== null
                ? (inherited.get(decoded.headerHash.toString("hex")) ?? own)
                : own;
            if (decoded.kind === "node" && decoded.headerHash !== null)
              created.add(decoded.headerHash.toString("hex"));
            const queue = decoded.kind === "root" || decoded.kind === "node";
            await tx.query(
              `INSERT INTO ${WATCHER_QUEUE_OUTPUTS_TABLE} (tx_hash, output_index, kind, header_hash, next_header_hash, header_cbor, state_queue_node_cbor, datum_cbor, malformed, anchor_tx_hash, anchor_slot, anchor_block_hash, anchor_height, from_slot, to_slot) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)`,
              [
                outRef.txHash,
                outRef.index,
                decoded.kind,
                queue ? decoded.headerHash : null,
                queue ? decoded.nextHeaderHash : null,
                queue ? decoded.headerCbor : null,
                queue ? decoded.stateQueueNodeCbor : null,
                decoded.kind === "malformed" ? null : decoded.datumCbor,
                decoded.kind === "malformed" ? decoded.reason : null,
                anchor.txHash,
                anchor.slot,
                anchor.blockHash,
                anchor.height,
                block.point.slot,
              ],
            );
          }
        } finally {
          body?.free();
        }
        if (
          await recordUnitHistory(
            context,
            entry,
            inherited,
            roots,
            created,
            deployment.stateQueueMint,
          )
        )
          departed = true;
      }
      if (departed) await closeUnreferencedDepartures(context);
    },
  };
};

/**
 * The watcher projection for one deployment. Its tracked set holds the
 * state-queue and CorrectionLock credentials, the hub-oracle output's
 * credential (so the protocol-init tx stays stored while that output is
 * live, which R3's check relies on), the fraud-proof, availability-challenge
 * and DA-bond-pool credentials (the outputs a correction references and the
 * availability snapshot reads), the DA-attestation credential (the DAAT
 * output every AddSignatures re-creates, whose creating tx Apply's
 * commitment recovery reads), the state-queue, hub-oracle, DAAT and
 * availability-challenge policies (the init tx qualifies through its
 * hub-oracle mint; a DAAT's creating tx holds the attested commitment; a
 * challenge's units carry the published payload), and every followed
 * script as both a credential and a policy (the fault-proof families'
 * snapshot scopes and unit histories).
 *
 * Retention: a tx stays stored while a live queue output is anchored at it,
 * while it is in the unit history of a header still queued (or removed less
 * than k blocks ago), while it created a DAAT of such a header, and while it
 * is in the history of a followed unit still on chain.
 */
export const watcherProjection = (
  deployment: WatcherProjectionDeployment,
): WatcherProjection => {
  const credentials = new Set([
    deployment.stateQueueSpend,
    deployment.correctionLockSpend,
  ]);
  return {
    name: "watcher",
    trackedSet: {
      addresses: new Set<string>(),
      paymentCredentials: new Set([
        deployment.stateQueueSpend,
        deployment.correctionLockSpend,
        deployment.hubOracleMint,
        deployment.fraudProofSpend,
        deployment.availabilityChallengeSpend,
        deployment.daBondPoolSpend,
        // One validator mints the DAAT and holds its output, so AddSignatures
        // (which only re-outputs the DAAT) is stored for the Apply recovery.
        deployment.daAttestationMint,
        ...(deployment.followedScripts ?? []),
      ]),
      policies: new Set([
        deployment.stateQueueMint,
        ...watcherUnitHistoryPolicies(deployment),
      ]),
    },
    temporalTables: WATCHER_TEMPORAL_TABLES,
    migrations: watcherMigrations,
    derivations: [
      checkpointDerivation(deployment),
      derivation(deployment, credentials),
    ],
    retentionPins: {
      txs: [
        { table: WATCHER_QUEUE_OUTPUTS_TABLE, column: "anchor_tx_hash" },
        // A node's history txs stay readable while the header is queued.
        { table: WATCHER_QUEUE_UNIT_HISTORY_TABLE, column: "tx_hash" },
        // The attested commitment's DAAT, while the header can be challenged.
        { table: WATCHER_DA_ATTESTATIONS_TABLE, column: "tx_hash" },
        // A followed unit's history (a challenge's tranche walk, a
        // family snapshot's unit history), while the unit is on chain.
        { table: WATCHER_UNIT_HISTORY_TABLE, column: "tx_hash" },
      ],
    },
  };
};
