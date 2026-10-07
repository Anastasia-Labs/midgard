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
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import {
  lockOutput,
  queueOutput,
} from "../indexers/authenticated-state-queue-observation.queue-output.js";
import {
  CHECKPOINT_TEMPORAL_TABLES,
  checkpointDerivation,
  checkpointMigrationSql,
} from "./checkpoints.js";
import {
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
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
  WATCHER_QUEUE_CHECKPOINTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
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
}>;

export type WatcherProjection = Readonly<{
  name: string;
  trackedSet: TrackedSet;
  temporalTables: readonly TemporalTableSpec[];
  migrations: (dialect: DialectName) => MigrationSet;
  derivations: readonly DerivationHook[];
  retentionPins: RetentionPins;
}>;

const TEMPORAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: WATCHER_QUEUE_OUTPUTS_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  ...CHECKPOINT_TEMPORAL_TABLES,
];

const migrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: current rows forever; closed rows once to_slot is k deep
CREATE TABLE ${WATCHER_QUEUE_OUTPUTS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  kind text NOT NULL CHECK (kind IN ('root', 'node', 'lock', 'malformed')),
  header_hash ${bytes},
  next_header_hash ${bytes},
  header_cbor ${bytes},
  state_queue_node_cbor ${bytes},
  datum_cbor ${bytes},
  malformed text,
  anchor_tx_hash ${bytes} NOT NULL,
  anchor_slot ${int8} NOT NULL,
  anchor_block_hash ${bytes} NOT NULL,
  anchor_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_from ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_to ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_header ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (header_hash);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_anchor ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (anchor_tx_hash);
`;
};

/**
 * Per state-queue node header, every canonical tx that created or spent one
 * of its node outputs: the unit history the old Kupmios reads return for
 * the node unit. A header's rows close when its node leaves the queue.
 */
const unitHistoryMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows of a header in the queue forever; closed rows once to_slot (the header's removal) is k deep
CREATE TABLE ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (
  header_hash ${bytes} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (header_hash, tx_hash)
);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_from ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_to ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_tx ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (tx_hash);
`;
};

const watcherMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "watcher",
  migrations: [
    { id: "0001_watcher_queue_outputs", sql: migrationSql(dialect) },
    {
      id: "0002_watcher_queue_unit_history",
      sql: unitHistoryMigrationSql(dialect),
    },
    {
      id: "0003_watcher_queue_checkpoints",
      sql: checkpointMigrationSql(dialect),
    },
  ],
});

type Anchor = Readonly<{
  txHash: Buffer;
  slot: number;
  blockHash: Buffer;
  height: number;
}>;

type DecodedRow =
  | Readonly<{
      kind: "root" | "node";
      headerHash: Buffer | null;
      nextHeaderHash: Buffer | null;
      headerCbor: Buffer | null;
      stateQueueNodeCbor: Buffer | null;
      datumCbor: Buffer;
    }>
  | Readonly<{ kind: "lock"; datumCbor: Buffer }>
  | Readonly<{ kind: "malformed"; reason: string }>;

const hexBytes = (hex: string): Buffer => Buffer.from(hex, "hex");

const describe = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * Decodes one created output. Null: neither a queue nor a lock output (for
 * example a payment to the queue address without the policy, or the
 * hub-oracle output).
 */
const decodeOutput = (
  output: CML.TransactionOutput,
  outRef: string,
  addresses: Readonly<{ stateQueue: string; correctionLock: string }>,
  deployment: WatcherProjectionDeployment,
): DecodedRow | null => {
  try {
    const queue = queueOutput({
      output,
      outRef,
      stateQueueAddress: addresses.stateQueue,
      stateQueuePolicyId: deployment.stateQueueMint,
    });
    if (queue !== null) {
      if (queue.node.headerHash === null) {
        const datum = output.datum()?.as_datum();
        if (datum === undefined)
          return { kind: "malformed", reason: "root has no inline datum" };
        return {
          kind: "root",
          headerHash: null,
          nextHeaderHash:
            queue.nextHeaderHash === null
              ? null
              : hexBytes(queue.nextHeaderHash),
          headerCbor: null,
          stateQueueNodeCbor: null,
          datumCbor: hexBytes(datum.to_canonical_cbor_hex()),
        };
      }
      const header = queue.header;
      if (header === null)
        return { kind: "malformed", reason: "node has no decoded header" };
      return {
        kind: "node",
        headerHash: hexBytes(header.headerHash),
        nextHeaderHash:
          queue.nextHeaderHash === null ? null : hexBytes(queue.nextHeaderHash),
        headerCbor: hexBytes(header.headerCborHex),
        stateQueueNodeCbor: hexBytes(header.stateQueueNodeCborHex),
        datumCbor: hexBytes(header.linkedListDatumCborHex),
      };
    }
    const lock = lockOutput({
      output,
      outRef,
      correctionLockAddress: addresses.correctionLock,
      hubOraclePolicyId: deployment.hubOracleMint,
    });
    if (lock === null) return null;
    return {
      kind: "lock",
      datumCbor: hexBytes(Data.to(lock.datum, SDK.CorrectionLockDatum)),
    };
  } catch (error) {
    return { kind: "malformed", reason: describe(error) };
  }
};

/** The CML outputs a tx created on chain: its outputs, or a failed tx's collateral return. */
const createdCmlOutput = (
  body: CML.TransactionBody,
  isValid: boolean,
  index: number,
): CML.TransactionOutput | undefined => {
  if (isValid) {
    const outputs = body.outputs();
    return index < outputs.len() ? outputs.get(index) : undefined;
  }
  return body.collateral_return();
};

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
 * anchors it (`anchoredHeaderObservation`).
 */
const closeSpent = async (
  tx: SqlTx,
  context: DerivationContext,
  spent: DerivationContext["qualified"][number]["spent"],
): Promise<Map<string, Anchor>> => {
  const inherited = new Map<string, Anchor>();
  const slot = context.block.point.slot;
  for (const outRef of spent) {
    const rows = await tx.query(
      `SELECT kind, header_hash, anchor_tx_hash, anchor_slot, anchor_block_hash, anchor_height FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL`,
      [outRef.txHash, outRef.index],
    );
    const row = rows[0];
    if (row === undefined) continue;
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
  return inherited;
};

/**
 * Appends this tx to the unit history of every node header it spent or
 * created, and closes the history of a header whose node it spent without
 * re-outputting it (the header left the queue).
 */
const recordUnitHistory = async (
  context: DerivationContext,
  txHash: Buffer,
  spent: ReadonlyMap<string, Anchor>,
  created: ReadonlySet<string>,
): Promise<void> => {
  const { tx, block } = context;
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
    if (!created.has(header))
      await tx.query(
        `UPDATE ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} SET to_slot = ? WHERE header_hash = ? AND to_slot IS NULL`,
        [block.point.slot, hexBytes(header)],
      );
  }
};

const derivation = (
  deployment: WatcherProjectionDeployment,
  credentials: ReadonlySet<string>,
): DerivationHook => {
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
  return {
    name: "watcher_state_queue",
    writes: [WATCHER_QUEUE_OUTPUTS_TABLE, WATCHER_QUEUE_UNIT_HISTORY_TABLE],
    apply: async (context) => {
      const { tx, block } = context;
      for (const entry of context.qualified) {
        const inherited = await closeSpent(tx, context, entry.spent);
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
        await recordUnitHistory(context, entry.tx.hash, inherited, created);
      }
    },
  };
};

/**
 * The watcher projection for one deployment. Its tracked set holds the
 * state-queue and CorrectionLock credentials, the hub-oracle output's
 * credential (so the protocol-init tx stays stored while that output is
 * live, which R3's check relies on), the fraud-proof, availability-challenge
 * and DA-bond-pool credentials (the outputs a correction references and the
 * availability snapshot reads), and the state-queue and hub-oracle policies
 * (the init tx qualifies through its hub-oracle mint).
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
      ]),
      policies: new Set([deployment.stateQueueMint, deployment.hubOracleMint]),
    },
    temporalTables: TEMPORAL_TABLES,
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
      ],
    },
  };
};
