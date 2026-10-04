import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import { Effect, Schema } from "effect";
import JSONBig from "json-bigint";

import type { EventHistoryListReplay } from "../l1-event-history-list-replay.js";
import type {
  BoundHistoryChainBlock,
  EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { requireSourceTransaction } from "./eventHistoryAuthority.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_census_frontier";
const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const bytes = (value: string) => Buffer.from(value, "hex");
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const checked = <A>(work: () => A) =>
  Effect.try({
    try: work,
    catch: (cause) =>
      new DatabaseError({
        table,
        message: "Invalid canonical event census",
        cause,
      }),
  });
const admissionSchema = Schema.Struct({
  key: Schema.String,
  datumCbor: Schema.String,
  transactionHash: Schema.String,
  transactionIndex: Schema.Number.pipe(Schema.int(), Schema.nonNegative()),
  outputIndex: Schema.Number.pipe(Schema.int(), Schema.nonNegative()),
  eventUnit: Schema.String,
});
export type ForcedHistoryAdmission = typeof admissionSchema.Type;
type Frontier = {
  manifest_id: Buffer;
  activation_hash: Buffer;
  activation_height: string;
  activation_transaction_index: string;
  head_hash: Buffer;
  head_slot: string;
  head_height: string;
};
export type CensusBlockRow = {
  block_hash: Buffer;
  parent_hash: Buffer;
  block_slot: string;
  block_height: string;
  receipt_digest: Buffer;
  admissions_record: string;
  admissions_digest: Buffer;
};

/** Fold only a full block admitted by the bound history follower. A source
 * transaction, not a Ready producer, owns this API. Its compact projection
 * retains every forced NFT's immutable admission even after it is consumed. */
export const append = (input: {
  binding: EventHistorySourceBinding;
  block: BoundHistoryChainBlock;
  receipt: string;
  activation?: EventHistoryListReplay["activation"];
}) =>
  Effect.gen(function* () {
    const token = yield* requireSourceTransaction;
    const sql = yield* SqlClient.SqlClient;
    if (token.deploymentIdentity !== input.binding.manifestId)
      return yield* checked(() => {
        throw new Error("Census owner belongs to another deployment");
      });
    const bindingKey = bytes(input.binding.digest);
    const [frontier] =
      yield* sql<Frontier>`SELECT * FROM event_history_census_frontier WHERE binding_digest = ${bindingKey} FOR UPDATE`;
    const activation = input.activation;
    yield* checked(() => {
      // Domains are the already checked replay and application receipts. This
      // additionally binds the compact projection to the exact admitted block.
      const raw: unknown = lossless.parse(input.receipt);
      if (
        typeof raw !== "object" ||
        raw === null ||
        !("domain" in raw) ||
        !("block" in raw) ||
        !("bindingDigest" in raw) ||
        ![
          "midgard-node-authenticated-history-block-v1",
          "midgard-history-ledger-application-v1",
        ].includes(String(raw.domain)) ||
        raw.bindingDigest !== input.binding.digest ||
        eventHistoryCanonicalJson(raw) !== input.receipt ||
        eventHistoryCanonicalJson(raw.block) !==
          eventHistoryCanonicalJson(input.block)
      )
        throw new Error("Census receipt is not the exact admitted full block");
      if (frontier === undefined) {
        if (
          activation === undefined ||
          activation.point.id !== input.block.point.id ||
          activation.point.height !== input.block.point.height
        )
          throw new Error(
            "Complete census must begin at authenticated activation",
          );
      } else if (
        frontier.manifest_id.toString("hex") !== input.binding.manifestId ||
        !(
          frontier.head_hash.toString("hex") === input.block.point.id ||
          (frontier.head_hash.toString("hex") === input.block.parent &&
            Number(frontier.head_height) + 1 === input.block.point.height &&
            Number(frontier.head_slot) < input.block.point.slot)
        )
      )
        throw new Error(
          "Census block does not extend its complete canonical prefix",
        );
    });
    const hub = yield* checked(() =>
      Data.from(input.binding.hubDatumCbor, SDK.HubOracleDatum),
    );
    const firstIndex =
      frontier === undefined
        ? activation!.transactionIndex
        : input.block.point.id === frontier.activation_hash.toString("hex")
          ? Number(frontier.activation_transaction_index)
          : 0;
    const admissions: ForcedHistoryAdmission[] = [];
    for (
      let transactionIndex = firstIndex;
      transactionIndex < input.block.transactions.length;
      transactionIndex++
    ) {
      const transaction = input.block.transactions[transactionIndex]!;
      if (transaction.spends !== "inputs") continue;
      for (const output of transaction.outputs) {
        const units = Object.keys(output.assets).filter(
          (unit) =>
            unit.startsWith(hub.tx_order) &&
            unit.length === 120 &&
            transaction.mint[unit] === 1n,
        );
        if (units.length === 0) continue;
        if (
          units.length !== 1 ||
          output.assets[units[0]!] !== 1n ||
          output.datum === undefined
        )
          return yield* checked(() => {
            throw new Error("Forced NFT admission has no exact order output");
          });
        const address = yield* SDK.addressDataFromBech32(output.address);
        if (
          Data.to(address, SDK.AddressData) !==
          Data.to(hub.tx_order_addr, SDK.AddressData)
        )
          return yield* checked(() => {
            throw new Error(
              "Forced NFT admission belongs to another order address",
            );
          });
        const orders = yield* SDK.utxosToTxOrderUTxOs(
          [
            {
              txHash: output.txHash,
              outputIndex: output.outputIndex,
              address: output.address,
              assets: { ...output.assets },
              datum: output.datum,
            },
          ],
          hub.tx_order,
        );
        const order = orders[0];
        if (orders.length !== 1 || order === undefined)
          return yield* checked(() => {
            throw new Error(
              "Forced NFT admission failed canonical datum authentication",
            );
          });
        admissions.push({
          key: order.idCbor.toString("hex"),
          datumCbor: output.datum,
          transactionHash: transaction.txHash,
          transactionIndex,
          outputIndex: output.outputIndex,
          eventUnit: units[0]!,
        });
      }
    }
    const record = eventHistoryCanonicalJson(admissions);
    const [existing] =
      yield* sql<CensusBlockRow>`SELECT * FROM event_history_census_blocks WHERE binding_digest = ${bindingKey} AND block_hash = ${bytes(input.block.point.id)}`;
    if (existing !== undefined) {
      yield* checked(() => {
        if (
          existing.admissions_record !== record ||
          existing.admissions_digest.toString("hex") !== hash(record) ||
          existing.receipt_digest.toString("hex") !==
            hash(eventHistoryCanonicalJson(input.block)) ||
          existing.parent_hash.toString("hex") !== input.block.parent ||
          Number(existing.block_slot) !== input.block.point.slot ||
          Number(existing.block_height) !== input.block.point.height
        )
          throw new Error("Conflicting immutable canonical census block");
      });
      yield* sql`UPDATE event_history_census_blocks SET canonical = true WHERE binding_digest = ${bindingKey} AND block_hash = ${bytes(input.block.point.id)}`;
    } else
      yield* sql`INSERT INTO event_history_census_blocks (binding_digest,manifest_id,block_hash,parent_hash,block_slot,block_height,receipt_digest,admissions_record,admissions_digest,canonical)
    VALUES (${bindingKey},${bytes(input.binding.manifestId)},${bytes(input.block.point.id)},${bytes(input.block.parent)},${input.block.point.slot},${input.block.point.height},${bytes(hash(eventHistoryCanonicalJson(input.block)))},${record},${bytes(hash(record))},true)`;
    if (frontier === undefined)
      yield* sql`INSERT INTO event_history_census_frontier (binding_digest,manifest_id,activation_hash,activation_height,activation_transaction_index,head_hash,head_slot,head_height)
    VALUES (${bindingKey},${bytes(input.binding.manifestId)},${bytes(input.block.point.id)},${input.block.point.height},${activation!.transactionIndex},${bytes(input.block.point.id)},${input.block.point.slot},${input.block.point.height})`;
    else
      yield* sql`UPDATE event_history_census_frontier SET head_hash = ${bytes(input.block.point.id)},head_slot = ${input.block.point.slot},head_height = ${input.block.point.height} WHERE binding_digest = ${bindingKey}`;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to append canonical event census"),
  );

/** Called by the existing source recovery undo in the same transaction. */
export const undo = (
  binding: EventHistorySourceBinding,
  head: BoundHistoryChainBlock["point"],
  parent: BoundHistoryChainBlock["point"],
) =>
  Effect.gen(function* () {
    yield* requireSourceTransaction;
    const sql = yield* SqlClient.SqlClient;
    const key = bytes(binding.digest);
    const rows =
      yield* sql`UPDATE event_history_census_frontier SET head_hash = ${bytes(parent.id)},head_slot = ${parent.slot},head_height = ${parent.height}
    WHERE binding_digest = ${key} AND manifest_id = ${bytes(binding.manifestId)} AND head_hash = ${bytes(head.id)} RETURNING binding_digest`;
    if (rows.length !== 1)
      return yield* checked(() => {
        throw new Error("Rollback census frontier differs from source journal");
      });
    yield* sql`UPDATE event_history_census_blocks SET canonical = false WHERE binding_digest = ${key} AND block_hash = ${bytes(head.id)}`;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to rewind canonical event census"),
  );

/** Recovery reacquisition uses the same owner/follower, starting at activation.
 * Immutable block projections survive; only canonical placement is rebuilt. */
export const beginReacquisition = (binding: EventHistorySourceBinding) =>
  Effect.gen(function* () {
    yield* requireSourceTransaction;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM event_history_census_frontier WHERE binding_digest = ${bytes(binding.digest)}`;
    yield* sql`UPDATE event_history_census_blocks SET canonical = false WHERE binding_digest = ${bytes(binding.digest)}`;
  });

export const exists = (binding: EventHistorySourceBinding) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT 1 FROM event_history_census_frontier WHERE binding_digest = ${bytes(binding.digest)} AND manifest_id = ${bytes(binding.manifestId)}`;
    return rows.length === 1;
  });

export const covers = (
  binding: EventHistorySourceBinding,
  point: { id: string; slot: number },
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT 1 FROM event_history_census_frontier f JOIN event_history_census_blocks b ON b.binding_digest = f.binding_digest AND b.block_hash = f.head_hash
    WHERE f.binding_digest = ${bytes(binding.digest)} AND f.manifest_id = ${bytes(binding.manifestId)} AND f.head_hash = ${bytes(point.id)} AND f.head_slot = ${point.slot} AND b.canonical`;
    return rows.length === 1;
  });

export const decodeAdmissions = (
  row: CensusBlockRow,
): readonly ForcedHistoryAdmission[] => {
  const raw: unknown = lossless.parse(row.admissions_record);
  if (
    eventHistoryCanonicalJson(raw) !== row.admissions_record ||
    hash(row.admissions_record) !== row.admissions_digest.toString("hex")
  )
    throw new Error("Forced admission projection digest disagrees");
  return Schema.decodeUnknownSync(Schema.Array(admissionSchema))(raw);
};
