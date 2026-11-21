import { createHash } from "node:crypto";
import type { SQLOutputValue } from "node:sqlite";

import {
  type InventoryFamily,
  inventoryRefuse,
} from "./availability-operation-journal-inventory.schema.js";
import {
  isPlainRecord,
  isUnknownArray,
  parseJsonUnknown,
} from "./narrowing.js";

type SqlRow = Record<string, SQLOutputValue>;
export type InventoryRow = Readonly<Record<string, unknown>>;

/** Scalar and JSON bounds are applied by SQL before values cross into JS. */
export const inventorySelection = (
  family: InventoryFamily,
  fieldBytes: number,
  recordBytes: number,
): string => {
  const bounded = (field: string, bound = fieldBytes) =>
    `CASE WHEN typeof(r.${field}) = 'text' AND length(CAST(r.${field} AS BLOB)) <= ${bound} THEN r.${field} END AS ${field}`;
  switch (family) {
    case "metadata":
      return `${bounded("key")}, typeof(r.value) AS value_type, length(CAST(r.value AS BLOB)) AS value_bytes`;
    case "leases":
      return `${bounded("scope")}, ${bounded("owner")},
        CASE WHEN typeof(r.generation) = 'integer' THEN r.generation END AS generation,
        CASE WHEN typeof(r.expires_at) = 'integer' THEN r.expires_at END AS expires_at`;
    case "intents":
      return (
        ["id", "deployment", "actor", "state", "tx_hash"]
          .map((key) => bounded(key))
          .join(", ") + `, ${bounded("record", recordBytes)}`
      );
    case "resources":
      return (
        ["resource", "intent_id", "kind", "actor"]
          .map((key) => bounded(key))
          .join(", ") +
        `,
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.id = r.intent_id) AS intent_present,
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.id = r.intent_id AND i.actor = r.actor) AS actor_matches`
      );
    case "dependencies":
      return `${bounded("parent_tx_hash")}, ${bounded("child_id")},
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.id = r.child_id) AS child_present,
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.tx_hash = r.parent_tx_hash) AS local_parent_present`;
    case "workflows":
      return (
        ["actor", "deployment", "header_hash", "retired_by"]
          .map((key) => bounded(key))
          .join(", ") +
        `,
      r.retired_by IS NULL AS live, r.release IS NULL AS release_absent, ${bounded("release", recordBytes)},
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.id = r.retired_by) AS retired_intent_present,
      EXISTS(SELECT 1 FROM availability_operation_intents i WHERE i.id = r.retired_by AND i.actor = r.actor AND i.deployment = r.deployment) AS retired_scope_matches,
      (SELECT CASE WHEN length(CAST(i.record AS BLOB)) <= ${recordBytes} THEN
        CASE WHEN json_valid(i.record) THEN json_extract(i.record, '$.intent.headerHash') = r.header_hash ELSE 0 END
        ELSE 0 END FROM availability_operation_intents i WHERE i.id = r.retired_by) AS retired_header_matches,
      (SELECT CASE WHEN length(CAST(i.record AS BLOB)) <= ${recordBytes} THEN
        CASE WHEN json_valid(i.record) THEN i.state = 'confirmed' AND json_extract(i.record, '$.intent.action') = 'open' ELSE 0 END
        ELSE 0 END FROM availability_operation_intents i WHERE i.id = r.retired_by) AS retired_confirmed_open`
      );
  }
};

const integer = (value: unknown): number => {
  if (
    (typeof value !== "number" && typeof value !== "bigint") ||
    value < 0 ||
    value > Number.MAX_SAFE_INTEGER ||
    !Number.isSafeInteger(Number(value))
  )
    inventoryRefuse("inventory_record_malformed");
  return Number(value);
};
const identifier = (value: unknown, fieldBytes: number): string => {
  if (
    typeof value !== "string" ||
    !value ||
    Buffer.byteLength(value) > fieldBytes ||
    !/^[A-Za-z0-9._:#/@+-]+$/u.test(value)
  )
    inventoryRefuse("inventory_field_unsafe_or_oversized");
  return value;
};
const object = (value: unknown): Record<string, unknown> => {
  if (!isPlainRecord(value)) inventoryRefuse("inventory_record_malformed");
  return value;
};
const decode = (value: unknown): Record<string, unknown> => {
  if (typeof value !== "string")
    inventoryRefuse("inventory_record_oversized_or_malformed");
  try {
    return object(parseJsonUnknown(value));
  } catch {
    return inventoryRefuse("inventory_record_malformed");
  }
};
const flag = (value: unknown): boolean => {
  const parsed = integer(value);
  if (parsed > 1) inventoryRefuse("inventory_record_malformed");
  return parsed === 1;
};

export const projectInventoryRow = (
  family: InventoryFamily,
  row: SqlRow,
  fieldBytes: number,
): InventoryRow => {
  const id = (value: unknown) => identifier(value, fieldBytes);
  switch (family) {
    case "metadata": {
      const key = id(row.key);
      if (row.value_type !== "text")
        inventoryRefuse("inventory_record_malformed");
      return {
        key: key === "schema" || key === "halt" ? key : "other",
        keyDigest: createHash("sha256").update(key).digest("hex"),
        valuePresent: true,
        valueBytes: integer(row.value_bytes),
      };
    }
    case "leases": {
      const generation = integer(row.generation);
      if (generation === 0) inventoryRefuse("inventory_record_malformed");
      const expiresAtMs = integer(row.expires_at);
      return {
        actor: id(row.scope),
        owner: id(row.owner),
        generation,
        expiresAtMs,
        releasedMarker: expiresAtMs === 0,
      };
    }
    case "intents": {
      const record = decode(row.record);
      const intent = object(record.intent);
      for (const [column, key] of [
        ["id", "id"],
        ["deployment", "deploymentIdentity"],
        ["actor", "actor"],
        ["tx_hash", "txHash"],
      ])
        if (row[column!] !== intent[key!])
          inventoryRefuse("inventory_record_inconsistent");
      if (
        !["pending", "included", "confirmed", "expired", "conflict"].includes(
          id(row.state),
        ) ||
        record.state !== row.state ||
        typeof intent.signedCbor !== "string" ||
        typeof intent.completesWorkflow !== "boolean" ||
        (record.detail !== null && typeof record.detail !== "string")
      )
        inventoryRefuse("inventory_record_malformed");
      for (const key of [
        "spentOutRefs",
        "collateralOutRefs",
        "expectedOutRefs",
      ]) {
        const refs = intent[key];
        if (
          !isUnknownArray(refs) ||
          !refs.every((ref) => typeof ref === "string")
        )
          inventoryRefuse("inventory_record_malformed");
      }
      const inclusionPoint =
        record.inclusionPoint === null ? null : id(record.inclusionPoint);
      if (
        (row.state === "included" || row.state === "confirmed") &&
        inclusionPoint === null
      )
        inventoryRefuse("inventory_record_inconsistent");
      return {
        id: id(row.id),
        actor: id(row.actor),
        deployment: id(row.deployment),
        headerHash: id(intent.headerHash),
        action: id(intent.action),
        txHash: id(row.tx_hash),
        state: id(row.state),
        inclusionPoint,
        validUntilSlot: integer(intent.validUntilSlot),
        completesWorkflow: intent.completesWorkflow,
        retentionBlockNo:
          record.retentionBlockNo === undefined
            ? null
            : integer(record.retentionBlockNo),
        detailPresent: record.detail !== null,
        detailDigest:
          typeof record.detail === "string"
            ? createHash("sha256").update(record.detail).digest("hex")
            : null,
      };
    }
    case "resources": {
      if (row.kind !== "spend" && row.kind !== "collateral")
        inventoryRefuse("inventory_record_malformed");
      return {
        resource: id(row.resource),
        intentId: id(row.intent_id),
        kind: row.kind,
        actor: id(row.actor),
        intentPresent: flag(row.intent_present),
        actorMatches: flag(row.actor_matches),
      };
    }
    case "dependencies":
      return {
        parentTxHash: id(row.parent_tx_hash),
        childId: id(row.child_id),
        childPresent: flag(row.child_present),
        parentReference: flag(row.local_parent_present)
          ? "local"
          : "external_or_unresolved",
      };
    case "workflows": {
      const live = flag(row.live);
      const releaseAbsent = flag(row.release_absent);
      let release: InventoryRow | null = null;
      if (!releaseAbsent) {
        const evidence = decode(row.release);
        if (
          evidence.reason !== "header-node-burned" &&
          evidence.reason !== "challenge-closed"
        )
          inventoryRefuse("inventory_record_malformed");
        release = {
          reason: evidence.reason,
          txHash: id(evidence.txHash),
          spendPoint: id(evidence.spendPoint),
          authority: "stored_unreobserved",
        };
      }
      if (live && !releaseAbsent)
        inventoryRefuse("inventory_record_inconsistent");
      return {
        actor: id(row.actor),
        deployment: id(row.deployment),
        headerHash: id(row.header_hash),
        live,
        retiredBy: live ? null : id(row.retired_by),
        retiredIntentPresent: flag(row.retired_intent_present),
        retiredScopeMatches: flag(row.retired_scope_matches),
        retiredHeaderMatches:
          row.retired_header_matches === null
            ? false
            : flag(row.retired_header_matches),
        retiredConfirmedOpen:
          row.retired_confirmed_open === null
            ? false
            : flag(row.retired_confirmed_open),
        release,
      };
    }
  }
};
