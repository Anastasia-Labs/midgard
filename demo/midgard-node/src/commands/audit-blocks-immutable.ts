import { Effect } from "effect";
import { BlocksDB, ImmutableDB, TxUtils } from "@/database/index.js";
import { Database } from "@/services/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";

type AuditIssueKind =
  | "MISSING_IMMUTABLE"
  | "MALFORMED_NATIVE_TX"
  | "TX_ID_MISMATCH";

type AuditIssue = {
  readonly kind: AuditIssueKind;
  readonly headerHashHex: string;
  readonly txIdHex: string;
  readonly details: string;
};

export type AuditBlocksImmutableOptions = {
  readonly repair?: boolean;
  readonly maxIssuesToLog?: number;
};

export type AuditBlocksImmutableSummary = {
  readonly scannedBlockLinks: number;
  readonly uniqueTxIds: number;
  readonly issueCount: number;
  readonly affectedHeaderCount: number;
  readonly malformedTxCount: number;
  readonly mismatchTxCount: number;
  readonly missingTxCount: number;
  readonly repaired: boolean;
};

const countByKind = (
  issues: readonly AuditIssue[],
  kind: AuditIssueKind,
): number => issues.filter((issue) => issue.kind === kind).length;

/**
 * Audits BlocksDB -> ImmutableDB linkage and validates that immutable payloads
 * decode as Midgard-native txs with matching tx_id hashes.
 *
 * Optional repair mode removes:
 * 1) all block links for affected headers, and
 * 2) malformed/mismatched immutable tx rows.
 */
export const auditBlocksImmutableProgram = (
  options: AuditBlocksImmutableOptions = {},
): Effect.Effect<AuditBlocksImmutableSummary, DatabaseError, Database> =>
  Effect.gen(function* () {
    const maxIssuesToLog = Math.max(1, options.maxIssuesToLog ?? 20);
    const blocksRows = yield* BlocksDB.retrieve;
    if (blocksRows.length <= 0) {
      yield* Effect.logInfo(
        "audit-blocks-immutable: no block rows found; nothing to audit.",
      );
      return {
        scannedBlockLinks: 0,
        uniqueTxIds: 0,
        issueCount: 0,
        affectedHeaderCount: 0,
        malformedTxCount: 0,
        mismatchTxCount: 0,
        missingTxCount: 0,
        repaired: false,
      } satisfies AuditBlocksImmutableSummary;
    }

    const uniqueTxIds = Array.from(
      new Set(
        blocksRows.map((row) => row[BlocksDB.Columns.TX_ID].toString("hex")),
      ),
    ).map((hex) => Buffer.from(hex, "hex"));
    const immutableRows =
      yield* ImmutableDB.retrieveTxEntriesByHashes(uniqueTxIds);
    const immutableByTxId = new Map<string, Buffer>();
    for (const row of immutableRows) {
      immutableByTxId.set(
        row[TxUtils.Columns.TX_ID].toString("hex"),
        row[TxUtils.Columns.TX],
      );
    }

    const issues: AuditIssue[] = [];
    const affectedHeaders = new Set<string>();
    const malformedOrMismatchedTxIds = new Set<string>();

    for (const row of blocksRows) {
      const headerHashHex = row[BlocksDB.Columns.HEADER_HASH].toString("hex");
      const txIdHex = row[BlocksDB.Columns.TX_ID].toString("hex");
      const txPayload = immutableByTxId.get(txIdHex);
      if (txPayload === undefined) {
        issues.push({
          kind: "MISSING_IMMUTABLE",
          headerHashHex,
          txIdHex,
          details: "No immutable row found for tx_id referenced by blocks row",
        });
        affectedHeaders.add(headerHashHex);
        continue;
      }
      try {
        const decoded = decodeMidgardNativeTxFull(txPayload);
        const computedTxIdHex =
          computeMidgardNativeTxIdFromFull(decoded).toString("hex");
        if (computedTxIdHex !== txIdHex) {
          issues.push({
            kind: "TX_ID_MISMATCH",
            headerHashHex,
            txIdHex,
            details: `stored_tx_id=${txIdHex},computed_tx_id=${computedTxIdHex}`,
          });
          affectedHeaders.add(headerHashHex);
          malformedOrMismatchedTxIds.add(txIdHex);
        }
      } catch (error) {
        issues.push({
          kind: "MALFORMED_NATIVE_TX",
          headerHashHex,
          txIdHex,
          details:
            error instanceof Error
              ? `${error.name}: ${error.message}`
              : String(error),
        });
        affectedHeaders.add(headerHashHex);
        malformedOrMismatchedTxIds.add(txIdHex);
      }
    }

    if (issues.length > 0) {
      yield* Effect.logWarning(
        `audit-blocks-immutable: found ${issues.length} issue(s) across ${affectedHeaders.size} header(s).`,
      );
      for (const issue of issues.slice(0, maxIssuesToLog)) {
        yield* Effect.logWarning(
          `audit-blocks-immutable issue kind=${issue.kind},header=${issue.headerHashHex},tx_id=${issue.txIdHex},details=${issue.details}`,
        );
      }
      if (issues.length > maxIssuesToLog) {
        yield* Effect.logWarning(
          `audit-blocks-immutable: ${issues.length - maxIssuesToLog} additional issue(s) omitted from logs.`,
        );
      }
    } else {
      yield* Effect.logInfo(
        `audit-blocks-immutable: scanned ${blocksRows.length} block links; no issues found.`,
      );
    }

    if (options.repair === true && issues.length > 0) {
      yield* Effect.logWarning(
        `audit-blocks-immutable: applying repair (affected_headers=${affectedHeaders.size},invalid_immutable_txs=${malformedOrMismatchedTxIds.size}).`,
      );
      for (const headerHashHex of affectedHeaders) {
        yield* BlocksDB.clearBlock(Buffer.from(headerHashHex, "hex"));
      }
      if (malformedOrMismatchedTxIds.size > 0) {
        yield* ImmutableDB.clearTxs(
          Array.from(malformedOrMismatchedTxIds).map((hex) =>
            Buffer.from(hex, "hex"),
          ),
        );
      }
    }

    return {
      scannedBlockLinks: blocksRows.length,
      uniqueTxIds: uniqueTxIds.length,
      issueCount: issues.length,
      affectedHeaderCount: affectedHeaders.size,
      malformedTxCount: countByKind(issues, "MALFORMED_NATIVE_TX"),
      mismatchTxCount: countByKind(issues, "TX_ID_MISMATCH"),
      missingTxCount: countByKind(issues, "MISSING_IMMUTABLE"),
      repaired: options.repair === true && issues.length > 0,
    } satisfies AuditBlocksImmutableSummary;
  });
