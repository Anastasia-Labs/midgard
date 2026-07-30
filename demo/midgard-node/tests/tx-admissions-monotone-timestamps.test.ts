import "./utils.js";

import { createHash } from "node:crypto";

import {
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  hashMidgardCekTermNodeV1,
} from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { MigrationRunner, TxAdmissionsDB } from "@/database/index.js";

import { provideDatabaseLayers } from "./utils.js";

describe("durable admission monotone timestamps", () => {
  it("survives a database clock observation behind the admitted row", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* MigrationRunner.migrate({
            appVersion: "tx-admissions-monotone-timestamps-test",
            actor: "tx-admissions-monotone-timestamps-test",
          });
          yield* sql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;

          const txCanonicalCbor = Buffer.from("phase1-monotone-timestamp");
          const terminalNode = { kind: "error" as const };
          const programMaterialSidecarCbor =
            encodeMidgardCekProgramMaterialSidecarV1([
              {
                kind: "term",
                root: hashMidgardCekTermNodeV1(terminalNode),
                preimage: encodeMidgardCekTermNodeV1(terminalNode),
              },
            ]);
          const txId = createHash("sha256").update(txCanonicalCbor).digest();
          const admitted = yield* TxAdmissionsDB.tryInsert({
            txId,
            txCanonicalCbor,
            programMaterialSidecarCbor,
            submitSource: "native",
          });
          expect(admitted).not.toBeNull();

          const future = yield* sql<{ readonly future: Date }>`
            UPDATE tx_admissions
            SET
              first_seen_at = NOW() + INTERVAL '5 seconds',
              last_seen_at = NOW() + INTERVAL '5 seconds',
              updated_at = NOW() + INTERVAL '5 seconds',
              next_attempt_at = NOW()
            WHERE tx_id = ${txId}
            RETURNING first_seen_at AS future`;
          const duplicate = yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            programMaterialSidecarCbor,
            submitSource: "native",
            currentBacklog: 1n,
            maxBacklog: 10,
          });
          expect(duplicate.kind).toBe("duplicate");
          expect(duplicate.entry.last_seen_at.getTime()).toBeGreaterThanOrEqual(
            future[0]!.future.getTime(),
          );
          expect(duplicate.entry.updated_at.getTime()).toBeGreaterThanOrEqual(
            future[0]!.future.getTime(),
          );

          const secondCbor = Buffer.from("phase1-monotone-timestamp-second");
          const secondTxId = createHash("sha256").update(secondCbor).digest();
          const second = yield* TxAdmissionsDB.tryInsert({
            txId: secondTxId,
            txCanonicalCbor: secondCbor,
            programMaterialSidecarCbor,
            submitSource: "native",
          });
          expect(second).not.toBeNull();

          const leaseOwner = "tx-admissions-monotone-timestamps-test";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 2,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          expect(claimed.map((row) => row.tx_id)).toStrictEqual([
            txId,
            secondTxId,
          ]);
          const validating = yield* TxAdmissionsDB.getByTxId(txId);
          expect(validating).toMatchObject({
            status: TxAdmissionsDB.Status.Validating,
            lease_owner: leaseOwner,
            attempt_count: 1,
          });
          expect(validating?.lease_expires_at?.getTime()).toBeGreaterThan(
            validating?.validation_started_at?.getTime() ?? 0,
          );
          yield* TxAdmissionsDB.markRejected({
            rows: claimed,
            leaseOwner,
            rejectedTxs: [
              {
                txId,
                code: "E_CBOR_DESERIALIZATION",
                detail: "forced future-timestamp regression",
              },
              {
                txId: secondTxId,
                code: "E_CBOR_DESERIALIZATION",
                detail: "ordering companion",
              },
            ],
          });
          const terminal = yield* TxAdmissionsDB.getByTxId(txId);
          expect(terminal?.status).toBe(TxAdmissionsDB.Status.Rejected);
          expect(
            terminal?.[
              TxAdmissionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
            ],
          ).toEqual(encodeMidgardCekProgramMaterialSidecarV1([]));
          expect(terminal?.terminal_at?.getTime()).toBeGreaterThanOrEqual(
            future[0]!.future.getTime(),
          );
          expect(terminal?.updated_at.getTime()).toBeGreaterThanOrEqual(
            future[0]!.future.getTime(),
          );
          const terminalAt = terminal?.terminal_at?.getTime();
          const idempotentSubmit = yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            programMaterialSidecarCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          expect(idempotentSubmit).toMatchObject({
            kind: "duplicate",
            entry: {
              status: TxAdmissionsDB.Status.Rejected,
              request_count: 3n,
            },
          });
          expect(idempotentSubmit.entry.terminal_at?.getTime()).toBe(
            terminalAt,
          );
          const [microbatchSubmit] = yield* TxAdmissionsDB.admitReservedBatch([
            {
              txId,
              txCanonicalCbor,
              programMaterialSidecarCbor,
              submitSource: "native",
            },
          ]);
          expect(microbatchSubmit).toMatchObject({
            _tag: "Success",
            result: {
              kind: "duplicate",
              entry: {
                status: TxAdmissionsDB.Status.Rejected,
                request_count: 4n,
              },
            },
          });
          const rejectionCounts = yield* sql<{
            readonly count: bigint | number | string;
          }>`SELECT COUNT(*) AS count FROM tx_rejections`;
          expect(Number(rejectionCounts[0]?.count ?? -1)).toBe(2);
        }),
      ),
    );
  });
});
