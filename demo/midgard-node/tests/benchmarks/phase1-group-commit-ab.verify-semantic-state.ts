import { SqlClient } from "@effect/sql";
import { Effect, Exit } from "effect";
import { expect } from "vitest";

import { TxAdmissionsDB } from "../../src/database/index.js";
import {
  type AdmissionFixture,
  classifyOutcomes,
  expectStrictlyIncreasingArrival,
  laneCount,
  mergedRows,
  requestForLane,
  rowsPerLane,
  toBigInt,
  type Variant,
} from "./phase1-group-commit-ab.build-fixture.js";

export const verifySemanticState = (
  variant: Variant,
  fixture: AdmissionFixture,
  outcomes: readonly TxAdmissionsDB.ReservedAdmissionOutcome[],
) =>
  Effect.gen(function* () {
    expect(outcomes).toHaveLength(mergedRows);
    expect(classifyOutcomes(outcomes)).toEqual(fixture.expectedKinds);
    const sql = yield* SqlClient.SqlClient;
    const counts = yield* sql<{
      readonly admission_count: bigint | number | string;
      readonly payload_count: bigint | number | string;
    }>`SELECT
        (SELECT COUNT(*)::bigint FROM tx_admissions) AS admission_count,
        (SELECT COUNT(*)::bigint FROM tx_admission_payloads) AS payload_count`;
    expect(toBigInt(counts[0]?.admission_count ?? -1)).toBe(250n);
    expect(toBigInt(counts[0]?.payload_count ?? -1)).toBe(250n);

    for (let lane = 0; lane < laneCount; lane += 1) {
      const duplicateGroup = yield* TxAdmissionsDB.getByTxId(
        fixture.duplicateGroupRequests[lane]!.txId,
      );
      expect(duplicateGroup?.request_count).toBe(2n);
      expect(duplicateGroup?.tx_canonical_cbor).toEqual(
        fixture.duplicateGroupRequests[lane]!.txCanonicalCbor,
      );
      const existing = yield* TxAdmissionsDB.getByTxId(
        fixture.existingRequests[lane]!.txId,
      );
      expect(existing?.request_count).toBe(2n);
      expect(existing?.tx_canonical_cbor).toEqual(
        fixture.existingRequests[lane]!.txCanonicalCbor,
      );
    }

    const arrivalRows = yield* sql<{
      readonly tx_id_hex: string;
      readonly arrival_seq: bigint | number | string;
    }>`SELECT encode(tx_id, 'hex') AS tx_id_hex, arrival_seq
      FROM tx_admissions
      ORDER BY arrival_seq ASC`;
    const arrivalByTxId = new Map(
      arrivalRows.map((row) => [row.tx_id_hex, toBigInt(row.arrival_seq)]),
    );
    for (const laneRequests of fixture.laneFirstNewRequests) {
      expectStrictlyIncreasingArrival(laneRequests, arrivalByTxId);
    }
    if (variant === "one_ordered_256") {
      expectStrictlyIncreasingArrival(
        fixture.mergedFirstNewRequests,
        arrivalByTxId,
      );
    }
  });

export const verifyRollbackParity = (
  variant: Variant,
  batchSql: SqlClient.SqlClient,
) =>
  Effect.gen(function* () {
    yield* batchSql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
    const lanes = Array.from({ length: laneCount }, (_, lane) => {
      const requests = Array.from({ length: rowsPerLane }, (_, index) =>
        requestForLane(
          lane,
          `rollback:${variant}:lane-${lane.toString()}:${index.toString()}`,
        ),
      );
      requests[0] = requestForLane(
        lane,
        `rollback:${variant}:lane-${lane.toString()}:invalid`,
        31,
      );
      return requests;
    });
    if (variant === "two_concurrent_128") {
      const exits = yield* Effect.all(
        lanes.map((requests) =>
          Effect.exit(TxAdmissionsDB.admitReservedBatch(requests)),
        ),
        { concurrency: "unbounded" },
      );
      expect(exits).toHaveLength(2);
      expect(exits.every(Exit.isFailure)).toBe(true);
    } else {
      const merged = Array.from({ length: rowsPerLane }, (_, index) =>
        lanes.flatMap((requests) => requests[index]!),
      ).flat();
      expect(merged).toHaveLength(mergedRows);
      expect(
        Exit.isFailure(
          yield* Effect.exit(TxAdmissionsDB.admitReservedBatch(merged)),
        ),
      ).toBe(true);
    }
    const counts = yield* batchSql<{
      readonly admission_count: bigint | number | string;
      readonly payload_count: bigint | number | string;
    }>`SELECT
        (SELECT COUNT(*)::bigint FROM tx_admissions) AS admission_count,
        (SELECT COUNT(*)::bigint FROM tx_admission_payloads) AS payload_count`;
    expect(toBigInt(counts[0]?.admission_count ?? -1)).toBe(0n);
    expect(toBigInt(counts[0]?.payload_count ?? -1)).toBe(0n);
  });
