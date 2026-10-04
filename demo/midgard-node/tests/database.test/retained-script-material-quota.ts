import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  CekProgramMaterialDB,
  DaPayloadsDB,
} from "../../src/database/index.js";
import { NodeConfig } from "../../src/services/config.js";
import {
  daPayloadInsertFixture,
  isolatedDb,
  makeMaterialProofSubmitTx,
  readCekProgramMaterialStoreStats,
} from "./fixtures.js";

export const registerRetainedScriptMaterialQuotaTests = (
  scriptOutput: (
    attempt: ReturnType<typeof makeMaterialProofSubmitTx>,
  ) => Buffer,
) => {
  it.effect(
    "reserves permanent material bytes while retained and keeps durable retries available after prune",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const first = makeMaterialProofSubmitTx(181);
          const next = makeMaterialProofSubmitTx(182);
          yield* CekProgramMaterialDB.persistVerifiedBundles(
            [first.envelope],
            [first.material],
          );
          const firstBytes = Number(
            (yield* readCekProgramMaterialStoreStats).total_bytes,
          );
          const insert = {
            ...daPayloadInsertFixture("live-pin-durable-reservation"),
            block_start_time: new Date("2026-01-01"),
            block_end_time: new Date("2026-01-02"),
          };
          yield* DaPayloadsDB.upsertAvailable(insert);
          yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
            headerHash: insert.header_hash,
            outputs: [scriptOutput(first)],
            material: [],
          });
          const config = yield* NodeConfig;
          const capped = {
            ...config,
            CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: firstBytes,
          };
          const refused = yield* CekProgramMaterialDB.persistVerifiedBundles(
            [next.envelope],
            [next.material],
          ).pipe(Effect.provideService(NodeConfig, capped), Effect.either);
          expect(refused).toMatchObject({
            _tag: "Left",
            left: {
              message:
                "CEK program material store exceeds its durable aggregate byte cap",
            },
          });
          expect(
            Number((yield* readCekProgramMaterialStoreStats).total_bytes),
          ).toBe(firstBytes);
          // These equal-sized distinct valid programs isolate ownership accounting.
          const bothCapped = {
            ...config,
            CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: firstBytes * 2,
          };
          yield* CekProgramMaterialDB.persistVerifiedBundles(
            [next.envelope],
            [next.material],
          ).pipe(Effect.provideService(NodeConfig, bothCapped));
          const head = {
            ...daPayloadInsertFixture("live-pin-durable-reservation-head"),
            block_start_time: new Date("2026-01-02"),
            block_end_time: new Date("2026-01-03"),
          };
          yield* DaPayloadsDB.upsertAvailable(head);
          expect(
            yield* DaPayloadsDB.pruneBeyondRetention({
              challengeableCutoff: new Date("2027-01-01"),
              view: {
                confirmedHeadHash: head.header_hash,
                liveQueueHeaderHashes: [],
              },
              deploymentIdentityDigest: undefined,
            }),
          ).toBe(1);
          const sql = yield* SqlClient.SqlClient;
          expect(
            yield* sql`SELECT * FROM cek_program_material_retained_state_owners`,
          ).toEqual([]);
          expect(
            Number((yield* readCekProgramMaterialStoreStats).total_bytes),
          ).toBe(firstBytes * 2);
          yield* CekProgramMaterialDB.releaseAdmissionOwnership([
            first.txId,
            next.txId,
          ]);
          for (const attempt of [first, next]) {
            yield* CekProgramMaterialDB.persistVerifiedBundles(
              [attempt.envelope],
              [attempt.material],
            ).pipe(Effect.provideService(NodeConfig, bothCapped));
            expect(
              yield* CekProgramMaterialDB.retrieveVerifiedBundles([
                attempt.envelope,
              ]),
            ).toEqual([attempt.material]);
          }
        }),
      ),
  );

  it.effect(
    "reserves admission bytes while retained until their admission owner releases",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const first = makeMaterialProofSubmitTx(183);
          const next = makeMaterialProofSubmitTx(184);
          yield* CekProgramMaterialDB.persistVerifiedBundles(
            [first.envelope],
            [first.material],
            { kind: "admission", txId: first.txId },
          );
          const firstBytes = Number(
            (yield* readCekProgramMaterialStoreStats).total_bytes,
          );
          // Keep room for the first owner's three 32-byte keys, while
          // leaving no room for two complete material bundles.
          const ownerBytes =
            96 * Number((yield* readCekProgramMaterialStoreStats).owner_count);
          const insert = daPayloadInsertFixture(
            "live-pin-admission-reservation",
          );
          yield* DaPayloadsDB.upsertAvailable(insert);
          yield* CekProgramMaterialDB.pinRetainedStateScriptRefs({
            headerHash: insert.header_hash,
            outputs: [scriptOutput(first)],
            material: [],
          });
          const config = yield* NodeConfig;
          const capped = {
            ...config,
            CEK_PROGRAM_MATERIAL_STORE_MAX_BYTES: firstBytes + ownerBytes,
          };
          const persistNext = CekProgramMaterialDB.persistVerifiedBundles(
            [next.envelope],
            [next.material],
            { kind: "admission", txId: next.txId },
          ).pipe(Effect.provideService(NodeConfig, capped));
          expect(yield* persistNext.pipe(Effect.either)).toMatchObject({
            _tag: "Left",
            left: {
              message:
                "CEK program material store exceeds its durable aggregate byte cap",
            },
          });
          yield* CekProgramMaterialDB.releaseAdmissionOwnership([first.txId]);
          yield* persistNext;
          expect(
            yield* CekProgramMaterialDB.retrieveVerifiedBundles([
              first.envelope,
            ]),
          ).toEqual([first.material]);
          expect(
            yield* CekProgramMaterialDB.retrieveVerifiedBundles([
              next.envelope,
            ]),
          ).toEqual([next.material]);
        }),
      ),
  );
};
