/**
 * The production ledger wiring end to end: the native owner over the node's
 * LevelDB ledger store, its outputs read from the node database by out-ref
 * (`readLedgerOutputs`). A ledger the TypeScript side applied and persisted
 * is restored to by the pinned native child, which reads the trie nodes the
 * store wrote, and a promotion from it reaches the root the TypeScript side
 * computes for the same edit.
 */
import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Runtime } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { readLedgerOutputs } from "../src/landed-blocks/ledger-outputs.js";
import { ledgerOutputToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import type { Database } from "../src/services/database.js";
import { openNativeOwnerLedger } from "../src/services/mpf-native-owner/service.open-ledger.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";
import { seedOwnerLedger } from "./helpers/native-owner-ledger.js";
import { seed } from "./landed-blocks-foreign-ledger.fixture.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent)
  warnNativeOwnerBinaryAbsent("mpf-native-owner-persisted-restore");

const entryOf = (label: string, lovelace: bigint) => ({
  outref: makeOutRefCbor(simDigest(`persisted-restore:${label}`), 0),
  output: simOutput(lovelace),
});

/** Enough entries for the trie to have branches below its root. */
const CONFIRMED = Array.from({ length: 16 }, (_, index) =>
  entryOf(`confirmed-${index.toString()}`, 2_000_000n + BigInt(index)),
);
const SPENT = CONFIRMED[3]!;
const KEPT = CONFIRMED[0]!;
const PRODUCED = entryOf("produced", 9_000_000n);

const outputsOf = async (ledger: Ledger, outRefs: readonly Buffer[]) => {
  const read = await ledger.read(outRefs);
  return outRefs.map((outRef) => read.get(outRef.toString("hex"))?.output);
};

describe.skipIf(!binaryPresent)("native restore to a persisted ledger", () => {
  const paths: string[] = [];
  const services: ProductionNativeMpfOwnerService[] = [];
  let binarySha256: string;

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(nativeOwnerBinaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    await Promise.all(services.splice(0).map((service) => service.close()));
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  it("restores the native child to a root the TypeScript store persisted, and promotes from it to the root the TypeScript side computes", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const runtime = yield* Effect.runtime<Database>();
          const directory = yield* Effect.promise(() =>
            mkdtemp(join(tmpdir(), "midgard-persisted-restore-")),
          );
          paths.push(directory);
          const levelPath = join(directory, "ledger");
          const service = yield* Effect.promise(async () => {
            await seedOwnerLedger(levelPath, SDK.EMPTY_MERKLE_TREE_ROOT);
            const ledger = await openNativeOwnerLedger(levelPath, (outRefs) =>
              Runtime.runPromise(runtime)(readLedgerOutputs(outRefs)),
            );
            const opened = await ProductionNativeMpfOwnerService.create(
              ledger,
              { binaryPath: nativeOwnerBinaryPath, binarySha256 },
            );
            services.push(opened);
            return opened;
          });
          const empty = (yield* Effect.promise(() => service.diagnostics()))
            .durableRoot;
          const confirmed = yield* Effect.promise(async () =>
            (await service.ledger(empty)).apply(
              CONFIRMED.map(({ outref, output }) => ({
                type: "insert" as const,
                outRef: outref,
                output,
              })),
            ),
          );
          yield* seed(CONFIRMED, {
            headerHash: "33".repeat(28),
            utxosRoot: confirmed.root,
          });

          const promoted = yield* Effect.promise(async () => {
            await service.persistLedger(confirmed);
            await service.restoreCanonicalRoot({
              recoveryId: createHash("sha256")
                .update(`${empty}:${confirmed.root}`)
                .digest("hex"),
              expectedRoot: empty,
              targetRoot: confirmed.root,
            });
            const restored = (await service.diagnostics()).durableRoot;
            const handle = await service.fork(confirmed.root);
            const applied = await service.applyEvents(
              handle,
              encodeNativeMpfEventLog(confirmed.root, [
                [
                  { type: "delete", key: SPENT.outref },
                  ledgerOutputToInsertBatchOp({
                    outRef: PRODUCED.outref,
                    outputCbor: PRODUCED.output,
                  }),
                ],
              ]),
            );
            await service.promote(handle);
            return { restored, candidate: applied.candidateRoot };
          });
          expect(promoted.restored).toBe(confirmed.root);

          const expected = yield* Effect.promise(() =>
            confirmed.apply([
              { type: "delete", outRef: SPENT.outref },
              {
                type: "insert",
                outRef: PRODUCED.outref,
                output: PRODUCED.output,
              },
            ]),
          );
          expect(promoted.candidate).toBe(expected.root);
          const durable = yield* Effect.promise(() => service.diagnostics());
          expect(durable.durableRoot).toBe(expected.root);
          // The promoted root's ledger, through the store: its trie holds the
          // confirmed output still unspent, read from `confirmed_ledger`, and
          // not the spent one.
          expect(
            yield* Effect.promise(async () =>
              outputsOf(await service.ledger(expected.root), [
                KEPT.outref,
                SPENT.outref,
              ]),
            ),
          ).toEqual([KEPT.output, undefined]);
        }),
      ),
    );
  }, 120_000);
});
