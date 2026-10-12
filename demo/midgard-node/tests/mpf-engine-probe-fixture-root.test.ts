import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import * as Ledger from "../src/database/utils/ledger.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  keyValuePhasRoot,
  MidgardMpf,
} from "../src/mpf/index.js";
import {
  buildCanonicalFixtureEntries,
  ledgerFixtureMpfEntries,
} from "../src/workers/mpf-engine-probe-corpus.js";

const scriptOutput = (addressByte: string): Buffer =>
  Buffer.from(`a200581d70${addressByte.repeat(28)}018200a0`, "hex");

describe("architecture G probe Level fixture", () => {
  it.effect(
    "has the root production hydrates from the staged confirmed_ledger rows",
    () =>
      Effect.gen(function* () {
        const funding = new Map<string, Uint8Array>([
          [`${"11".repeat(32)}#0`, scriptOutput("aa")],
          [`${"22".repeat(32)}#1`, scriptOutput("bb")],
          [`${"33".repeat(32)}#2`, scriptOutput("cc")],
        ]);
        const entries = buildCanonicalFixtureEntries(funding, 8);
        // The rows the candidate seed stages into confirmed_ledger: the same
        // entries, with the raw output CBOR.
        const confirmedLedger = entries.map(({ key, value }) => ({
          [Ledger.Columns.TX_ID]: Buffer.from(
            decodeMidgardSpendInputItem(key).txId,
          ),
          [Ledger.Columns.OUTREF]: key,
          [Ledger.Columns.OUTPUT]: value,
        }));
        const directory = yield* Effect.promise(() =>
          mkdtemp(join(tmpdir(), "midgard-probe-fixture-root-")),
        );
        const marker = yield* Effect.gen(function* () {
          const fixture = yield* MidgardMpf.createLevelFromListForBenchmark(
            "test-probe-fixture-root",
            join(directory, "level"),
            ledgerFixtureMpfEntries(entries),
            { mode: "overlay" },
          );
          const root = yield* fixture.rootHex();
          yield* fixture.close();
          return root;
        }).pipe(
          Effect.ensuring(
            Effect.promise(() =>
              rm(directory, { recursive: true, force: true }),
            ),
          ),
        );
        const confirmedLedgerRoot =
          yield* computeLedgerMpfRootFromLedgerEntries(confirmedLedger);
        const rawValueRoot = yield* keyValuePhasRoot(
          entries.map(({ key }) => key),
          entries.map(({ value }) => value),
        );

        expect(entries).toHaveLength(8);
        expect(marker).toBe(confirmedLedgerRoot);
        expect(rawValueRoot).not.toBe(confirmedLedgerRoot);
      }),
  );
});
