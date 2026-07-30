import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import * as Ledger from "@/database/utils/ledger.js";
import { resolveWithdrawalLedgerOutputAtSelectedBaseV1 } from "@/workers/utils/mpf/withdrawal-classification.js";

describe("withdrawal classification selected-base ledger", () => {
  it("uses the speculative base snapshot instead of stale persisted state", async () => {
    const outRef = Buffer.from("aa", "hex");
    const selectedBaseOutput = Buffer.from("01", "hex");
    let persistedRead = false;

    const output = await Effect.runPromise(
      resolveWithdrawalLedgerOutputAtSelectedBaseV1({
        ledgerOutRef: outRef,
        deferDatabaseWrites: true,
        initialLedgerEntries: [
          {
            [Ledger.Columns.OUTREF]: outRef,
            [Ledger.Columns.OUTPUT]: selectedBaseOutput,
          },
        ],
        retrievePersisted: () => {
          persistedRead = true;
          return Effect.succeed(Option.some(Buffer.from("02", "hex")));
        },
      }),
    );

    expect(persistedRead).toBe(false);
    expect(Option.getOrThrow(output)).toEqual(selectedBaseOutput);
  });

  it("fails closed when a speculative base snapshot is absent", async () => {
    let persistedRead = false;
    const result = await Effect.runPromise(
      Effect.either(
        resolveWithdrawalLedgerOutputAtSelectedBaseV1({
          ledgerOutRef: Buffer.from("aa", "hex"),
          deferDatabaseWrites: true,
          initialLedgerEntries: undefined,
          retrievePersisted: () => {
            persistedRead = true;
            return Effect.succeed(Option.some(Buffer.from("02", "hex")));
          },
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(persistedRead).toBe(false);
  });

  it("uses persisted state for non-speculative builds", async () => {
    let persistedRead = false;
    const persistedOutput = Buffer.from("02", "hex");

    const output = await Effect.runPromise(
      resolveWithdrawalLedgerOutputAtSelectedBaseV1({
        ledgerOutRef: Buffer.from("aa", "hex"),
        deferDatabaseWrites: false,
        initialLedgerEntries: undefined,
        retrievePersisted: () => {
          persistedRead = true;
          return Effect.succeed(Option.some(persistedOutput));
        },
      }),
    );

    expect(persistedRead).toBe(true);
    expect(Option.getOrThrow(output)).toEqual(persistedOutput);
  });
});
