import { describe, expect, it } from "vitest";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import * as DepositsDB from "@/database/deposits.js";
import {
  encodeDepositStatus,
  parseDepositStatusLookup,
} from "@/commands/deposit-status.js";

const makeDepositEventId = (
  transactionId: string,
  outputIndex: bigint,
): Buffer =>
  Buffer.from(
    LucidData.to(
      {
        transactionId,
        outputIndex,
      },
      SDK.OutputReference,
    ),
    "hex",
  );

const makeDepositEntry = (
  overrides: Partial<DepositsDB.Entry> = {},
): DepositsDB.Entry => ({
  [DepositsDB.Columns.ID]:
    overrides[DepositsDB.Columns.ID] ?? makeDepositEventId("11".repeat(32), 0n),
  [DepositsDB.Columns.INFO]:
    overrides[DepositsDB.Columns.INFO] ?? Buffer.from("aa".repeat(48), "hex"),
  [DepositsDB.Columns.INCLUSION_TIME]:
    overrides[DepositsDB.Columns.INCLUSION_TIME] ??
    new Date("2026-04-13T17:28:10.999Z"),
  [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]:
    overrides[DepositsDB.Columns.DEPOSIT_L1_TX_HASH] ??
    Buffer.from("22".repeat(32), "hex"),
  [DepositsDB.Columns.LEDGER_TX_ID]:
    overrides[DepositsDB.Columns.LEDGER_TX_ID] ??
    Buffer.from("33".repeat(32), "hex"),
  [DepositsDB.Columns.LEDGER_OUTPUT]:
    overrides[DepositsDB.Columns.LEDGER_OUTPUT] ??
    Buffer.from("44".repeat(80), "hex"),
  [DepositsDB.Columns.LEDGER_ADDRESS]:
    overrides[DepositsDB.Columns.LEDGER_ADDRESS] ??
    "addr_test1wqp2djasm6s2p6e23v2w5ks70zw66cr2uyelsn7acktlw2q3k02jh",
  [DepositsDB.Columns.PROJECTED_HEADER_HASH]:
    overrides[DepositsDB.Columns.PROJECTED_HEADER_HASH] ?? null,
  [DepositsDB.Columns.STATUS]:
    overrides[DepositsDB.Columns.STATUS] ?? DepositsDB.Status.Awaiting,
});

describe("deposit-status command helpers", () => {
  it("parses eventId and canonicalizes OutputReference CBOR", () => {
    const eventId = makeDepositEventId("ab".repeat(32), 7n);

    const parsed = parseDepositStatusLookup({
      eventId: eventId.toString("hex").toUpperCase(),
    });

    expect(parsed.eventId?.equals(eventId)).toBe(true);
    expect(parsed.cardanoTxHash).toBeUndefined();
  });

  it("parses cardanoTxHash as a 32-byte hex selector", () => {
    const parsed = parseDepositStatusLookup({
      cardanoTxHash: "cd".repeat(32).toUpperCase(),
    });

    expect(parsed.cardanoTxHash?.toString("hex")).toBe("cd".repeat(32));
    expect(parsed.eventId).toBeUndefined();
  });

  it("rejects requests without any selector", () => {
    expect(() => parseDepositStatusLookup({})).toThrow(
      "GET /deposit-status requires `eventId` or `cardanoTxHash`.",
    );
  });

  it("rejects malformed event ids", () => {
    expect(() =>
      parseDepositStatusLookup({
        eventId: "deadbeef",
      }),
    ).toThrow("Invalid eventId: failed to decode OutputReference CBOR");
  });

  it("encodes the agreed stable HTTP response shape", () => {
    const entry = makeDepositEntry({
      [DepositsDB.Columns.PROJECTED_HEADER_HASH]: Buffer.from(
        "55".repeat(28),
        "hex",
      ),
      [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
    });

    expect(encodeDepositStatus(entry)).toStrictEqual({
      eventId: entry[DepositsDB.Columns.ID].toString("hex"),
      eventInfo: entry[DepositsDB.Columns.INFO].toString("hex"),
      inclusionTime: "2026-04-13T17:28:10.999Z",
      cardanoTxHash:
        entry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].toString("hex"),
      ledgerTxId: entry[DepositsDB.Columns.LEDGER_TX_ID].toString("hex"),
      ledgerOutput: entry[DepositsDB.Columns.LEDGER_OUTPUT].toString("hex"),
      ledgerAddress: entry[DepositsDB.Columns.LEDGER_ADDRESS],
      projectedHeaderHash:
        entry[DepositsDB.Columns.PROJECTED_HEADER_HASH]?.toString("hex") ??
        null,
      status: DepositsDB.Status.Projected,
    });
  });
});
