import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either, Logger, LogLevel } from "effect";
import { describe, expect, it } from "vitest";

import { DatabaseError } from "../src/database/utils/common.js";
import type { UtxoPayloadSizeAggregate } from "../src/mpf/index.js";
import {
  assertPreSubmitDaPayloadSize,
  DA_PAYLOAD_FRAME_PRESSURE_RATIO,
} from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";

const header: SDK.Header = {
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "aa".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
};

type Logged = { readonly level: LogLevel.LogLevel; readonly text: string };

// The UTxO list is sized from the durable aggregate, so a ledger of any size
// costs nothing to project here. Only the aggregate varies between cases.
const assertSize = (
  utxoPayloadAggregate: UtxoPayloadSizeAggregate,
  envelopeMode: DaPayloadEmissionMode,
  cekProgramMaterial: readonly SDK.DaPayloadEntry[] = [],
) =>
  Effect.gen(function* () {
    const logs: Logged[] = [];
    const result = yield* Effect.either(
      assertPreSubmitDaPayloadSize({
        headerHash: "33".repeat(28),
        header,
        utxoPayloadAggregate,
        includedDepositEntries: [],
        includedForcedTransactionEntries: [],
        includedWithdrawalEntries: [],
        processedMempoolTxs: [],
        transitionTraceMembers: [],
        eventToStepMembers: [],
        validationTraceMembers: [],
        cekProgramMaterial,
        envelopeMode,
      }).pipe(
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ logLevel, message }) => {
              logs.push({
                level: logLevel,
                text: (Array.isArray(message) ? message : [message])
                  .map(String)
                  .join(" "),
              });
            }),
          ),
        ),
      ),
    );
    return { result, logs };
  });

const BASELINE_TUPLE_BYTES = 400;

/** Builds the UTxO aggregate whose block projects to exactly `innerBytes`. */
const aggregateForInnerBytes = async (
  innerBytes: number,
  entryCount: number,
  envelopeMode: DaPayloadEmissionMode,
  cekProgramMaterial: readonly SDK.DaPayloadEntry[] = [],
): Promise<UtxoPayloadSizeAggregate> => {
  const { result } = await Effect.runPromise(
    assertSize(
      { entryCount: 1, encodedTupleBytes: BASELINE_TUPLE_BYTES },
      envelopeMode,
      cekProgramMaterial,
    ),
  );
  if (result._tag !== "Right") throw new Error("baseline block was refused");
  // The UTxO list is one indefinite-length list, so the inner size moves byte
  // for byte with the aggregate's tuple bytes.
  return {
    entryCount,
    encodedTupleBytes: innerBytes - (result.right - BASELINE_TUPLE_BYTES),
  };
};

const pressureWarnings = (logs: readonly Logged[]) =>
  logs.filter((log) => log.text.includes("da_payload_frame_pressure=high"));

const field = (text: string, name: string): string | undefined =>
  new RegExp(`(?:^|[ ,])${name}=([^ ,]+)`).exec(text)?.[1];

const MODES = ["identity", "zstd"] as const;

describe("pre-submit DA payload frame pressure", () => {
  it.each(MODES)("stays quiet well below the frame (%s)", async (mode) => {
    const limit = maxDaPayloadInnerBytes(mode);
    const target = Math.floor(limit * 0.2);
    const aggregate = await aggregateForInnerBytes(target, 10_000, mode);
    const { result, logs } = await Effect.runPromise(
      assertSize(aggregate, mode),
    );
    expect(Either.getOrUndefined(result)).toBe(target);
    expect(pressureWarnings(logs)).toEqual([]);
  });

  // The identity limit is even, so its half is exact and this pins `>=`.
  it.each(MODES)(
    "starts warning exactly at the pressure ratio (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const atRatio = Math.ceil(limit * DA_PAYLOAD_FRAME_PRESSURE_RATIO);
      for (const [target, warnings] of [
        [atRatio - 1, 0],
        [atRatio, 1],
      ] as const) {
        const aggregate = await aggregateForInnerBytes(target, 10_000, mode);
        const { result, logs } = await Effect.runPromise(
          assertSize(aggregate, mode),
        );
        expect(Either.getOrUndefined(result)).toBe(target);
        expect(pressureWarnings(logs)).toHaveLength(warnings);
      }
    },
  );

  it.each(MODES)(
    "reports the approaching frame limit with the UTxO headroom, and still commits (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const target = Math.ceil(limit * 0.6);
      const entryCount = 200_000;
      const aggregate = await aggregateForInnerBytes(target, entryCount, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(aggregate, mode),
      );
      expect(Either.getOrUndefined(result)).toBe(target);
      const warnings = pressureWarnings(logs);
      expect(warnings).toHaveLength(1);
      const [warning] = warnings;
      expect(warning?.level).toBe(LogLevel.Warning);
      const text = warning?.text ?? "";
      expect(field(text, "da_payload_frame_utilisation")).toBe(
        (target / limit).toFixed(4),
      );
      expect(field(text, "da_payload_headroom_bytes")).toBe(
        (limit - target).toString(),
      );
      expect(field(text, "utxo_list_bytes")).toBe(
        (aggregate.encodedTupleBytes + 2).toString(),
      );
      expect(field(text, "utxo_entries_until_frame_at_mean_size")).toBe(
        Math.floor(
          (limit - target) / (aggregate.encodedTupleBytes / entryCount),
        ).toString(),
      );
    },
  );

  it.each(MODES)(
    "admits a block that exactly fills the frame (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const aggregate = await aggregateForInnerBytes(limit, 400_000, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(aggregate, mode),
      );
      expect(Either.getOrUndefined(result)).toBe(limit);
      expect(pressureWarnings(logs)).toHaveLength(1);
      expect(
        field(
          pressureWarnings(logs)[0]?.text ?? "",
          "da_payload_headroom_bytes",
        ),
      ).toBe("0");
    },
  );

  it.each(MODES)(
    "still refuses one byte past the frame at the same check (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const aggregate = await aggregateForInnerBytes(limit + 1, 400_000, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(aggregate, mode),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") return;
      expect(result.left).toBeInstanceOf(DatabaseError);
      expect(result.left.message).toBe(
        "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame",
      );
      const cause = String(result.left.cause);
      expect(field(cause, "inner_bytes")).toBe((limit + 1).toString());
      // This block carries no events or traces, so dropping them cannot help.
      expect(
        field(cause, "post_block_ledger_without_events_exceeds_frame"),
      ).toBe("true");
      expect(pressureWarnings(logs)).toEqual([]);
    },
  );

  it.each(MODES)(
    "tells a block whose own content tipped it over from the ledger (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const material: SDK.DaPayloadEntry[] = [
        ["44".repeat(32), "55".repeat(4096)],
      ];
      const aggregate = await aggregateForInnerBytes(
        limit + 1,
        400_000,
        mode,
        material,
      );
      const { result } = await Effect.runPromise(
        assertSize(aggregate, mode, material),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") return;
      const cause = String(result.left.cause);
      expect(field(cause, "inner_bytes")).toBe((limit + 1).toString());
      // The same ledger with no events or traces would still fit.
      expect(
        field(cause, "post_block_ledger_without_events_exceeds_frame"),
      ).toBe("false");
    },
  );

  it("names a UTxO list that by itself exceeds the frame", async () => {
    const limit = maxDaPayloadInnerBytes("identity");
    const aggregate: UtxoPayloadSizeAggregate = {
      entryCount: 500_000,
      encodedTupleBytes: limit,
    };
    const { result } = await Effect.runPromise(
      assertSize(aggregate, "identity"),
    );
    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") return;
    const cause = String(result.left.cause);
    expect(field(cause, "utxo_list_bytes")).toBe((limit + 2).toString());
    expect(field(cause, "post_block_ledger_without_events_exceeds_frame")).toBe(
      "true",
    );
  });
});
