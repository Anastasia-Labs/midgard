import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either, Logger, LogLevel } from "effect";
import { describe, expect, it } from "vitest";

import { DatabaseError } from "../src/database/utils/common.js";
import { assertPreSubmitDaPayloadSize } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";

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

// The block's size is set through its program material alone.
const assertSize = (
  cekProgramMaterial: readonly SDK.DaPayloadEntry[],
  envelopeMode: DaPayloadEmissionMode,
) =>
  Effect.gen(function* () {
    const logs: Logged[] = [];
    const result = yield* Effect.either(
      assertPreSubmitDaPayloadSize({
        headerHash: "33".repeat(28),
        header,
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

const innerBytesOf = async (
  material: readonly SDK.DaPayloadEntry[],
  envelopeMode: DaPayloadEmissionMode,
): Promise<number> => {
  const { result } = await Effect.runPromise(
    assertSize(material, envelopeMode),
  );
  if (Either.isRight(result)) return result.right;
  const inner = /inner_bytes=(\d+)/.exec(String(result.left.cause))?.[1];
  if (inner === undefined) throw new Error("block was refused unmeasured");
  return Number(inner);
};

const materialOf = (
  coarseBytes: number,
  fineBytes: number,
): SDK.DaPayloadEntry[] => [
  ["44".repeat(32), "55".repeat(coarseBytes)],
  ["66".repeat(32), "77".repeat(fineBytes)],
];

/**
 * Program material whose block projects to exactly `innerBytes`: a coarse
 * entry found by bisection, then a short entry (24 to 64 bytes, one encoded
 * byte per value byte) that closes the remaining gap.
 */
const materialForInnerBytes = async (
  innerBytes: number,
  envelopeMode: DaPayloadEmissionMode,
): Promise<SDK.DaPayloadEntry[]> => {
  let low = 0;
  let high = innerBytes;
  while (low < high) {
    const mid = Math.ceil((low + high) / 2);
    if ((await innerBytesOf(materialOf(mid, 24), envelopeMode)) <= innerBytes)
      low = mid;
    else high = mid - 1;
  }
  const gap =
    innerBytes - (await innerBytesOf(materialOf(low, 24), envelopeMode));
  if (gap < 0 || gap > 40) throw new Error(`unreachable size gap ${gap}`);
  const material = materialOf(low, 24 + gap);
  if ((await innerBytesOf(material, envelopeMode)) !== innerBytes)
    throw new Error("material does not reach the target size");
  return material;
};

const pressureWarnings = (logs: readonly Logged[]) =>
  logs.filter((log) => log.text.includes("da_payload_frame_pressure=high"));

const field = (text: string, name: string): string | undefined =>
  new RegExp(`(?:^|[ ,])${name}=([^ ,]+)`).exec(text)?.[1];

const MODES = ["identity", "zstd"] as const;

describe("pre-submit DA payload frame pressure", () => {
  it.each(
    MODES.flatMap((mode) => [50, 75, 90].map((stage) => ({ mode, stage }))),
  )(
    "reports measured pressure stage $stage at the usable $mode boundary",
    async ({ mode, stage }) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const boundary = Math.ceil((limit * stage) / 100);
      for (const target of [boundary - 1, boundary]) {
        const material = await materialForInnerBytes(target, mode);
        const { result, logs } = await Effect.runPromise(
          assertSize(material, mode),
        );
        expect(Either.getOrUndefined(result)).toBe(target);
        const expected =
          target === boundary
            ? stage
            : stage === 90
              ? 75
              : stage === 75
                ? 50
                : 0;
        const warnings = pressureWarnings(logs);
        if (expected === 0) expect(warnings).toEqual([]);
        else
          expect(
            field(warnings[0]?.text ?? "", "da_payload_pressure_stage_percent"),
          ).toBe(expected.toString());
      }
    },
  );

  it.each(MODES)("stays quiet well below the frame (%s)", async (mode) => {
    const limit = maxDaPayloadInnerBytes(mode);
    const target = Math.floor(limit * 0.2);
    const material = await materialForInnerBytes(target, mode);
    const { result, logs } = await Effect.runPromise(
      assertSize(material, mode),
    );
    expect(Either.getOrUndefined(result)).toBe(target);
    expect(pressureWarnings(logs)).toEqual([]);
  });

  // The identity limit is even, so its half is exact and this pins `>=`.
  it.each(MODES)(
    "starts warning exactly at the pressure ratio (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const atRatio = Math.ceil(limit * 0.5);
      for (const [target, warnings] of [
        [atRatio - 1, 0],
        [atRatio, 1],
      ] as const) {
        const material = await materialForInnerBytes(target, mode);
        const { result, logs } = await Effect.runPromise(
          assertSize(material, mode),
        );
        expect(Either.getOrUndefined(result)).toBe(target);
        expect(pressureWarnings(logs)).toHaveLength(warnings);
      }
    },
  );

  it.each(MODES)(
    "reports the approaching frame limit with its headroom, and still commits (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const target = Math.ceil(limit * 0.6);
      const material = await materialForInnerBytes(target, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(material, mode),
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
    },
  );

  it.each(MODES)(
    "admits a block that exactly fills the frame (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const material = await materialForInnerBytes(limit, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(material, mode),
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
      const material = await materialForInnerBytes(limit + 1, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(material, mode),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") return;
      expect(result.left).toBeInstanceOf(DatabaseError);
      expect(result.left.message).toBe(
        "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame",
      );
      const cause = String(result.left.cause);
      expect(field(cause, "inner_bytes")).toBe((limit + 1).toString());
      expect(pressureWarnings(logs)).toEqual([]);
    },
  );
});
