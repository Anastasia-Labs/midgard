import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either, Logger, LogLevel } from "effect";
import { describe, expect, it } from "vitest";

import * as DepositsDB from "../src/database/deposits.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { NodeConfig } from "../src/services/config.make-config.js";
import { assertPreSubmitDaPayloadSize } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

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

// Synthetic deposit payload bytes isolate frame sizing from event validation.
const assertSize = (
  deposits: readonly SDK.DaPayloadEntry[],
  envelopeMode: DaPayloadEmissionMode,
) =>
  Effect.gen(function* () {
    const logs: Logged[] = [];
    const result = yield* Effect.either(
      assertPreSubmitDaPayloadSize({
        headerHash: "33".repeat(28),
        header: {
          ...header,
          depositCount: BigInt(deposits.length),
          totalEventCount: BigInt(deposits.length),
        },
        includedDepositEntries: deposits.map(
          ([key, value]): DepositsDB.Entry => ({
            event_id: Buffer.from(key, "hex"),
            event_info: Buffer.from(value, "hex"),
            inclusion_time: new Date(1),
            deposit_l1_tx_hash: Buffer.alloc(32),
            ledger_tx_id: Buffer.alloc(32),
            ledger_output: Buffer.alloc(0),
            ledger_address: "",
            projected_header_hash: null,
            status: DepositsDB.Status.Awaiting,
          }),
        ),
        includedForcedTransactionEntries: [],
        includedWithdrawalEntries: [],
        processedMempoolTxs: [],
        transitionTraceMembers: [],
        eventToStepMembers: [],
        validationTraceMembers: [],
      }).pipe(
        Effect.provideService(
          NodeConfig,
          makeTestNodeConfig({ MIDGARD_DA_PAYLOAD_ENVELOPE: envelopeMode }),
        ),
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
  deposits: readonly SDK.DaPayloadEntry[],
  envelopeMode: DaPayloadEmissionMode,
): Promise<number> => {
  const { result } = await Effect.runPromise(
    assertSize(deposits, envelopeMode),
  );
  if (Either.isRight(result)) return result.right;
  const inner = /inner_bytes=(\d+)/.exec(String(result.left.cause))?.[1];
  if (inner === undefined) throw new Error("block was refused unmeasured");
  return Number(inner);
};

const depositsOf = (
  coarseBytes: number,
  fineBytes: number,
): SDK.DaPayloadEntry[] => [
  ["44".repeat(32), "55".repeat(coarseBytes)],
  ["66".repeat(32), "77".repeat(fineBytes)],
];

/**
 * The largest coarse entry in `[0, innerBytes]` whose block, beside a 24-byte
 * short entry, projects to at most `innerBytes`. A block's projection grows
 * with its coarse entry, so this is one bisection; it is bracketed first by
 * doubling steps from where the block's fixed bytes put it, which is a few
 * bytes off, rather than from the whole range.
 */
const largestCoarseWithin = async (
  innerBytes: number,
  envelopeMode: DaPayloadEmissionMode,
): Promise<number> => {
  const projected = (coarseBytes: number) =>
    innerBytesOf(depositsOf(coarseBytes, 24), envelopeMode);
  const fits = async (coarseBytes: number) =>
    (await projected(coarseBytes)) <= innerBytes;
  const fixed = await projected(0);
  if (fixed >= innerBytes) return 0;
  // The block's bytes per coarse byte, from one probe.
  const probe = innerBytes - fixed;
  const perByte = ((await projected(probe)) - fixed) / probe;
  // fits(low) holds, and the search stays within [0, innerBytes].
  let low = 0;
  let high = innerBytes;
  const guess = Math.min(
    innerBytes,
    Math.max(1, Math.floor((innerBytes - fixed) / perByte)),
  );
  if (await fits(guess)) {
    low = guess;
    for (let step = 1; low < high; step *= 2) {
      const next = Math.min(high, low + step);
      if (!(await fits(next))) {
        high = next - 1;
        break;
      }
      low = next;
    }
  } else {
    high = guess - 1;
    for (let step = 1; low < high; step *= 2) {
      const next = Math.max(low, high - step + 1);
      if (await fits(next)) {
        low = next;
        break;
      }
      high = next - 1;
    }
  }
  while (low < high) {
    const mid = Math.ceil((low + high) / 2);
    if (await fits(mid)) low = mid;
    else high = mid - 1;
  }
  return low;
};

/**
 * Deposit bytes whose block projects to exactly `innerBytes`: the largest
 * coarse entry within it, then a short entry (24 to 64 bytes, one encoded
 * byte per value byte) that closes the remaining gap. Each size is built
 * once per mode and shared, unchanged, by the cases that use it.
 */
const depositsBySize = new Map<string, Promise<SDK.DaPayloadEntry[]>>();
const depositsForInnerBytes = (
  innerBytes: number,
  envelopeMode: DaPayloadEmissionMode,
): Promise<SDK.DaPayloadEntry[]> => {
  const key = `${envelopeMode}:${innerBytes.toString()}`;
  const built =
    depositsBySize.get(key) ?? buildDeposits(innerBytes, envelopeMode);
  depositsBySize.set(key, built);
  return built;
};

const buildDeposits = async (
  innerBytes: number,
  envelopeMode: DaPayloadEmissionMode,
): Promise<SDK.DaPayloadEntry[]> => {
  const low = await largestCoarseWithin(innerBytes, envelopeMode);
  const gap =
    innerBytes - (await innerBytesOf(depositsOf(low, 24), envelopeMode));
  if (gap < 0 || gap > 40) throw new Error(`unreachable size gap ${gap}`);
  const deposits = depositsOf(low, 24 + gap);
  if ((await innerBytesOf(deposits, envelopeMode)) !== innerBytes)
    throw new Error("deposits does not reach the target size");
  return deposits;
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
        const deposits = await depositsForInnerBytes(target, mode);
        const { result, logs } = await Effect.runPromise(
          assertSize(deposits, mode),
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
    const deposits = await depositsForInnerBytes(target, mode);
    const { result, logs } = await Effect.runPromise(
      assertSize(deposits, mode),
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
        const deposits = await depositsForInnerBytes(target, mode);
        const { result, logs } = await Effect.runPromise(
          assertSize(deposits, mode),
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
      const deposits = await depositsForInnerBytes(target, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(deposits, mode),
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
      const deposits = await depositsForInnerBytes(limit, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(deposits, mode),
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
      const deposits = await depositsForInnerBytes(limit + 1, mode);
      const { result, logs } = await Effect.runPromise(
        assertSize(deposits, mode),
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
