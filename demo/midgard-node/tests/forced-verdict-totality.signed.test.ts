import { MIDGARD_POSIX_TIME_NONE } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { FORCED_REJECTION_NO_ARM_CODES } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { classifyForcedTransactions } from "../src/mpf/index.js";
import {
  forcedEntry,
  makeOutput,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
} from "./forced-transactions.make-signed-effectful-transaction.js";

/**
 * Forced verdict totality past the signature checks. Each order is signed by
 * the key that owns its live input, so it passes the witness and signer arms
 * and reaches the fee and validity-interval arms with mutated scalars. Every
 * mint-admissible order must still get a leaf: never a hold, and never a
 * rejection code without a reason arm.
 */

const SAMPLES = 160;
const BLOCK_TIME = new Date("2026-07-23T12:01:00.000Z");
const BLOCK_TIME_MS = BigInt(BLOCK_TIME.getTime());
const MIN_FEE_A = 44n;
const MIN_FEE_B = 155_381n;
const INPUT_LOVELACE = 10_000_000n;
const SPEND = outputReferenceFromHash(Buffer.alloc(32, 0x5a));

const random = (() => {
  let state = 0x2b2f_00d5;
  return () => {
    state = (Math.imul(state, 1_103_515_245) + 12_345) >>> 0;
    return state / 0x1_0000_0000;
  };
})();
const pick = <T>(choices: readonly T[]): T =>
  choices[Math.floor(random() * choices.length)]!;
const scalar = (max: bigint) => BigInt(Math.floor(random() * Number(max)));

const FEES = [0n, 1n, MIN_FEE_B - 1n, 300_000n, 2_000_000n] as const;
const STARTS = [
  MIDGARD_POSIX_TIME_NONE,
  0n,
  BLOCK_TIME_MS - 60_000n,
  BLOCK_TIME_MS,
  BLOCK_TIME_MS + 60_000n,
  BLOCK_TIME_MS * 2n,
] as const;
const ENDS = [
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_POSIX_TIME_NONE,
  BLOCK_TIME_MS + 60_000n,
] as const;

/** One signed order whose fee and validity scalars are drawn per sample. */
const signedOrder = () => {
  const fee = random() < 0.5 ? pick(FEES) : scalar(3_000_000n);
  return makeSignedEffectfulTransaction(
    SPEND,
    makeOutput(INPUT_LOVELACE - fee),
    {
      fee,
      validityIntervalStart:
        random() < 0.75
          ? pick(STARTS)
          : scalar(BigInt(Number.MAX_SAFE_INTEGER)),
      validityIntervalEnd: pick(ENDS),
    },
  );
};

const leafOf = (transaction: ReturnType<typeof signedOrder>, label: number) =>
  Effect.gen(function* () {
    const entry = yield* Effect.promise(() =>
      forcedEntry({ label, transaction }),
    );
    const [classified] = yield* classifyForcedTransactions({
      entries: [entry],
      initialState: new Map([
        [SPEND.toString("hex"), makeOutput(INPUT_LOVELACE)],
      ]),
      blockTime: BLOCK_TIME,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      validation: {
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        expectedNetworkId: 0n,
        minFeeA: MIN_FEE_A,
        minFeeB: MIN_FEE_B,
        bucketConcurrency: 1,
        slotForUnixTime: () => 100n,
      },
    });
    const leaf = Data.from(
      classified!.entry[
        ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
      ].toString("hex"),
      SDK.ForcedInclusionTxV1,
    );
    const reason =
      leaf.verdict === "ForcedTxValid"
        ? "ForcedTxValid"
        : typeof leaf.verdict.ForcedTxInvalid.reason === "string"
          ? leaf.verdict.ForcedTxInvalid.reason
          : Object.keys(leaf.verdict.ForcedTxInvalid.reason)[0]!;
    return { reason, rejectionCode: classified!.rejectionCode };
  });

describe("forced verdict totality over signed orders", () => {
  it("gives every admitted signed order a reason arm past the signature checks", async () => {
    const residue: string[] = [];
    const reasons = new Map<string, number>();
    let checked = 0;
    for (let sample = 0; sample < SAMPLES; sample += 1) {
      const order = signedOrder();
      if (
        screenMidgardForcedTxAdmission({
          body: order.transaction.body,
          witnessSet: order.transaction.witnessSet,
        }) !== null
      )
        continue;
      checked += 1;
      const exit = await Effect.runPromiseExit(leafOf(order, sample % 60));
      if (!Exit.isSuccess(exit)) {
        residue.push(
          `${order.canonicalCbor.toString("hex")}: ${Cause.pretty(exit.cause).split("\n")[0]}`,
        );
        continue;
      }
      const { reason, rejectionCode } = exit.value;
      if (
        rejectionCode !== null &&
        (FORCED_REJECTION_NO_ARM_CODES as readonly string[]).includes(
          rejectionCode,
        )
      )
        residue.push(
          `${order.canonicalCbor.toString("hex")}: ${rejectionCode}`,
        );
      reasons.set(reason, (reasons.get(reason) ?? 0) + 1);
    }
    expect(residue).toEqual([]);
    expect(checked).toBeGreaterThan(SAMPLES / 2);
    // Real keys: no order stops at a witness or signer arm, and the fee and
    // validity arms behind them are reached, as is a valid order.
    expect(reasons.get("AddressWitnessSignatureInvalid")).toBeUndefined();
    expect(reasons.get("SpendInputSignerMissing")).toBeUndefined();
    expect(reasons.get("ForcedTxValid") ?? 0).toBeGreaterThan(0);
    expect(reasons.get("FeeBelowMinimum") ?? 0).toBeGreaterThan(0);
    expect(
      reasons.get("ValidityIntervalExcludesBlockSlot") ?? 0,
    ).toBeGreaterThan(0);
  }, 600_000);
});
