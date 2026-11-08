// Per-tx cost of the submit check every `/submit` and `/peer/txs` tx passes
// before durable admission (`checkSubmittedTransaction`), on the shapes the
// ingress design sizes the check against: 1 in / 2 out, 4/4 and 16/16, each
// input with its own vkey witness.
//
//   pnpm --dir demo --filter midgard-node run bench:submit-check
//
// `check` is the check as it runs now: one decode, the consensus check on the
// decoded tx, Phase A with the public fee rule (1 lovelace per byte plus
// 2,000) and the expiry check, and the facts admission needs. `twoDecodes` is
// the earlier
// pipeline, rebuilt from the same parts: the submitted-tx decode, then the
// consensus check decoding the bytes again.
//
// To time one shape or one variant alone:
//   SUBMIT_CHECK_BENCH_SHAPE=16/16 SUBMIT_CHECK_BENCH_VARIANT=check
// SUBMIT_CHECK_BENCH_TXS sets the distinct txs per shape (default 256) and
// SUBMIT_CHECK_BENCH_ROUNDS the timed passes over them (default 30).
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { describe, expect, it } from "vitest";

import { buildLoadTxShape } from "../../scripts/submit-ingress-load.tx.mjs";
import {
  checkSubmittedTransaction,
  type SubmittedTxRules,
} from "../../src/commands/http/transaction-admission.js";
import {
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "../../src/commands/listen-utils.js";

const TXS_PER_SHAPE = Number(process.env.SUBMIT_CHECK_BENCH_TXS ?? "256");
const ROUNDS = Number(process.env.SUBMIT_CHECK_BENCH_ROUNDS ?? "30");
const MAX_TX_BYTES = 32_768;
// The public testnet's rules, which `buildLoadTxShape` pays.
const RULES: SubmittedTxRules = {
  expectedNetworkId: 0n,
  minFeeA: 1n,
  minFeeB: 2_000n,
  slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
};

const SHAPES = [
  { inputCount: 1, outputCount: 2 },
  { inputCount: 4, outputCount: 4 },
  { inputCount: 16, outputCount: 16 },
].filter(
  (shape) =>
    process.env.SUBMIT_CHECK_BENCH_SHAPE === undefined ||
    process.env.SUBMIT_CHECK_BENCH_SHAPE ===
      `${shape.inputCount.toString()}/${shape.outputCount.toString()}`,
);

const twoDecodes = (txCbor: Uint8Array): boolean => {
  const validation = validateSubmitTxCanonicalCbor(
    txCbor,
    Math.min(MAX_TX_BYTES, MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes),
  );
  if (!validation.ok) return false;
  const normalized = normalizeSubmitTxCanonicalCborToNative(
    validation.txCanonicalCbor,
  );
  return (
    normalized.ok &&
    validateMidgardConsensusTxCbor(normalized.txCanonicalCbor) === null
  );
};

const VARIANTS: Readonly<Record<string, (txCbor: Uint8Array) => boolean>> = {
  check: (txCbor) =>
    checkSubmittedTransaction(txCbor, MAX_TX_BYTES, RULES, Date.now()).ok,
  twoDecodes,
};

const median = (values: readonly number[]): number => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.floor(sorted.length / 2)]!;
};

describe("submit check cost", () => {
  it("measures the submit check per tx", async () => {
    await ledgerEd25519SignatureReady();
    const variants = Object.entries(VARIANTS).filter(
      ([name]) =>
        process.env.SUBMIT_CHECK_BENCH_VARIANT === undefined ||
        process.env.SUBMIT_CHECK_BENCH_VARIANT === name,
    );
    expect(variants.length * SHAPES.length).toBeGreaterThan(0);
    const rows: Record<string, number | string>[] = [];
    for (const shape of SHAPES) {
      const txs = Array.from(
        { length: TXS_PER_SHAPE },
        (_, nonce) => buildLoadTxShape({ ...shape, nonce }).txCbor,
      );
      for (const [variant, run] of variants) {
        for (const tx of txs) expect(run(tx)).toBe(true);
        const perTxMs: number[] = [];
        for (let round = 0; round < ROUNDS; round += 1) {
          const startedAt = process.hrtime.bigint();
          for (const tx of txs) run(tx);
          perTxMs.push(
            Number(process.hrtime.bigint() - startedAt) / 1e6 / txs.length,
          );
        }
        rows.push({
          shape: `${shape.inputCount.toString()}/${shape.outputCount.toString()}`,
          variant,
          txBytes: txs[0]!.length,
          medianMsPerTx: Number(median(perTxMs).toFixed(4)),
          minMsPerTx: Number(Math.min(...perTxMs).toFixed(4)),
        });
      }
    }
    console.table(rows);
    console.log(
      `SUBMIT_CHECK_COST ${JSON.stringify({ txsPerShape: TXS_PER_SHAPE, rounds: ROUNDS, rows })}`,
    );
  });
});
