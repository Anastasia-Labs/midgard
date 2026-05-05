import { readFileSync, readdirSync, statSync } from "node:fs";
import { join, resolve } from "node:path";
import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import {
  BuilderInvariantError,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  outRefToCbor,
  type Address,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
} from "../src/index.js";

const productionSources = (directory: string): readonly string[] =>
  readdirSync(directory).flatMap((entry) => {
    const path = join(directory, entry);
    const stats = statSync(path);
    if (stats.isDirectory()) {
      return productionSources(path);
    }
    return path.endsWith(".ts") ? [path] : [];
  });

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const makeUtxo = (
  ref: OutRef,
  owner: Address,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(owner, assets),
  });

const makeProvider = (
  utxos: readonly MidgardUtxo[] = [],
  counters?: { protocolParameters: number; utxos: number },
): MidgardProvider => ({
  getUtxos: async () => {
    if (counters !== undefined) {
      counters.utxos += 1;
    }
    return utxos;
  },
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: { maxSubmitTxCborBytes: 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => {
    if (counters !== undefined) {
      counters.protocolParameters += 1;
    }
    return {
      minFeeA: 0n,
      minFeeB: 0n,
      networkId: 0n,
      currentSlot: 0n,
      strictnessProfile: "phase1_midgard",
    };
  },
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({ endpoint: "memory://safe", protocolInfoSource: "node" }),
});

describe("safe and Effect builder APIs", () => {
  it("returns completed transactions and structured errors from completeSafe", async () => {
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x11), address, { lovelace: 1_000_000n });

    const success = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .completeSafe({ fee: 0n });
    expect(success.ok).toBe(true);
    if (success.ok) {
      expect(success.value.txIdHex).toMatch(/^[0-9a-f]{64}$/);
    }

    const failure = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1n })
      .completeSafe({ fee: 0n });
    expect(failure.ok).toBe(false);
    if (!failure.ok) {
      expect(failure.error).toBeInstanceOf(BuilderInvariantError);
      expect(failure.error.toJSON()).toMatchObject({
        code: "BUILDER_INVARIANT",
      });
    }
  });

  it("keeps completeProgram lazy and fails through the Effect error channel", async () => {
    const counters = { protocolParameters: 0, utxos: 0 };
    const input = makeUtxo(makeOutRef(0x22), address, { lovelace: 2_000_000n });
    const midgard = await LucidMidgard.new(makeProvider([input], counters), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromAddress(address);
    counters.protocolParameters = 0;
    counters.utxos = 0;

    const program = midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .completeProgram();
    expect(counters).toEqual({ protocolParameters: 0, utxos: 0 });

    const completed = await Effect.runPromise(program);
    expect(completed.metadata.balanced).toBe(true);
    expect(counters.protocolParameters).toBeGreaterThan(0);
    expect(counters.utxos).toBeGreaterThan(0);

    const failingMidgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const failure = await Effect.runPromise(
      Effect.either(
        failingMidgard
          .newTx()
          .pay.ToAddress(address, { lovelace: 1n })
          .completeProgram({ fee: 0n }),
      ),
    );
    expect(failure._tag).toBe("Left");
    if (failure._tag === "Left") {
      expect(failure.left).toBeInstanceOf(BuilderInvariantError);
    }
  });

  it("wraps local chaining with safe and Effect variants", async () => {
    const input = makeUtxo(makeOutRef(0x33), address, { lovelace: 2_000_000n });
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromAddress(address, [input]);

    const safe = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .chainSafe({ fee: 0n });
    expect(safe.ok).toBe(true);
    if (safe.ok) {
      const [newWalletUtxos, derivedOutputs, completed] = safe.value;
      expect(newWalletUtxos.length).toBeGreaterThan(0);
      expect(derivedOutputs.length).toBeGreaterThan(0);
      expect(completed.txIdHex).toMatch(/^[0-9a-f]{64}$/);
    }

    const chained = await Effect.runPromise(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .chainProgram({ fee: 0n }),
    );
    expect(chained[2].txIdHex).toMatch(/^[0-9a-f]{64}$/);
  });

  it("does not use unsafe Effect escape hatches in production source", () => {
    const srcDir = resolve(import.meta.dirname, "../src");
    const forbidden = /Effect\.orDie|unsafeRun|catchAllDefect|catchAllCause|catchCause/;
    const offenders = productionSources(srcDir).filter((path) =>
      forbidden.test(readFileSync(path, "utf8")),
    );
    expect(offenders).toEqual([]);
  });
});
