/**
 * Every UTxO that enters a builder, whether a caller passes it to
 * `collectFrom`/`readFrom` or the provider returns it as a wallet coin, is
 * refused as `InvalidUtxoError` (code `INVALID_UTXO`) when it is malformed or
 * disagrees with its own output CBOR, instead of crashing with a `TypeError`
 * or surfacing as a generic builder invariant.
 */
import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import {
  type Address,
  BuilderInvariantError,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  InvalidUtxoError,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "../src/index.js";

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

const makeProvider = (utxos: readonly unknown[] = []): MidgardProvider => ({
  getUtxos: async () => utxos as readonly MidgardUtxo[],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://invalid-utxo",
    protocolInfoSource: "node",
  }),
});

const newMidgard = (utxos: readonly unknown[] = []) =>
  LucidMidgard.new(makeProvider(utxos), { network: "Preview", networkId: 0 });

const valid = makeUtxo(makeOutRef(0x11), address, { lovelace: 2_000_000n });
const other = makeUtxo(makeOutRef(0x22), address, { lovelace: 3_000_000n });

const malformed: ReadonlyArray<readonly [string, unknown]> = [
  ["a missing entry", null],
  ["a number", 7],
  ["an entry with no output", { txHash: valid.txHash, outputIndex: 0 }],
  ["a short transaction hash", { ...valid, txHash: "ab" }],
  ["a fractional output index", { ...valid, outputIndex: 0.5 }],
  [
    "an address that disagrees with its output CBOR",
    {
      ...valid,
      output: { ...valid.output, address: other.output.address + "x" },
    },
  ],
  [
    "assets that disagree with its output CBOR",
    { ...valid, output: { ...valid.output, assets: { lovelace: 1n } } },
  ],
  [
    "output CBOR that does not decode",
    { ...valid, cbor: { ...valid.cbor, output: Uint8Array.of(0xff) } },
  ],
];

const thrownBy = (run: () => unknown): unknown => {
  try {
    run();
  } catch (error) {
    return error;
  }
  throw new Error("call unexpectedly succeeded");
};

describe("builders refuse malformed UTxOs as InvalidUtxoError", () => {
  it.each(malformed)("collectFrom refuses %s", async (_label, utxo) => {
    const midgard = await newMidgard();
    const error = thrownBy(() =>
      midgard.newTx().collectFrom([valid, utxo as MidgardUtxo]),
    );
    expect(error).toBeInstanceOf(InvalidUtxoError);
    expect(error).toBeInstanceOf(BuilderInvariantError);
    expect((error as InvalidUtxoError).code).toBe("INVALID_UTXO");
    expect((error as InvalidUtxoError).name).toBe("InvalidUtxoError");
  });

  it.each(malformed)("readFrom refuses %s", async (_label, utxo) => {
    const midgard = await newMidgard();
    const error = thrownBy(() =>
      midgard.newTx().readFrom([utxo as MidgardUtxo]),
    );
    expect(error).toBeInstanceOf(InvalidUtxoError);
    expect((error as InvalidUtxoError).code).toBe("INVALID_UTXO");
  });

  it("refuses a provider wallet view holding a malformed UTxO", async () => {
    const midgard = await newMidgard([valid, { ...other, outputIndex: -1 }]);
    midgard.selectWallet.fromAddress(address);
    const result = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .completeSafe({ fee: 0n });
    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.error).toBeInstanceOf(InvalidUtxoError);
      expect(result.error.code).toBe("INVALID_UTXO");
    }
  });

  it("accepts well-formed UTxOs", async () => {
    const midgard = await newMidgard();
    const tx = midgard
      .newTx()
      .collectFrom([valid, other])
      .readFrom([makeUtxo(makeOutRef(0x33), address, { lovelace: 1n })]);
    expect(tx).toBeDefined();
  });
});
