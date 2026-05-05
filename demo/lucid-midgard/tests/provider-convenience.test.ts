import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  BuilderInvariantError,
  decodeMidgardTxOutput,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  MidgardNodeProvider,
  outputAddressProtected,
  outRefToCbor,
  ProviderCapabilityError,
  ProviderError,
  ProviderPayloadError,
  type MidgardProvider,
  type MidgardProtocolInfo,
  type MidgardUtxo,
  type OutRef,
  type TxStatus,
} from "../src/index.js";

const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const otherAddress =
  "addr_test1wq5vz59jgr8e33lgn2ku2nv9y64t6q0dd6mc8gy44jz3ggqg7c2ms";
const unit = `${"ab".repeat(28)}${"cd".repeat(2)}`;

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const protocolInfo: MidgardProtocolInfo = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: 123n,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
  submissionLimits: { maxSubmitTxCborBytes: 32768 },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

const makeUtxo = (
  ref: OutRef,
  utxoAddress = address,
  assets: Readonly<Record<string, bigint>> = { lovelace: 2_000_000n },
  options: Parameters<typeof encodeMidgardTxOutput>[2] = {},
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(utxoAddress, assets, options),
  });

const makeProvider = (
  opts: {
    readonly utxos?: readonly MidgardUtxo[];
    readonly currentSlot?: bigint;
    readonly byOutRefs?: (
      outRefs: readonly OutRef[],
    ) => Promise<readonly MidgardUtxo[]>;
    readonly byUnit?: (unit: string) => Promise<readonly MidgardUtxo[]>;
    readonly status?: (txId: string) => Promise<TxStatus>;
  } = {},
): MidgardProvider => ({
  getUtxos: async (requestedAddress) =>
    (opts.utxos ?? []).filter(
      (utxo) => utxo.output.address === requestedAddress,
    ),
  getUtxoByOutRef: async (outRef) =>
    (opts.utxos ?? []).find(
      (utxo) =>
        utxo.txHash === outRef.txHash &&
        utxo.outputIndex === outRef.outputIndex,
    ),
  ...(opts.byOutRefs === undefined
    ? {}
    : { getUtxosByOutRefs: opts.byOutRefs }),
  ...(opts.byUnit === undefined ? {} : { getUtxosByUnit: opts.byUnit }),
  getProtocolInfo: async () => ({
    ...protocolInfo,
    currentSlot: opts.currentSlot ?? protocolInfo.currentSlot,
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: opts.currentSlot ?? protocolInfo.currentSlot,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => opts.currentSlot ?? protocolInfo.currentSlot,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus:
    opts.status ?? (async (txId) => ({ kind: "accepted" as const, txId })),
  diagnostics: () => ({
    endpoint: "memory://provider-convenience",
    protocolInfoSource: "node",
  }),
});

const jsonResponse = (payload: unknown, status = 200): Response =>
  new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json" },
  });

describe("LucidMidgard provider convenience methods", () => {
  it("reads current slot from Midgard protocol info", async () => {
    const node = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () =>
        jsonResponse({
          ...protocolInfo,
          currentSlot: "987654",
          protocolFeeParameters: { minFeeA: "0", minFeeB: "0" },
        }),
    });
    const midgard = await LucidMidgard.new(node, {
      network: "Preview",
      networkId: 0,
    });

    await expect(midgard.currentSlot()).resolves.toBe(987654n);
  });

  it("queries address UTxOs with validation and canonical ordering", async () => {
    const high = makeUtxo(makeOutRef(0x22));
    const low = makeUtxo(makeOutRef(0x11));
    const midgard = await LucidMidgard.new(
      makeProvider({ utxos: [high, low] }),
      {
        network: "Preview",
        networkId: 0,
      },
    );

    await expect(midgard.utxosAt(address)).resolves.toMatchObject([
      { txHash: "11".repeat(32), outputIndex: 0 },
      { txHash: "22".repeat(32), outputIndex: 0 },
    ]);

    await expect(
      midgard.utxosAt({} as unknown as string),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });

  it("rejects malformed UTxO payloads from convenience queries", async () => {
    const valid = makeUtxo(makeOutRef(0x11));
    const malformed: MidgardUtxo = {
      ...valid,
      cbor: {
        ...valid.cbor,
        output: Buffer.from("00", "hex"),
      },
    };
    const midgard = await LucidMidgard.new(
      makeProvider({ utxos: [malformed] }),
      { network: "Preview", networkId: 0 },
    );

    await expect(midgard.utxosAt(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("derives omitted optional UTxO output metadata from authoritative CBOR", async () => {
    const datum = Buffer.from(
      CML.PlutusData.new_integer(CML.BigInteger.from_str("7")).to_cbor_bytes(),
    ).toString("hex");
    const script = "4d".repeat(8);
    const ref = makeOutRef(0x12);
    const outputCbor = encodeMidgardTxOutput(
      address,
      { lovelace: 2_000_000n },
      {
        kind: "protected",
        datum: { kind: "inline", data: Buffer.from(datum, "hex") },
        scriptRef: { type: "PlutusV3", script },
      },
    );
    const decoded = decodeMidgardTxOutput(outputCbor);
    const cbor = {
      outRef: outRefToCbor(ref),
      output: outputCbor,
    };
    const sparse: MidgardUtxo = {
      ...ref,
      output: {
        address: decoded.address,
        assets: { lovelace: 2_000_000n },
      },
      cbor,
    };
    const midgard = await LucidMidgard.new(makeProvider({ utxos: [sparse] }), {
      network: "Preview",
      networkId: 0,
    });

    const [utxo] = await midgard.utxosAt(decoded.address);
    expect(utxo?.output.datum).toEqual({ kind: "inline", cbor: datum });
    expect(utxo?.output.scriptRef).toEqual({ type: "PlutusV3", script });
    expect(outputAddressProtected(utxo!.output.address)).toBe(true);
  });

  it("preserves explicit MidgardV1 scriptRef metadata when CBOR bytes match", async () => {
    const script = "5e".repeat(8);
    const ref = makeOutRef(0x13);
    const cbor = {
      outRef: outRefToCbor(ref),
      output: encodeMidgardTxOutput({
        address,
        assets: { lovelace: 2_000_000n },
        scriptRef: { type: "MidgardV1", script },
      }),
    };
    const withMetadata: MidgardUtxo = {
      ...ref,
      output: {
        address,
        assets: { lovelace: 2_000_000n },
        scriptRef: { type: "MidgardV1", script },
      },
      cbor,
    };
    const midgard = await LucidMidgard.new(
      makeProvider({ utxos: [withMetadata] }),
      { network: "Preview", networkId: 0 },
    );

    await expect(midgard.utxosAt(address)).resolves.toMatchObject([
      {
        output: {
          scriptRef: { type: "MidgardV1", script },
        },
      },
    ]);
  });

  it("filters address UTxOs by lovelace and multi-asset units", async () => {
    const lovelaceOnly = makeUtxo(makeOutRef(0x11), address, {
      lovelace: 1_000_000n,
    });
    const withUnit = makeUtxo(makeOutRef(0x22), address, {
      lovelace: 2_000_000n,
      [unit]: 1n,
    });
    const midgard = await LucidMidgard.new(
      makeProvider({ utxos: [withUnit, lovelaceOnly] }),
      { network: "Preview", networkId: 0 },
    );

    await expect(
      midgard.utxosAtWithUnit(address, "lovelace"),
    ).resolves.toHaveLength(2);
    await expect(midgard.utxosAtWithUnit(address, unit)).resolves.toMatchObject(
      [{ txHash: "22".repeat(32), outputIndex: 0 }],
    );
    await expect(
      midgard.utxosAtWithUnit(address, "not-a-unit"),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("queries by outref in request order with strict and omit missing modes", async () => {
    const first = makeUtxo(makeOutRef(0x11));
    const second = makeUtxo(makeOutRef(0x22));
    const missing = makeOutRef(0x33);
    const all = [second, first];
    const midgard = await LucidMidgard.new(
      makeProvider({
        byOutRefs: async (requested) => {
          const labels = new Set(
            requested.map(
              (outRef) => `${outRef.txHash}#${outRef.outputIndex.toString()}`,
            ),
          );
          return all.filter((utxo) =>
            labels.has(`${utxo.txHash}#${utxo.outputIndex.toString()}`),
          );
        },
      }),
      { network: "Preview", networkId: 0 },
    );

    await expect(
      midgard.utxosByOutRef([makeOutRef(0x22), makeOutRef(0x11)]),
    ).resolves.toMatchObject([
      { txHash: "22".repeat(32) },
      { txHash: "11".repeat(32) },
    ]);
    await expect(
      midgard.utxosByOutRef([makeOutRef(0x11), makeOutRef(0x11)]),
    ).rejects.toBeInstanceOf(BuilderInvariantError);
    await expect(
      midgard.utxosByOutRef([missing], {
        missing: "typo" as "error",
      }),
    ).rejects.toBeInstanceOf(BuilderInvariantError);
    await expect(
      midgard.utxosByOutRef([makeOutRef(0x11), missing]),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
    await expect(
      midgard.utxosByOutRef([missing, makeOutRef(0x11)], { missing: "omit" }),
    ).resolves.toMatchObject([{ txHash: "11".repeat(32) }]);
  });

  it("wraps non-array provider UTxO convenience results as payload errors", async () => {
    const addressProvider = await LucidMidgard.new(
      {
        ...makeProvider(),
        getUtxos: async () => null as unknown as readonly MidgardUtxo[],
      },
      { network: "Preview", networkId: 0 },
    );
    await expect(addressProvider.utxosAt(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const outRefProvider = await LucidMidgard.new(
      makeProvider({
        byOutRefs: async () => null as unknown as readonly MidgardUtxo[],
      }),
      { network: "Preview", networkId: 0 },
    );
    await expect(
      outRefProvider.utxosByOutRef([makeOutRef(0x11)]),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    const unitProvider = await LucidMidgard.new(
      makeProvider({
        byUnit: async () => null as unknown as readonly MidgardUtxo[],
      }),
      { network: "Preview", networkId: 0 },
    );
    await expect(unitProvider.utxoByUnit(unit)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("gates unit-index lookup on a native provider capability", async () => {
    const hit = makeUtxo(makeOutRef(0x11), address, {
      lovelace: 2_000_000n,
      [unit]: 1n,
    });
    const unsupported = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    await expect(unsupported.utxoByUnit(unit)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );

    const success = await LucidMidgard.new(
      makeProvider({ byUnit: async () => [hit] }),
      { network: "Preview", networkId: 0 },
    );
    await expect(success.utxoByUnit(unit)).resolves.toMatchObject({
      txHash: "11".repeat(32),
    });

    const none = await LucidMidgard.new(
      makeProvider({ byUnit: async () => [] }),
      { network: "Preview", networkId: 0 },
    );
    await expect(none.utxoByUnit(unit)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const multiple = await LucidMidgard.new(
      makeProvider({
        byUnit: async () => [
          hit,
          makeUtxo(makeOutRef(0x22), address, {
            lovelace: 2_000_000n,
            [unit]: 1n,
          }),
        ],
      }),
      { network: "Preview", networkId: 0 },
    );
    await expect(multiple.utxoByUnit(unit)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const wrongUnit = await LucidMidgard.new(
      makeProvider({ byUnit: async () => [makeUtxo(makeOutRef(0x33))] }),
      { network: "Preview", networkId: 0 },
    );
    await expect(wrongUnit.utxoByUnit(unit)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("polls Midgard tx status without hiding rejection, unknown statuses, or aborts", async () => {
    const txId = "aa".repeat(32);
    const accepted = await LucidMidgard.new(
      makeProvider({ status: async () => ({ kind: "accepted", txId }) }),
    );
    await expect(
      accepted.awaitTx(txId, { pollIntervalMs: 1 }),
    ).resolves.toEqual({
      kind: "accepted",
      txId,
    });

    const rejected = await LucidMidgard.new(
      makeProvider({
        status: async () => ({
          kind: "rejected",
          txId,
          code: "E_TEST",
          detail: "bad tx",
        }),
      }),
    );
    await expect(
      rejected.awaitTx(txId, { pollIntervalMs: 1 }),
    ).resolves.toEqual({
      kind: "rejected",
      txId,
      code: "E_TEST",
      detail: "bad tx",
    });

    const unsupported = await LucidMidgard.new(
      makeProvider({
        status: async () => {
          throw new ProviderCapabilityError("/tx-status", "unavailable");
        },
      }),
    );
    await expect(unsupported.awaitTx(txId)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );

    const unknown = await LucidMidgard.new(
      makeProvider({
        status: async () => ({ kind: "mystery", txId }) as unknown as TxStatus,
      }),
    );
    await expect(unknown.txStatus(txId)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const malformed = await LucidMidgard.new(
      makeProvider({
        status: async () => null as unknown as TxStatus,
      }),
    );
    await expect(malformed.txStatus(txId)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(
      malformed.awaitTx(txId, { pollIntervalMs: 1, timeoutMs: 10 }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    const controller = new AbortController();
    controller.abort("stop");
    await expect(
      accepted.awaitTx(txId, { signal: controller.signal }),
    ).rejects.toMatchObject({
      name: "ProviderError",
      detail: "stop",
    } satisfies Partial<ProviderError>);
  });

  it("extracts inline datums and verifies requested datum hashes", async () => {
    const datum = CML.PlutusData.new_integer(CML.BigInteger.from_str("42"));
    const datumHash = CML.hash_plutus_data(datum).to_hex();
    const withDatum = makeUtxo(
      makeOutRef(0x11),
      address,
      { lovelace: 2_000_000n },
      { datum },
    );
    const withoutDatum = makeUtxo(makeOutRef(0x22));
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });

    await expect(midgard.datumOf(withDatum, datumHash)).resolves.toEqual(
      Buffer.from(datum.to_cbor_bytes()),
    );
    await expect(
      midgard.datumOf(withDatum, "00".repeat(32)),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
    await expect(midgard.datumOf(withoutDatum)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });
});
