import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  BuilderInvariantError,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  MIDGARD_POSIX_TIME_NONE,
  outRefToCbor,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
} from "../src/index.js";

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";

const fakeProvider: MidgardProvider = {
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 44n, minFeeB: 155381n },
    submissionLimits: { maxSubmitTxCborBytes: 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 44n,
    minFeeB: 155381n,
    networkId: 255n,
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
    endpoint: "memory://test",
    protocolInfoSource: "node",
  }),
};

const zeroFeeProvider: MidgardProvider = {
  ...fakeProvider,
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
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
};

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const scriptAddress = (scriptHash: string): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
  )
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

describe("TxBuilder finalization", () => {
  it("completes a simple balanced unsigned native transaction", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([
        makeUtxo(makeOutRef(0x22, 1), { lovelace: 2_000_000n }),
        makeUtxo(makeOutRef(0x11, 0), { lovelace: 1_000_000n }),
      ])
      .addSigner("bb".repeat(28))
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .pay.ToProtectedAddress(address, { lovelace: 2_000_000n })
      .complete();

    const decoded = decodeMidgardNativeTxFull(completed.txCbor);
    expect(completed.txId).toEqual(computeMidgardNativeTxIdFromFull(decoded));
    expect(completed.txId).toEqual(decoded.compact.transactionBodyHash);
    expect(completed.toHash()).toBe(completed.txIdHex);
    expect(completed.toCBOR()).toBe(completed.txHex);
    expect(completed.toJSON()).toMatchObject({
      txId: completed.txIdHex,
      txCbor: completed.txHex,
    });
    expect(completed.metadata).toMatchObject({
      fee: 0n,
      inputCount: 2,
      outputCount: 2,
      requiredSignerCount: 1,
    });
  });

  it("applies setMinFee as an auditable fee floor", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .setMinFee(10n)
      .collectFrom([makeUtxo(makeOutRef(0x10), { lovelace: 1_000_010n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    expect(completed.metadata.fee).toBe(10n);
  });

  it("materializes canonical roots, empty buckets, and default sentinels", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(tx.body.spendInputsRoot).toEqual(
      computeHash32(tx.body.spendInputsPreimageCbor),
    );
    expect(tx.body.referenceInputsRoot).toEqual(computeHash32(EMPTY_CBOR_LIST));
    expect(tx.body.requiredObserversRoot).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
    expect(tx.body.mintRoot).toEqual(computeHash32(EMPTY_CBOR_LIST));
    expect(tx.body.scriptIntegrityHash).toEqual(EMPTY_NULL_ROOT);
    expect(tx.body.auxiliaryDataHash).toEqual(EMPTY_NULL_ROOT);
    expect(tx.witnessSet.addrTxWitsRoot).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
    expect(tx.witnessSet.scriptTxWitsRoot).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
    expect(tx.witnessSet.redeemerTxWitsRoot).toEqual(
      computeHash32(EMPTY_CBOR_LIST),
    );
    expect(tx.body.validityIntervalStart).toBe(MIDGARD_POSIX_TIME_NONE);
    expect(tx.body.validityIntervalEnd).toBe(MIDGARD_POSIX_TIME_NONE);
  });

  it("sorts spend inputs while preserving authored output order", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([
        makeUtxo(makeOutRef(0x22, 1), { lovelace: 2_000_000n }),
        makeUtxo(makeOutRef(0x11, 0), { lovelace: 1_000_000n }),
      ])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .pay.ToProtectedAddress(address, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    const spendInputs = decodeMidgardNativeByteListPreimage(
      tx.body.spendInputsPreimageCbor,
    ).map((bytes) => CML.TransactionInput.from_cbor_bytes(bytes));
    expect(
      spendInputs.map(
        (input) =>
          `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      ),
    ).toEqual([`${"11".repeat(32)}#0`, `${"22".repeat(32)}#1`]);

    const outputs = decodeMidgardNativeByteListPreimage(
      tx.body.outputsPreimageCbor,
    );
    expect(outputs).toHaveLength(2);
    expect(
      outputs[0]?.equals(
        encodeMidgardTxOutput(address, { lovelace: 1_000_000n }),
      ),
    ).toBe(true);
    expect(
      outputs[1]?.equals(
        encodeMidgardTxOutput(
          address,
          { lovelace: 2_000_000n },
          { kind: "protected" },
        ),
      ),
    ).toBe(true);
  });

  it("rejects unbalanced explicit completion before producing bytes", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);

    await expect(
      midgard
        .newTx()
        .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 2_000_000n })])
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("runs shared Phase A local validation during completion", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const balanced = midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n });

    const completed = await balanced.complete({ localValidation: "phase-a" });

    expect(completed.metadata.localValidation).toMatchObject({
      phase: "phase-a",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
  });

  it("runs explicit local preflight from completed transactions without marking final acceptance", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    const report = await completed.validate("phase-a");
    const programReport = await Effect.runPromise(
      completed.validateProgram("phase-a"),
    );
    const safeReport = await completed.validateSafe("phase-a");

    expect(report).toMatchObject({
      phase: "phase-a",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
    expect(programReport.acceptedTxIds).toEqual([completed.txIdHex]);
    expect(safeReport).toMatchObject({
      ok: true,
      value: { phase: "phase-a", acceptedTxIds: [completed.txIdHex] },
    });
    expect(completed.metadata.localValidation).toBeUndefined();
  });

  it("requires explicit pre-state for Phase B local validation", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete({ localValidation: "phase-b" }),
    ).rejects.toThrow(
      'complete({ localValidation: "phase-b" }) requires localPreState',
    );
  });

  it("requires explicit pre-state for Phase B local preflight", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    await expect(completed.validate("phase-b")).rejects.toThrow(
      'validate("phase-b") requires localPreState',
    );
    await expect(completed.validateSafe("phase-b")).resolves.toMatchObject({
      ok: false,
      error: { name: "BuilderInvariantError" },
    });
  });

  it("runs shared Phase B local validation against explicit pre-state", async () => {
    const children = CML.NativeScriptList.new();
    const native = CML.NativeScript.new_script_all(children);
    const nativeAddress = scriptAddress(native.hash().to_hex());
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = decodeMidgardUtxo({
      outRef: makeOutRef(0x11),
      outRefCbor: outRefToCbor(makeOutRef(0x11)),
      outputCbor: encodeMidgardTxOutput(nativeAddress, {
        lovelace: 1_000_000n,
      }),
    });
    const inputOutRefCbor = Buffer.from(input.cbor!.outRef!);
    const inputOutputCbor = Buffer.from(input.cbor!.output!);
    const completed = await midgard
      .newTx()
      .attach.NativeScript(native)
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete({
        localValidation: "phase-b",
        localPreState: new Map([
          [inputOutRefCbor.toString("hex"), inputOutputCbor],
        ]),
      });

    expect(completed.metadata.localValidation).toMatchObject({
      phase: "phase-b",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
      statePatch: {
        deletedOutRefs: [inputOutRefCbor.toString("hex")],
      },
    });
  });

  it("runs explicit Phase B local preflight against shared validator pre-state", async () => {
    const children = CML.NativeScriptList.new();
    const native = CML.NativeScript.new_script_all(children);
    const nativeAddress = scriptAddress(native.hash().to_hex());
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = decodeMidgardUtxo({
      outRef: makeOutRef(0x12),
      outRefCbor: outRefToCbor(makeOutRef(0x12)),
      outputCbor: encodeMidgardTxOutput(nativeAddress, {
        lovelace: 1_000_000n,
      }),
    });
    const inputOutRefCbor = Buffer.from(input.cbor!.outRef!);
    const inputOutputCbor = Buffer.from(input.cbor!.output!);
    const completed = await midgard
      .newTx()
      .attach.NativeScript(native)
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    const report = await completed.validate("phase-b", {
      localPreState: new Map([[inputOutRefCbor.toString("hex"), inputOutputCbor]]),
    });

    expect(report).toMatchObject({
      phase: "phase-b",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
      preStateSource: "explicit",
      preStateAuthoritative: false,
      statePatch: {
        deletedOutRefs: [inputOutRefCbor.toString("hex")],
      },
    });
  });

  it("exposes completed transaction buffers as immutable snapshots", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    const txId = completed.txId;
    txId[0] ^= 0xff;
    expect(completed.txId.toString("hex")).toBe(completed.txIdHex);

    const txCbor = completed.txCbor;
    txCbor[0] ^= 0xff;
    expect(completed.txCbor.toString("hex")).toBe(completed.txHex);

    const tx = completed.tx;
    tx.body.outputsPreimageCbor[0] ^= 0xff;
    expect(completed.tx.body.outputsPreimageCbor.toString("hex")).toBe(
      decodeMidgardNativeTxFull(
        Buffer.from(completed.txHex, "hex"),
      ).body.outputsPreimageCbor.toString("hex"),
    );
  });
});
