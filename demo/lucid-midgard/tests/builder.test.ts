import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  outputAddressProtected,
  outRefToCbor,
  BuilderInvariantError,
  type AuthoredOutput,
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

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const makeUtxo = (ref: OutRef): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, { lovelace: 3_000_000n }),
  });

describe("LucidMidgard builder fluent API", () => {
  it("records fluent builder state deterministically", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x11));
    const tx = midgard
      .newTx()
      .collectFrom([input])
      .readFrom([makeUtxo(makeOutRef(0x22))])
      .addSigner("aa".repeat(28))
      .addSignerKey("bb".repeat(28))
      .setMinFee(123n)
      .validFrom(10n)
      .validTo(20n)
      .pay.ToAddress(address, { lovelace: 1_000_000n });

    expect(tx.debugSnapshot()).toMatchObject({
      spendInputs: [{ txHash: input.txHash, outputIndex: 0 }],
      referenceInputs: [{ txHash: "22".repeat(32), outputIndex: 0 }],
      requiredSigners: ["aa".repeat(28), "bb".repeat(28)],
      minimumFee: 123n,
      validityIntervalStart: 10n,
      validityIntervalEnd: 20n,
      outputs: [
        {
          kind: "ordinary",
          address,
          assets: { lovelace: 1_000_000n },
        },
      ],
    });
    expect(tx.config()).toMatchObject({ network: "Preview", networkId: 0 });
    expect(tx.rawConfig()).toMatchObject({
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    });
  });

  it("keeps builder instances and snapshots immutable by behavior", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const base = midgard.newTx();
    const withOutput = base.pay.ToAddress(address, { lovelace: 1_000_000n });

    expect(base.debugSnapshot().outputs).toHaveLength(0);
    const snapshot = withOutput.debugSnapshot() as unknown as {
      outputs: AuthoredOutput[];
    };
    snapshot.outputs[0] = {
      ...snapshot.outputs[0],
      assets: { lovelace: 99n },
    };

    expect(withOutput.debugSnapshot().outputs[0]?.assets).toEqual({
      lovelace: 1_000_000n,
    });
  });

  it("rejects duplicate spend/reference inputs and overlap", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const input = makeUtxo(makeOutRef(0x11));

    expect(() => midgard.newTx().collectFrom([input, input])).toThrow(
      BuilderInvariantError,
    );
    expect(() => midgard.newTx().readFrom([input, input])).toThrow(
      BuilderInvariantError,
    );
    expect(() =>
      midgard.newTx().collectFrom([input]).readFrom([input]),
    ).toThrow(BuilderInvariantError);
  });

  it("preserves authored output order and protected output intent", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const tx = midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .pay.ToProtectedAddress(address, { lovelace: 2_000_000n });

    const outputs = tx.debugSnapshot().outputs;
    expect(outputs).toMatchObject([
      { kind: "ordinary", address, assets: { lovelace: 1_000_000n } },
      {
        kind: "protected",
        assets: { lovelace: 2_000_000n },
      },
    ]);
    expect(outputAddressProtected(outputs[1]!.address)).toBe(true);
  });

  it("rejects runtime output kind options on explicit pay helpers", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const ordinaryOptions = { kind: "protected" } as never;
    const protectedOptions = { kind: "ordinary" } as never;

    expect(() =>
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n }, ordinaryOptions),
    ).toThrow(BuilderInvariantError);
    expect(() =>
      midgard
        .newTx()
        .pay.ToContract(
          address,
          CML.PlutusData.new_integer(CML.BigInteger.from_str("1")),
          { lovelace: 1_000_000n },
          ordinaryOptions,
        ),
    ).toThrow(BuilderInvariantError);
    expect(() =>
      midgard
        .newTx()
        .pay.ToProtectedAddress(
          address,
          { lovelace: 1_000_000n },
          protectedOptions,
        ),
    ).toThrow(BuilderInvariantError);
  });

  it("rejects inverted validity intervals as soon as both bounds are present", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);

    expect(() => midgard.newTx().validFrom(20n).validTo(10n)).toThrow(
      BuilderInvariantError,
    );
    expect(() => midgard.newTx().validTo(10n).validFrom(20n)).toThrow(
      BuilderInvariantError,
    );
  });

  it("records spend redeemer placeholders without deriving indexes yet", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const tx = midgard.newTx().collectFrom([makeUtxo(makeOutRef(0x11))], {
      data: CML.PlutusData.new_integer(CML.BigInteger.from_str("1")),
      exUnits: { mem: 1n, steps: 2n },
    });

    expect(tx.debugSnapshot().scripts.spendRedeemers).toHaveLength(1);
  });

  it("keeps redeemer data immutable from caller and snapshot mutation", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const originalData = Buffer.from([0x01, 0x02, 0x03]);
    const tx = midgard.newTx().collectFrom([makeUtxo(makeOutRef(0x11))], {
      data: originalData,
      exUnits: { mem: 1n, steps: 2n },
    });

    originalData[0] = 0xff;
    const firstSnapshot = tx.debugSnapshot();
    const snapshotData = firstSnapshot.scripts.spendRedeemers[0]?.redeemer
      ?.data as Uint8Array;
    expect(Buffer.from(snapshotData).toString("hex")).toBe("010203");

    snapshotData[1] = 0xee;
    const secondSnapshotData = tx.debugSnapshot().scripts.spendRedeemers[0]
      ?.redeemer?.data as Uint8Array;
    expect(Buffer.from(secondSnapshotData).toString("hex")).toBe("010203");
  });

  it("keeps output datum and script reference bytes immutable", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const datum = Buffer.from([0x41, 0x00]);
    const scriptRef = Buffer.from([0x82, 0x00]);
    const tx = midgard.newTx().pay.ToAddress(
      address,
      { lovelace: 1_000_000n },
      {
        datum,
        scriptRef,
      },
    );

    datum[1] = 0xff;
    scriptRef[1] = 0xff;
    const firstSnapshot = tx.debugSnapshot();
    const snapshotDatum = firstSnapshot.outputs[0]?.datum;
    const snapshotDatumData =
      snapshotDatum?.kind === "inline"
        ? (snapshotDatum.data as Uint8Array)
        : undefined;
    const snapshotScriptRef = firstSnapshot.outputs[0]?.scriptRef as Uint8Array;
    expect(Buffer.from(snapshotDatumData ?? []).toString("hex")).toBe("4100");
    expect(Buffer.from(snapshotScriptRef).toString("hex")).toBe("8200");

    if (snapshotDatumData !== undefined) {
      snapshotDatumData[1] = 0xee;
    }
    snapshotScriptRef[1] = 0xee;
    const secondSnapshot = tx.debugSnapshot();
    const secondDatum = secondSnapshot.outputs[0]?.datum;
    const secondDatumData =
      secondDatum?.kind === "inline"
        ? (secondDatum.data as Uint8Array)
        : undefined;
    expect(Buffer.from(secondDatumData ?? []).toString("hex")).toBe("4100");
    expect(
      Buffer.from(secondSnapshot.outputs[0]?.scriptRef as Uint8Array).toString(
        "hex",
      ),
    ).toBe("8200");
  });
});
