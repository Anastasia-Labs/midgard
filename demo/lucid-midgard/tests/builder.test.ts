import {
  encodeMidgardAddressText,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  midgardAddressFromText,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile-v1";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type AuthoredOutput,
  BuilderInvariantError,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  ProviderPayloadError,
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
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 44n, minFeeB: 155381n },
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

const makeUtxoAtAddress = (ref: OutRef, outputAddress: string): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(outputAddress, { lovelace: 3_000_000n }),
  });

describe("LucidMidgard builder fluent API", () => {
  it("rejects direct-provider script-language self-attestation", async () => {
    const canonicalInfo = await fakeProvider.getProtocolInfo();
    const falseLanguageSet = [{ name: "PlutusV3", tag: 2 }] as const;
    const cases = [
      {
        supportedScriptLanguages: [],
        codecSupportedScriptLanguages: [],
      },
      {
        supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
        codecSupportedScriptLanguages: [],
      },
      {
        supportedScriptLanguages: falseLanguageSet,
        codecSupportedScriptLanguages: falseLanguageSet,
      },
      {
        supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
        codecSupportedScriptLanguages: [{ name: "MidgardV1", tag: Number.NaN }],
      },
    ];

    for (const languageClaims of cases) {
      const directProvider: MidgardProvider = {
        ...fakeProvider,
        getProtocolInfo: async () =>
          ({
            ...canonicalInfo,
            ...languageClaims,
          }) as unknown as Awaited<
            ReturnType<MidgardProvider["getProtocolInfo"]>
          >,
      };
      await expect(LucidMidgard.new(directProvider)).rejects.toBeInstanceOf(
        ProviderPayloadError,
      );
    }
  });

  it("accepts a bounded lower submit cap from a direct provider", async () => {
    const canonicalInfo = await fakeProvider.getProtocolInfo();
    const maxSubmitTxCborBytes =
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes - 1;
    const directProvider: MidgardProvider = {
      ...fakeProvider,
      getProtocolInfo: async () => ({
        ...canonicalInfo,
        submissionLimits: { maxSubmitTxCborBytes },
      }),
    };

    const midgard = await LucidMidgard.new(directProvider, {
      network: "Preview",
      networkId: 0,
    });

    expect(midgard.newTx().rawConfig().submissionLimits).toEqual({
      maxSubmitTxCborBytes,
    });
  });

  it("records fluent builder state deterministically", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x11));
    const tx = midgard
      .newTx()
      .collectFrom([input])
      .addSigner("aa".repeat(28))
      .addSignerKey("bb".repeat(28))
      .setMinFee(123n)
      .validFrom(10n)
      .validTo(20n)
      .pay.ToAddress(address, { lovelace: 1_000_000n });

    expect(tx.debugSnapshot()).toMatchObject({
      spendInputs: [{ txHash: input.txHash, outputIndex: 0 }],
      referenceInputs: [],
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
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
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
      { kind: "protected", assets: { lovelace: 2_000_000n } },
    ]);
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

  it("retains spend redeemers in canonical V1 builder state", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const tx = midgard.newTx().collectFrom([makeUtxo(makeOutRef(0x11))], {
      data: CML.PlutusData.new_integer(CML.BigInteger.from_str("1")),
      exUnits: { mem: 1n, steps: 2n },
    });
    expect(tx.debugSnapshot().scripts.spendRedeemers).toHaveLength(1);
  });

  it("retains script credentials and protected spend inputs", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const scriptAddressBytes = midgardAddressFromText(address);
    scriptAddressBytes[0] = (scriptAddressBytes[0]! & 0x0f) | 0x10;
    const scriptAddress = encodeMidgardAddressText(scriptAddressBytes);
    const protectedAddress = encodeMidgardAddressText(
      protectMidgardAddress(midgardAddressFromText(address)),
    );

    const tx = midgard
      .newTx()
      .collectFrom([
        makeUtxoAtAddress(makeOutRef(0x12), scriptAddress),
        makeUtxoAtAddress(makeOutRef(0x13), protectedAddress),
      ]);
    expect(tx.debugSnapshot().spendInputs).toHaveLength(2);
  });

  it("clones byte redeemers before retaining caller data", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const originalData = Buffer.from([0x01, 0x02, 0x03]);
    const tx = midgard.newTx().collectFrom([makeUtxo(makeOutRef(0x11))], {
      data: originalData,
      exUnits: { mem: 1n, steps: 2n },
    });
    originalData[0] = 0xff;
    const retained = tx.debugSnapshot().scripts.spendRedeemers[0]?.redeemer
      ?.data as Uint8Array;
    expect(Buffer.from(retained).toString("hex")).toBe("010203");
  });

  it("keeps output datum bytes immutable and retains reference scripts", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const datum = Buffer.from([0x41, 0x00]);
    const tx = midgard.newTx().pay.ToAddress(
      address,
      { lovelace: 1_000_000n },
      {
        datum,
      },
    );

    datum[1] = 0xff;
    const firstSnapshot = tx.debugSnapshot();
    const snapshotDatum = firstSnapshot.outputs[0]?.datum;
    const snapshotDatumData =
      snapshotDatum?.kind === "inline"
        ? (snapshotDatum.data as Uint8Array)
        : undefined;
    expect(Buffer.from(snapshotDatumData ?? []).toString("hex")).toBe("4100");

    if (snapshotDatumData !== undefined) {
      snapshotDatumData[1] = 0xee;
    }
    const secondSnapshot = tx.debugSnapshot();
    const secondDatum = secondSnapshot.outputs[0]?.datum;
    const secondDatumData =
      secondDatum?.kind === "inline"
        ? (secondDatum.data as Uint8Array)
        : undefined;
    expect(Buffer.from(secondDatumData ?? []).toString("hex")).toBe("4100");
    const withReferenceScript = midgard
      .newTx()
      .pay.ToAddress(
        address,
        { lovelace: 1_000_000n },
        { scriptRef: { type: "PlutusV3", script: "0102" } },
      );
    expect(withReferenceScript.debugSnapshot().outputs[0]?.scriptRef).toEqual({
      type: "PlutusV3",
      script: "0102",
    });
  });
});
