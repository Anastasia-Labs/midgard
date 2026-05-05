import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  BuilderInvariantError,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  makeVKeyWitness,
  outRefToCbor,
  ProviderCapabilityError,
  ProviderPayloadError,
  walletFromExternalSigner,
  type MidgardProvider,
  type MidgardProtocolInfo,
  type MidgardUtxo,
  type OutRef,
} from "../src/index.js";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const addressFromKeyHash = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
  networkId = 0,
): string =>
  CML.EnterpriseAddress.new(networkId, CML.Credential.new_pub_key(keyHash))
    .to_address()
    .to_bech32();

const makeWalletFixture = (networkId = 0) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const keyHash = privateKey.to_public().hash();
  const address = addressFromKeyHash(keyHash, networkId);
  const wallet = walletFromExternalSigner({
    address,
    keyHash: keyHash.to_hex(),
    signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  });
  return { address, keyHash: keyHash.to_hex(), wallet };
};

const makeUtxo = (
  ref: OutRef,
  address: string,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

const protocolInfo = (
  opts: {
    readonly network?: string;
    readonly nativeVersion?: number;
    readonly minFeeA?: bigint;
    readonly minFeeB?: bigint;
    readonly currentSlot?: bigint;
  } = {},
): MidgardProtocolInfo => ({
  apiVersion: 1,
  network: opts.network ?? "Preview",
  midgardNativeTxVersion: opts.nativeVersion ?? 1,
  currentSlot: opts.currentSlot ?? 0n,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: opts.minFeeA ?? 0n,
    minFeeB: opts.minFeeB ?? 0n,
  },
  submissionLimits: { maxSubmitTxCborBytes: 32768 },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
});

const makeProvider = (opts: {
  readonly name: string;
  readonly utxos?: readonly MidgardUtxo[];
  readonly info?: MidgardProtocolInfo;
  readonly protocolInfoError?: Error;
  readonly delayParameters?: Promise<void>;
  readonly onGetUtxos?: () => void;
  readonly diagnostics?: MidgardProvider["diagnostics"];
}): MidgardProvider => {
  const info = opts.info ?? protocolInfo();
  return {
    getUtxos: async (address) => {
      opts.onGetUtxos?.();
      return (opts.utxos ?? []).filter((utxo) => utxo.output.address === address);
    },
    getUtxoByOutRef: async () => undefined,
    getProtocolInfo: async () => {
      if (opts.protocolInfoError !== undefined) {
        throw opts.protocolInfoError;
      }
      return info;
    },
    getProtocolParameters: async () => {
      await opts.delayParameters;
      return {
        apiVersion: info.apiVersion,
        network: info.network,
        midgardNativeTxVersion: info.midgardNativeTxVersion,
        currentSlot: info.currentSlot,
        supportedScriptLanguages: info.supportedScriptLanguages,
        minFeeA: info.protocolFeeParameters.minFeeA,
        minFeeB: info.protocolFeeParameters.minFeeB,
        networkId: BigInt(info.network === "Mainnet" ? 1 : 0),
        maxSubmitTxCborBytes: info.submissionLimits.maxSubmitTxCborBytes,
        strictnessProfile: info.validation.strictnessProfile,
      };
    },
    getCurrentSlot: async () => info.currentSlot,
    submitTx: async () => ({
      txId: "00".repeat(32),
      status: "queued",
      httpStatus: 202,
      duplicate: false,
    }),
    getTxStatus: async (txId) => ({ kind: "queued", txId }),
    diagnostics:
      opts.diagnostics ??
      (() => ({
        endpoint: `memory://${opts.name}`,
        protocolInfoSource: "node",
      })),
  };
};

const inputLabels = (txHex: string): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    decodeMidgardNativeTxFull(Buffer.from(txHex, "hex")).body
      .spendInputsPreimageCbor,
  ).map((bytes) => {
    const input = CML.TransactionInput.from_cbor_bytes(bytes);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });

const deferred = (): {
  readonly promise: Promise<void>;
  readonly resolve: () => void;
} => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};

describe("provider switching and wallet input overrides", () => {
  it("switches providers atomically and refreshes immutable config snapshots", async () => {
    const first = makeProvider({
      name: "first",
      info: protocolInfo({ minFeeB: 1n, currentSlot: 10n }),
    });
    const second = makeProvider({
      name: "second",
      info: protocolInfo({ minFeeB: 2n, currentSlot: 20n }),
    });
    const midgard = await LucidMidgard.new(first, {
      network: "Preview",
      networkId: 0,
    });
    const before = midgard.config();

    await expect(midgard.switchProvider(second)).resolves.toBe(midgard);

    const after = midgard.config();
    expect(before.providerGeneration).toBe(0);
    expect(before.protocolFeeParameters.minFeeB).toBe(1n);
    expect(after.providerGeneration).toBe(1);
    expect(after.protocolFeeParameters.minFeeB).toBe(2n);
    expect(after.currentSlot).toBe(20n);
    expect(after.providerDiagnostics.endpoint).toBe("memory://second");
    expect(() => {
      (after as { protocolFeeParameters: { minFeeB: bigint } })
        .protocolFeeParameters.minFeeB = 9n;
    }).toThrow(TypeError);
  });

  it("preserves the previous provider when switch validation fails", async () => {
    const first = makeProvider({ name: "first" });
    const mainnet = makeProvider({
      name: "mainnet",
      info: protocolInfo({ network: "Mainnet" }),
    });
    const wrongVersion = makeProvider({
      name: "wrong-version",
      info: protocolInfo({ nativeVersion: 99 }),
    });
    const unavailable = makeProvider({
      name: "unavailable",
      protocolInfoError: new ProviderCapabilityError(
        "/protocol-info",
        "missing",
      ),
    });
    const midgard = await LucidMidgard.new(first, {
      network: "Preview",
      networkId: 0,
    });

    await expect(midgard.switchProvider(mainnet)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
    expect(midgard.config().providerDiagnostics.endpoint).toBe("memory://first");

    await expect(midgard.switchProvider(wrongVersion)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
    expect(midgard.config().providerGeneration).toBe(0);

    await expect(midgard.switchProvider(unavailable)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
    expect(midgard.provider).toBe(first);
  });

  it("fails closed when protocol network is unknown without explicit network id", async () => {
    await expect(
      LucidMidgard.new(
        makeProvider({
          name: "unknown-network",
          info: protocolInfo({ network: "Experimental" }),
        }),
      ),
    ).rejects.toBeInstanceOf(ProviderCapabilityError);

    await expect(
      LucidMidgard.new(
        makeProvider({
          name: "explicit-network-id",
          info: protocolInfo({ network: "Experimental" }),
        }),
        { networkId: 0 },
      ),
    ).resolves.toBeInstanceOf(LucidMidgard);
  });

  it("rejects Cardano-provider-shaped objects at runtime", async () => {
    const midgard = await LucidMidgard.new(makeProvider({ name: "midgard" }), {
      network: "Preview",
      networkId: 0,
    });
    const blockfrostShape = {
      getProtocolParameters: async () => ({}),
      getUtxos: async () => [],
      getUtxosByOutRef: async () => [],
      submitTx: async () => "00".repeat(32),
      evaluateTx: async () => [],
    };
    const emulatorShape = {
      ...blockfrostShape,
      awaitSlot: () => undefined,
      awaitBlock: () => undefined,
    };

    await expect(
      midgard.switchProvider(blockfrostShape as unknown as MidgardProvider),
    ).rejects.toBeInstanceOf(ProviderCapabilityError);
    await expect(
      midgard.switchProvider(emulatorShape as unknown as MidgardProvider),
    ).rejects.toBeInstanceOf(ProviderCapabilityError);
  });

  it("rejects malformed provider diagnostics before committing a switch", async () => {
    const first = makeProvider({ name: "first" });
    const missingSource = makeProvider({
      name: "bad-source",
      diagnostics: () =>
        ({
          endpoint: "memory://bad-source",
        }) as unknown as ReturnType<MidgardProvider["diagnostics"]>,
    });
    const missingFallbackReason = makeProvider({
      name: "bad-fallback",
      diagnostics: () => ({
        endpoint: "memory://bad-fallback",
        protocolInfoSource: "fallback",
      }),
    });
    const midgard = await LucidMidgard.new(first, {
      network: "Preview",
      networkId: 0,
    });

    await expect(midgard.switchProvider(missingSource)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    expect(midgard.config().providerDiagnostics.endpoint).toBe("memory://first");

    await expect(
      midgard.switchProvider(missingFallbackReason),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
    expect(midgard.config().providerGeneration).toBe(0);
  });

  it("keeps builders and in-flight completions pinned to their provider snapshot", async () => {
    const wallet = makeWalletFixture();
    const firstInput = makeUtxo(makeOutRef(0x11), wallet.address, {
      lovelace: 3_000_000n,
    });
    const secondInput = makeUtxo(makeOutRef(0x22), wallet.address, {
      lovelace: 3_000_000n,
    });
    const first = makeProvider({
      name: "first",
      utxos: [firstInput],
      info: protocolInfo({ minFeeB: 100n }),
    });
    const second = makeProvider({
      name: "second",
      utxos: [secondInput],
      info: protocolInfo({ minFeeB: 200n }),
    });
    const midgard = await LucidMidgard.new(first, {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromExternalSigner(wallet.wallet);
    const oldBuilder = midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n });

    await midgard.switchProvider(second);
    const oldCompleted = await oldBuilder.complete();
    const newCompleted = await midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete();

    expect(oldCompleted.metadata.providerGeneration).toBe(0);
    expect(oldCompleted.metadata.fee).toBe(100n);
    expect(inputLabels(oldCompleted.txHex)).toEqual([`${"11".repeat(32)}#0`]);
    expect(newCompleted.metadata.providerGeneration).toBe(1);
    expect(newCompleted.metadata.fee).toBe(200n);
    expect(inputLabels(newCompleted.txHex)).toEqual([`${"22".repeat(32)}#0`]);

    const gate = deferred();
    const slow = makeProvider({
      name: "slow",
      utxos: [firstInput],
      info: protocolInfo({ minFeeB: 300n }),
      delayParameters: gate.promise,
    });
    await midgard.switchProvider(slow);
    const completion = midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete();
    await midgard.switchProvider(second);
    gate.resolve();

    const inFlight = await completion;
    expect(inFlight.metadata.providerDiagnostics?.endpoint).toBe(
      "memory://slow",
    );
    expect(inFlight.metadata.fee).toBe(300n);
  });

  it("uses instance overrides and completion presets with deterministic snapshots", async () => {
    const wallet = makeWalletFixture();
    const providerInput = makeUtxo(makeOutRef(0x10), wallet.address, {
      lovelace: 3_000_000n,
    });
    const firstOverride = makeUtxo(makeOutRef(0x11), wallet.address, {
      lovelace: 3_000_000n,
    });
    const secondOverride = makeUtxo(makeOutRef(0x22), wallet.address, {
      lovelace: 3_000_000n,
    });
    let providerFetches = 0;
    const midgard = await LucidMidgard.new(
      makeProvider({
        name: "provider",
        utxos: [providerInput],
        onGetUtxos: () => {
          providerFetches += 1;
        },
      }),
      { network: "Preview", networkId: 0 },
    );
    midgard.selectWallet.fromExternalSigner(wallet.wallet);

    midgard.overrideUTxOs([firstOverride]);
    const pinnedBuilder = midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n });
    midgard.overrideUTxOs([secondOverride]);

    const pinned = await pinnedBuilder.complete();
    const latest = await midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete();
    const preset = await midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete({ presetWalletInputs: [providerInput] });

    expect(providerFetches).toBe(0);
    expect(pinned.metadata.walletInputSource).toBe("instance-override");
    expect(inputLabels(pinned.txHex)).toEqual([`${"11".repeat(32)}#0`]);
    expect(latest.metadata.walletInputSource).toBe("instance-override");
    expect(inputLabels(latest.txHex)).toEqual([`${"22".repeat(32)}#0`]);
    expect(preset.metadata.walletInputSource).toBe("completion-preset");
    expect(inputLabels(preset.txHex)).toEqual([`${"10".repeat(32)}#0`]);

    midgard.clearUTxOOverrides();
    const fromProvider = await midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete();
    expect(fromProvider.metadata.walletInputSource).toBe("provider");
    expect(providerFetches).toBe(1);
    expect(inputLabels(fromProvider.txHex)).toEqual([`${"10".repeat(32)}#0`]);
  });

  it("keeps in-flight completion pinned to the override snapshot", async () => {
    const wallet = makeWalletFixture();
    const gate = deferred();
    const firstOverride = makeUtxo(makeOutRef(0x11), wallet.address, {
      lovelace: 3_000_000n,
    });
    const secondOverride = makeUtxo(makeOutRef(0x22), wallet.address, {
      lovelace: 3_000_000n,
    });
    const midgard = await LucidMidgard.new(
      makeProvider({
        name: "slow",
        info: protocolInfo({ minFeeB: 100n }),
        delayParameters: gate.promise,
      }),
      { network: "Preview", networkId: 0 },
    );
    midgard.selectWallet.fromExternalSigner(wallet.wallet);
    midgard.overrideUTxOs([firstOverride]);

    const completion = midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete();
    midgard.overrideUTxOs([secondOverride]);
    gate.resolve();

    const completed = await completion;
    expect(completed.metadata.walletInputSource).toBe("instance-override");
    expect(inputLabels(completed.txHex)).toEqual([`${"11".repeat(32)}#0`]);
  });

  it("rejects malformed, duplicate, overlapping, and wrong-owner override inputs", async () => {
    const wallet = makeWalletFixture();
    const otherWallet = makeWalletFixture();
    const input = makeUtxo(makeOutRef(0x11), wallet.address, {
      lovelace: 2_000_000n,
    });
    const otherInput = makeUtxo(makeOutRef(0x22), otherWallet.address, {
      lovelace: 2_000_000n,
    });
    const malformed: MidgardUtxo = {
      ...input,
      cbor: {
        ...input.cbor,
        output: Buffer.from("00", "hex"),
      },
    };
    const midgard = await LucidMidgard.new(makeProvider({ name: "provider" }), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromExternalSigner(wallet.wallet);

    expect(() => midgard.overrideUTxOs([input, input])).toThrow(
      BuilderInvariantError,
    );
    expect(() => midgard.overrideUTxOs([malformed])).toThrow(
      BuilderInvariantError,
    );

    midgard.overrideUTxOs([otherInput]);
    await expect(
      midgard
        .newTx()
        .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(/does not belong/);

    await expect(
      midgard
        .newTx()
        .collectFrom([input])
        .pay.ToAddress(wallet.address, { lovelace: 1_000_000n })
      .complete({ presetWalletInputs: [input] }),
    ).rejects.toThrow(/duplicates/);
  });

  it("keeps fromAddress wallet and overrides atomic when supplied UTxOs fail", async () => {
    const firstWallet = makeWalletFixture();
    const secondWallet = makeWalletFixture();
    const firstInput = makeUtxo(makeOutRef(0x11), firstWallet.address, {
      lovelace: 3_000_000n,
    });
    const secondInput = makeUtxo(makeOutRef(0x22), secondWallet.address, {
      lovelace: 3_000_000n,
    });
    const midgard = await LucidMidgard.new(makeProvider({ name: "provider" }), {
      network: "Preview",
      networkId: 0,
    });

    midgard.selectWallet.fromAddress(firstWallet.address, [firstInput]);
    const previousOverrideGeneration = midgard.config().utxoOverrideGeneration;

    expect(() =>
      midgard.selectWallet.fromAddress(secondWallet.address, [firstInput]),
    ).toThrow(BuilderInvariantError);

    await expect(midgard.walletAddress()).resolves.toBe(firstWallet.address);
    expect(midgard.config()).toMatchObject({
      utxoOverrideGeneration: previousOverrideGeneration,
      hasUtxoOverrides: true,
    });

    const completed = await midgard
      .newTx()
      .pay.ToAddress(firstWallet.address, { lovelace: 1_000_000n })
      .complete();
    expect(inputLabels(completed.txHex)).toEqual([`${"11".repeat(32)}#0`]);

    midgard.selectWallet.fromAddress(secondWallet.address, [secondInput]);
    await expect(midgard.walletAddress()).resolves.toBe(secondWallet.address);
  });

  it("marks override-derived local validation pre-state as non-authoritative", async () => {
    const wallet = makeWalletFixture();
    const input = makeUtxo(makeOutRef(0x11), wallet.address, {
      lovelace: 2_000_000n,
    });
    const midgard = await LucidMidgard.new(makeProvider({ name: "provider" }), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromExternalSigner(wallet.wallet);
    midgard.overrideUTxOs([input]);

    const completed = await midgard
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 2_000_000n })
      .complete({
        localValidation: "phase-a",
        localPreState: new Map([
          [
            Buffer.from(input.cbor!.outRef!).toString("hex"),
            Buffer.from(input.cbor!.output!),
          ],
        ]),
        localPreStateSource: "instance-override",
      });

    expect(completed.metadata.localValidation).toMatchObject({
      phase: "phase-a",
      preStateSource: "instance-override",
      preStateAuthoritative: false,
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
  });

  it("revalidates selected-wallet networks during provider validation", async () => {
    const previewWallet = makeWalletFixture(0);
    const mainnetProvider = makeProvider({
      name: "mainnet",
      info: protocolInfo({ network: "Mainnet" }),
    });
    const midgard = await LucidMidgard.new(mainnetProvider, {
      network: "Mainnet",
      networkId: 1,
    });

    midgard.selectWallet.fromExternalSigner({
      address: previewWallet.address,
      keyHash: previewWallet.keyHash,
      signBodyHash: previewWallet.wallet.signBodyHash,
    });

    await expect(
      midgard
        .newTx()
        .pay.ToAddress(
          addressFromKeyHash(
            CML.PrivateKey.generate_ed25519().to_public().hash(),
            1,
          ),
          { lovelace: 1_000_000n },
        )
        .complete(),
    ).rejects.toThrow(/network mismatch/);
  });
});
