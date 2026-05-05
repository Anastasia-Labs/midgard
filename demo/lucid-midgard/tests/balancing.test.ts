import { describe, expect, it } from "vitest";
import {
  BuilderInvariantError,
  decodeMidgardUtxo,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  encodeMidgardTxOutput,
  InsufficientFundsError,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  makeVKeyWitness,
  outRefToCbor,
  walletFromExternalSigner,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
} from "../src/index.js";
import { CML, valueToAssets } from "@lucid-evolution/lucid";

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";

const addressFromKeyHash = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
): string =>
  CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(keyHash))
    .to_address()
    .to_bech32();

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const makeUtxo = (
  ref: OutRef,
  assets: Readonly<Record<string, bigint>>,
  utxoAddress = address,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(utxoAddress, assets),
  });

const makeProvider = (
  utxos: readonly MidgardUtxo[],
  feePolicy = { minFeeA: 0n, minFeeB: 0n },
): MidgardProvider => ({
  getUtxos: async (requestedAddress) =>
    utxos.filter((utxo) => utxo.output.address === requestedAddress),
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: feePolicy,
    submissionLimits: { maxSubmitTxCborBytes: 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    ...feePolicy,
    networkId: 0n,
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
});

const decodedOutputAssets = (outputBytes: Uint8Array) =>
  valueToAssets(
    CML.TransactionOutput.from_cbor_bytes(outputBytes).amount(),
  ) as Record<string, bigint>;

const walletForAddress = () => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  return walletFromExternalSigner({
    address,
    keyHash: privateKey.to_public().hash().to_hex(),
    signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  });
};

const walletFixture = () => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const keyHash = privateKey.to_public().hash();
  const walletAddress = addressFromKeyHash(keyHash);
  const wallet = walletFromExternalSigner({
    address: walletAddress,
    keyHash: keyHash.to_hex(),
    signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, privateKey),
  });
  return { address: walletAddress, wallet };
};

describe("TxBuilder balancing and fees", () => {
  it("converges fees for a simple transfer and reports change", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider([makeUtxo(makeOutRef(0x11), { lovelace: 5_000_000n })]),
      { network: "Preview", networkId: 0 },
    );

    const completed = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete({
        changeAddress: address,
        feePolicy: { minFeeA: 1n, minFeeB: 0n },
      });

    expect(completed.metadata.balanced).toBe(true);
    expect(completed.metadata.expectedAddrWitnessCount).toBe(1);
    expect(completed.metadata.estimatedSignedTxByteLength).toBeGreaterThan(
      completed.metadata.txByteLength,
    );
    expect(completed.metadata.fee).toBe(
      BigInt(completed.metadata.estimatedSignedTxByteLength ?? 0),
    );
    expect(completed.metadata.changeOutputIndex).toBe(1);
    expect(completed.metadata.changeAssets).toEqual({
      lovelace: 5_000_000n - 1_000_000n - completed.metadata.fee,
    });
  });

  it("converges selected-wallet fees against the signed byte length", async () => {
    const fixture = walletFixture();
    const midgard = await LucidMidgard.new(
      makeProvider(
        [
          makeUtxo(
            makeOutRef(0x12),
            { lovelace: 5_000_000n },
            fixture.address,
          ),
        ],
        { minFeeA: 1n, minFeeB: 0n },
      ),
      { network: "Preview", networkId: 0 },
    );
    midgard.selectWallet.fromExternalSigner(fixture.wallet);

    const completed = await midgard
      .newTx()
      .pay.ToAddress(fixture.address, { lovelace: 1_000_000n })
      .complete();
    const signed = await completed.sign();

    expect(completed.metadata.expectedAddrWitnessCount).toBe(1);
    expect(completed.metadata.estimatedSignedTxByteLength).toBe(
      signed.txCbor.length,
    );
    expect(completed.metadata.fee).toBe(BigInt(signed.txCbor.length));
    expect(signed.metadata.txByteLength).toBe(signed.txCbor.length);
  });

  it("preserves multi-asset value with deterministic change", async () => {
    const unit = `${"ab".repeat(28)}${"cd".repeat(2)}`;
    const midgard = await LucidMidgard.new(
      makeProvider(
        [
          makeUtxo(makeOutRef(0x22), {
            lovelace: 5_000_000n,
            [unit]: 10n,
          }),
        ],
        { minFeeA: 0n, minFeeB: 200_000n },
      ),
      { network: "Preview", networkId: 0 },
    );

    const completed = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 2_000_000n, [unit]: 4n })
      .complete({ changeAddress: address, feePolicy: "provider" });
    const tx = decodeMidgardNativeTxFull(completed.txCbor);
    const outputs = decodeMidgardNativeByteListPreimage(
      tx.body.outputsPreimageCbor,
    );

    expect(outputs).toHaveLength(2);
    expect(decodedOutputAssets(outputs[1] ?? Buffer.alloc(0))).toEqual({
      lovelace: 2_800_000n,
      [unit]: 6n,
    });
    expect(completed.metadata.changeAssets).toEqual({
      lovelace: 2_800_000n,
      [unit]: 6n,
    });
  });

  it("does not create a change output for exact-spend completion", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider(
        [makeUtxo(makeOutRef(0x11), { lovelace: 1_200_000n })],
        { minFeeA: 0n, minFeeB: 200_000n },
      ),
      { network: "Preview", networkId: 0 },
    );

    const completed = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete({ changeAddress: address, feePolicy: "provider" });

    expect(completed.metadata.fee).toBe(200_000n);
    expect(completed.metadata.outputCount).toBe(1);
    expect(completed.metadata.changeOutputIndex).toBeUndefined();
    expect(completed.metadata.changeAssets).toEqual({});
  });

  it("fails when available lovelace cannot cover outputs and fees", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })]),
    );

    await expect(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 2_000_000n })
        .complete({
          changeAddress: address,
          feePolicy: { minFeeA: 0n, minFeeB: 0n },
        }),
    ).rejects.toBeInstanceOf(InsufficientFundsError);
  });

  it("marks insufficient lovelace errors as fee-inclusive after convergence", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider(
        [makeUtxo(makeOutRef(0x11), { lovelace: 1_100_000n })],
        { minFeeA: 0n, minFeeB: 200_000n },
      ),
    );

    await expect(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete({ changeAddress: address, feePolicy: "provider" }),
    ).rejects.toMatchObject({ feeIncluded: true });
  });

  it("fails when available tokens cannot cover outputs", async () => {
    const unit = `${"ab".repeat(28)}${"cd".repeat(2)}`;
    const midgard = await LucidMidgard.new(
      makeProvider([
        makeUtxo(makeOutRef(0x11), { lovelace: 5_000_000n, [unit]: 1n }),
      ]),
    );

    await expect(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n, [unit]: 2n })
        .complete({
          changeAddress: address,
          feePolicy: { minFeeA: 0n, minFeeB: 0n },
        }),
    ).rejects.toBeInstanceOf(InsufficientFundsError);
  });

  it("fails hard when fee convergence cannot run within the configured limit", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider([makeUtxo(makeOutRef(0x11), { lovelace: 5_000_000n })]),
    );

    await expect(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete({
          changeAddress: address,
          feePolicy: { minFeeA: 1n, minFeeB: 0n },
          maxFeeIterations: 0,
        }),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("uses provider balancing by default when a wallet is selected", async () => {
    const midgard = await LucidMidgard.new(
      makeProvider(
        [makeUtxo(makeOutRef(0x11), { lovelace: 2_000_000n })],
        { minFeeA: 0n, minFeeB: 200_000n },
      ),
      { network: "Preview", networkId: 0 },
    );
    midgard.selectWallet.fromExternalSigner(walletForAddress());

    const completed = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    expect(completed.metadata.balanced).toBe(true);
    expect(completed.metadata.fee).toBe(200_000n);
    expect(completed.metadata.changeAssets).toEqual({ lovelace: 800_000n });
  });

  it("prefers token-covering UTxOs before unrelated lovelace inputs", async () => {
    const unit = `${"ab".repeat(28)}${"cd".repeat(2)}`;
    const midgard = await LucidMidgard.new(
      makeProvider(
        [
          makeUtxo(makeOutRef(0x11), { lovelace: 10_000_000n }),
          makeUtxo(makeOutRef(0xff), { lovelace: 2_000_000n, [unit]: 1n }),
        ],
        { minFeeA: 0n, minFeeB: 0n },
      ),
      { network: "Preview", networkId: 0 },
    );

    const completed = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n, [unit]: 1n })
      .complete({ changeAddress: address, feePolicy: "provider" });
    const tx = decodeMidgardNativeTxFull(completed.txCbor);
    const inputs = decodeMidgardNativeByteListPreimage(
      tx.body.spendInputsPreimageCbor,
    ).map((bytes) => CML.TransactionInput.from_cbor_bytes(bytes));

    expect(
      inputs.map((input) => input.transaction_id().to_hex()),
    ).toEqual(["ff".repeat(32)]);
  });
});
