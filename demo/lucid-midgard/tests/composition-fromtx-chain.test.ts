import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  BuilderInvariantError,
  computeHash32,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardUtxo,
  deriveMidgardNativeTxCompact,
  encodeCbor,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  outRefToCbor,
  SigningError,
  CompleteTx,
  outputAddressProtected,
  type Address,
  type MidgardProvider,
  type MidgardProtocolInfo,
  type MidgardNativeTxFull,
  type MidgardUtxo,
  type OutRef,
} from "../src/index.js";

const protocolInfo = (
  overrides: Partial<MidgardProtocolInfo> = {},
): MidgardProtocolInfo => ({
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
  ...overrides,
});

const makeProvider = (
  endpoint = "memory://compose-a",
  utxos: readonly MidgardUtxo[] = [],
): MidgardProvider => ({
  getUtxos: async () => utxos,
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => protocolInfo(),
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
    endpoint,
    protocolInfoSource: "node",
  }),
});

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const enterpriseAddressFor = (
  privateKey: InstanceType<typeof CML.PrivateKey>,
): Address =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  address: Address,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";
const otherAddress = enterpriseAddressFor(CML.PrivateKey.generate_ed25519());

describe("fromTx, compose, and local chaining", () => {
  it("imports canonical Midgard native bytes while rejecting Cardano bytes", async () => {
    const provider = makeProvider();
    const midgard = await LucidMidgard.new(provider, {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x11), address, { lovelace: 2_000_000n });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete({ fee: 0n });

    const importedFromHex = midgard.fromTx(completed.txHex, {
      resolvedSpendInputs: [input],
    });
    const importedFromBytes = midgard.fromTx(completed.txCbor, {
      resolvedSpendInputs: [input],
    });

    expect(importedFromHex.txIdHex).toBe(completed.txIdHex);
    expect(importedFromHex.txHex).toBe(completed.txHex);
    expect(importedFromBytes.txHex).toBe(completed.txHex);
    expect(importedFromHex.metadata.expectedAddrWitnessesComplete).toBe(true);
    await expect(importedFromHex.status()).resolves.toEqual({
      kind: "queued",
      txId: completed.txIdHex,
    });

    const cardanoBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    expect(() => midgard.fromTx(cardanoBody.to_cbor_bytes())).toThrow(
      BuilderInvariantError,
    );
  });

  it("fails closed for signed imports without resolved spend pre-state", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const walletAddress = enterpriseAddressFor(privateKey);
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromPrivateKey(privateKey, walletAddress);
    const input = makeUtxo(makeOutRef(0x22), walletAddress, {
      lovelace: 2_000_000n,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(walletAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });
    const signed = await completed.sign();

    expect(() => midgard.fromTx(signed.txHex)).toThrow(SigningError);
    const imported = midgard.fromTx(signed.txHex, {
      resolvedSpendInputs: [input],
    });

    expect(imported.txHex).toBe(signed.txHex);
    expect(imported.metadata.signedBy).toEqual([
      privateKey.to_public().hash().to_hex(),
    ]);
  });

  it("does not trust existing CompleteTx metadata or stale witness bytes", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const walletAddress = enterpriseAddressFor(privateKey);
    let submitCount = 0;
    const provider: MidgardProvider = {
      ...makeProvider(),
      submitTx: async (txHex) => {
        submitCount += 1;
        return {
          txId: txHex.slice(0, 64).padEnd(64, "0"),
          status: "queued",
          httpStatus: 202,
          duplicate: false,
        };
      },
    };
    const midgard = await LucidMidgard.new(provider, {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x23), walletAddress, {
      lovelace: 2_000_000n,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(walletAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });
    const forged = new CompleteTx(completed.tx, {
      ...completed.metadata,
      expectedAddrWitnessCount: 0,
      expectedAddrWitnessKeyHashes: [],
      expectedAddrWitnessesComplete: true,
    });
    const imported = midgard.fromTx(forged);

    await expect(imported.submit()).rejects.toBeInstanceOf(SigningError);

    const staleWitness = CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(Buffer.from("99".repeat(32), "hex")),
      privateKey,
    );
    const addrTxWitsPreimageCbor = encodeCbor([
      Buffer.from(staleWitness.to_cbor_bytes()),
    ]);
    const witnessSet = {
      ...completed.tx.witnessSet,
      addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
      addrTxWitsPreimageCbor,
    };
    const staleTx: MidgardNativeTxFull = {
      ...completed.tx,
      witnessSet,
      compact: deriveMidgardNativeTxCompact(
        completed.tx.body,
        witnessSet,
        completed.tx.compact.validity,
        completed.tx.version,
      ),
    };
    const forgedSigned = new CompleteTx(staleTx, {
      ...completed.metadata,
      expectedAddrWitnessCount: 1,
      expectedAddrWitnessKeyHashes: [privateKey.to_public().hash().to_hex()],
      expectedAddrWitnessesComplete: true,
      addrWitnessCount: 1,
      signedBy: [privateKey.to_public().hash().to_hex()],
    });

    expect(() =>
      midgard.fromTx(forgedSigned, { resolvedSpendInputs: [input] }),
    ).toThrow(SigningError);
    await expect(forgedSigned.submit({ provider })).rejects.toBeInstanceOf(
      BuilderInvariantError,
    );
    expect(submitCount).toBe(0);
  });

  it("rejects imported transactions for the active wrong network", async () => {
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x24), address, { lovelace: 1_000_000n });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete({ fee: 0n });
    const wrongNetwork = await LucidMidgard.new(makeProvider(), {
      networkId: 1,
    });

    expect(() => wrongNetwork.fromTx(completed.txHex)).toThrow(
      BuilderInvariantError,
    );
  });

  it("rejects imported native transaction objects with root or compact drift", async () => {
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x33), address, { lovelace: 1_000_000n });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete({ fee: 0n });

    expect(() =>
      midgard.fromTx({
        ...completed.tx,
        body: { ...completed.tx.body, fee: 1n },
      }),
    ).toThrow(BuilderInvariantError);
  });

  it("composes builders deterministically without mutating fragments", async () => {
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x44), address, { lovelace: 3_000_000n });
    const first = midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n });
    const second = midgard
      .newTx()
      .validFrom(10n)
      .validTo(50n)
      .pay.ToAddress(otherAddress, { lovelace: 2_000_000n });
    const direct = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .validFrom(10n)
      .validTo(50n)
      .pay.ToAddress(otherAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });

    const composed = first.compose(second);
    const completed = await composed.complete({ fee: 0n });

    expect(completed.txHex).toBe(direct.txHex);
    expect(first.debugSnapshot().outputs).toHaveLength(1);
    expect(second.debugSnapshot().spendInputs).toHaveLength(0);
    expect(composed.debugSnapshot().composition).toEqual({ fragmentCount: 2 });
    expect(() => first.compose(midgard.newTx().collectFrom([input]))).toThrow(
      BuilderInvariantError,
    );
    expect(() =>
      midgard.newTx().validTo(9n).compose(midgard.newTx().validFrom(10n)),
    ).toThrow(BuilderInvariantError);
  });

  it("exposes exact local outputs and chains them into dependent builders", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const walletAddress = enterpriseAddressFor(privateKey);
    const firstInput = makeUtxo(makeOutRef(0x55), walletAddress, {
      lovelace: 5_000_000n,
    });
    const secondInput = makeUtxo(makeOutRef(0x66), walletAddress, {
      lovelace: 7_000_000n,
    });
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet
      .fromPrivateKey(privateKey, walletAddress)
      .overrideUTxOs([firstInput, secondInput]);

    const [newWalletUtxos, derivedOutputs, completed] = await midgard
      .newTx()
      .pay.ToProtectedAddress(otherAddress, { lovelace: 2_000_000n })
      .chain({ fee: 0n });
    const outputBytes = decodeMidgardNativeByteListPreimage(
      completed.tx.body.outputsPreimageCbor,
      "native.outputs",
    );

    expect(
      derivedOutputs.map((utxo) =>
        Buffer.from(utxo.cbor!.output!).toString("hex"),
      ),
    ).toEqual(outputBytes.map((bytes) => bytes.toString("hex")));
    expect(outputAddressProtected(derivedOutputs[0]!.output.address)).toBe(
      true,
    );
    expect(
      Buffer.from(completed.producedOutput(0).cbor!.outRef!).toString("hex"),
    ).toBe(
      outRefToCbor({ txHash: completed.txIdHex, outputIndex: 0 }).toString(
        "hex",
      ),
    );
    expect(() => completed.producedOutput(99)).toThrow(BuilderInvariantError);
    expect(newWalletUtxos).toHaveLength(2);
    expect(
      newWalletUtxos.reduce(
        (total, utxo) => total + (utxo.output.assets.lovelace ?? 0n),
        0n,
      ),
    ).toBe(10_000_000n);
    expect(
      newWalletUtxos.some((utxo) => utxo.txHash === completed.txIdHex),
    ).toBe(true);
    expect(
      newWalletUtxos.some(
        (utxo) =>
          utxo.txHash === firstInput.txHash ||
          utxo.txHash === secondInput.txHash,
      ),
    ).toBe(true);

    const dependent = await midgard
      .newTx()
      .collectFrom([derivedOutputs[0]!])
      .pay.ToAddress(walletAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });
    const dependentInput = decodeMidgardNativeByteListPreimage(
      dependent.tx.body.spendInputsPreimageCbor,
      "native.spend_inputs",
    )[0]!;
    expect(
      CML.TransactionInput.from_cbor_bytes(dependentInput)
        .transaction_id()
        .to_hex(),
    ).toBe(completed.txIdHex);

    await midgard.switchProvider(makeProvider("memory://compose-b"));
    expect(() =>
      midgard.newTx().collectFrom([derivedOutputs[0]!]),
    ).not.toThrow();

    const detached = new CompleteTx(completed.tx, {
      ...completed.metadata,
      providerGeneration: undefined,
      providerDiagnostics: undefined,
    });
    expect(() => detached.producedOutput(0)).toThrow(BuilderInvariantError);
  });
});
