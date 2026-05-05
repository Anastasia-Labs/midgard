import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  CompleteTx,
  PartiallySignedTx,
  SigningError,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  decodePartialWitnessBundle,
  encodeCbor,
  encodeMidgardTxOutput,
  encodePartialWitnessBundle,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  makeVKeyWitness,
  outRefToCbor,
  parsePartialWitnessBundle,
  walletFromExternalSigner,
  walletFromPrivateKey,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  type TxStatus,
} from "../src/index.js";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const addressFromKeyHash = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
): string =>
  CML.EnterpriseAddress.new(0, CML.Credential.new_pub_key(keyHash))
    .to_address()
    .to_bech32();

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

const makeProvider = (opts?: {
  readonly status?: (txId: string) => Promise<TxStatus>;
}): MidgardProvider => ({
  getUtxos: async () => [],
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
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async (txCborHex) => {
    const tx = decodeMidgardNativeTxFull(Buffer.from(txCborHex, "hex"));
    return {
      txId: computeMidgardNativeTxIdFromFull(tx).toString("hex"),
      status: "queued",
      httpStatus: 202,
      duplicate: false,
    };
  },
  getTxStatus: opts?.status ?? (async (txId) => ({ kind: "queued", txId })),
  diagnostics: () => ({
    endpoint: "memory://partial-signing",
    protocolInfoSource: "node",
  }),
});

const makeFixture = async () => {
  const firstKey = CML.PrivateKey.generate_ed25519();
  const secondKey = CML.PrivateKey.generate_ed25519();
  const firstHash = firstKey.to_public().hash().to_hex();
  const secondHash = secondKey.to_public().hash().to_hex();
  const firstAddress = addressFromKeyHash(firstKey.to_public().hash());
  const secondAddress = addressFromKeyHash(secondKey.to_public().hash());
  const provider = makeProvider();
  const { LucidMidgard } = await import("../src/index.js");
  const midgard = await LucidMidgard.new(provider, {
    network: "Preview",
    networkId: 0,
  });
  const completed = await midgard
    .newTx()
    .collectFrom([
      makeUtxo(makeOutRef(0x11), firstAddress, { lovelace: 2_000_000n }),
      makeUtxo(makeOutRef(0x22), secondAddress, { lovelace: 2_000_000n }),
    ])
    .pay.ToAddress(firstAddress, { lovelace: 4_000_000n })
    .complete({ fee: 0n });

  return {
    completed,
    provider,
    firstKey,
    secondKey,
    firstHash,
    secondHash,
    firstAddress,
    secondAddress,
    midgard,
  };
};

const witnessCount = (tx: CompleteTx | PartiallySignedTx): number =>
  decodeMidgardNativeByteListPreimage(
    tx.tx.witnessSet.addrTxWitsPreimageCbor,
    "native.addr_tx_wits",
  ).length;

const expectComplete = (tx: CompleteTx | PartiallySignedTx): CompleteTx => {
  expect(tx).toBeInstanceOf(CompleteTx);
  if (!(tx instanceof CompleteTx)) {
    throw new Error("expected CompleteTx");
  }
  return tx;
};

const expectPartial = (
  tx: CompleteTx | PartiallySignedTx,
): PartiallySignedTx => {
  expect(tx).toBeInstanceOf(PartiallySignedTx);
  if (!(tx instanceof PartiallySignedTx)) {
    throw new Error("expected PartiallySignedTx");
  }
  return tx;
};

describe("partial signing", () => {
  it("assembles two detached signer bundles into deterministic signed bytes", async () => {
    const {
      completed,
      provider,
      firstKey,
      secondKey,
      firstHash,
      secondHash,
      secondAddress,
    } = await makeFixture();
    const secondWallet = walletFromPrivateKey(secondKey, secondAddress, {
      expectedNetworkId: 0,
    });

    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const secondBundle = await completed.sign.withWallet(secondWallet).partial();
    const bodyHash = completed.tx.compact.transactionBodyHash;
    const signedFromSignBuilder = await completed.sign
      .withWitnesses([
        makeVKeyWitness(bodyHash, secondKey),
        makeVKeyWitness(bodyHash, firstKey),
      ])
      .complete();
    const signedAB = completed.assemble([firstBundle, secondBundle]);
    const signedBA = completed.assemble([secondBundle, firstBundle]);

    const completeAB = expectComplete(signedAB);
    const completeBA = expectComplete(signedBA);
    expect(completeAB.txIdHex).toBe(completed.txIdHex);
    expect(completeAB.txHex).toBe(completeBA.txHex);
    expect(signedFromSignBuilder.txHex).toBe(completeAB.txHex);
    expect(completeAB.metadata.signedBy).toEqual([firstHash, secondHash].sort());
    expect(witnessCount(completeAB)).toBe(2);

    await expect(completeAB.submit({ provider })).resolves.toMatchObject({
      txIdHex: completeAB.txIdHex,
    });
  });

  it("keeps partial assembly non-submit-capable until all expected witnesses exist", async () => {
    const { completed, firstKey, secondKey } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const secondBundle = await completed.sign.withPrivateKey(secondKey).partial();

    expect(() => completed.assemble(firstBundle)).toThrow(SigningError);
    const partial = expectPartial(
      completed.assemble(firstBundle, { allowPartial: true }),
    );
    expect("submit" in partial).toBe(false);
    expect(witnessCount(partial)).toBe(1);

    const continued = expectComplete(partial.assemble(secondBundle));
    const direct = expectComplete(completed.assemble([firstBundle, secondBundle]));
    expect(continued.txHex).toBe(direct.txHex);
  });

  it("exports, imports, and reuses canonical partial witness bundles", async () => {
    const { completed, firstKey, secondKey } = await makeFixture();
    const bodyHash = completed.tx.compact.transactionBodyHash;
    const firstWitness = makeVKeyWitness(bodyHash, firstKey);
    const secondWitness = makeVKeyWitness(bodyHash, secondKey);
    const combinedBundle = await completed.sign
      .withWitnesses([secondWitness, firstWitness])
      .partial();

    const cbor = encodePartialWitnessBundle(combinedBundle);
    expect(decodePartialWitnessBundle(cbor)).toEqual(combinedBundle);
    expect(decodePartialWitnessBundle(cbor.toString("hex"))).toEqual(
      combinedBundle,
    );
    expect(() =>
      decodePartialWitnessBundle(Buffer.concat([cbor, Buffer.from([0])])),
    ).toThrow();

    const unsortedCbor = encodeCbor([
      "MidgardPartialWitnessBundle",
      1,
      combinedBundle.midgardNativeTxVersion,
      Buffer.from(combinedBundle.txId, "hex"),
      Buffer.from(combinedBundle.bodyHash, "hex"),
      [...combinedBundle.witnesses]
        .reverse()
        .map((witness) => Buffer.from(witness, "hex")),
      [...combinedBundle.signerKeyHashes]
        .reverse()
        .map((keyHash) => Buffer.from(keyHash, "hex")),
    ]);
    expect(() => decodePartialWitnessBundle(unsortedCbor)).toThrow(SigningError);

    const signed = expectComplete(completed.assemble({ cbor }));
    expect(witnessCount(signed)).toBe(2);
    expect(signed.toPartialWitnessBundle()).toEqual(combinedBundle);
  });

  it("rejects malformed, mismatched, and unexpected partial witness bundles", async () => {
    const { completed, firstKey, firstHash } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const thirdKey = CML.PrivateKey.generate_ed25519();
    const thirdBundle = await completed.sign.withPrivateKey(thirdKey).partial();

    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, kind: "Bad" } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, version: 2 } as never),
    ).toThrow(SigningError);
    expect(() => parsePartialWitnessBundle(null as never)).toThrow(SigningError);
    expect(() => parsePartialWitnessBundle(123 as never)).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, txId: 123 } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, witnesses: "not-array" } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        signerKeyHashes: "not-array",
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        witnesses: [123],
        signerKeyHashes: [firstHash],
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        signerKeyHashes: [123],
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      completed.assemble({
        ...firstBundle,
        midgardNativeTxVersion: firstBundle.midgardNativeTxVersion + 1,
      }),
    ).toThrow(SigningError);
    expect(() =>
      completed.assemble({ ...firstBundle, txId: "00".repeat(32) }),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        witnesses: ["00"],
        signerKeyHashes: [firstHash],
      }),
    ).toThrow(SigningError);
    expect(() => completed.assemble(thirdBundle)).toThrow(SigningError);

    const cardanoBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    const cardanoWitness = CML.make_vkey_witness(
      CML.hash_transaction(cardanoBody),
      firstKey,
    );
    await expect(
      completed.sign.withWitness(cardanoWitness).partial(),
    ).rejects.toBeInstanceOf(SigningError);
  });

  it("collapses identical bundles and rejects conflicting external signer identity", async () => {
    const { completed, firstKey, secondKey, secondHash } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const secondBundle = await completed.sign.withExternalSigner({
      keyHash: secondHash,
      signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, secondKey),
    }).partial();

    const signed = expectComplete(
      completed.assemble([firstBundle, firstBundle, secondBundle]),
    );
    expect(witnessCount(signed)).toBe(2);

    await expect(
      completed.sign.withExternalSigner({
        keyHash: secondHash,
        signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, firstKey),
      }).partial(),
    ).rejects.toBeInstanceOf(SigningError);
  });

  it("imports signed bytes as partial only when explicitly requested", async () => {
    const { completed, firstKey, secondKey, midgard } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const partial = expectPartial(
      completed.assemble(firstBundle, { allowPartial: true }),
    );

    expect(() => midgard.fromTx(partial.txHex)).toThrow(SigningError);
    const imported = midgard.fromTx(partial.txHex, { partial: true });
    expect("submit" in imported).toBe(false);

    const secondBundle = await completed.sign.withPrivateKey(secondKey).partial();
    expect(() => imported.assemble(secondBundle)).toThrow(SigningError);
    const stillPartial = expectPartial(
      imported.assemble(secondBundle, { allowPartial: true }),
    );
    expect(() => stillPartial.toPartialWitnessBundle()).not.toThrow();
  });

  it("wraps partial signing with safe and Effect-compatible APIs", async () => {
    const { completed, firstKey } = await makeFixture();
    const safe = await completed.sign.withPrivateKeySafe(firstKey);
    expect(safe.ok).toBe(true);
    if (safe.ok) {
      expect(safe.value.txId).toBe(completed.txIdHex);
    }

    const external = walletFromExternalSigner({
      keyHash: firstKey.to_public().hash().to_hex(),
      signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, firstKey),
    });
    const programBundle = await Effect.runPromise(
      completed.sign.withWalletProgram(external),
    );
    expect(programBundle.txId).toBe(completed.txIdHex);

    const completeSafe = await completed.sign.withPrivateKey(firstKey).completeSafe();
    expect(completeSafe.ok).toBe(false);
  });
});
