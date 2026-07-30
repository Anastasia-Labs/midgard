import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  CompleteTx,
  decodeMidgardUtxo,
  decodePartialWitnessBundle,
  encodeMidgardTxOutput,
  encodePartialWitnessBundle,
  makeVKeyWitness,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  parsePartialWitnessBundle,
  PartiallySignedTx,
  SigningError,
  type TxStatus,
  walletFromExternalSigner,
  walletFromPrivateKey,
} from "../src/index.js";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const addressFromKeyHash = (keyHash: CML.Ed25519KeyHash): string =>
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
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
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
  submitTx: async (txCborHex) => {
    const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(txCborHex, "hex"),
    );
    return {
      txId: computeMidgardNativeTxIdV1(tx).toString("hex"),
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

const makeFixture = async (keys?: {
  readonly firstKey: CML.PrivateKey;
  readonly secondKey: CML.PrivateKey;
}) => {
  const firstKey = keys?.firstKey ?? CML.PrivateKey.generate_ed25519();
  const secondKey = keys?.secondKey ?? CML.PrivateKey.generate_ed25519();
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
    const secondBundle = await completed.sign
      .withWallet(secondWallet)
      .partial();
    const bodyHash = computeMidgardNativeTxIdV1(completed.tx);
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
    expect(completeAB.metadata.signedBy).toEqual(
      [firstHash, secondHash].sort(),
    );
    expect(witnessCount(completeAB)).toBe(2);

    await expect(completeAB.submit({ provider })).resolves.toMatchObject({
      txIdHex: completeAB.txIdHex,
    });
  });

  it("keeps partial assembly non-submit-capable until all expected witnesses exist", async () => {
    const { completed, firstKey, secondKey } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const secondBundle = await completed.sign
      .withPrivateKey(secondKey)
      .partial();

    expect(() => completed.assemble(firstBundle)).toThrow(SigningError);
    const partial = expectPartial(
      completed.assemble(firstBundle, { allowPartial: true }),
    );
    expect("submit" in partial).toBe(false);
    expect(witnessCount(partial)).toBe(1);

    const continued = expectComplete(partial.assemble(secondBundle));
    const direct = expectComplete(
      completed.assemble([firstBundle, secondBundle]),
    );
    expect(continued.txHex).toBe(direct.txHex);
  });

  it("exports, imports, and reuses canonical partial witness bundles", async () => {
    const { completed, firstKey, secondKey } = await makeFixture({
      firstKey: CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 1)),
      secondKey: CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 2)),
    });
    const bodyHash = computeMidgardNativeTxIdV1(completed.tx);
    const firstWitness = makeVKeyWitness(bodyHash, firstKey);
    const secondWitness = makeVKeyWitness(bodyHash, secondKey);
    const combinedBundle = await completed.sign
      .withWitnesses([secondWitness, firstWitness])
      .partial();

    const cbor = encodePartialWitnessBundle(combinedBundle);
    const canonicalTuple = [
      "MidgardPartialWitnessBundleV1",
      1,
      1,
      Buffer.from(combinedBundle.txId, "hex"),
      Buffer.from(combinedBundle.bodyHash, "hex"),
      combinedBundle.witnesses.map((witness) => Buffer.from(witness, "hex")),
      combinedBundle.signerKeyHashes.map((keyHash) =>
        Buffer.from(keyHash, "hex"),
      ),
    ];
    expect(combinedBundle).toEqual({
      kind: "MidgardPartialWitnessBundleV1",
      version: 1,
      midgardNativeTxVersion: 1,
      txId: "8ce56e901e97cd310fdaf62766161dad1a89a466a317fbaefe51eddf2a5507d1",
      bodyHash:
        "8ce56e901e97cd310fdaf62766161dad1a89a466a317fbaefe51eddf2a5507d1",
      witnesses: [
        "8258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b39458403d667c4d6ff85d29fd766f9f80c768424eb5fb278327f7c1feeee3cb5342cb40e587266b6b8bba79ebbb966e9609e9363bcf75a2fb0b8ca7491e240ef616cd07",
        "8258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584080a61e1180948da1d693ddd2c0b0fb752fb93fd2306edbc7c0bbdc88c4ebb017817efefa0b15310043de30ff803bdb90695f0e520a3c6f2ea9c3e55ba90d1d09",
      ],
      signerKeyHashes: [
        "008b47844d92812fc30d1f0ac9b6fbf38778ccba9db8312ad9079079",
        "0d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb06",
      ],
    });
    expect(cbor.toString("hex")).toBe(
      "87781d4d6964676172645061727469616c5769746e65737342756e646c655631010158208ce56e901e97cd310fdaf62766161dad1a89a466a317fbaefe51eddf2a5507d158208ce56e901e97cd310fdaf62766161dad1a89a466a317fbaefe51eddf2a5507d18258658258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b39458403d667c4d6ff85d29fd766f9f80c768424eb5fb278327f7c1feeee3cb5342cb40e587266b6b8bba79ebbb966e9609e9363bcf75a2fb0b8ca7491e240ef616cd0758658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584080a61e1180948da1d693ddd2c0b0fb752fb93fd2306edbc7c0bbdc88c4ebb017817efefa0b15310043de30ff803bdb90695f0e520a3c6f2ea9c3e55ba90d1d0982581c008b47844d92812fc30d1f0ac9b6fbf38778ccba9db8312ad9079079581c0d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb06",
    );
    expect(cbor).toEqual(encodeCbor(canonicalTuple));
    expect(decodePartialWitnessBundle(cbor)).toEqual(combinedBundle);
    expect(decodePartialWitnessBundle(cbor.toString("hex"))).toEqual(
      combinedBundle,
    );
    expect(() =>
      decodePartialWitnessBundle(Buffer.concat([cbor, Buffer.from([0])])),
    ).toThrow();

    const unsortedCbor = encodeCbor([
      "MidgardPartialWitnessBundleV1",
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
    expect(() => decodePartialWitnessBundle(unsortedCbor)).toThrow(
      SigningError,
    );
    expect(() =>
      parsePartialWitnessBundle({
        ...combinedBundle,
        witnesses: [...combinedBundle.witnesses].reverse(),
      }),
    ).toThrow(SigningError);
    expect(() =>
      decodePartialWitnessBundle(
        encodeCbor([canonicalTuple[0], 2, ...canonicalTuple.slice(2)]),
      ),
    ).toThrow(SigningError);
    expect(() =>
      decodePartialWitnessBundle(
        encodeCbor([
          canonicalTuple[0],
          canonicalTuple[1],
          2,
          ...canonicalTuple.slice(3),
        ]),
      ),
    ).toThrow(SigningError);
    expect(() =>
      decodePartialWitnessBundle(encodeCbor(canonicalTuple.slice(0, -1))),
    ).toThrow(SigningError);
    expect(() =>
      decodePartialWitnessBundle(encodeCbor([...canonicalTuple, 0])),
    ).toThrow(SigningError);
    const retiredKind = combinedBundle.kind.slice(0, -2);
    for (const kind of [retiredKind, `${retiredKind}V2`]) {
      expect(() =>
        decodePartialWitnessBundle(
          encodeCbor([kind, ...canonicalTuple.slice(1)]),
        ),
      ).toThrow(/Unsupported partial witness bundle kind/u);
    }

    const signed = expectComplete(completed.assemble({ cbor }));
    expect(witnessCount(signed)).toBe(2);
    expect(signed.toPartialWitnessBundle()).toEqual(combinedBundle);
  });

  it("rejects malformed, mismatched, and unexpected partial witness bundles", async () => {
    const { completed, firstKey, firstHash } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const thirdKey = CML.PrivateKey.generate_ed25519();
    const thirdBundle = await completed.sign.withPrivateKey(thirdKey).partial();
    const foreign = await makeFixture();
    const foreignBundle = await foreign.completed.sign
      .withPrivateKey(foreign.firstKey)
      .partial();

    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, kind: "Bad" } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, version: 23 } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        midgardNativeTxVersion: 2,
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        unknown: true,
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle(
        Object.fromEntries(
          Object.entries(firstBundle).filter(([key]) => key !== "bodyHash"),
        ) as never,
      ),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        cbor: encodePartialWitnessBundle(firstBundle),
        unknown: true,
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        cbor: encodePartialWitnessBundle(firstBundle),
        cborHex: encodePartialWitnessBundle(firstBundle).toString("hex"),
      } as never),
    ).toThrow(SigningError);
    expect(() => parsePartialWitnessBundle({ cbor: 23 } as never)).toThrow(
      SigningError,
    );
    expect(() => parsePartialWitnessBundle({ cborHex: 23 } as never)).toThrow(
      SigningError,
    );
    expect(() => parsePartialWitnessBundle(null as never)).toThrow(
      SigningError,
    );
    expect(() => parsePartialWitnessBundle(123 as never)).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({ ...firstBundle, txId: 123 } as never),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        witnesses: "not-array",
      } as never),
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
      } as never),
    ).toThrow(SigningError);
    expect(() =>
      completed.assemble({ ...firstBundle, txId: "00".repeat(32) }),
    ).toThrow(SigningError);
    expect(() => completed.assemble(foreignBundle)).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        txId: firstBundle.txId.toUpperCase(),
        bodyHash: firstBundle.bodyHash.toUpperCase(),
      }),
    ).toThrow(SigningError);
    expect(() =>
      parsePartialWitnessBundle({
        ...firstBundle,
        witnesses: [firstBundle.witnesses[0], firstBundle.witnesses[0]],
        signerKeyHashes: [
          firstBundle.signerKeyHashes[0],
          firstBundle.signerKeyHashes[0],
        ],
      }),
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

  it("rejects duplicate bundles and conflicting external signer identity", async () => {
    const { completed, firstKey, secondKey, secondHash } = await makeFixture();
    const firstBundle = await completed.sign.withPrivateKey(firstKey).partial();
    const secondBundle = await completed.sign
      .withExternalSigner({
        keyHash: secondHash,
        signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, secondKey),
      })
      .partial();

    expect(() =>
      completed.assemble([firstBundle, firstBundle, secondBundle]),
    ).toThrow(SigningError);
    const signed = expectComplete(
      completed.assemble([firstBundle, secondBundle]),
    );
    expect(witnessCount(signed)).toBe(2);
    await expect(
      completed.sign
        .withWitnesses([
          makeVKeyWitness(computeMidgardNativeTxIdV1(completed.tx), firstKey),
          makeVKeyWitness(computeMidgardNativeTxIdV1(completed.tx), firstKey),
        ])
        .partial(),
    ).rejects.toBeInstanceOf(SigningError);

    await expect(
      completed.sign
        .withExternalSigner({
          keyHash: secondHash,
          signBodyHash: (bodyHash) => makeVKeyWitness(bodyHash, firstKey),
        })
        .partial(),
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

    const secondBundle = await completed.sign
      .withPrivateKey(secondKey)
      .partial();
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

    const completeSafe = await completed.sign
      .withPrivateKey(firstKey)
      .completeSafe();
    expect(completeSafe.ok).toBe(false);
  });
});
