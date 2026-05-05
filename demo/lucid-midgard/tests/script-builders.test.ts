import { describe, expect, it } from "vitest";
import { blake2b } from "@noble/hashes/blake2.js";
import { CML } from "@lucid-evolution/lucid";
import {
  BuilderInvariantError,
  computeHash32,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  decodeSingleCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  outRefToCbor,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  type Redeemer,
} from "../src/index.js";

const pubkeyAddress =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";
const scriptBytes = Buffer.from("4e4d01000033222220051200120011", "hex");

const fakeProvider: MidgardProvider = {
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

const scriptAddress = (scriptHash: string): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
  )
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  address: string,
  assets: Readonly<Record<string, bigint>>,
  options: { readonly scriptRef?: InstanceType<typeof CML.Script> } = {},
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets, options),
  });

const plutusV3Hash = (bytes = scriptBytes): string =>
  CML.Script.new_plutus_v3(CML.PlutusV3Script.from_raw_bytes(bytes))
    .hash()
    .to_hex();

const midgardV1Hash = (bytes = scriptBytes): string =>
  Buffer.from(
    blake2b(Buffer.concat([Buffer.from([0x80]), bytes]), { dkLen: 28 }),
  ).toString("hex");

const redeemer = (value: bigint, mem = 1n, steps = 2n): Redeemer => ({
  data: CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString())),
  exUnits: { mem, steps },
});

const redeemerPointers = (preimageCbor: Uint8Array): string[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    return decoded.map((entry) => {
      const [tag, index] = entry as [bigint, bigint];
      return `${tag.toString()}:${index.toString()}`;
    });
  }
  if (!(decoded instanceof Map)) {
    return [];
  }
  return [...decoded.keys()].map((key) => {
    const [tag, index] = key as [bigint, bigint];
    return `${tag.toString()}:${index.toString()}`;
  });
};

describe("script and redeemer builders", () => {
  it("derives spend redeemer indexes from sorted spent outrefs", async () => {
    const hash = plutusV3Hash();
    const address = scriptAddress(hash);
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: scriptBytes,
      })
      .collectFrom(
        [
          makeUtxo(makeOutRef(0x22, 0), address, { lovelace: 2_000_000n }),
          makeUtxo(makeOutRef(0x11, 0), address, { lovelace: 1_000_000n }),
        ],
        redeemer(42n),
      )
      .pay.ToAddress(pubkeyAddress, { lovelace: 3_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
      "0:1",
    ]);
    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "PlutusV3",
      ]),
    );
  });

  it("derives mint and observer redeemer indexes and preimages", async () => {
    const hash = plutusV3Hash();
    const unit = `${hash}abcd`;
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: scriptBytes,
      })
      .collectFrom([
        makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
      ])
      .mintAssets(hash, { abcd: 5n }, redeemer(7n))
      .observe(hash, redeemer(8n))
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n, [unit]: 5n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "1:0",
      "3:0",
    ]);
    expect(tx.body.mintPreimageCbor).not.toEqual(EMPTY_CBOR_LIST);
    expect(tx.body.requiredObserversPreimageCbor).not.toEqual(EMPTY_CBOR_LIST);
  });

  it("attaches typed PlutusV3 and MidgardV1 observer validators explicitly", async () => {
    const plutusHash = plutusV3Hash();
    const midgardHash = midgardV1Hash();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.ObserverValidator({
        language: "PlutusV3",
        script: scriptBytes,
      })
      .attachObserverScript({
        language: "MidgardV1",
        script: scriptBytes,
      })
      .collectFrom([
        makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
      ])
      .observe(midgardHash, redeemer(8n))
      .observe(plutusHash, redeemer(7n))
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);
    const requiredObservers = (
      decodeSingleCbor(tx.body.requiredObserversPreimageCbor) as Uint8Array[]
    ).map((hash) => Buffer.from(hash).toString("hex"));

    expect(requiredObservers).toEqual([midgardHash, plutusHash].sort());
    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "3:0",
      "3:1",
    ]);
    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "MidgardV1",
        "PlutusV3",
      ]),
    );
  });

  it("does not let observer validator attachment create observer execution implicitly", async () => {
    const hash = plutusV3Hash();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .attach.ObserverValidator({
          language: "PlutusV3",
          script: scriptBytes,
        })
        .collectFrom([
          makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
        ])
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);

    expect(() =>
      midgard.newTx().attach.ObserverValidator({
        language: "PlutusV2",
        script: scriptBytes,
      } as never),
    ).toThrow(BuilderInvariantError);
    await expect(
      midgard
        .newTx()
        .collectFrom([
          makeUtxo(makeOutRef(0x22), pubkeyAddress, { lovelace: 2_000_000n }),
        ])
        .observe(hash, redeemer(9n))
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("strictly validates typed validator languages and wrapped CML scripts", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const wrappedPlutusV2 = CML.Script.new_plutus_v2(
      CML.PlutusV2Script.from_raw_bytes(scriptBytes),
    );
    const hash = wrappedPlutusV3.hash().to_hex();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.ObserverValidator({
        language: "PlutusV3",
        script: wrappedPlutusV3,
      })
      .collectFrom([
        makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
      ])
      .observe(hash, redeemer(10n))
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "PlutusV3",
      ]),
    );
    expect(() =>
      midgard.newTx().attach.ObserverValidator({
        language: "PlutusV3",
        script: wrappedPlutusV2,
      }),
    ).toThrow(BuilderInvariantError);
    expect(() =>
      midgard.newTx().attach.ObserverValidator({
        language: "Unknown",
        script: scriptBytes,
      } as never),
    ).toThrow(BuilderInvariantError);
    expect(() =>
      midgard.newTx().attach.SpendingValidator({
        language: "PlutusV2",
        script: scriptBytes,
      } as never),
    ).toThrow(BuilderInvariantError);
    expect(() =>
      midgard.newTx().attach.MintingPolicy({
        language: "Unknown",
        script: scriptBytes,
      } as never),
    ).toThrow(BuilderInvariantError);
  });

  it("resolves PlutusV3 executions from reference script inputs without inline witnesses", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const hash = wrappedPlutusV3.hash().to_hex();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .readFrom([
        makeUtxo(
          makeOutRef(0x55),
          pubkeyAddress,
          { lovelace: 2_000_000n },
          { scriptRef: wrappedPlutusV3 },
        ),
      ])
      .collectFrom(
        [
          makeUtxo(makeOutRef(0x11), scriptAddress(hash), {
            lovelace: 2_000_000n,
          }),
        ],
        redeemer(11n),
      )
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(tx.witnessSet.scriptTxWitsPreimageCbor).toEqual(EMPTY_CBOR_LIST);
    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
    ]);
    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "PlutusV3",
      ]),
    );
  });

  it("uses trusted reference script metadata when local reference output bytes do not include a script ref", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const hash = wrappedPlutusV3.hash().to_hex();
    const referenceOutRef = makeOutRef(0x56);
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .readFrom([
        makeUtxo(referenceOutRef, pubkeyAddress, { lovelace: 2_000_000n }),
      ])
      .attach.ReferenceScriptMetadata({
        ...referenceOutRef,
        language: "PlutusV3",
        scriptHash: hash,
      })
      .collectFrom(
        [
          makeUtxo(makeOutRef(0x12), scriptAddress(hash), {
            lovelace: 2_000_000n,
          }),
        ],
        redeemer(12n),
      )
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(tx.witnessSet.scriptTxWitsPreimageCbor).toEqual(EMPTY_CBOR_LIST);
    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "PlutusV3",
      ]),
    );
  });

  it("rejects trusted reference script metadata that disagrees with local script-ref bytes", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const plutusHash = wrappedPlutusV3.hash().to_hex();
    const referenceOutRef = makeOutRef(0x57);
    const referenceUtxo = makeUtxo(
      referenceOutRef,
      pubkeyAddress,
      { lovelace: 2_000_000n },
      { scriptRef: wrappedPlutusV3 },
    );
    const scriptCborHash = computeHash32(
      Buffer.from(wrappedPlutusV3.to_cbor_bytes()),
    ).toString("hex");
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .readFrom([referenceUtxo], {
          trustedReferenceScripts: [
            {
              ...referenceOutRef,
              language: "MidgardV1",
              scriptHash: plutusHash,
              scriptCborHash,
            },
          ],
        })
        .collectFrom(
          [
            makeUtxo(makeOutRef(0x13), scriptAddress(plutusHash), {
              lovelace: 2_000_000n,
            }),
          ],
          redeemer(13n),
        )
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);

    await expect(
      midgard
        .newTx()
        .readFrom([referenceUtxo], {
          trustedReferenceScripts: [
            {
              ...referenceOutRef,
              language: "PlutusV3",
              scriptHash: plutusHash,
              scriptCborHash: "00".repeat(32),
            },
          ],
        })
        .collectFrom(
          [
            makeUtxo(makeOutRef(0x14), scriptAddress(plutusHash), {
              lovelace: 2_000_000n,
            }),
          ],
          redeemer(14n),
        )
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("rejects using a PlutusV3 reference script in the MidgardV1 domain", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const midgardHash = midgardV1Hash();
    const referenceOutRef = makeOutRef(0x58);
    const referenceUtxo = makeUtxo(
      referenceOutRef,
      pubkeyAddress,
      { lovelace: 2_000_000n },
      { scriptRef: wrappedPlutusV3 },
    );
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .readFrom([referenceUtxo])
        .collectFrom([
          makeUtxo(makeOutRef(0x15), pubkeyAddress, {
            lovelace: 2_000_000n,
          }),
        ])
        .receiveRedeemer(midgardHash, redeemer(15n))
        .pay.ToProtectedAddress(scriptAddress(midgardHash), {
          lovelace: 2_000_000n,
        })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);

    await expect(
      midgard
        .newTx()
        .readFrom([referenceUtxo], {
          trustedReferenceScripts: [
            {
              ...referenceOutRef,
              language: "MidgardV1",
              scriptHash: midgardHash,
            },
          ],
        })
        .collectFrom([
          makeUtxo(makeOutRef(0x16), pubkeyAddress, {
            lovelace: 2_000_000n,
          }),
        ])
        .receiveRedeemer(midgardHash, redeemer(16n))
        .pay.ToProtectedAddress(scriptAddress(midgardHash), {
          lovelace: 2_000_000n,
        })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("rejects one typed PlutusV3 reference script being reused in the MidgardV1 domain", async () => {
    const wrappedPlutusV3 = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(scriptBytes),
    );
    const plutusHash = wrappedPlutusV3.hash().to_hex();
    const midgardHash = midgardV1Hash();
    const referenceOutRef = makeOutRef(0x59);
    const referenceUtxo = makeUtxo(
      referenceOutRef,
      pubkeyAddress,
      { lovelace: 2_000_000n },
      { scriptRef: wrappedPlutusV3 },
    );
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .readFrom([referenceUtxo], {
          trustedReferenceScripts: [
            {
              ...referenceOutRef,
              language: "MidgardV1",
              scriptHash: midgardHash,
            },
          ],
        })
        .collectFrom(
          [
            makeUtxo(makeOutRef(0x17), scriptAddress(plutusHash), {
              lovelace: 2_000_000n,
            }),
          ],
          redeemer(17n),
        )
        .receiveRedeemer(midgardHash, redeemer(18n))
        .pay.ToProtectedAddress(scriptAddress(midgardHash), {
          lovelace: 2_000_000n,
        })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("derives MidgardV1 receive redeemers from protected script outputs", async () => {
    const hash = midgardV1Hash();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.Script({
        kind: "midgard-v1",
        language: "MidgardV1",
        script: scriptBytes,
      })
      .collectFrom([
        makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
      ])
      .receiveRedeemer(hash, redeemer(99n))
      .pay.ToProtectedAddress(scriptAddress(hash), { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "6:0",
    ]);
    expect(tx.body.scriptIntegrityHash).toEqual(
      computeScriptIntegrityHashForLanguages(tx.witnessSet.redeemerTxWitsRoot, [
        "MidgardV1",
      ]),
    );
  });

  it("rejects PlutusV3 receive and extraneous non-native witnesses", async () => {
    const hash = plutusV3Hash();
    const base = (
      await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      })
    )
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: scriptBytes,
      })
      .collectFrom([
        makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 2_000_000n }),
      ]);

    await expect(
      base
        .receiveRedeemer(hash, redeemer(99n))
        .pay.ToProtectedAddress(scriptAddress(hash), { lovelace: 2_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);

    await expect(
      base.pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n }).complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("rejects unconsumed redeemer intents and ineffective mint redeemers", async () => {
    const hash = plutusV3Hash();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .collectFrom(
          [makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 1_000_000n })],
          redeemer(1n),
        )
        .pay.ToAddress(pubkeyAddress, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);

    await expect(
      midgard
        .newTx()
        .attach.Script({
          kind: "plutus-v3",
          language: "PlutusV3",
          script: scriptBytes,
        })
        .collectFrom([
          makeUtxo(makeOutRef(0x22), pubkeyAddress, { lovelace: 1_000_000n }),
        ])
        .mintAssets(hash, { abcd: 1n }, redeemer(2n))
        .mintAssets(hash, { abcd: -1n })
        .pay.ToAddress(pubkeyAddress, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("rejects duplicate observer intents before redeemer data can be dropped", async () => {
    const hash = plutusV3Hash();
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const base = midgard.newTx().observe(hash, redeemer(1n));

    expect(() => base.observe(hash, redeemer(2n))).toThrow(
      BuilderInvariantError,
    );

    const emptyFirst = midgard.newTx().observe(hash);
    expect(() => emptyFirst.observe(hash, redeemer(3n))).toThrow(
      BuilderInvariantError,
    );
  });

  it("rejects datum witnesses because native transactions require inline datums", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .attach.Datum(redeemer(1n).data)
        .collectFrom([
          makeUtxo(makeOutRef(0x11), pubkeyAddress, { lovelace: 1_000_000n }),
        ])
        .pay.ToAddress(pubkeyAddress, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("keeps no-script and native-only script integrity at EMPTY_NULL_ROOT", async () => {
    const keyHash = CML.PrivateKey.generate_ed25519().to_public().hash();
    const native = CML.NativeScript.new_script_pubkey(keyHash);
    const nativeAddress = scriptAddress(native.hash().to_hex());
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    const completed = await midgard
      .newTx()
      .attach.NativeScript(native)
      .collectFrom([
        makeUtxo(makeOutRef(0x11), nativeAddress, { lovelace: 1_000_000n }),
      ])
      .pay.ToAddress(pubkeyAddress, { lovelace: 1_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFull(completed.txCbor);

    expect(tx.body.scriptIntegrityHash).toEqual(EMPTY_NULL_ROOT);
    expect(tx.witnessSet.redeemerTxWitsPreimageCbor).toEqual(EMPTY_CBOR_LIST);
  });

  it("rejects unused native script witnesses instead of building node-invalid bytes", async () => {
    const keyHash = CML.PrivateKey.generate_ed25519().to_public().hash();
    const native = CML.NativeScript.new_script_pubkey(keyHash);
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .attach.NativeScript(native)
        .collectFrom([
          makeUtxo(makeOutRef(0x12), pubkeyAddress, { lovelace: 1_000_000n }),
        ])
        .pay.ToAddress(pubkeyAddress, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });
});
