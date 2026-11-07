import {
  computeHash32,
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardRedeemerWitnessFieldPreimage,
  decodeMidgardVersionedScriptListPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "../src/index.js";

const pubkeyAddress =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";
const rawPlutusV3Script = Buffer.from("010100480001", "hex");
// A single CBOR byte string containing the original Flat program.
const storedPlutusV3Script = Buffer.from("46010100480001", "hex");
const plutusV3Hash = hashMidgardVersionedScript({
  language: "PlutusV3",
  scriptBytes: storedPlutusV3Script,
});
const midgardHash = hashMidgardVersionedScript({
  language: "MidgardV1",
  scriptBytes: storedPlutusV3Script,
});

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
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "production",
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
    endpoint: "memory://canonical-v1",
    protocolInfoSource: "node",
  }),
};

const dummyRedeemer = {
  data: Buffer.from([0x80]),
  exUnits: { mem: 1n, steps: 1n },
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
  options: Parameters<typeof encodeMidgardTxOutput>[2] = {},
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets, options),
  });

const makeReferenceUtxo = (ref: OutRef, script: Uint8Array): MidgardUtxo =>
  makeUtxo(
    ref,
    pubkeyAddress,
    { lovelace: 3_000_000n },
    {
      scriptRef: {
        type: "MidgardV1",
        script: Buffer.from(script).toString("hex"),
      },
    },
  );

/** `purpose_tag:index` per redeemer, read back through the §5.3 field-8 decoder. */
const redeemerPointers = (preimageCbor: Uint8Array): readonly string[] =>
  decodeMidgardRedeemerWitnessFieldPreimage(preimageCbor).map(
    (witness) =>
      `${String(MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose])}:${witness.index.toString(10)}`,
  );

const expectScriptIntegrity = (
  tx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>,
  languages: readonly ("PlutusV3" | "MidgardV1")[],
): void => {
  expect(tx.body.scriptIntegrityHash).toEqual(
    computeScriptIntegrityHashForLanguages(
      deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet).redeemerTxWitsHash,
      languages,
    ),
  );
};

describe("V1 script and mint feature surface", () => {
  it("retains mint/burn, scripts, observers, and receive redeemers", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const builder = midgard
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: Buffer.from([0x01]),
      })
      .mintAssets("00".repeat(28), { abcd: 1n }, dummyRedeemer)
      .observe("11".repeat(28), dummyRedeemer)
      .receiveRedeemer("22".repeat(28), dummyRedeemer);

    expect(builder.config().midgardNativeTxVersion).toBe(1);
    expect(builder.snapshot().scripts).toMatchObject({
      scripts: [{ language: "PlutusV3" }],
      mints: [{ policyId: "00".repeat(28) }],
      observers: [{ scriptHash: "11".repeat(28) }],
      receiveRedeemers: [{ scriptHash: "22".repeat(28) }],
    });
  });

  it("completes inline PlutusV3 spends with canonical witnesses and identity", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spendAddress = scriptAddress(plutusV3Hash);
    const completed = await midgard
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: storedPlutusV3Script,
      })
      .collectFrom(
        [
          makeUtxo(makeOutRef(0x22), spendAddress, {
            lovelace: 2_000_000n,
          }),
          makeUtxo(makeOutRef(0x11), spendAddress, {
            lovelace: 1_000_000n,
          }),
        ],
        dummyRedeemer,
      )
      .pay.ToAddress(pubkeyAddress, { lovelace: 3_000_000n })
      .complete({ fee: 0n });
    const tx = decodeMidgardNativeTxFullFromCanonicalCbor(completed.txCbor);

    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
      "0:1",
    ]);
    expect(
      decodeMidgardVersionedScriptListPreimage(
        tx.witnessSet.scriptTxWitsPreimageCbor,
      ),
    ).toEqual([{ language: "PlutusV3", scriptBytes: storedPlutusV3Script }]);
    expectScriptIntegrity(tx, ["PlutusV3"]);
    expect(completed.txId).toEqual(computeMidgardNativeTxId(tx));
    expect(completed.toHash()).toBe(completed.txIdHex);
  });

  describe("redeemer data encoding", () => {
    const cmlInteger = (value: number): CML.PlutusData =>
      CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString()));
    const cmlList = (items: readonly CML.PlutusData[]): CML.PlutusDataList => {
      const list = CML.PlutusDataList.new();
      for (const item of items) list.add(item);
      return list;
    };
    const cmlMap = (pairs: readonly [number, number][]): CML.PlutusData => {
      const map = CML.PlutusMap.new();
      for (const [key, value] of pairs)
        map.set(cmlInteger(key), cmlInteger(value));
      return CML.PlutusData.new_map(map);
    };

    const committedRedeemerData = async (
      data: CML.PlutusData | Uint8Array | string,
    ): Promise<string> => {
      const midgard = await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      });
      const completed = await midgard
        .newTx()
        .attach.Script({
          kind: "plutus-v3",
          language: "PlutusV3",
          script: storedPlutusV3Script,
        })
        .collectFrom(
          [
            makeUtxo(makeOutRef(0x41), scriptAddress(plutusV3Hash), {
              lovelace: 2_000_000n,
            }),
          ],
          { data, exUnits: { mem: 1n, steps: 1n } },
        )
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete({ fee: 0n });
      const tx = decodeMidgardNativeTxFullFromCanonicalCbor(completed.txCbor);
      const [witness, ...rest] = decodeMidgardRedeemerWitnessFieldPreimage(
        tx.witnessSet.redeemerTxWitsPreimageCbor,
      );
      expect(rest).toEqual([]);
      return Buffer.from(witness!.redeemerCbor).toString("hex");
    };

    // CML spells each of these definite-length (d8798101, 8101, a single
    // 70-byte string); the committed bytes must be `serialiseData`'s.
    it.each([
      [
        "constructor",
        () =>
          CML.PlutusData.new_constr_plutus_data(
            CML.ConstrPlutusData.new(0n, cmlList([cmlInteger(1)])),
          ),
        "d8799f01ff",
      ],
      [
        "list",
        () => CML.PlutusData.new_list(cmlList([cmlInteger(1)])),
        "9f01ff",
      ],
      [
        "map in insertion order",
        () =>
          cmlMap([
            [2, 1],
            [1, 2],
          ]),
        "a202010102",
      ],
      [
        "70-byte string",
        () => CML.PlutusData.new_bytes(new Uint8Array(70).fill(1)),
        `5f5840${"01".repeat(64)}46${"01".repeat(6)}ff`,
      ],
    ] as const)(
      "commits a CML %s in its serialiseData form",
      async (_label, build, expected) => {
        await expect(committedRedeemerData(build())).resolves.toBe(expected);
      },
    );

    it.each(["d8799f01ff", "9f01ff", "a202010102", "80", "d87980"])(
      "passes canonical caller bytes %s through unchanged",
      async (hex) => {
        await expect(committedRedeemerData(hex)).resolves.toBe(hex);
        await expect(
          committedRedeemerData(Buffer.from(hex, "hex")),
        ).resolves.toBe(hex);
      },
    );

    it.each(["d8798101", "8101", "bf0102ff", "1801", "60"])(
      "refuses non-canonical caller bytes %s rather than rewriting them",
      async (hex) => {
        await expect(committedRedeemerData(hex)).rejects.toThrow(
          /Redeemer data bytes must be the serialiseData encoding/u,
        );
        await expect(
          committedRedeemerData(Buffer.from(hex, "hex")),
        ).rejects.toThrow(
          /Redeemer data bytes must be the serialiseData encoding/u,
        );
      },
    );
  });

  it("completes reference-script spends using the exact stored Flat identity", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x11), scriptAddress(midgardHash), {
      lovelace: 2_000_000n,
    });
    const reference = makeReferenceUtxo(makeOutRef(0x22), storedPlutusV3Script);
    const completed = await midgard
      .newTx()
      .collectFrom([spend], dummyRedeemer)
      .readFrom([reference], {
        trustedReferenceScripts: [
          {
            ...reference,
            language: "MidgardV1",
            scriptHash: midgardHash,
            scriptCborHash: computeHash32(
              encodeMidgardVersionedScript({
                language: "MidgardV1",
                scriptBytes: storedPlutusV3Script,
              }),
            ).toString("hex"),
          },
        ],
      })
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });
    const tx = decodeMidgardNativeTxFullFromCanonicalCbor(completed.txCbor);
    const referenceKey = Buffer.from(outRefToCbor(reference)).toString("hex");

    expect(tx.witnessSet.scriptTxWitsPreimageCbor).toEqual(EMPTY_CBOR_LIST);
    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
    ]);
    expectScriptIntegrity(tx, ["MidgardV1"]);
    expect(completed.resolvedReferenceOutputsByOutRef?.size).toBe(1);
    expect(
      Buffer.from(
        completed.resolvedReferenceOutputsByOutRef?.get(referenceKey) ?? [],
      ),
    ).toEqual(Buffer.from(reference.cbor!.output!));
    expect(completed.txId).toEqual(computeMidgardNativeTxId(tx));
  });

  it.each([
    ["language", { language: "PlutusV3" as const }, "language"],
    ["script hash", { scriptHash: "ff".repeat(28) }, "hash"],
    ["script CBOR hash", { scriptCborHash: "ff".repeat(32) }, "scriptCborHash"],
  ] as const)(
    "rejects trusted reference metadata with a mismatched %s",
    async (_label, mismatch, field) => {
      const midgard = await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      });
      const reference = makeReferenceUtxo(
        makeOutRef(0x42),
        storedPlutusV3Script,
      );
      const spend = makeUtxo(makeOutRef(0x41), scriptAddress(midgardHash), {
        lovelace: 2_000_000n,
      });
      await expect(
        midgard
          .newTx()
          .collectFrom([spend], dummyRedeemer)
          .readFrom([reference], {
            trustedReferenceScripts: [
              {
                ...reference,
                language: "MidgardV1",
                scriptHash: midgardHash,
                ...mismatch,
              },
            ],
          })
          .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
          .complete({ fee: 0n }),
      ).rejects.toThrow(
        `Reference script metadata ${field} does not match local script reference`,
      );
    },
  );

  it("rejects a reference script whose stored bytes do not match the spend credential", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x31), scriptAddress(midgardHash), {
      lovelace: 2_000_000n,
    });
    const rawReference = makeReferenceUtxo(makeOutRef(0x33), rawPlutusV3Script);
    await expect(
      midgard
        .newTx()
        .collectFrom([spend], dummyRedeemer)
        .readFrom([rawReference])
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete({ fee: 0n }),
    ).rejects.toThrow(/Missing script source for spend/u);
  });
});
