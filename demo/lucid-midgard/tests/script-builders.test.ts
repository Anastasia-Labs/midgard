import {
  computeMidgardNativeTxIdV1,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  decodeSingleCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  hashMidgardVersionedScript,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { buildMidgardCanonicalCekProgramV1 } from "@al-ft/midgard-validation/cek-program";
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
const canonicalPlutusV3 = buildMidgardCanonicalCekProgramV1(rawPlutusV3Script);
const canonicalEnvelope = Buffer.from(canonicalPlutusV3.envelopeCbor);
const canonicalProgramMaterial = [...canonicalPlutusV3.material.values()];
const sortedCanonicalProgramMaterial = [...canonicalProgramMaterial].sort(
  (left, right) =>
    Buffer.compare(Buffer.from(left.root), Buffer.from(right.root)),
);
const plutusV3Hash = hashMidgardVersionedScript({
  language: "PlutusV3",
  scriptBytes: canonicalEnvelope,
});
const midgardV1Hash = hashMidgardVersionedScript({
  language: "MidgardV1",
  scriptBytes: canonicalEnvelope,
});

const fakeProvider: MidgardProvider = {
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

const redeemerPointers = (preimageCbor: Uint8Array): readonly string[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (!Array.isArray(decoded)) {
    throw new Error("redeemer preimage must decode to an array");
  }
  return decoded.map((entry) => {
    if (!Array.isArray(entry) || entry.length < 2) {
      throw new Error("redeemer entry must contain a pointer");
    }
    return `${String(entry[0])}:${String(entry[1])}`;
  });
};

const expectScriptIntegrity = (
  tx: ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor>,
  languages: readonly ("PlutusV3" | "MidgardV1")[],
): void => {
  expect(tx.body.scriptIntegrityHash).toEqual(
    computeScriptIntegrityHashForLanguages(
      deriveMidgardNativeTxWitnessSetCompactV1(tx.witnessSet)
        .redeemerTxWitsHash,
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
        script: rawPlutusV3Script,
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
    const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(completed.txCbor);

    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
      "0:1",
    ]);
    expect(
      decodeMidgardVersionedScriptListPreimage(
        tx.witnessSet.scriptTxWitsPreimageCbor,
      ),
    ).toEqual([{ language: "PlutusV3", scriptBytes: canonicalEnvelope }]);
    expect(completed.programMaterial).toEqual(sortedCanonicalProgramMaterial);
    expectScriptIntegrity(tx, ["PlutusV3"]);
    expect(completed.txId).toEqual(computeMidgardNativeTxIdV1(tx));
    expect(completed.toHash()).toBe(completed.txIdHex);
  });

  it("completes canonical historical reference scripts with exact material", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x11), scriptAddress(midgardV1Hash), {
      lovelace: 2_000_000n,
    });
    const reference = makeReferenceUtxo(
      makeOutRef(0x22),
      canonicalPlutusV3.envelopeCbor,
    );
    const completed = await midgard
      .newTx()
      .collectFrom([spend], dummyRedeemer)
      .readFrom([reference])
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n, programMaterial: canonicalProgramMaterial });
    const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(completed.txCbor);
    const referenceKey = Buffer.from(outRefToCbor(reference)).toString("hex");

    expect(tx.witnessSet.scriptTxWitsPreimageCbor).toEqual(EMPTY_CBOR_LIST);
    expect(redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor)).toEqual([
      "0:0",
    ]);
    expectScriptIntegrity(tx, ["MidgardV1"]);
    expect(completed.programMaterial).toEqual(sortedCanonicalProgramMaterial);
    expect(completed.resolvedReferenceOutputsByOutRef?.size).toBe(1);
    expect(
      Buffer.from(
        completed.resolvedReferenceOutputsByOutRef?.get(referenceKey) ?? [],
      ),
    ).toEqual(Buffer.from(reference.cbor!.output!));
    expect(completed.txId).toEqual(computeMidgardNativeTxIdV1(tx));
  });

  it("rejects missing, corrupted, and raw historical reference material", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x31), scriptAddress(midgardV1Hash), {
      lovelace: 2_000_000n,
    });
    const reference = makeReferenceUtxo(
      makeOutRef(0x32),
      canonicalPlutusV3.envelopeCbor,
    );
    const builder = midgard
      .newTx()
      .collectFrom([spend], dummyRedeemer)
      .readFrom([reference])
      .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n });

    await expect(builder.complete({ fee: 0n })).rejects.toThrow(
      /Incomplete or mismatched CEK program material/u,
    );
    const corrupted = canonicalProgramMaterial.map((entry, index) =>
      index === 0
        ? { ...entry, preimage: Buffer.concat([entry.preimage, Buffer.of(0)]) }
        : entry,
    );
    await expect(
      builder.complete({ fee: 0n, programMaterial: corrupted }),
    ).rejects.toThrow(/Invalid canonical CEK program material/u);

    const rawReference = makeReferenceUtxo(makeOutRef(0x33), rawPlutusV3Script);
    await expect(
      midgard
        .newTx()
        .collectFrom([spend], dummyRedeemer)
        .readFrom([rawReference])
        .pay.ToAddress(pubkeyAddress, { lovelace: 2_000_000n })
        .complete({ fee: 0n, programMaterial: canonicalProgramMaterial }),
    ).rejects.toThrow(
      /V1 reference script must contain a canonical CEK program envelope/u,
    );
  });
});
