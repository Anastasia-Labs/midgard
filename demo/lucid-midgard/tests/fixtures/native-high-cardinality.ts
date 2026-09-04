import type { MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardMintFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeSingleCbor,
  hashMidgardVersionedScript,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation/cek-program";
import { CML } from "@lucid-evolution/lucid";

import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  type Redeemer,
  walletFromSeedPhrase,
} from "../../src/index.js";
import {
  deriveNativeTxFixtureFacets,
  type NativeTxFixtureEnvelope,
} from "./native-tx-fixture-shape.js";

export const HIGH_CARDINALITY_FIXTURE_NAME =
  "high-cardinality-combined-v1" as const;

export const HIGH_CARDINALITY_COUNTS = {
  spendInputs: 8,
  referenceInputs: 4,
  outputs: 12,
  mintPolicies: 6,
  spendRedeemers: 3,
  mintRedeemers: 6,
  observerRedeemers: 2,
  receiveRedeemers: 2,
  totalRedeemers: 13,
} as const;

export const HIGH_CARDINALITY_PRODUCER =
  "pnpm --dir demo/lucid-midgard run fixtures:native-high-cardinality:sync";

export type HighCardinalityNativeTxFixture = NativeTxFixtureEnvelope<{
  readonly name: typeof HIGH_CARDINALITY_FIXTURE_NAME;
  readonly producer: string;
  readonly counts: typeof HIGH_CARDINALITY_COUNTS;
}>;
const seedPhrase =
  "test test test test test test test test test test test junk";

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
    endpoint: "memory://high-cardinality-fixture",
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

const scriptOrdinal = (domain: string, index: number): number => {
  const base = {
    spend: 0,
    mint: 3,
    observer: 9,
    receive: 11,
    "output-plutus-ref": 13,
    "output-midgard-ref": 14,
  }[domain];
  if (base === undefined) {
    throw new Error(`Unknown fixture script domain: ${domain}`);
  }
  return base + index;
};

const scriptBytes = (domain: string, index: number): Buffer => {
  const ordinal = scriptOrdinal(domain, index);
  if (ordinal > 63) {
    throw new Error(`Fixture script ordinal exceeds the compact integer form`);
  }
  // UPLC 1.1.0, constant integer. Distinct constants keep every attached
  // script/policy hash unique while remaining valid for strict V1 wrapping.
  const encoded = [
    "010100480001",
    "010100480081",
    "010100480101",
    "010100480181",
    "010100480201",
    "010100480281",
    "010100480301",
    "010100480381",
    "010100480401",
    "010100480481",
    "010100480501",
    "010100480581",
    "010100480601",
    "010100480681",
    "010100480701",
  ][ordinal];
  if (encoded === undefined) {
    throw new Error(`Fixture script ordinal is not encoded`);
  }
  return Buffer.from(encoded, "hex");
};

type CanonicalScript = {
  readonly rawBytes: Buffer;
  readonly bytes: Buffer;
  readonly hash: string;
  readonly material: readonly MidgardCekProgramMaterialEntry[];
};

const canonicalScript = (
  language: "PlutusV3" | "MidgardV1",
  raw: Uint8Array,
): CanonicalScript => {
  const canonical = buildMidgardCanonicalCekProgram(raw);
  const bytes = Buffer.from(canonical.envelopeCbor);
  return {
    rawBytes: Buffer.from(raw),
    bytes,
    hash: hashMidgardVersionedScript({ language, scriptBytes: bytes }),
    material: [...canonical.material.values()],
  };
};

const mergeProgramMaterial = (
  scripts: readonly CanonicalScript[],
): readonly MidgardCekProgramMaterialEntry[] => {
  const material = new Map<string, MidgardCekProgramMaterialEntry>();
  for (const script of scripts) {
    for (const entry of script.material) {
      const root = Buffer.from(entry.root).toString("hex");
      const prior = material.get(root);
      if (
        prior !== undefined &&
        (prior.kind !== entry.kind ||
          !Buffer.from(prior.preimage).equals(entry.preimage))
      ) {
        throw new Error(`Conflicting canonical fixture material root: ${root}`);
      }
      material.set(root, entry);
    }
  }
  return [...material.values()];
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

const redeemer = (value: bigint, mem = 1n, steps = 2n): Redeemer => ({
  data: CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString())),
  exUnits: { mem, steps },
});

const asArray = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must decode to an array`);
  }
  return value;
};

const makeAssetName = (prefix: number, index: number): string =>
  `${prefix.toString(16).padStart(2, "0")}${index
    .toString(16)
    .padStart(2, "0")}`;

export const buildHighCardinalityNativeTxFixture =
  async (): Promise<HighCardinalityNativeTxFixture> => {
    const wallet = walletFromSeedPhrase(seedPhrase, {
      network: "Preview",
      expectedNetworkId: 0,
    });
    const walletAddress = await wallet.address();
    const walletKeyHash = await wallet.keyHash();

    const spendScripts = [0, 1, 2].map((index) => {
      return canonicalScript("PlutusV3", scriptBytes("spend", index));
    });
    const mintScripts = [0, 1, 2, 3, 4, 5].map((index) => {
      return canonicalScript("PlutusV3", scriptBytes("mint", index));
    });
    const observerScripts = [0, 1].map((index) => {
      return canonicalScript("PlutusV3", scriptBytes("observer", index));
    });
    const receiveScripts = [0, 1].map((index) => {
      return canonicalScript("MidgardV1", scriptBytes("receive", index));
    });
    const outputPlutusScript = canonicalScript(
      "PlutusV3",
      scriptBytes("output-plutus-ref", 0),
    );
    const outputMidgardScript = canonicalScript(
      "MidgardV1",
      scriptBytes("output-midgard-ref", 0),
    );
    const canonicalScripts = [
      ...spendScripts,
      ...mintScripts,
      ...observerScripts,
      ...receiveScripts,
      outputPlutusScript,
      outputMidgardScript,
    ];
    const programMaterial = mergeProgramMaterial(canonicalScripts);

    let builder = (
      await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      })
    ).newTx();

    for (const script of [
      ...spendScripts,
      ...mintScripts,
      ...observerScripts,
    ]) {
      builder = builder.attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: script.rawBytes,
      });
    }
    for (const script of receiveScripts) {
      builder = builder.attach.Script({
        kind: "midgard-v1",
        language: "MidgardV1",
        script: script.rawBytes,
      });
    }

    const pubKeyInputs = [0x31, 0x11, 0x27, 0x05, 0x19].map((byte) =>
      makeUtxo(makeOutRef(byte), walletAddress, { lovelace: 10_000_000n }),
    );
    const scriptInputs = spendScripts.map((script, index) =>
      makeUtxo(
        makeOutRef([0x22, 0x09, 0x3a][index]!),
        scriptAddress(script.hash),
        { lovelace: 10_000_000n },
      ),
    );
    const referenceInputUtxos = [0x70, 0x45, 0x66, 0x52].map((byte) =>
      makeUtxo(makeOutRef(byte), walletAddress, { lovelace: 2_000_000n }),
    );

    builder = builder
      .collectFrom(pubKeyInputs)
      .collectFrom([scriptInputs[0]!], redeemer(101n))
      .collectFrom([scriptInputs[1]!], redeemer(102n))
      .collectFrom([scriptInputs[2]!], redeemer(103n))
      .readFrom(referenceInputUtxos)
      .addSignerKey(walletKeyHash);

    const mintedOutputs: Record<string, bigint>[] = [];
    for (let index = 0; index < mintScripts.length; index += 1) {
      const script = mintScripts[index]!;
      const firstAssetName = makeAssetName(0xa0 + index, index);
      const secondAssetName = makeAssetName(0xb0 + index, index);
      const firstUnit = `${script.hash}${firstAssetName}`;
      const secondUnit = `${script.hash}${secondAssetName}`;
      const firstQuantity = BigInt(index + 1);
      const secondQuantity = BigInt(index + 11);

      builder = builder.mintAssets(
        script.hash,
        {
          [firstAssetName]: firstQuantity,
          [secondAssetName]: secondQuantity,
        },
        redeemer(BigInt(200 + index)),
      );
      mintedOutputs.push({
        lovelace: 4_000_000n,
        [firstUnit]: firstQuantity,
        [secondUnit]: secondQuantity,
      });
    }

    for (const script of observerScripts) {
      builder = builder.observe(script.hash, redeemer(300n));
    }
    for (let index = 0; index < receiveScripts.length; index += 1) {
      builder = builder.receiveRedeemer(
        receiveScripts[index]!.hash,
        redeemer(BigInt(400 + index)),
      );
    }

    for (const value of mintedOutputs) {
      builder = builder.pay.ToAddress(walletAddress, value);
    }
    builder = builder.pay
      .ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          datum: CML.PlutusData.new_integer(CML.BigInteger.from_str("77")),
        },
      )
      .pay.ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          scriptRef: {
            type: "PlutusV3",
            script: outputPlutusScript.rawBytes.toString("hex"),
          },
        },
      )
      .pay.ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          scriptRef: {
            type: "MidgardV1",
            script: outputMidgardScript.rawBytes.toString("hex"),
          },
        },
      )
      .pay.ToProtectedAddress(scriptAddress(receiveScripts[0]!.hash), {
        lovelace: 4_000_000n,
      })
      .pay.ToProtectedAddress(scriptAddress(receiveScripts[1]!.hash), {
        lovelace: 4_000_000n,
      })
      .pay.ToAddress(walletAddress, { lovelace: 36_000_000n });

    const completed = await builder.complete({ programMaterial });
    const signed = await completed.sign(wallet);
    const tx = decodeMidgardNativeTxFullFromCanonicalCbor(signed.txCbor);
    // Every field the fixture advertises past this point is derived from the
    // signed canonical bytes by the one shared derivation, so the three fixtures
    // in this directory cannot disagree about what a compact form or a field
    // commitment is. The witness-set fields in particular commit per
    // `deriveMidgardNativeTxWitnessSetCompactV1`, not to a raw blake2b of the
    // preimage CBOR.
    const facets = deriveNativeTxFixtureFacets(signed.txCbor);

    const spendInputs = asArray(
      decodeSingleCbor(tx.body.spendInputsPreimageCbor),
      "spend inputs",
    );
    const decodedReferenceInputs = asArray(
      decodeSingleCbor(tx.body.referenceInputsPreimageCbor),
      "reference inputs",
    );
    const outputs = asArray(
      decodeSingleCbor(tx.body.outputsPreimageCbor),
      "outputs",
    );
    // §5.6: field 5 is the enveloped per-policy item list, so the policy count is
    // the item count rather than a map size.
    const mintPolicies = decodeMidgardMintFieldPreimage(
      tx.body.mintPreimageCbor,
    );

    if (
      spendInputs.length !== HIGH_CARDINALITY_COUNTS.spendInputs ||
      decodedReferenceInputs.length !==
        HIGH_CARDINALITY_COUNTS.referenceInputs ||
      outputs.length !== HIGH_CARDINALITY_COUNTS.outputs ||
      mintPolicies.length !== HIGH_CARDINALITY_COUNTS.mintPolicies ||
      facets.redeemerPointers.length !== HIGH_CARDINALITY_COUNTS.totalRedeemers
    ) {
      throw new Error("High-cardinality native tx fixture shape drifted");
    }

    return {
      name: HIGH_CARDINALITY_FIXTURE_NAME,
      producer: HIGH_CARDINALITY_PRODUCER,
      txIdHex: facets.txIdHex,
      fullTxCborHex: facets.fullTxCborHex,
      compactTxCborHex: facets.compactTxCborHex,
      compactBodyCborHex: facets.compactBodyCborHex,
      counts: HIGH_CARDINALITY_COUNTS,
      sizes: facets.sizes,
      mintPolicyIdsInTxInfoOrder: facets.mintPolicyIdsInTxInfoOrder,
      redeemerPointers: facets.redeemerPointers,
      preimages: facets.preimages,
      hashes: facets.hashes,
    };
  };
