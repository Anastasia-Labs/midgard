import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  CompleteTx,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  PartiallySignedTx,
  type TxStatus,
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
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async (txCborHex) => {
    const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(txCborHex, "hex"),
    );
    return {
      txId: computeMidgardNativeTxId(tx).toString("hex"),
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

export const makeFixture = async (keys?: {
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

export const witnessCount = (tx: CompleteTx | PartiallySignedTx): number =>
  decodeMidgardNativeByteListPreimage(
    tx.tx.witnessSet.addrTxWitsPreimageCbor,
    "native.addr_tx_wits",
  ).length;

export const expectComplete = (
  tx: CompleteTx | PartiallySignedTx,
): CompleteTx => {
  expect(tx).toBeInstanceOf(CompleteTx);
  if (!(tx instanceof CompleteTx)) {
    throw new Error("expected CompleteTx");
  }
  return tx;
};

export const expectPartial = (
  tx: CompleteTx | PartiallySignedTx,
): PartiallySignedTx => {
  expect(tx).toBeInstanceOf(PartiallySignedTx);
  if (!(tx instanceof PartiallySignedTx)) {
    throw new Error("expected PartiallySignedTx");
  }
  return tx;
};
