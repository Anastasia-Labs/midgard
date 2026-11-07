import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";

import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  makeVKeyWitness,
  type MidgardProtocolInfo,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
  walletFromExternalSigner,
} from "../src/index.js";

export const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

export const addressFromKeyHash = (
  keyHash: CML.Ed25519KeyHash,
  networkId = 0,
): string =>
  CML.EnterpriseAddress.new(networkId, CML.Credential.new_pub_key(keyHash))
    .to_address()
    .to_bech32();

export const makeWalletFixture = (networkId = 0) => {
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

export const makeUtxo = (
  ref: OutRef,
  address: string,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets),
  });

export const protocolInfo = (
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
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: opts.minFeeA ?? 0n,
    minFeeB: opts.minFeeB ?? 0n,
  },
  submissionLimits: {
    maxSubmitTxCborBytes:
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
});

export const makeProvider = (opts: {
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
      return (opts.utxos ?? []).filter(
        (utxo) => utxo.output.address === address,
      );
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

export const inputLabels = (txHex: string): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    decodeMidgardNativeTxFullFromCanonicalCbor(Buffer.from(txHex, "hex")).body
      .spendInputsPreimageCbor,
  ).map((bytes) => {
    const input = CML.TransactionInput.from_cbor_bytes(bytes);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });

export const deferred = (): {
  readonly promise: Promise<void>;
  readonly resolve: () => void;
} => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
