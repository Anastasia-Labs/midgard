import {
  type CompleteTx,
  decodeMidgardUtxo,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
} from "@al-ft/lucid-midgard";
import { normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardSpendInputItem,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { type Assets, CML, type Network } from "@lucid-evolution/lucid";

import { outRefLabel } from "../tx-context.js";
import { type NodeUtxo } from "./command-utils.js";

export type TransferNetworkName = Network;

export type TransferConsensusProfile = MidgardConsensusProfile;

export type PrivateKeyInput =
  | ReturnType<typeof CML.PrivateKey.from_bech32>
  | string;

export type BuiltTransferTx = {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCbor: Buffer;
  readonly txHex: string;
  readonly fee: bigint;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly changeAssets: Readonly<Assets>;
};

export const DEFAULT_TERMINAL_DRAIN_FEE_CAP_LOVELACE = 100_000n;

export const DEFAULT_TERMINAL_DRAIN_MAX_FEE_ITERATIONS = 32;

/**
 * Subtracts one asset map from another, failing if the subtraction would go
 * negative.
 */
export const subtractAssetMaps = (
  lhs: Readonly<Assets>,
  rhs: Readonly<Assets>,
): Readonly<Assets> => {
  const remaining: Assets = { ...normalizeAssets(lhs) };
  for (const [unit, quantity] of Object.entries(rhs)) {
    const available = remaining[unit] ?? 0n;
    if (available < quantity) {
      throw new Error(
        `Insufficient ${unit} while calculating transfer change (${available} < ${quantity}).`,
      );
    }
    const next = available - quantity;
    if (next === 0n) {
      delete remaining[unit];
    } else {
      remaining[unit] = next;
    }
  }
  return remaining;
};

export const toMidgardUtxo = (utxo: NodeUtxo): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: {
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
    },
    outRefCbor: Buffer.from(utxo.outrefCbor),
    outputCbor: Buffer.from(utxo.outputCbor),
  });

export const privateKeyHash = (privateKey: PrivateKeyInput): string => {
  const parsed =
    typeof privateKey === "string"
      ? CML.PrivateKey.from_bech32(privateKey)
      : privateKey;
  return parsed.to_public().hash().to_hex();
};

export const makeStaticMidgardProvider = ({
  address,
  utxos,
  network,
  networkId,
  minFeeA,
  minFeeB,
  maxSubmitTxCborBytes,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly address: string;
  readonly utxos: readonly NodeUtxo[];
  readonly network: TransferNetworkName;
  readonly networkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly maxSubmitTxCborBytes: number;
  readonly consensusProfile?: TransferConsensusProfile;
}): MidgardProvider => ({
  getUtxos: async (requestedAddress) =>
    requestedAddress === address ? utxos.map(toMidgardUtxo) : [],
  getUtxoByOutRef: async (outRef) =>
    utxos
      .filter(
        (utxo) =>
          utxo.txHash === outRef.txHash &&
          utxo.outputIndex === outRef.outputIndex,
      )
      .map(toMidgardUtxo)[0],
  getProtocolInfo: async () => {
    if (!isMidgardConsensusProfile(consensusProfile)) {
      throw new Error("Unsupported consensus profile");
    }
    if (
      !Number.isSafeInteger(maxSubmitTxCborBytes) ||
      maxSubmitTxCborBytes <= 0 ||
      maxSubmitTxCborBytes > MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes
    ) {
      throw new Error(
        `maxSubmitTxCborBytes must be between 1 and ${MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes.toString()}`,
      );
    }
    return {
      apiVersion: 1,
      network,
      midgardNativeTxVersion: Number(MIDGARD_NATIVE_TX_VERSION) as 1,
      currentSlot: 0n,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
      protocolFeeParameters: { minFeeA, minFeeB },
      submissionLimits: {
        maxSubmitTxCborBytes,
      },
      validation: {
        strictnessProfile: "production",
        localValidationIsAuthoritative: false,
      },
    };
  },
  getProtocolParameters: async () => ({
    minFeeA,
    minFeeB,
    networkId,
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => {
    throw new Error("Static Midgard transfer provider cannot submit.");
  },
  getTxStatus: async (txId) => ({ kind: "not_found", txId }),
  diagnostics: () => ({
    endpoint: "memory://submit-l2-transfer",
    protocolInfoSource: "offline",
  }),
});

export const makeTransferMidgard = async ({
  senderAddress,
  signer,
  utxos,
  network,
  networkId,
  minFeeA,
  minFeeB,
  maxSubmitTxCborBytes = MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly senderAddress: string;
  readonly signer: PrivateKeyInput;
  readonly utxos: readonly NodeUtxo[];
  readonly network: TransferNetworkName;
  readonly networkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly maxSubmitTxCborBytes?: number;
  readonly consensusProfile?: TransferConsensusProfile;
}): Promise<LucidMidgard> => {
  const midgard = await LucidMidgard.new(
    makeStaticMidgardProvider({
      address: senderAddress,
      utxos,
      network,
      networkId,
      minFeeA,
      minFeeB,
      maxSubmitTxCborBytes,
      consensusProfile,
    }),
    { network, networkId: Number(networkId) },
  );
  midgard.selectWallet.fromPrivateKey(signer, senderAddress);
  return midgard;
};

const selectedInputsFromCompletedTx = (
  completed: CompleteTx,
  availableUtxos: readonly NodeUtxo[],
): readonly NodeUtxo[] => {
  const byLabel = new Map(
    availableUtxos.map((utxo) => [outRefLabel(utxo), utxo]),
  );
  return decodeMidgardNativeByteListPreimage(
    completed.tx.body.spendInputsPreimageCbor,
  ).map((bytes) => {
    // Each field-0 preimage item is the §5.3 field-0/1 item form (38 bytes,
    // `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`) matching on-chain
    // `ledger_outref_key`, not CML's minimal-index `TransactionInput` CBOR.
    const input = decodeMidgardSpendInputItem(bytes);
    const label = `${Buffer.from(input.txId).toString("hex")}#${input.outputIndex.toString()}`;
    const utxo = byLabel.get(label);
    if (utxo === undefined) {
      throw new Error(`Built transfer selected unknown input ${label}.`);
    }
    return utxo;
  });
};

export const toBuiltTransferTx = ({
  signed,
  senderAddress,
  destinationAddress,
  availableUtxos,
  requestedAssets,
  changeAssets,
}: {
  readonly signed: CompleteTx;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly availableUtxos: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly changeAssets?: Readonly<Assets>;
}): BuiltTransferTx => {
  const txCbor = signed.txCbor;
  const txId = signed.txId;
  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCbor,
    txHex: txCbor.toString("hex"),
    fee: signed.metadata.fee,
    senderAddress,
    destinationAddress,
    selectedInputs: selectedInputsFromCompletedTx(signed, availableUtxos),
    requestedAssets,
    changeAssets: changeAssets ?? signed.metadata.changeAssets ?? {},
  };
};
