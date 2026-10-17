import {
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardLedgerTxFromCanonicalCbor } from "@al-ft/midgard-validation/ledger-tx/codec";
import { CML, type Network } from "@lucid-evolution/lucid";

import { STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION } from "./constants.js";
import { type StressWalletOperationScope } from "./scope.js";
import { type StressWalletRecord } from "./types.js";

export type ConsolidationStateEntry = {
  readonly walletId: string;
  readonly address: string;
  readonly beforeLovelace: string;
  readonly beforeOutrefs: readonly string[];
  readonly requestedLovelace: string;
  readonly txHash?: string;
  readonly signedTxCbor?: string;
  readonly selectedInputs?: readonly string[];
  readonly selectedInputLovelace?: string;
  readonly acceptedStatus?: string;
};

export type ConsolidationState = {
  readonly schemaVersion: typeof STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION;
  readonly treasuryAddress: string;
  readonly nodeEndpoint: string;
  readonly reserveLovelace: string;
  readonly scope: StressWalletOperationScope;
  readonly entries: readonly ConsolidationStateEntry[];
};

export const assertConsolidationTransferIntent = ({
  entry,
  source,
  treasuryAddress,
  reserveLovelace,
  network,
}: {
  readonly entry: ConsolidationStateEntry;
  readonly source: StressWalletRecord;
  readonly treasuryAddress: string;
  readonly reserveLovelace: bigint;
  readonly network: Network;
}): void => {
  if (
    entry.txHash === undefined ||
    entry.signedTxCbor === undefined ||
    entry.selectedInputs === undefined ||
    entry.selectedInputLovelace === undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is incomplete.`,
    );
  }
  let nativeTx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>;
  let tx: ReturnType<typeof decodeMidgardLedgerTxFromCanonicalCbor>;
  try {
    const signedTxCbor = Buffer.from(entry.signedTxCbor, "hex");
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(signedTxCbor);
    tx = decodeMidgardLedgerTxFromCanonicalCbor(signedTxCbor);
  } catch (cause) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not decodable: ${String(cause)}`,
    );
  }
  if (tx.txId.toString("hex") !== entry.txHash) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has a tx hash mismatch.`,
    );
  }
  const expectedNetworkId = network === "Mainnet" ? 1n : 0n;
  if (tx.networkId !== expectedNetworkId) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has network id ${String(tx.networkId)}, expected ${expectedNetworkId.toString()}.`,
    );
  }
  if (tx.validity !== "TxIsValid") {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is marked invalid.`,
    );
  }
  if (
    nativeTx.version !== MIDGARD_NATIVE_TX_VERSION ||
    nativeTx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
    nativeTx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE ||
    tx.validityIntervalStart !== undefined ||
    tx.validityIntervalEnd !== undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must use the supported native version with unbounded validity intervals.`,
    );
  }
  const decodedInputs = tx.spendInputs
    .map((input) => `${input.txId.toString("hex")}#${input.index.toString()}`)
    .sort();
  const journaledInputs = [...entry.selectedInputs].sort();
  if (
    decodedInputs.length !== journaledInputs.length ||
    decodedInputs.some((input, index) => input !== journaledInputs[index])
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not spend exactly its journaled selected inputs.`,
    );
  }
  if (
    new Set(journaledInputs).size !== journaledInputs.length ||
    journaledInputs.some((input) => !entry.beforeOutrefs.includes(input))
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} selects inputs outside its exact source snapshot.`,
    );
  }
  const requestedLovelace = BigInt(entry.requestedLovelace);
  const selectedInputLovelace = BigInt(entry.selectedInputLovelace);
  if (tx.fee < 0n || tx.fee > reserveLovelace) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has fee ${tx.fee.toString()} outside reserve ${reserveLovelace.toString()}.`,
    );
  }
  const expectedChange = selectedInputLovelace - requestedLovelace - tx.fee;
  if (expectedChange < 0n) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not conserve selected input lovelace.`,
    );
  }
  let treasuryOutputCount = 0;
  let sourceOutputCount = 0;
  let sourceChange = 0n;
  for (const output of tx.outputs) {
    const outputAddress = encodeMidgardAddressText(output.address);
    if (
      output.datum !== undefined ||
      output.scriptRef !== undefined ||
      output.value.assets.size !== 0
    ) {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} contains datum, script, or non-ADA output content.`,
      );
    }
    if (outputAddress === treasuryAddress) {
      treasuryOutputCount += 1;
      if (output.value.lovelace !== requestedLovelace) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has the wrong treasury value.`,
        );
      }
    } else if (outputAddress === source.l2Address) {
      sourceOutputCount += 1;
      sourceChange += output.value.lovelace;
    } else {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} pays an unexpected address ${outputAddress}.`,
      );
    }
  }
  if (treasuryOutputCount !== 1 || sourceOutputCount > 1) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must contain exactly one treasury output and at most one source change output.`,
    );
  }
  if (sourceChange !== expectedChange) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has source change ${sourceChange.toString()}, expected ${expectedChange.toString()}.`,
    );
  }
  const expectedSigner = source.paymentKeyHash;
  const requiredSigners = tx.requiredSignerHashes.map((hash) =>
    hash.toString("hex"),
  );
  const witnessSigners = tx.witnessKeyHashes.map((hash) =>
    hash.toString("hex"),
  );
  if (
    requiredSigners.length !== 1 ||
    requiredSigners[0] !== expectedSigner ||
    witnessSigners.length !== 1 ||
    witnessSigners[0] !== expectedSigner ||
    tx.vkeyWitnesses.length !== 1
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not signed solely by its source payment key.`,
    );
  }
  const witness = tx.vkeyWitnesses[0]!;
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try {
    const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try {
      if (!publicKey.verify(tx.txId, signature)) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has an invalid source signature.`,
        );
      }
    } finally {
      signature.free();
    }
  } finally {
    publicKey.free();
  }
  if (
    tx.referenceInputs.length !== 0 ||
    tx.requiredObserverHashes.length !== 0 ||
    tx.scriptWitnesses.length !== 0 ||
    tx.nativeScriptHashes.length !== 0 ||
    tx.plutusScriptHashes.length !== 0 ||
    tx.redeemers.length !== 0 ||
    tx.mint.assets.length !== 0 ||
    tx.requiresPlutusEvaluation ||
    !tx.auxiliaryDataHash.equals(EMPTY_NULL_ROOT) ||
    !tx.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} contains forbidden reference, mint, observer, auxiliary, or script content.`,
    );
  }
};
