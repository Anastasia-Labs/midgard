import {
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardLedgerTxFromCanonicalCbor } from "@al-ft/midgard-validation/ledger-tx/codec";
import { CML } from "@lucid-evolution/lucid";

import {
  type TerminalDrainEntry,
  type TerminalDrainState,
  terminalSnapshotHash,
} from "./terminal-drain.terminal-snapshot-hash.js";
import { type StressWalletRecord } from "./types.js";

export const assertTerminalDrainIntent = (
  entry: TerminalDrainEntry,
  source: StressWalletRecord,
  state: TerminalDrainState,
): void => {
  if (
    !/^[0-9a-f]{64}$/.test(entry.beforeValueSha256) ||
    new Set(entry.beforeOutrefs).size !== entry.beforeOutrefs.length
  )
    throw new Error("Terminal drain snapshot digest/outrefs are malformed.");
  if (entry.status === "already_empty") {
    if (
      entry.beforeOutrefs.length !== 0 ||
      entry.beforeLovelace !== "0" ||
      entry.beforeValueSha256 !== terminalSnapshotHash([]) ||
      entry.txHash !== undefined
    )
      throw new Error(
        "Terminal drain already_empty entry is not exactly empty.",
      );
    return;
  }
  if (
    !entry.txHash ||
    !entry.signedTxCbor ||
    !entry.selectedInputs ||
    entry.requestedLovelace === undefined ||
    entry.feeLovelace === undefined ||
    entry.signedTxBytes === undefined
  )
    throw new Error("Terminal drain prepared entry is incomplete.");
  if (
    !/^[0-9a-f]{64}$/.test(entry.txHash) ||
    !/^[0-9a-f]+$/.test(entry.signedTxCbor) ||
    entry.signedTxCbor.length % 2 !== 0
  )
    throw new Error("Terminal drain hash/CBOR encoding is malformed.");
  const bytes = Buffer.from(entry.signedTxCbor, "hex");
  const native = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
  const tx = decodeMidgardLedgerTxFromCanonicalCbor(bytes);
  if (
    tx.txId.toString("hex") !== entry.txHash ||
    computeMidgardNativeTxId(native).toString("hex") !== entry.txHash
  )
    throw new Error("Terminal drain hash does not bind its exact signed CBOR.");
  if (
    tx.networkId !== (state.network === "Mainnet" ? 1n : 0n) ||
    tx.validity !== "TxIsValid" ||
    native.version !== MIDGARD_NATIVE_TX_VERSION ||
    native.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
    native.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE ||
    tx.validityIntervalStart !== undefined ||
    tx.validityIntervalEnd !== undefined
  )
    throw new Error(
      "Terminal drain network/version/validity invariant failed.",
    );
  const decodedInputs = tx.spendInputs
    .map((x) => x.txId.toString("hex") + "#" + x.index.toString())
    .sort();
  const selected = [...entry.selectedInputs].sort();
  const before = [...entry.beforeOutrefs].sort();
  if (
    new Set(selected).size !== selected.length ||
    decodedInputs.join("|") !== selected.join("|") ||
    selected.join("|") !== before.join("|")
  )
    throw new Error(
      "Terminal drain must spend every and only snapshotted input.",
    );
  const total = BigInt(entry.beforeLovelace);
  const requested = BigInt(entry.requestedLovelace);
  const fee = BigInt(entry.feeLovelace);
  const requiredFee =
    BigInt(state.minFeeA) * BigInt(bytes.length) + BigInt(state.minFeeB);
  if (
    requested <= 0n ||
    tx.fee !== fee ||
    entry.signedTxBytes !== bytes.length ||
    requested + fee !== total ||
    fee < requiredFee ||
    fee > BigInt(state.feeCapLovelace)
  )
    throw new Error("Terminal drain fee/size/conservation invariant failed.");
  if (tx.outputs.length !== 1)
    throw new Error(
      "Terminal drain must have exactly one output and zero source change.",
    );
  const output = tx.outputs[0]!;
  if (
    encodeMidgardAddressText(output.address) !== state.treasuryAddress ||
    output.value.lovelace !== requested ||
    output.value.assets.size !== 0 ||
    output.datum !== undefined ||
    output.scriptRef !== undefined
  )
    throw new Error(
      "Terminal drain output is not the exact ADA-only treasury payment.",
    );
  const signer = source.paymentKeyHash;
  if (
    tx.requiredSignerHashes.length !== 1 ||
    tx.requiredSignerHashes[0]!.toString("hex") !== signer ||
    tx.witnessKeyHashes.length !== 1 ||
    tx.witnessKeyHashes[0]!.toString("hex") !== signer ||
    tx.vkeyWitnesses.length !== 1
  )
    throw new Error("Terminal drain is not signed solely by the source key.");
  const witness = tx.vkeyWitnesses[0]!;
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try {
    const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try {
      if (!publicKey.verify(tx.txId, signature))
        throw new Error("Terminal drain signature is invalid.");
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
  )
    throw new Error(
      "Terminal drain contains forbidden script/mint/reference/auxiliary content.",
    );
};
