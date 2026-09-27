/**
 * Ledger trie hydration, ledger roots and transaction-root values from ledger entries.
 */

import { encodeMidgardTxOutput, outRefToCbor } from "@al-ft/lucid-midgard";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as FS from "fs";

import * as Ledger from "../database/utils/ledger.js";
import { FileSystemError } from "../utils.js";
import { keyValuePhasRoot } from "../workers/utils/mpf/phas.js";
import { MpfError } from "./errors.js";
import {
  ledgerEntryToInsertBatchOp,
  ledgerOutputToInsertBatchOp,
} from "./ledger-delta.js";
import { MidgardMpf } from "./store.js";
import { type MpfInsertBatchOp } from "./types.js";

export const encodeTransactionRootValue = (
  txCanonicalCbor: Buffer,
  consensusProfile: MidgardConsensusProfile = MIDGARD_CONSENSUS_PROFILE,
): Buffer => {
  if (!isMidgardConsensusProfile(consensusProfile)) {
    throw new Error("Refusing transaction under a non-V1 consensus profile");
  }
  const violation = validateMidgardConsensusTxCbor(txCanonicalCbor);
  if (violation !== null) {
    throw new Error(
      `Refusing transaction outside the exact canonical V1 consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
    );
  }
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(txCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  const value: SDK.L2TransactionSource = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return Buffer.from(
    aikenSerialisedPlutusDataCbor(LucidData.to(value, SDK.L2TransactionSource)),
    "hex",
  );
};

export const computeLedgerMpfRootFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.try({
    try: () => entries.map(ledgerEntryToInsertBatchOp),
    catch: (cause) => MpfError.rootBuild("ledger descriptor root", cause),
  }).pipe(
    Effect.flatMap((ops) =>
      keyValuePhasRoot(
        ops.map((op) => op.key),
        ops.map((op) => op.value),
      ),
    ),
  );

export const hydrateLedgerMpfFromLedgerEntries = (
  ledgerMpf: MidgardMpf,
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    yield* ledgerMpf.resetToEmpty();
    const ops = yield* Effect.try({
      try: () => entries.map(ledgerEntryToInsertBatchOp),
      catch: (cause) =>
        MpfError.rootBuild("ledger descriptor hydration", cause),
    });
    yield* ledgerMpf.applyBatch(ops);
    return yield* ledgerMpf.rootHex();
  });

export const utxoToLedgerInsertMaterial = (
  utxo: UTxO,
): Effect.Effect<
  {
    readonly ledgerOp: MpfInsertBatchOp;
    readonly outputCbor: Buffer;
  },
  SDK.CmlDeserializationError
> =>
  Effect.gen(function* () {
    // The MPF trie key is the §5.3 field-0/1 item encoding, byte-for-byte what
    // on-chain `ledger_outref_key` derives through `encode_midgard_tx_input`.
    // CML's minimal-index TransactionInput CBOR is 36 bytes for indices 0–23 and
    // would key the trie where the on-chain side never looks.
    const outRef = yield* Effect.try({
      try: () => outRefToCbor(utxo),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to encode UTxO outref as the §5.3 ledger key",
          cause: e,
        }),
    });
    const output = yield* Effect.try({
      try: () =>
        encodeMidgardTxOutput(utxo.address, utxo.assets, {
          ...(utxo.datum == null
            ? {}
            : { datum: { kind: "inline" as const, data: utxo.datum } }),
        }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to convert UTxO to Midgard output CBOR",
          cause: e,
        }),
    });
    const ledgerOp = yield* Effect.try({
      try: () => ledgerOutputToInsertBatchOp({ outRef, outputCbor: output }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to derive the canonical V1 genesis descriptor",
          cause: e,
        }),
    });
    return { ledgerOp, outputCbor: output };
  });

export const deleteMpfStore = (
  path: string,
  name: string,
): Effect.Effect<void, FileSystemError> =>
  Effect.try({
    try: () => FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s MPF LevelDB store from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPF store`));
