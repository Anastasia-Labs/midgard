import {
  type MidgardValidationPhaseName,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { MidgardTxCodecError } from "@al-ft/midgard-core/codec";
import { type MidgardConsensusViolationCode } from "@al-ft/midgard-core/consensus-validation";

import { MidgardLedgerTxDecodeError } from "./ledger-tx/codec.js";
import type { MidgardLedgerTx } from "./ledger-tx/types.js";
import type { RejectSubject } from "./reject-subject.js";
import { RejectCode, RejectCodes, RejectedTx } from "./types.js";
import { midgardOutRefToCborHex } from "./validation-candidate.js";

export const reject = (
  txId: Buffer,
  code: RejectCode,
  detail: string | null = null,
  consensusPhase: MidgardValidationPhaseName = "canonicalDecode",
  subject?: RejectSubject,
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
  ...(subject === undefined ? {} : { subject }),
});

export const codecErrorDetail = (error: unknown): string => {
  if (error instanceof MidgardLedgerTxDecodeError) {
    return codecErrorDetail(error.causeValue);
  }
  if (error instanceof MidgardTxCodecError) {
    return error.detail === null
      ? `${error.code}: ${error.message}`
      : `${error.code}: ${error.message} (${error.detail})`;
  }
  return String(error);
};

export const consensusProfileRejectCode = (
  code: MidgardConsensusViolationCode,
): RejectCode => {
  switch (code) {
    case "E_TX_VERSION":
      return RejectCodes.TxVersion;
    case "E_TX_SIZE":
      return RejectCodes.TxSize;
    case "E_IS_VALID_FALSE_FORBIDDEN":
      return RejectCodes.IsValidFalseForbidden;
    case "E_AUX_DATA_FORBIDDEN":
      return RejectCodes.AuxDataForbidden;
    case "E_INPUT_COUNT":
      return RejectCodes.InputCount;
    case "E_REFERENCE_INPUT_COUNT":
      return RejectCodes.ReferenceInputCount;
    case "E_OUTPUT_COUNT":
      return RejectCodes.OutputCount;
    case "E_ADDRESS_WITNESS_COUNT":
      return RejectCodes.AddressWitnessCount;
    case "E_REQUIRED_SIGNER_COUNT":
      return RejectCodes.RequiredSignerCount;
    case "E_SCRIPT_EXECUTION_COUNT":
      return RejectCodes.ScriptExecutionCount;
    case "E_OBSERVER_COUNT":
      return RejectCodes.ObserverCount;
    case "E_FIELD_PREIMAGE_SIZE":
      return RejectCodes.FieldPreimageSize;
    case "E_LEDGER_OUTPUT_SIZE":
      return RejectCodes.LedgerOutputSize;
    case "E_VALUE_SIZE":
      return RejectCodes.ValueSize;
    case "E_SCRIPT_PROGRAM_SIZE":
      return RejectCodes.ScriptProgramSize;
    case "E_SCRIPT_PROGRAM_ENCODING":
      return RejectCodes.ScriptProgramEncoding;
  }
};

const EMPTY_HASH_HEXES: readonly string[] = [];

export const hashHexes = (hashes: readonly Buffer[]): readonly string[] =>
  hashes.length === 0
    ? EMPTY_HASH_HEXES
    : hashes.map((hash) => hash.toString("hex"));

/** Ordinals of the earliest-completed duplicate pair, first before second. */
const firstDuplicate = (
  values: readonly string[],
): { readonly first: number; readonly second: number } | undefined => {
  const seen = new Map<string, number>();
  for (let index = 0; index < values.length; index += 1) {
    const first = seen.get(values[index]!);
    if (first !== undefined) {
      return { first, second: index };
    }
    seen.set(values[index]!, index);
  }
  return undefined;
};

const duplicateInputSubject = (
  firstField: bigint,
  first: number,
  secondField: bigint,
  second: number,
): RejectSubject => ({
  arm: "DuplicateInput",
  first: { fieldIndex: firstField, itemIndex: BigInt(first) },
  second: { fieldIndex: secondField, itemIndex: BigInt(second) },
});

const outRefIdentity = (
  outRef: MidgardLedgerTx["spendInputs"][number],
): string => `${outRef.txId.toString("hex")}#${outRef.index.toString()}`;

export const validateInputSets = (
  tx: Pick<MidgardLedgerTx, "txId" | "spendInputs" | "referenceInputs">,
): RejectedTx | null => {
  if (tx.spendInputs.length === 0) {
    return reject(tx.txId, RejectCodes.EmptyInputs, null, "inputSets");
  }

  if (tx.spendInputs.length === 1 && tx.referenceInputs.length === 0) {
    return null;
  }

  const spendOutRefIdentities = tx.spendInputs.map(outRefIdentity);
  if (spendOutRefIdentities.length > 1) {
    const duplicateSpend = firstDuplicate(spendOutRefIdentities);
    if (duplicateSpend !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        midgardOutRefToCborHex(tx.spendInputs[duplicateSpend.first]!),
        "inputSets",
        duplicateInputSubject(
          0n,
          duplicateSpend.first,
          0n,
          duplicateSpend.second,
        ),
      );
    }
  }

  if (tx.referenceInputs.length === 0) {
    return null;
  }

  const spent = new Map(
    spendOutRefIdentities.map((identity, index) => [identity, index]),
  );
  const referenceOutRefs = tx.referenceInputs.map(outRefIdentity);
  if (referenceOutRefs.length > 1) {
    const duplicateReference = firstDuplicate(referenceOutRefs);
    if (duplicateReference !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `duplicate reference input ${midgardOutRefToCborHex(
          tx.referenceInputs[duplicateReference.first]!,
        )}`,
        "inputSets",
        duplicateInputSubject(
          1n,
          duplicateReference.first,
          1n,
          duplicateReference.second,
        ),
      );
    }
  }

  for (let index = 0; index < referenceOutRefs.length; index += 1) {
    const spendIndex = spent.get(referenceOutRefs[index]!);
    if (spendIndex !== undefined) {
      return reject(
        tx.txId,
        RejectCodes.DuplicateInputInTx,
        `outref appears in both spend and reference inputs ${midgardOutRefToCborHex(
          tx.referenceInputs[index]!,
        )}`,
        "inputSets",
        duplicateInputSubject(0n, spendIndex, 1n, index),
      );
    }
  }

  return null;
};

export const validateValidityInterval = (
  tx: Pick<
    MidgardLedgerTx,
    "txId" | "validityIntervalStart" | "validityIntervalEnd"
  >,
): RejectedTx | null => {
  // A present bound is already a nonnegative slot: the native body codec
  // refuses any bound below the `-1` sentinel (`asValidityBound`,
  // `docs/spec/midgard-tx.md` §2.1) and the ledger view maps `-1` to
  // `undefined`. Mirrors `validity_interval_is_malformed` on-chain.
  if (
    tx.validityIntervalStart !== undefined &&
    tx.validityIntervalEnd !== undefined &&
    tx.validityIntervalStart >= tx.validityIntervalEnd
  ) {
    return reject(
      tx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `${tx.validityIntervalStart} >= ${tx.validityIntervalEnd}`,
      "inputSets",
    );
  }

  return null;
};

/**
 * Reports the first address witness whose signature does not verify, by the
 * ledger's own Ed25519 check. A failure of the check itself is raised.
 */
export const verifyVKeyWitnessSignatures = (
  tx: Pick<MidgardLedgerTx, "txId" | "vkeyWitnesses">,
): RejectedTx | null => {
  for (const witness of tx.vkeyWitnesses) {
    if (
      !verifyLedgerEd25519Signature(witness.vkey, tx.txId, witness.signature)
    ) {
      return reject(
        tx.txId,
        RejectCodes.InvalidSignature,
        `invalid native vkey witness #${witness.index}`,
        "signatures",
        { arm: "AddressWitnessSignatureInvalid", index: BigInt(witness.index) },
      );
    }
  }
  return null;
};

export const validateRequiredSigners = (
  tx: Pick<
    MidgardLedgerTx,
    "txId" | "requiredSignerHashes" | "witnessKeyHashes"
  >,
): RejectedTx | null => {
  if (tx.requiredSignerHashes.length === 0) {
    return null;
  }
  const witnessSignerSet = new Set(hashHexes(tx.witnessKeyHashes));
  const requiredSigners = hashHexes(tx.requiredSignerHashes);
  for (let index = 0; index < requiredSigners.length; index += 1) {
    const requiredSigner = requiredSigners[index]!;
    if (!witnessSignerSet.has(requiredSigner)) {
      return reject(
        tx.txId,
        RejectCodes.MissingRequiredWitness,
        `missing witness for signer ${requiredSigner}`,
        "signatures",
        { arm: "RequiredSignerUnsigned", index: BigInt(index) },
      );
    }
  }
  return null;
};
