import { CML } from "@lucid-evolution/lucid";

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { computeHash32 } from "./hash.js";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxProofFieldLengths,
  midgardNativeTxProofFieldPreimageLengths,
  proofFieldPreimageLengths,
} from "./native.decode-midgard-native-tx-canonical-envelope-for-fault-evidence.js";
import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  type MidgardNativeTxProofSource,
  verifyMidgardNativeTxFullConsistency,
} from "./native.validate-midgard-native-tx-canonical.js";
import { cardanoTxBytesToMidgardNativeTxCanonical } from "./native-cardano-conversion.js";
import {
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "./native-constants.js";
import { decodeMidgardFieldPreimage } from "./native-tx-field-access.js";

export const deriveMidgardNativeTxProofSource = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxProofSource => {
  if (tx.version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 transaction sources require native transaction version 1",
      `actual=${tx.version.toString()}`,
    );
  }
  verifyMidgardNativeTxFullConsistency(tx);
  return {
    compactCbor: encodeMidgardNativeTxCompact(tx.compact),
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet),
    ),
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
      proofFieldPreimageLengths(tx),
    ),
  };
};

export const deriveMidgardNativeTxProofSourceFromCanonicalCbor = (
  canonicalTransactionCbor: Uint8Array,
): MidgardNativeTxProofSource =>
  deriveMidgardNativeTxProofSource(
    decodeMidgardNativeTxFullFromCanonicalCbor(canonicalTransactionCbor),
  );

export type MidgardNativeTxFaultEvidenceMaterial = Readonly<{
  canonical: MidgardNativeTxCanonical;
  compact: MidgardNativeTxCompact;
  transactionId: Buffer;
  proofSource: MidgardNativeTxProofSource;
  fieldPreimages: readonly Buffer[];
}>;

/**
 * Derives the one canonical compact/source identity from a raw fault-evidence
 * envelope without first accepting the nine inner field grammars.
 */
export const deriveMidgardNativeTxFaultEvidenceMaterial = (
  canonicalTransactionCbor: Uint8Array,
): MidgardNativeTxFaultEvidenceMaterial => {
  const canonical = decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidence(
    canonicalTransactionCbor,
  );
  const compact = deriveMidgardNativeTxCompact(
    canonical.body,
    canonical.witnessSet,
    canonical.validity,
    canonical.version,
  );
  const witnessSetCompact = deriveMidgardNativeTxWitnessSetCompact(
    canonical.witnessSet,
  );
  const proofSource: MidgardNativeTxProofSource = {
    compactCbor: encodeMidgardNativeTxCompact(compact),
    witnessSetCompactCbor:
      encodeMidgardNativeTxWitnessSetCompact(witnessSetCompact),
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
      midgardNativeTxProofFieldPreimageLengths(canonical),
    ),
  };
  return Object.freeze({
    canonical,
    compact,
    transactionId: computeMidgardNativeTxId(compact),
    proofSource,
    fieldPreimages: Object.freeze([
      Buffer.from(canonical.body.spendInputsPreimageCbor),
      Buffer.from(canonical.body.referenceInputsPreimageCbor),
      Buffer.from(canonical.body.outputsPreimageCbor),
      Buffer.from(canonical.body.requiredObserversPreimageCbor),
      Buffer.from(canonical.body.requiredSignersPreimageCbor),
      Buffer.from(canonical.body.mintPreimageCbor),
      Buffer.from(canonical.witnessSet.scriptTxWitsPreimageCbor),
      Buffer.from(canonical.witnessSet.addrTxWitsPreimageCbor),
      Buffer.from(canonical.witnessSet.redeemerTxWitsPreimageCbor),
    ]),
  });
};

export const verifyMidgardNativeTxProofSource = ({
  transactionId,
  source,
}: {
  readonly transactionId: Uint8Array;
  readonly source: MidgardNativeTxProofSource;
}): MidgardNativeTxCompact => {
  const compact = decodeMidgardNativeTxCompact(source.compactCbor);
  const canonicalCompact = encodeMidgardNativeTxCompact(compact);
  if (!canonicalCompact.equals(source.compactCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact transaction source is not canonical",
    );
  }
  if (compact.version !== MIDGARD_NATIVE_TX_VERSION) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact transaction source has an unsupported native version",
      `actual=${compact.version.toString()}`,
    );
  }
  const witnessSetCompact = decodeMidgardNativeTxWitnessSetCompact(
    source.witnessSetCompactCbor,
  );
  const canonicalWitnessSetCompact =
    encodeMidgardNativeTxWitnessSetCompact(witnessSetCompact);
  if (!canonicalWitnessSetCompact.equals(source.witnessSetCompactCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 compact witness-set source is not canonical",
    );
  }
  const expectedWitnessSetHash = computeHash32(canonicalWitnessSetCompact);
  if (!expectedWitnessSetHash.equals(compact.transactionWitnessSetHash)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "V1 compact witness set does not match the transaction source",
    );
  }
  const fieldLengths = decodeMidgardNativeTxProofFieldLengths(
    source.fieldPreimageLengthsCbor,
  );
  const canonicalFieldLengths =
    encodeMidgardNativeTxProofFieldLengths(fieldLengths);
  if (!canonicalFieldLengths.equals(source.fieldPreimageLengthsCbor)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "V1 field-preimage lengths are not canonical",
    );
  }
  const expectedTransactionId = computeMidgardNativeTxId(compact);
  if (!expectedTransactionId.equals(transactionId)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.HashMismatch,
      "V1 compact transaction does not match the transaction id",
    );
  }
  return compact;
};

/**
 * §5.1's one uniform byte-list decode, which all nine fields share.
 *
 * Under the retired counted scheme this was a general `asArray`/`asBytes` pass
 * that accepted any CBOR array of byte strings. §5.1 is narrower and fails
 * closed: a non-minimal array or item header, an item count that disagrees with
 * the walked content, and trailing bytes after item `N-1` all reject. Routing
 * this through the one §5.1 decoder is what makes the loose reader and the
 * field-access door agree on which byte forms exist.
 */
export const decodeMidgardNativeByteListPreimage = (
  preimageCbor: Uint8Array,
  fieldName = "preimage_cbor",
): Buffer[] => {
  try {
    return [...decodeMidgardFieldPreimage(preimageCbor)];
  } catch (error) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `${fieldName} is not a canonical §5.1 field preimage`,
      String(error),
    );
  }
};

export const cardanoTxBytesToMidgardNativeTxFull = (
  cardanoTxBytes: Uint8Array,
): MidgardNativeTxFull => {
  const canonical = cardanoTxBytesToMidgardNativeTxCanonical(cardanoTxBytes, {
    nativeTxVersion: MIDGARD_NATIVE_TX_VERSION,
    posixTimeNone: MIDGARD_POSIX_TIME_NONE,
    networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
  });
  return materializeMidgardNativeTxFromCanonical(canonical);
};

export const cardanoTxBytesToMidgardNativeTxCanonicalCbor = (
  cardanoTxBytes: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxCanonical(
    cardanoTxBytesToMidgardNativeTxCanonical(cardanoTxBytes, {
      nativeTxVersion: MIDGARD_NATIVE_TX_VERSION,
      posixTimeNone: MIDGARD_POSIX_TIME_NONE,
      networkIdNone: MIDGARD_NATIVE_NETWORK_ID_NONE,
    }),
  );

const decodeNativeCredentialObserver = (
  observerBytes: Uint8Array,
  fieldName: string,
): CML.Credential => {
  if (observerBytes.length === 28) {
    return CML.Credential.new_script(
      CML.ScriptHash.from_raw_bytes(observerBytes),
    );
  }
  try {
    const credential = CML.Credential.from_cbor_bytes(observerBytes);
    if (credential.kind() !== CML.CredentialKind.Script) {
      throw new Error("observer credential must be a script credential");
    }
    return credential;
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Midgard observer must be a script hash or a CBOR-encoded script credential",
      `${fieldName}: ${String(e)}`,
    );
  }
};

export const toCardanoNetworkId = (
  networkId: bigint,
  fieldName: string,
): CML.NetworkId | undefined => {
  if (networkId === MIDGARD_NATIVE_NETWORK_ID_NONE) {
    return undefined;
  }
  if (networkId === 0n) {
    return CML.NetworkId.testnet();
  }
  if (networkId === 1n) {
    return CML.NetworkId.mainnet();
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    "Unsupported Cardano network id for reverse conversion",
    `${fieldName}: ${networkId.toString(10)}`,
  );
};

export const decodeNativeRequiredSignersToCardano = (
  preimageCbor: Uint8Array,
): CML.Ed25519KeyHashList => {
  const signerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_signers",
  );
  const signers = CML.Ed25519KeyHashList.new();
  for (let i = 0; i < signerBytes.length; i++) {
    const signer = signerBytes[i];
    if (signer.length !== 28) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.InvalidFieldType,
        "Required signer must be 28 bytes",
        `native.required_signers[${i}]`,
      );
    }
    signers.add(CML.Ed25519KeyHash.from_raw_bytes(signer));
  }
  return signers;
};

export const decodeNativeObserversToWithdrawals = (
  preimageCbor: Uint8Array,
  networkId: CML.NetworkId | undefined,
): CML.MapRewardAccountToCoin | undefined => {
  const observerBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_observers",
  );
  if (observerBytes.length === 0) {
    return undefined;
  }
  if (networkId === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "Observer-to-withdrawal conversion requires an explicit Cardano network id",
      "native.network_id",
    );
  }
  const withdrawals = CML.MapRewardAccountToCoin.new();
  for (let i = 0; i < observerBytes.length; i++) {
    const credential = decodeNativeCredentialObserver(
      observerBytes[i],
      `native.required_observers[${i}]`,
    );
    withdrawals.insert(
      CML.RewardAddress.new(Number(networkId.network()), credential),
      0n,
    );
  }
  return withdrawals;
};
