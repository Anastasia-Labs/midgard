import type {
  Hash32,
  MidgardAddress,
  MidgardDatum,
  MidgardNativeTxCompact,
  MidgardNativeTxWitnessSetCompact,
  MidgardTxValidity,
  MidgardValue,
  MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";

/**
 * A 32-byte Blake2b hash.
 *
 * Midgard uses this for transaction-body commitments and other codec-level
 * roots. The alias documents the digest algorithm; callers should still use
 * codec constructors or validators when accepting untrusted bytes.
 */
export type Blake2b256Hash = Hash32;

/**
 * Midgard transaction id.
 *
 * This is Blake2b-256 over the canonical compact Midgard transaction body. It
 * identifies the transaction body, not the witness set.
 */
export type MidgardTxId = Blake2b256Hash;

/**
 * Script integrity hash declared by a Midgard transaction body.
 *
 * When Plutus or Midgard script execution is required, validation recomputes
 * this commitment from the redeemer witness root and required script languages.
 */
export type MidgardScriptIntegrityHash = Blake2b256Hash;

/**
 * Auxiliary data hash declared by a Midgard transaction body.
 *
 * Phase A currently admits only the canonical empty auxiliary-data commitment,
 * but the semantic type keeps the field explicit because it is part of the
 * body commitment.
 */
export type MidgardAuxiliaryDataHash = Blake2b256Hash;

/**
 * Hash of a public-key credential.
 *
 * In the current Midgard/Cardano ledger encoding this is a 28-byte hash used
 * for required signers, witness key hashes, and pubkey payment credentials.
 */
export type MidgardCredentialHash = Buffer;

/**
 * Hash of a script credential or versioned script.
 *
 * Midgard script hashes are 28 bytes. For versioned scripts the hash is over a
 * language-prefix byte and the script payload.
 */
export type MidgardScriptHash = Buffer;

/**
 * Minting policy id.
 *
 * Policy ids are script hashes in the ledger model, so this alias is kept
 * separate only to make mint-related fields read clearly.
 */
export type MidgardPolicyId = MidgardScriptHash;

/**
 * Native asset name bytes under a minting policy.
 *
 * Cardano asset names may be empty and are bounded by ledger rules; keep them
 * as bytes here and derive hex/text only at display or indexing boundaries.
 */
export type MidgardAssetName = Buffer;

/**
 * Decoded transaction input reference used by Midgard ledger validation.
 *
 * Encoding details such as CBOR bytes and hex strings belong at persistence,
 * proof, logging, or indexing boundaries. This semantic model keeps ledger data
 * in the shape validation rules operate on.
 */
export type MidgardOutRef = {
  /** Transaction id of the producing Midgard transaction. */
  readonly txId: MidgardTxId;

  /** Zero-based output index in the producing transaction. */
  readonly index: bigint;
};

/**
 * Decoded Midgard transaction output.
 *
 * This is the ledger-facing output shape used for value accounting, address
 * authorization, datum lookup, and reference-script discovery. It deliberately
 * avoids retaining the output's original CBOR bytes.
 */
export type MidgardLedgerOutput = {
  /** Midgard address bytes decoded into the codec's address model. */
  readonly address: MidgardAddress;

  /** Value carried by the output, including lovelace and native assets. */
  readonly value: MidgardValue;

  /** Optional inline datum attached to the output. */
  readonly datum?: MidgardDatum;

  /** Optional reference script made available by this output. */
  readonly scriptRef?: MidgardVersionedScript;
};

/**
 * Decoded vkey witness for a Midgard transaction.
 *
 * The witness authorizes the Midgard native transaction body hash. The raw key
 * and signature bytes are kept because signature verification operates on those
 * bytes, while `keyHash` is the credential hash used by ledger rules.
 */
export type MidgardLedgerVKeyWitness = {
  /** Position of this witness in the submitted witness list. */
  readonly index: number;

  /** 28-byte hash of the verification key. */
  readonly keyHash: MidgardCredentialHash;

  /** Raw verification key bytes. */
  readonly vkey: Buffer;

  /** Raw Ed25519 signature bytes. */
  readonly signature: Buffer;
};

/**
 * Inline script witness submitted with a Midgard transaction.
 *
 * Reference scripts live on resolved ledger outputs; this type covers scripts
 * carried directly by the transaction witness set.
 */
export type MidgardLedgerScriptWitness = {
  /** Position of this script in the submitted script witness list. */
  readonly index: number;

  /** 28-byte hash of the versioned script. */
  readonly hash: MidgardScriptHash;

  /** Decoded native, Plutus V3, or Midgard V1 script payload. */
  readonly script: MidgardVersionedScript;
};

/**
 * Decoded script redeemer and execution budget.
 *
 * `tag` and `index` identify the script purpose pointer. `data` is decoded
 * Plutus data, not CBOR or hex, so script-context builders can consume it
 * without re-decoding witness bytes.
 */
export type MidgardLedgerRedeemer = {
  /** Cardano redeemer tag, plus Midgard's receiving-script tag. */
  readonly tag: number;

  /** Purpose index within the tag-specific redeemer namespace. */
  readonly index: bigint;

  /** Decoded Plutus data supplied to the script. */
  readonly data: unknown;

  /** Execution units declared by the transaction for this redeemer. */
  readonly exUnits: {
    readonly memory: bigint;
    readonly steps: bigint;
  };
};

/**
 * One native asset quantity from the transaction mint field.
 *
 * Quantities are signed: positive values mint assets and negative values burn
 * assets. Zero quantities are not valid in the Midgard mint encoding.
 */
export type MidgardLedgerMintAsset = {
  /** 28-byte policy id authorizing the asset. */
  readonly policyId: MidgardPolicyId;

  /** Asset name bytes under the policy. */
  readonly assetName: MidgardAssetName;

  /** Signed mint delta for this policy and asset name. */
  readonly quantity: bigint;
};

/**
 * Decoded mint section of a Midgard ledger transaction.
 *
 * The section preserves the signed per-asset mint deltas exactly as ledger
 * semantics need them: positive quantities mint assets, negative quantities
 * burn assets.
 */
export type MidgardLedgerMint = {
  /** Signed mint entries from the transaction body. */
  readonly assets: readonly MidgardLedgerMintAsset[];
};

/**
 * Fully decoded semantic transaction used by Midgard ledger validation.
 *
 * This is not the canonical codec/proof representation. It contains the facts
 * Phase A and Phase B validate against: body fields, inputs, outputs, witnesses,
 * scripts, redeemers, and minting information. Exact CBOR bytes, hex encodings,
 * queue metadata, and persistence rows should be modeled by adjacent boundary
 * types rather than mixed into this ledger model.
 */
export type MidgardLedgerTx = {
  /** Blake2b-256 id of the compact Midgard transaction body. */
  readonly txId: MidgardTxId;

  /** Submitted validity flag; Phase A currently admits only TxIsValid. */
  readonly validity: MidgardTxValidity;

  /** Fee paid by the transaction in lovelace. */
  readonly fee: bigint;

  /** Optional network id; absent when the native sentinel means "no network". */
  readonly networkId?: bigint;

  /** Inclusive lower validity bound, absent when unbounded. */
  readonly validityIntervalStart?: bigint;

  /** Inclusive upper validity bound, absent when unbounded. */
  readonly validityIntervalEnd?: bigint;

  /** Auxiliary-data commitment declared by the transaction body. */
  readonly auxiliaryDataHash: MidgardAuxiliaryDataHash;

  /** Script-data commitment declared by the transaction body. */
  readonly scriptIntegrityHash: MidgardScriptIntegrityHash;

  /** UTxOs consumed by the transaction. */
  readonly spendInputs: readonly MidgardOutRef[];

  /** UTxOs read without spending, including possible reference-script sources. */
  readonly referenceInputs: readonly MidgardOutRef[];

  /** Outputs created by the transaction, in output-index order. */
  readonly outputs: readonly MidgardLedgerOutput[];

  /** Pubkey credential hashes that must have matching vkey witnesses. */
  readonly requiredSignerHashes: readonly MidgardCredentialHash[];

  /** Script hashes that must be observed by script witness material. */
  readonly requiredObserverHashes: readonly MidgardScriptHash[];

  /** Decoded vkey witnesses submitted with the transaction. */
  readonly vkeyWitnesses: readonly MidgardLedgerVKeyWitness[];

  /** Unique vkey credential hashes derived from submitted vkey witnesses. */
  readonly witnessKeyHashes: readonly MidgardCredentialHash[];

  /** Inline script witnesses submitted with the transaction. */
  readonly scriptWitnesses: readonly MidgardLedgerScriptWitness[];

  /** Hashes of inline native script witnesses. */
  readonly nativeScriptHashes: readonly MidgardScriptHash[];

  /** Hashes of inline non-native script witnesses. */
  readonly plutusScriptHashes: readonly MidgardScriptHash[];

  /** Decoded redeemers submitted with the transaction. */
  readonly redeemers: readonly MidgardLedgerRedeemer[];

  /** Decoded mint section, including signed per-asset deltas. */
  readonly mint: MidgardLedgerMint;

  /** True when script witnesses, redeemers, or script-data hash require eval. */
  readonly requiresPlutusEvaluation: boolean;
};

/**
 * Normal validation-facing decode result for submitted transaction CBOR.
 *
 * `ledgerTx` is the semantic transaction that ledger rules consume.
 * `commitments` is the proof/hash evidence derived from the canonical
 * submitted preimages: transaction body compact for tx-id commitments, and
 * witness-set compact for witness roots such as the redeemer root used by
 * script integrity checks.
 *
 * This keeps `MidgardLedgerTx` pure while avoiding Phase A having to traffic in
 * the lower-level canonical preimage object on its happy path.
 */
export type MidgardSubmittedTx = {
  /** Canonical submitted transaction CBOR bytes retained for persistence. */
  readonly txCbor: Buffer;

  /** Fully decoded ledger transaction. */
  readonly ledgerTx: MidgardLedgerTx;

  /** Hash commitments derived from the submitted canonical transaction. */
  readonly commitments: {
    /** Compact transaction commitment whose body hash is the transaction id. */
    readonly transactionCompact: MidgardNativeTxCompact;

    /** Compact witness-set roots derived from submitted witness preimages. */
    readonly witnessSetCompact: MidgardNativeTxWitnessSetCompact;

    /** Redeemer witness root committed by script-integrity validation. */
    readonly redeemerWitnessHash: Blake2b256Hash;
  };
};
