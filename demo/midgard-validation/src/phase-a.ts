import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { blake2b } from "@noble/hashes/blake2.js";
import {
  decodeTransaction,
  encodeTransactionOutput,
  transactionBodyHash,
  transactionId,
  type Mint,
  type OutputReference,
  type Transaction,
  type TransactionOutput,
  type Value as MidgardTsValue,
  type VersionedScript,
} from "@al-ft/midgard-ts";
import {
  MidgardScriptHashPrefixes,
  MidgardTxCodecError,
  decodeMidgardNativeScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import { decodeMidgardRedeemers } from "./midgard-redeemers.js";
import { encodeMidgardAddressText } from "@al-ft/midgard-core/codec";
import {
  PhaseAAccepted,
  PhaseAConfig,
  PhaseAResult,
  QueuedTx,
  RejectCode,
  RejectedTx,
  RejectCodes,
} from "./types.js";

const reject = (
  txId: Buffer,
  code: RejectCode,
  detail: string | null = null,
): RejectedTx => ({
  txId,
  code,
  detail,
});

// Canonical CBOR encoding of a midgard-ts OutputReference as a Cardano
// TransactionInput. This is the format the rest of the system uses for outref
// keys (MPT keys, processedTx.spent, referenceInputs in PhaseAAccepted).
const outRefToCborInput = (ref: OutputReference): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(ref.tx_id),
      BigInt(ref.index),
    ).to_cbor_bytes(),
  );

// blake2b-224 hex hash of a midgard-ts VersionedScript, matching the
// `hashMidgardVersionedScript` helper in midgard-core
// (prefix_byte || script_bytes).
const hashVersionedScript = (script: VersionedScript): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from([MidgardScriptHashPrefixes[script.language]]),
        Buffer.from(script.bytes),
      ]),
      { dkLen: 28 },
    ),
  ).toString("hex");

// Convert a midgard-ts Value to a CML.Value for the running output / minted /
// burned aggregates that phase-A exposes.
const midgardTsValueToCmlValue = (
  value: MidgardTsValue,
): InstanceType<typeof CML.Value> => {
  if (value.type === "Coin") {
    return CML.Value.from_coin(value.coin);
  }
  const multiasset = CML.MultiAsset.new();
  let policyCount = 0;
  for (const [policyId, entries] of value.assets) {
    const cmlAssets = CML.MapAssetNameToCoin.new();
    let assetCount = 0;
    for (const [name, amount] of entries) {
      if (amount <= 0n) continue;
      cmlAssets.insert(
        CML.AssetName.from_raw_bytes(Buffer.from(name)),
        amount,
      );
      assetCount += 1;
    }
    if (assetCount > 0) {
      multiasset.insert_assets(
        CML.ScriptHash.from_raw_bytes(Buffer.from(policyId)),
        cmlAssets,
      );
      policyCount += 1;
    }
  }
  return policyCount === 0
    ? CML.Value.from_coin(value.coin)
    : CML.Value.new(value.coin, multiasset);
};

// Summarise a midgard-ts Mint into the policy-hash list and the minted /
// burned CML.Value aggregates phase-A returns.
const summariseMint = (
  mint: Mint,
): {
  readonly policyIds: readonly string[];
  readonly mintedValue: InstanceType<typeof CML.Value>;
  readonly burnedValue: InstanceType<typeof CML.Value>;
} => {
  const policyIds: string[] = [];
  const minted = CML.MultiAsset.new();
  const burned = CML.MultiAsset.new();
  let mintedPolicies = 0;
  let burnedPolicies = 0;
  for (const [policyId, entries] of mint) {
    const policyHash = CML.ScriptHash.from_raw_bytes(Buffer.from(policyId));
    policyIds.push(policyHash.to_hex());
    const mintedAssets = CML.MapAssetNameToCoin.new();
    const burnedAssets = CML.MapAssetNameToCoin.new();
    let mintedCount = 0;
    let burnedCount = 0;
    for (const [name, amount] of entries) {
      const assetName = CML.AssetName.from_raw_bytes(Buffer.from(name));
      if (amount > 0n) {
        mintedAssets.insert(assetName, amount);
        mintedCount += 1;
      } else if (amount < 0n) {
        burnedAssets.insert(assetName, -amount);
        burnedCount += 1;
      }
    }
    if (mintedCount > 0) {
      minted.insert_assets(policyHash, mintedAssets);
      mintedPolicies += 1;
    }
    if (burnedCount > 0) {
      burned.insert_assets(policyHash, burnedAssets);
      burnedPolicies += 1;
    }
  }
  return {
    policyIds,
    mintedValue:
      mintedPolicies === 0 ? CML.Value.zero() : CML.Value.new(0n, minted),
    burnedValue:
      burnedPolicies === 0 ? CML.Value.zero() : CML.Value.new(0n, burned),
  };
};

type DecodedVKeyWitnesses = {
  readonly witnessKeyHashes: readonly string[];
  readonly witnessSignerSet: ReadonlySet<string>;
  readonly publicKeys: readonly InstanceType<typeof CML.PublicKey>[];
  readonly signatures: readonly InstanceType<typeof CML.Ed25519Signature>[];
};

const decodeVKeyWitnesses = (
  txId: Buffer,
  vkeyWitnesses: ReadonlyArray<{ vkey: Uint8Array; signature: Uint8Array }>,
): DecodedVKeyWitnesses | RejectedTx => {
  const witnessKeyHashes: string[] = [];
  const witnessSignerSet = new Set<string>();
  const publicKeys: InstanceType<typeof CML.PublicKey>[] = [];
  const signatures: InstanceType<typeof CML.Ed25519Signature>[] = [];
  for (let i = 0; i < vkeyWitnesses.length; i++) {
    const w = vkeyWitnesses[i];
    try {
      const publicKey = CML.PublicKey.from_bytes(w.vkey);
      const signature = CML.Ed25519Signature.from_raw_bytes(w.signature);
      publicKeys.push(publicKey);
      signatures.push(signature);
      const signerHex = publicKey.hash().to_hex();
      if (!witnessSignerSet.has(signerHex)) {
        witnessSignerSet.add(signerHex);
        witnessKeyHashes.push(signerHex);
      }
    } catch (e) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `vkey witness #${i} decode failed: ${String(e)}`,
      );
    }
  }
  return { witnessKeyHashes, witnessSignerSet, publicKeys, signatures };
};

const verifyVKeyWitnessSignatures = (
  txId: Buffer,
  txBodyHash: Uint8Array,
  publicKeys: readonly InstanceType<typeof CML.PublicKey>[],
  signatures: readonly InstanceType<typeof CML.Ed25519Signature>[],
): RejectedTx | null => {
  for (let i = 0; i < publicKeys.length; i++) {
    if (!publicKeys[i].verify(txBodyHash, signatures[i])) {
      return reject(
        txId,
        RejectCodes.InvalidSignature,
        `invalid vkey witness #${i}`,
      );
    }
  }
  return null;
};

type ClassifiedScripts = {
  readonly nativeScriptHashes: readonly string[];
  readonly plutusScriptHashes: readonly string[];
};

const classifyScriptWitnesses = (
  txId: Buffer,
  scripts: readonly VersionedScript[],
  validityIntervalStart: bigint | undefined,
  validityIntervalEnd: bigint | undefined,
  witnessSigners: ReadonlySet<string>,
): ClassifiedScripts | RejectedTx => {
  const nativeScriptHashes: string[] = [];
  const plutusScriptHashes: string[] = [];
  for (let i = 0; i < scripts.length; i++) {
    const script = scripts[i];
    const hash = hashVersionedScript(script);
    if (script.language === "NativeCardano") {
      nativeScriptHashes.push(hash);
      let nativeScript: ReturnType<typeof decodeMidgardNativeScript>;
      try {
        nativeScript = decodeMidgardNativeScript(script.bytes);
      } catch (e) {
        return reject(
          txId,
          RejectCodes.InvalidFieldType,
          `native script witness #${i} decode failed: ${String(e)}`,
        );
      }
      if (
        !verifyMidgardNativeScript(nativeScript.script, {
          validityIntervalStart,
          validityIntervalEnd,
          witnessSigners,
        })
      ) {
        return reject(
          txId,
          RejectCodes.NativeScriptInvalid,
          `native script verification failed for script index ${i}`,
        );
      }
      continue;
    }
    plutusScriptHashes.push(hash);
  }
  return { nativeScriptHashes, plutusScriptHashes };
};

const validateRequiredSigners = (
  txId: Buffer,
  required: ReadonlyArray<Uint8Array> | undefined,
): string[] | RejectedTx => {
  if (required === undefined) return [];
  const signers: string[] = [];
  for (let i = 0; i < required.length; i++) {
    const signer = required[i];
    if (signer.length !== 28) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `required signer at index ${i} must be 28 bytes`,
      );
    }
    signers.push(Buffer.from(signer).toString("hex"));
  }
  return signers;
};

const validateRequiredObservers = (
  txId: Buffer,
  required: ReadonlyArray<Uint8Array> | undefined,
): string[] | RejectedTx => {
  if (required === undefined) return [];
  const observers: string[] = [];
  const seen = new Set<string>();
  for (let i = 0; i < required.length; i++) {
    const observer = required[i];
    if (observer.length === 28) {
      const observerHex = Buffer.from(observer).toString("hex");
      if (seen.has(observerHex)) {
        return reject(
          txId,
          RejectCodes.InvalidFieldType,
          `duplicate required observer ${observerHex}`,
        );
      }
      seen.add(observerHex);
      observers.push(observerHex);
      continue;
    }

    let credential: InstanceType<typeof CML.Credential>;
    try {
      credential = CML.Credential.from_cbor_bytes(observer);
    } catch (e) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `required observer at index ${i} must be a 28-byte script hash or a CBOR-encoded script credential: ${String(e)}`,
      );
    }

    if (credential.kind() !== CML.CredentialKind.Script) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `required observer at index ${i} must be a script credential`,
      );
    }

    const scriptHash = credential.as_script();
    if (scriptHash === undefined) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `required observer at index ${i} failed to decode script hash`,
      );
    }

    const observerHex = scriptHash.to_hex();
    if (seen.has(observerHex)) {
      return reject(
        txId,
        RejectCodes.InvalidFieldType,
        `duplicate required observer ${observerHex}`,
      );
    }
    seen.add(observerHex);
    observers.push(observerHex);
  }
  return observers;
};

const validateRedeemerWitnesses = (
  txId: Buffer,
  redeemers: Uint8Array | undefined,
): boolean | RejectedTx => {
  if (redeemers === undefined) return false;
  try {
    decodeMidgardRedeemers(redeemers);
    return true;
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `redeemer witness decode failed: ${String(e)}`,
    );
  }
};

const validateOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => {
  let tx: Transaction;
  try {
    tx = decodeTransaction(queuedTx.txCbor);
  } catch (e) {
    if (e instanceof MidgardTxCodecError) {
      const detail =
        e.detail === null
          ? `${e.code}: ${e.message}`
          : `${e.code}: ${e.message} (${e.detail})`;
      return reject(queuedTx.txId, RejectCodes.CborDeserialization, detail);
    }
    return reject(
      queuedTx.txId,
      RejectCodes.CborDeserialization,
      `failed to decode tx: ${String(e)}`,
    );
  }

  const computedTxId = Buffer.from(transactionId(tx));
  if (!computedTxId.equals(queuedTx.txId)) {
    return reject(
      queuedTx.txId,
      RejectCodes.TxHashMismatch,
      `queued tx_id ${queuedTx.txId.toString("hex")} != computed ${computedTxId.toString("hex")}`,
    );
  }

  if (!tx.is_valid) {
    return reject(queuedTx.txId, RejectCodes.IsValidFalseForbidden);
  }

  const body = tx.body;
  const ws = tx.witness_set;

  if (body.auxiliary_data_hash !== undefined) {
    return reject(
      queuedTx.txId,
      RejectCodes.AuxDataForbidden,
      "auxiliary_data must be omitted",
    );
  }

  const expectedNetworkId = Number(config.expectedNetworkId);
  if (
    body.network_id !== undefined &&
    body.network_id !== expectedNetworkId
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.NetworkIdMismatch,
      `${body.network_id} != ${expectedNetworkId}`,
    );
  }

  const txFee = body.fee;
  const minFee =
    config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
  if (txFee < minFee) {
    return reject(queuedTx.txId, RejectCodes.MinFee, `${txFee} < ${minFee}`);
  }

  if (body.inputs.length === 0) {
    return reject(queuedTx.txId, RejectCodes.EmptyInputs);
  }
  const spent: Buffer[] = [];
  const seenInputs = new Set<string>();
  for (const input of body.inputs) {
    const cborInput = outRefToCborInput(input);
    const outRefHex = cborInput.toString("hex");
    if (seenInputs.has(outRefHex)) {
      return reject(queuedTx.txId, RejectCodes.DuplicateInputInTx, outRefHex);
    }
    seenInputs.add(outRefHex);
    spent.push(cborInput);
  }

  const referenceInputs: Buffer[] = [];
  const seenReferenceInputs = new Set<string>();
  for (const input of body.reference_inputs ?? []) {
    const cborInput = outRefToCborInput(input);
    const outRefHex = cborInput.toString("hex");
    if (seenReferenceInputs.has(outRefHex)) {
      return reject(
        queuedTx.txId,
        RejectCodes.DuplicateInputInTx,
        `duplicate reference input ${outRefHex}`,
      );
    }
    if (seenInputs.has(outRefHex)) {
      return reject(
        queuedTx.txId,
        RejectCodes.DuplicateInputInTx,
        `outref appears in both spend and reference inputs ${outRefHex}`,
      );
    }
    seenReferenceInputs.add(outRefHex);
    referenceInputs.push(cborInput);
  }

  const txHash = CML.TransactionHash.from_raw_bytes(queuedTx.txId);
  let outputSum = CML.Value.zero();
  const produced: LedgerEntry[] = [];
  for (let i = 0; i < body.outputs.length; i++) {
    const output: TransactionOutput = body.outputs[i];
    if (output.value.coin < 0n) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `negative coin in output ${i}`,
      );
    }
    try {
      outputSum = outputSum.checked_add(midgardTsValueToCmlValue(output.value));
    } catch (e) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `output ${i} value sum failed: ${String(e)}`,
      );
    }
    const outputBytes = Buffer.from(encodeTransactionOutput(output));
    let addressText: string;
    try {
      addressText = encodeMidgardAddressText(output.address);
    } catch (e) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `output ${i} address decode failed: ${String(e)}`,
      );
    }
    produced.push({
      [LedgerColumns.TX_ID]: queuedTx.txId,
      [LedgerColumns.OUTREF]: Buffer.from(
        CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
      ),
      [LedgerColumns.OUTPUT]: outputBytes,
      [LedgerColumns.ADDRESS]: addressText,
    });
  }

  const validityIntervalStart =
    body.validity_interval_start === undefined
      ? undefined
      : BigInt(body.validity_interval_start);
  const validityIntervalEnd =
    body.ttl === undefined ? undefined : BigInt(body.ttl);
  if (
    (validityIntervalStart !== undefined && validityIntervalStart < 0n) ||
    (validityIntervalEnd !== undefined && validityIntervalEnd < 0n)
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity bounds must be non-negative",
    );
  }
  if (
    validityIntervalStart !== undefined &&
    validityIntervalEnd !== undefined &&
    validityIntervalStart > validityIntervalEnd
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `${validityIntervalStart} > ${validityIntervalEnd}`,
    );
  }

  const decodedWitnesses = decodeVKeyWitnesses(
    queuedTx.txId,
    ws.vkey_witnesses ?? [],
  );
  if ("code" in decodedWitnesses) {
    return decodedWitnesses;
  }
  const { witnessKeyHashes, witnessSignerSet, publicKeys, signatures } =
    decodedWitnesses;

  const requiredSignersResult = validateRequiredSigners(
    queuedTx.txId,
    body.required_signers,
  );
  if ("code" in requiredSignersResult) {
    return requiredSignersResult;
  }
  const requiredSigners = requiredSignersResult;

  const requiredObserversResult = validateRequiredObservers(
    queuedTx.txId,
    body.required_observers,
  );
  if ("code" in requiredObserversResult) {
    return requiredObserversResult;
  }
  const requiredObserverHashes = requiredObserversResult;

  let mintPolicyHashes: readonly string[] = [];
  let mintedValue: InstanceType<typeof CML.Value> = CML.Value.zero();
  let burnedValue: InstanceType<typeof CML.Value> = CML.Value.zero();
  if (body.mint !== undefined) {
    const summary = summariseMint(body.mint);
    mintPolicyHashes = summary.policyIds;
    mintedValue = summary.mintedValue;
    burnedValue = summary.burnedValue;
  }

  if (requiredSigners.length > 0 && witnessKeyHashes.length === 0) {
    return reject(
      queuedTx.txId,
      RejectCodes.MissingRequiredWitness,
      "missing vkey witnesses",
    );
  }

  for (const requiredSigner of requiredSigners) {
    if (!witnessSignerSet.has(requiredSigner)) {
      return reject(
        queuedTx.txId,
        RejectCodes.MissingRequiredWitness,
        `missing witness for signer ${requiredSigner}`,
      );
    }
  }

  // Converted ingress must still prove authorization over the Midgard-native
  // body hash; Cardano-domain signature hashes are not admitted.
  const txBodyHashBytes = transactionBodyHash(body);
  const signatureResult = verifyVKeyWitnessSignatures(
    queuedTx.txId,
    txBodyHashBytes,
    publicKeys,
    signatures,
  );
  if (signatureResult !== null) {
    return signatureResult;
  }

  const classifiedScripts = classifyScriptWitnesses(
    queuedTx.txId,
    ws.scripts ?? [],
    validityIntervalStart,
    validityIntervalEnd,
    witnessSignerSet,
  );
  if ("code" in classifiedScripts) {
    return classifiedScripts;
  }
  const { nativeScriptHashes, plutusScriptHashes } = classifiedScripts;

  const redeemerWitnessesResult = validateRedeemerWitnesses(
    queuedTx.txId,
    ws.redeemers,
  );
  if (
    typeof redeemerWitnessesResult === "object" &&
    redeemerWitnessesResult !== null &&
    "code" in redeemerWitnessesResult
  ) {
    return redeemerWitnessesResult;
  }
  const hasRedeemerWitnesses = redeemerWitnessesResult;

  const requiresPlutusEvaluation =
    plutusScriptHashes.length > 0 ||
    hasRedeemerWitnesses ||
    body.script_data_hash !== undefined;

  if (requiresPlutusEvaluation && body.script_data_hash === undefined) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      "missing script_integrity_hash for plutus witness bundle",
    );
  }

  if (
    requiresPlutusEvaluation &&
    requiredObserverHashes.length > 0 &&
    body.network_id === undefined
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      "network_id is required when plutus witness bundles use required observers",
    );
  }

  return {
    txId: queuedTx.txId,
    txCbor: queuedTx.txCbor,
    arrivalSeq: queuedTx.arrivalSeq,
    fee: txFee,
    validityIntervalStart,
    validityIntervalEnd,
    referenceInputs,
    outputSum,
    witnessKeyHashes,
    requiredObserverHashes,
    mintPolicyHashes,
    mintedValue,
    burnedValue,
    nativeScriptHashes,
    plutusScriptHashes,
    requiresPlutusEvaluation,
    processedTx: {
      txId: queuedTx.txId,
      txCbor: queuedTx.txCbor,
      spent,
      produced,
    },
  };
};

export const runPhaseAValidation = (
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): Effect.Effect<PhaseAResult> =>
  Effect.gen(function* () {
    const orderedResults = yield* Effect.forEach(
      queuedTxs,
      (queuedTx) => Effect.sync(() => validateOne(queuedTx, config)),
      {
        concurrency: config.concurrency <= 0 ? "unbounded" : config.concurrency,
      },
    );

    const accepted: PhaseAAccepted[] = [];
    const rejected: RejectedTx[] = [];
    for (const item of orderedResults) {
      if ("processedTx" in item) {
        accepted.push(item);
      } else {
        rejected.push(item);
      }
    }

    return { accepted, rejected };
  });
