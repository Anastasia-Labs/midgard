import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeMint,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
  hashMidgardVersionedScript,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
  MidgardTxCodecError,
  midgardValueToCmlValue,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import { decodeMidgardRedeemers } from "./midgard-redeemers.js";
import {
  PhaseAAccepted,
  PhaseAConfig,
  PhaseAResult,
  QueuedTx,
  RejectCode,
  RejectCodes,
  RejectedTx,
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

const toOptionalValidity = (value: bigint): bigint | undefined =>
  value === MIDGARD_POSIX_TIME_NONE ? undefined : value;

const decodeOutRefListFromPreimage = (
  txId: Buffer,
  preimageCbor: Uint8Array,
  fieldName: string,
): Buffer[] | RejectedTx => {
  try {
    const raw = decodeMidgardNativeByteListPreimage(preimageCbor, fieldName);
    return raw.map((bytes) =>
      Buffer.from(CML.TransactionInput.from_cbor_bytes(bytes).to_cbor_bytes()),
    );
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `${fieldName} decode failed: ${String(e)}`,
    );
  }
};

type NativeWitnessVerification = {
  readonly witnessKeyHashes: readonly string[];
  readonly witnessSignerSet: ReadonlySet<string>;
  readonly witnessSigners: CML.Ed25519KeyHashList;
  readonly witnesses: readonly CML.Vkeywitness[];
};

type DecodedScriptWitnesses = {
  readonly nativeScriptHashes: readonly string[];
  readonly plutusScriptHashes: readonly string[];
};

const decodeNativeWitnesses = (
  txId: Buffer,
  preimageCbor: Uint8Array,
): NativeWitnessVerification | RejectedTx => {
  try {
    const witnessBytes = decodeMidgardNativeByteListPreimage(
      preimageCbor,
      "native.addr_tx_wits",
    );
    const witnessSigners = CML.Ed25519KeyHashList.new();
    const witnessSignerSet = new Set<string>();
    const witnessKeyHashes: string[] = [];
    const witnesses: CML.Vkeywitness[] = [];

    for (let i = 0; i < witnessBytes.length; i++) {
      const witness = CML.Vkeywitness.from_cbor_bytes(witnessBytes[i]);
      witnesses.push(witness);
      const signer = witness.vkey().hash();
      const signerHex = signer.to_hex();
      if (!witnessSignerSet.has(signerHex)) {
        witnessSignerSet.add(signerHex);
        witnessSigners.add(signer);
        witnessKeyHashes.push(signerHex);
      }
    }

    return {
      witnessKeyHashes,
      witnessSignerSet,
      witnessSigners,
      witnesses,
    };
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native vkey witness decode failed: ${String(e)}`,
    );
  }
};

const verifyNativeWitnessSignatures = (
  txId: Buffer,
  txBodyHash: Uint8Array,
  witnesses: readonly CML.Vkeywitness[],
): RejectedTx | null => {
  for (let i = 0; i < witnesses.length; i++) {
    const witness = witnesses[i];
    const signature = witness.ed25519_signature();
    if (!witness.vkey().verify(txBodyHash, signature)) {
      return reject(
        txId,
        RejectCodes.InvalidSignature,
        `invalid native vkey witness #${i}`,
      );
    }
  }
  return null;
};

const decodeAndClassifyScriptWitnesses = (
  txId: Buffer,
  preimageCbor: Uint8Array,
  validityIntervalStart: bigint | undefined,
  validityIntervalEnd: bigint | undefined,
  witnessSigners: ReadonlySet<string>,
): DecodedScriptWitnesses | RejectedTx => {
  try {
    const scripts = decodeMidgardVersionedScriptListPreimage(
      preimageCbor,
      "native.script_tx_wits",
    );
    const nativeScriptHashes: string[] = [];
    const plutusScriptHashes: string[] = [];
    for (let i = 0; i < scripts.length; i++) {
      const script = scripts[i];
      const hash = hashMidgardVersionedScript(script);
      if (script.language === "NativeCardano") {
        nativeScriptHashes.push(hash);
        if (
          !verifyMidgardNativeScript(script.nativeScript, {
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
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native script witness decode failed: ${String(e)}`,
    );
  }
};

const decodeNativeRedeemerWitnesses = (
  txId: Buffer,
  preimageCbor: Uint8Array,
): boolean | RejectedTx => {
  if (Buffer.from(preimageCbor).equals(EMPTY_CBOR_LIST)) {
    return false;
  }
  try {
    decodeMidgardRedeemers(preimageCbor);
    return true;
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native redeemer witness decode failed: ${String(e)}`,
    );
  }
};

const decodeNativeRequiredSigners = (
  txId: Buffer,
  preimageCbor: Uint8Array,
): string[] | RejectedTx => {
  try {
    const signerBytes = decodeMidgardNativeByteListPreimage(
      preimageCbor,
      "native.required_signers",
    );
    const signers: string[] = [];
    for (let i = 0; i < signerBytes.length; i++) {
      const signer = signerBytes[i];
      if (signer.length !== 28) {
        return reject(
          txId,
          RejectCodes.InvalidFieldType,
          `required signer at index ${i} must be 28 bytes`,
        );
      }
      signers.push(signer.toString("hex"));
    }
    return signers;
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native required signers decode failed: ${String(e)}`,
    );
  }
};

const decodeNativeRequiredObservers = (
  txId: Buffer,
  preimageCbor: Uint8Array,
): string[] | RejectedTx => {
  try {
    const observerBytes = decodeMidgardNativeByteListPreimage(
      preimageCbor,
      "native.required_observers",
    );
    const observers: string[] = [];
    const seenObservers = new Set<string>();
    for (let i = 0; i < observerBytes.length; i++) {
      const observer = observerBytes[i];
      let observerHex: string;
      if (observer.length === 28) {
        observerHex = observer.toString("hex");
      } else {
        let credential: CML.Credential;
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
        observerHex = credential.as_script()!.to_hex();
      }

      if (seenObservers.has(observerHex)) {
        return reject(
          txId,
          RejectCodes.InvalidFieldType,
          `duplicate required observer ${observerHex}`,
        );
      }
      seenObservers.add(observerHex);
      observers.push(observerHex);
    }
    return observers;
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native required observers decode failed: ${String(e)}`,
    );
  }
};

const validateNativeOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => {
  let nativeTx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>;
  try {
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(queuedTx.txCbor);
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
      `failed to decode native tx: ${String(e)}`,
    );
  }

  const computedTxId = computeMidgardNativeTxId(nativeTx);
  if (!computedTxId.equals(queuedTx.txId)) {
    return reject(
      queuedTx.txId,
      RejectCodes.TxHashMismatch,
      `queued tx_id ${queuedTx.txId.toString("hex")} != native ${computedTxId.toString("hex")}`,
    );
  }

  if (nativeTx.validity !== "TxIsValid") {
    return reject(queuedTx.txId, RejectCodes.IsValidFalseForbidden);
  }

  if (!nativeTx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    return reject(
      queuedTx.txId,
      RejectCodes.AuxDataForbidden,
      "auxiliary_data_hash must match canonical empty hash",
    );
  }

  if (
    nativeTx.body.networkId !== MIDGARD_NATIVE_NETWORK_ID_NONE &&
    nativeTx.body.networkId !== config.expectedNetworkId
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.NetworkIdMismatch,
      `${nativeTx.body.networkId} != ${config.expectedNetworkId}`,
    );
  }

  const txFee = nativeTx.body.fee;
  const minFee =
    config.minFeeA * BigInt(queuedTx.txCbor.length) + config.minFeeB;
  if (txFee < minFee) {
    return reject(queuedTx.txId, RejectCodes.MinFee, `${txFee} < ${minFee}`);
  }

  const spent = decodeOutRefListFromPreimage(
    queuedTx.txId,
    nativeTx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  if ("code" in spent) {
    return spent;
  }
  if (spent.length === 0) {
    return reject(queuedTx.txId, RejectCodes.EmptyInputs);
  }
  const seenInputs = new Set<string>();
  for (const input of spent) {
    const outRefHex = input.toString("hex");
    if (seenInputs.has(outRefHex)) {
      return reject(queuedTx.txId, RejectCodes.DuplicateInputInTx, outRefHex);
    }
    seenInputs.add(outRefHex);
  }

  const referenceInputs = decodeOutRefListFromPreimage(
    queuedTx.txId,
    nativeTx.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  if ("code" in referenceInputs) {
    return referenceInputs;
  }
  const seenReferenceInputs = new Set<string>();
  for (const referenceInput of referenceInputs) {
    const outRefHex = referenceInput.toString("hex");
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
  }

  /**
   * Normalizes an output into the byte representation used by Phase A validation.
   */
  const outputBytes = (() => {
    try {
      return decodeMidgardNativeByteListPreimage(
        nativeTx.body.outputsPreimageCbor,
        "native.outputs",
      );
    } catch (e) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `native outputs decode failed: ${String(e)}`,
      );
    }
  })();
  if ("code" in outputBytes) {
    return outputBytes;
  }

  const txHash = CML.TransactionHash.from_raw_bytes(queuedTx.txId);
  let outputSum = CML.Value.zero();
  const produced: LedgerEntry[] = [];
  for (let i = 0; i < outputBytes.length; i++) {
    const outputCbor = outputBytes[i];
    try {
      const output = decodeMidgardTxOutput(outputCbor);
      const amount = midgardValueToCmlValue(output.value);
      outputSum = outputSum.checked_add(amount);
      produced.push({
        [LedgerColumns.TX_ID]: queuedTx.txId,
        [LedgerColumns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [LedgerColumns.OUTPUT]: outputCbor,
        [LedgerColumns.ADDRESS]: encodeMidgardAddressText(output.address),
      });
    } catch (e) {
      return reject(
        queuedTx.txId,
        RejectCodes.InvalidOutput,
        `failed to decode output ${i}: ${String(e)}`,
      );
    }
  }

  const validityIntervalStart = toOptionalValidity(
    nativeTx.body.validityIntervalStart,
  );
  const validityIntervalEnd = toOptionalValidity(
    nativeTx.body.validityIntervalEnd,
  );
  if (
    (validityIntervalStart !== undefined && validityIntervalStart < 0n) ||
    (validityIntervalEnd !== undefined && validityIntervalEnd < 0n)
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity bounds must be non-negative unless unbounded sentinel",
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

  const witnessVerificationResult = decodeNativeWitnesses(
    queuedTx.txId,
    nativeTx.witnessSet.addrTxWitsPreimageCbor,
  );
  if ("code" in witnessVerificationResult) {
    return witnessVerificationResult;
  }
  const { witnessKeyHashes, witnessSignerSet } = witnessVerificationResult;

  const requiredSigners = decodeNativeRequiredSigners(
    queuedTx.txId,
    nativeTx.body.requiredSignersPreimageCbor,
  );
  if ("code" in requiredSigners) {
    return requiredSigners;
  }

  const requiredObserverHashes = decodeNativeRequiredObservers(
    queuedTx.txId,
    nativeTx.body.requiredObserversPreimageCbor,
  );
  if ("code" in requiredObserverHashes) {
    return requiredObserverHashes;
  }

  let mintPolicyHashes: readonly string[] = [];
  let mintedValue = CML.Value.zero();
  let burnedValue = CML.Value.zero();
  try {
    const decodedMint = decodeMidgardNativeMint(nativeTx.body.mintPreimageCbor);
    if (decodedMint !== undefined) {
      mintPolicyHashes = decodedMint.policyIds;
      mintedValue = decodedMint.mintedValue;
      burnedValue = decodedMint.burnedValue;
    }
  } catch (e) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      `native mint decode failed: ${String(e)}`,
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
  const signatureResult = verifyNativeWitnessSignatures(
    queuedTx.txId,
    computedTxId,
    witnessVerificationResult.witnesses,
  );
  if (signatureResult !== null) {
    return signatureResult;
  }

  const scriptWitnessesResult = decodeAndClassifyScriptWitnesses(
    queuedTx.txId,
    nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    validityIntervalStart,
    validityIntervalEnd,
    witnessSignerSet,
  );
  if ("code" in scriptWitnessesResult) {
    return scriptWitnessesResult;
  }
  const { nativeScriptHashes, plutusScriptHashes } = scriptWitnessesResult;

  const hasRedeemerWitnesses = decodeNativeRedeemerWitnesses(
    queuedTx.txId,
    nativeTx.witnessSet.redeemerTxWitsPreimageCbor,
  );
  if (typeof hasRedeemerWitnesses !== "boolean") {
    return hasRedeemerWitnesses;
  }

  const requiresPlutusEvaluation =
    plutusScriptHashes.length > 0 ||
    hasRedeemerWitnesses ||
    !nativeTx.body.scriptIntegrityHash.equals(EMPTY_NULL_ROOT);

  if (
    requiresPlutusEvaluation &&
    nativeTx.body.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)
  ) {
    return reject(
      queuedTx.txId,
      RejectCodes.InvalidFieldType,
      "missing script_integrity_hash for plutus witness bundle",
    );
  }

  if (
    requiresPlutusEvaluation &&
    requiredObserverHashes.length > 0 &&
    nativeTx.body.networkId === MIDGARD_NATIVE_NETWORK_ID_NONE
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
      (queuedTx) => Effect.sync(() => validateNativeOne(queuedTx, config)),
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
