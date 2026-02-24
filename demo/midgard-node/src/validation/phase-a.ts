import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { LedgerUtils } from "@/database/index.js";
import {
  MidgardTxCodecError,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
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

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);
const EMPTY_LIST_ROOT = computeHash32(EMPTY_CBOR_LIST);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

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
  readonly witnessSigners: InstanceType<typeof CML.Ed25519KeyHashList>;
  readonly witnesses: readonly InstanceType<typeof CML.Vkeywitness>[];
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
    const witnesses: InstanceType<typeof CML.Vkeywitness>[] = [];

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
  witnesses: readonly InstanceType<typeof CML.Vkeywitness>[],
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

const decodeAndVerifyNativeScripts = (
  txId: Buffer,
  preimageCbor: Uint8Array,
  validityIntervalStart: bigint | undefined,
  validityIntervalEnd: bigint | undefined,
  witnessSigners: InstanceType<typeof CML.Ed25519KeyHashList>,
): string[] | RejectedTx => {
  try {
    const scripts = decodeMidgardNativeByteListPreimage(
      preimageCbor,
      "native.script_tx_wits",
    );
    const hashes: string[] = [];
    for (let i = 0; i < scripts.length; i++) {
      const script = CML.NativeScript.from_cbor_bytes(scripts[i]);
      const hash = script.hash();
      if (hash === undefined) {
        throw new Error(`native script hash undefined at index ${i}`);
      }
      hashes.push(hash.to_hex());
      if (
        !script.verify(validityIntervalStart, validityIntervalEnd, witnessSigners)
      ) {
        return reject(
          txId,
          RejectCodes.NativeScriptInvalid,
          `native script verification failed for script index ${i}`,
        );
      }
    }
    return hashes;
  } catch (e) {
    return reject(
      txId,
      RejectCodes.InvalidFieldType,
      `native script witness decode failed: ${String(e)}`,
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

const validateNativeOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => {
  let nativeTx: ReturnType<typeof decodeMidgardNativeTxFull>;
  try {
    nativeTx = decodeMidgardNativeTxFull(queuedTx.txCbor);
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

  const computedTxId = computeMidgardNativeTxIdFromFull(nativeTx);
  if (!computedTxId.equals(queuedTx.txId)) {
    return reject(
      queuedTx.txId,
      RejectCodes.TxHashMismatch,
      `queued tx_id ${queuedTx.txId.toString("hex")} != native ${computedTxId.toString("hex")}`,
    );
  }

  if (nativeTx.compact.validity !== "TxIsValid") {
    return reject(queuedTx.txId, RejectCodes.IsValidFalseForbidden);
  }

  if (!nativeTx.body.auxiliaryDataHash.equals(EMPTY_NULL_ROOT)) {
    return reject(
      queuedTx.txId,
      RejectCodes.AuxDataForbidden,
      "auxiliary_data_hash must match canonical empty hash",
    );
  }

  if (!nativeTx.body.mintRoot.equals(EMPTY_LIST_ROOT)) {
    return reject(queuedTx.txId, RejectCodes.MintForbidden, "mint_root");
  }

  if (!nativeTx.body.requiredObserversRoot.equals(EMPTY_LIST_ROOT)) {
    return reject(
      queuedTx.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "required_observers",
    );
  }

  if (!nativeTx.body.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)) {
    return reject(
      queuedTx.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "script_integrity_hash",
    );
  }

  if (!nativeTx.witnessSet.redeemerTxWitsRoot.equals(EMPTY_LIST_ROOT)) {
    return reject(
      queuedTx.txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "redeemer_tx_wits",
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

  const spentDecoded = decodeOutRefListFromPreimage(
    queuedTx.txId,
    nativeTx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  if ("code" in spentDecoded) {
    return spentDecoded;
  }
  const spent = spentDecoded;
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

  const referenceInputsDecoded = decodeOutRefListFromPreimage(
    queuedTx.txId,
    nativeTx.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  if ("code" in referenceInputsDecoded) {
    return referenceInputsDecoded;
  }
  const referenceInputs = referenceInputsDecoded;

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
  const produced: LedgerUtils.Entry[] = [];
  for (let i = 0; i < outputBytes.length; i++) {
    const outputCbor = outputBytes[i];
    try {
      const output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
      const amount = output.amount();
      if (amount.coin() < 0n) {
        return reject(
          queuedTx.txId,
          RejectCodes.InvalidOutput,
          `negative coin in output ${i}`,
        );
      }
      outputSum = outputSum.checked_add(amount);
      produced.push({
        [LedgerUtils.Columns.TX_ID]: queuedTx.txId,
        [LedgerUtils.Columns.OUTREF]: Buffer.from(
          CML.TransactionInput.new(txHash, BigInt(i)).to_cbor_bytes(),
        ),
        [LedgerUtils.Columns.OUTPUT]: outputCbor,
        [LedgerUtils.Columns.ADDRESS]: output.address().to_bech32(),
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
  const validityIntervalEnd = toOptionalValidity(nativeTx.body.validityIntervalEnd);
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
  const {
    witnessKeyHashes,
    witnessSignerSet,
    witnessSigners,
  } = witnessVerificationResult;

  const requiredSignersResult = decodeNativeRequiredSigners(
    queuedTx.txId,
    nativeTx.body.requiredSignersPreimageCbor,
  );
  if ("code" in requiredSignersResult) {
    return requiredSignersResult;
  }
  const requiredSigners = requiredSignersResult;

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

  const signatureResult = verifyNativeWitnessSignatures(
    queuedTx.txId,
    nativeTx.compact.transactionBodyHash,
    witnessVerificationResult.witnesses,
  );
  if (signatureResult !== null) {
    return signatureResult;
  }

  const nativeScriptHashesResult = decodeAndVerifyNativeScripts(
    queuedTx.txId,
    nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    validityIntervalStart,
    validityIntervalEnd,
    witnessSigners,
  );
  if ("code" in nativeScriptHashesResult) {
    return nativeScriptHashesResult;
  }
  const nativeScriptHashes = nativeScriptHashesResult;

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
    nativeScriptHashes,
    processedTx: {
      txId: queuedTx.txId,
      txCbor: queuedTx.txCbor,
      spent,
      produced,
    },
  };
};

const validateOne = (
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx => validateNativeOne(queuedTx, config);

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
