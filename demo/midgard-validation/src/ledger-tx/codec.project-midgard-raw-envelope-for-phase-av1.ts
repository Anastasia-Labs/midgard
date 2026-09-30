import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  MidgardScriptHashPrefixes,
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "@al-ft/midgard-core/codec";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  copyBuffer,
  decodeHashList,
  decodeOutputs,
  decodeOutRefList,
  failDecode,
  MidgardLedgerTxDecodeError,
  type MidgardProjectedRawScriptWitness,
  type MidgardRawEnvelopePhaseAProjection,
  optionalNetworkId,
  optionalPosixTime,
} from "./codec.copy-native-tx-compact.js";
import {
  decodeMidgardSubmittedTxFromCanonicalCbor,
  decodeRedeemers,
  expectedRequiresPlutusEvaluation,
  toNativeTx,
} from "./codec.encode-mint.js";
import {
  decodeMint,
  decodeObserverHashes,
  decodeVKeyWitnesses,
} from "./codec.encode-vkey-witnesses.js";
import type {
  MidgardLedgerTx,
  MidgardScriptHash,
  MidgardSubmittedTx,
  MidgardTxId,
} from "./types.js";

const projectRawScriptWitnesses = (
  preimageCbor: Uint8Array,
): readonly MidgardProjectedRawScriptWitness[] =>
  decodeMidgardFieldPreimage(preimageCbor).map((item, index) => {
    const outer = readCborArrayHeader(item, 0, "raw_script_witness");
    if (outer.length !== 2)
      failDecode("raw script witness must contain two fields");
    const language = readCborUnsigned(
      item,
      outer.nextOffset,
      "raw_script_witness.language",
    );
    if (
      language.value !== 0n &&
      language.value !== 3n &&
      language.value !== 128n
    )
      failDecode("raw script witness language is unsupported");
    const payload = readCborBytes(
      item,
      language.nextOffset,
      "raw_script_witness.payload",
    );
    if (payload.nextOffset !== item.length)
      failDecode("raw script witness has trailing bytes");
    const languageTag = Number(language.value) as 0 | 3 | 128;
    const prefix =
      languageTag === 0
        ? MidgardScriptHashPrefixes.NativeCardano
        : languageTag === 3
          ? MidgardScriptHashPrefixes.PlutusV3
          : MidgardScriptHashPrefixes.MidgardV1;
    return Object.freeze({
      index,
      languageTag,
      scriptBytes: Buffer.from(payload.value),
      versionedItemBytes: Buffer.from(item),
      hash: Buffer.from(
        blake2b(
          Buffer.concat([Buffer.from([prefix]), Buffer.from(payload.value)]),
          { dkLen: 28 },
        ),
      ) as MidgardScriptHash,
    });
  });

const validateRawProjectionNonScriptFields = (
  canonical: MidgardRawEnvelopePhaseAProjection["canonical"],
): void => {
  decodeOutRefList(
    canonical.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  decodeOutRefList(
    canonical.body.referenceInputsPreimageCbor,
    "native.reference_inputs",
  );
  decodeOutputs(canonical.body.outputsPreimageCbor);
  decodeObserverHashes(canonical.body.requiredObserversPreimageCbor);
  decodeHashList(
    canonical.body.requiredSignersPreimageCbor,
    "native.required_signers",
  );
  decodeMint(canonical.body.mintPreimageCbor);
  decodeVKeyWitnesses(canonical.witnessSet.addrTxWitsPreimageCbor);
  decodeRedeemers(canonical.witnessSet.redeemerTxWitsPreimageCbor);
};

/**
 * Total Phase-A projection for the sole case where an authenticated field-6
 * native payload is structurally malformed. Every non-field-6 grammar remains
 * strict, while the exact native payload bytes and their script hash survive.
 */
export const projectMidgardRawEnvelopeForPhaseAV1 = (
  txCbor: Uint8Array,
  sourceKind: "normal" | "forced" = "normal",
): MidgardRawEnvelopePhaseAProjection => {
  const material = (
    sourceKind === "forced"
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(txCbor);
  try {
    validateRawProjectionNonScriptFields(material.canonical);
    const scriptWitnesses = projectRawScriptWitnesses(
      material.canonical.witnessSet.scriptTxWitsPreimageCbor,
    );
    let canonicalSubmittedTx: MidgardSubmittedTx | null = null;
    try {
      canonicalSubmittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(
        txCbor,
        sourceKind,
      );
    } catch (error) {
      const cause =
        error instanceof MidgardLedgerTxDecodeError
          ? error.causeValue
          : undefined;
      if (
        !(error instanceof MidgardLedgerTxDecodeError) ||
        error.stage !== "ledger" ||
        !(cause instanceof MidgardTxCodecError) ||
        cause.code !== MidgardTxCodecErrorCodes.CborDecode ||
        !/^native\.script_tx_wits\[\d+\] is not a canonical versioned script$/u.test(
          cause.message,
        )
      )
        throw error;
      // The opaque field-6 native payload is the sole admitted decode gap;
      // all other envelope/body/witness grammars were checked above.
    }
    if (canonicalSubmittedTx !== null) {
      if (
        canonicalSubmittedTx.ledgerTx.scriptWitnesses.length !==
          scriptWitnesses.length ||
        canonicalSubmittedTx.ledgerTx.scriptWitnesses.some(
          (witness, index) =>
            !Buffer.from(witness.hash).equals(scriptWitnesses[index]!.hash),
        )
      )
        failDecode("raw projection differs from canonical script witnesses");
    }
    const nativeScriptHashes = scriptWitnesses
      .filter(({ languageTag }) => languageTag === 0)
      .map(({ hash }) => hash);
    const plutusScriptHashes = scriptWitnesses
      .filter(({ languageTag }) => languageTag !== 0)
      .map(({ hash }) => hash);
    const vkeyWitnesses = decodeVKeyWitnesses(
      material.canonical.witnessSet.addrTxWitsPreimageCbor,
    );
    const redeemers = decodeRedeemers(
      material.canonical.witnessSet.redeemerTxWitsPreimageCbor,
    );
    const ledgerTx: MidgardRawEnvelopePhaseAProjection["ledgerTx"] = {
      txId: Buffer.from(material.transactionId) as MidgardTxId,
      ...("validity" in material.canonical
        ? { validity: material.canonical.validity }
        : {}),
      fee: material.canonical.body.fee,
      networkId: optionalNetworkId(material.canonical.body.networkId),
      validityIntervalStart: optionalPosixTime(
        material.canonical.body.validityIntervalStart,
      ),
      validityIntervalEnd: optionalPosixTime(
        material.canonical.body.validityIntervalEnd,
      ),
      auxiliaryDataHash: copyBuffer(material.canonical.body.auxiliaryDataHash),
      scriptIntegrityHash: copyBuffer(
        material.canonical.body.scriptIntegrityHash,
      ),
      spendInputs: decodeOutRefList(
        material.canonical.body.spendInputsPreimageCbor,
        "native.spend_inputs",
      ),
      referenceInputs: decodeOutRefList(
        material.canonical.body.referenceInputsPreimageCbor,
        "native.reference_inputs",
      ),
      outputs: decodeOutputs(material.canonical.body.outputsPreimageCbor),
      requiredSignerHashes: decodeHashList(
        material.canonical.body.requiredSignersPreimageCbor,
        "native.required_signers",
      ),
      requiredObserverHashes: decodeObserverHashes(
        material.canonical.body.requiredObserversPreimageCbor,
      ),
      vkeyWitnesses: vkeyWitnesses.vkeyWitnesses,
      witnessKeyHashes: vkeyWitnesses.witnessKeyHashes,
      scriptWitnesses,
      nativeScriptHashes,
      plutusScriptHashes,
      redeemers,
      mint: decodeMint(material.canonical.body.mintPreimageCbor),
      requiresPlutusEvaluation: expectedRequiresPlutusEvaluation({
        plutusScriptHashes,
        redeemers,
        scriptIntegrityHash: material.canonical.body.scriptIntegrityHash,
      }),
    };
    return Object.freeze({
      canonical: material.canonical,
      transactionId: Buffer.from(material.transactionId),
      scriptWitnesses: Object.freeze(scriptWitnesses),
      ledgerTx: Object.freeze(ledgerTx),
      canonicalSubmittedTx,
    });
  } catch (error) {
    throw new MidgardLedgerTxDecodeError("ledger", error);
  }
};

export const computeMidgardTxIdFromCanonicalCbor = (
  txCbor: Uint8Array,
): Buffer =>
  Buffer.from(decodeMidgardSubmittedTxFromCanonicalCbor(txCbor).ledgerTx.txId);

export const decodeMidgardTxCommitmentsFromCanonicalCbor = (
  txCbor: Uint8Array,
): MidgardSubmittedTx["commitments"] =>
  decodeMidgardSubmittedTxFromCanonicalCbor(txCbor).commitments;

export const decodeMidgardLedgerTxFromCanonicalCbor = (
  txCbor: Uint8Array,
  sourceKind: "normal" | "forced" = "normal",
): MidgardLedgerTx =>
  decodeMidgardSubmittedTxFromCanonicalCbor(txCbor, sourceKind).ledgerTx;

export const encodeMidgardLedgerTxToCanonicalCbor = (
  tx: MidgardLedgerTx,
): Buffer => encodeMidgardNativeTxCanonical(toNativeTx(tx));
