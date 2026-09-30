import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  MIDGARD_NATIVE_TX_VERSION,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  midgardRedeemerPurposeFromTag,
  sortMidgardMintItems,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec/forced";

import { decodeMidgardRedeemers } from "../midgard-redeemers.js";
import {
  assertBufferEquals,
  assertHash28,
  assertHash32,
  copyBuffer,
  copyNativeTxCompact,
  copyNativeWitnessSetCompact,
  decodeHashList,
  decodeOutputs,
  decodeOutRefList,
  encodeOptionalNetworkId,
  encodeOptionalPosixTime,
  encodeOutputs,
  encodeOutRefList,
  failEncode,
  MidgardLedgerOutputDecodeError,
  MidgardLedgerTxDecodeError,
  optionalNetworkId,
  optionalPosixTime,
} from "./codec.copy-native-tx-compact.js";
import {
  decodeMint,
  decodeObserverHashes,
  decodeScriptWitnesses,
  decodeVKeyWitnesses,
  encodeHashList,
  encodeScriptWitnesses,
  encodeVKeyWitnesses,
  ensureAssetName,
} from "./codec.encode-vkey-witnesses.js";
import type {
  MidgardAssetName,
  MidgardLedgerMint,
  MidgardLedgerRedeemer,
  MidgardLedgerTx,
  MidgardPolicyId,
  MidgardSubmittedTx,
  MidgardTxId,
} from "./types.js";

/**
 * §5.6: `82 \u2016 58 1C policy_id \u2016 map(k) \u2016 asset entries` per policy item, inside
 * the §5.1 envelope. The retired raw-map form is prohibited.
 *
 * The flat asset list is grouped by policy here — that grouping and its
 * duplicate check are this module's own invariant about `MidgardLedgerMint`'s
 * shape, and they have no counterpart in the byte grammar. Ordering is then
 * *enforced* by `encodeMidgardFieldPreimageForField` at both levels rather
 * than merely applied here.
 */
const encodeMint = (mint: MidgardLedgerMint): Buffer => {
  const policies = new Map<
    string,
    {
      readonly policyId: MidgardPolicyId;
      readonly assets: Map<
        string,
        { readonly assetName: MidgardAssetName; readonly quantity: bigint }
      >;
    }
  >();

  for (let index = 0; index < mint.assets.length; index += 1) {
    const entry = mint.assets[index];
    const policyId = assertHash28(
      entry.policyId,
      `mint.assets[${index}].policyId`,
    );
    const assetName = ensureAssetName(
      entry.assetName,
      `mint.assets[${index}].assetName`,
    );
    if (entry.quantity === 0n) {
      failEncode(`mint.assets[${index}].quantity cannot be zero`);
    }
    const policyKey = policyId.toString("hex");
    const assetKey = assetName.toString("hex");
    const policy = policies.get(policyKey) ?? {
      policyId,
      assets: new Map<
        string,
        { readonly assetName: MidgardAssetName; readonly quantity: bigint }
      >(),
    };
    if (policy.assets.has(assetKey)) {
      failEncode(
        "duplicate mint asset",
        `policy=${policyKey} asset=${assetKey}`,
      );
    }
    policy.assets.set(assetKey, { assetName, quantity: entry.quantity });
    policies.set(policyKey, policy);
  }

  return encodeMidgardFieldPreimageForField({
    fieldIndex: 5,
    items: sortMidgardMintItems(
      [...policies.values()].map((policy) => ({
        policyId: policy.policyId,
        assets: [...policy.assets.values()],
      })),
    ),
  });
};

export const decodeRedeemers = (
  preimageCbor: Uint8Array,
): MidgardLedgerRedeemer[] =>
  decodeMidgardRedeemers(preimageCbor).map((redeemer) => ({
    tag: redeemer.tag,
    index: redeemer.index,
    dataCbor: Buffer.from(redeemer.dataCborHex, "hex"),
    exUnits: {
      memory: redeemer.exUnits.memory,
      steps: redeemer.exUnits.steps,
    },
  }));

/**
 * §5.1/§5.3: field 8 is the enveloped list of `enc_8` items. Pointer ordering and
 * duplicate rejection stay here — they are this module's invariant about which
 * redeemers may coexist, not a property of the byte grammar.
 */
const encodeRedeemers = (
  redeemers: readonly MidgardLedgerRedeemer[],
): Buffer => {
  const seen = new Set<string>();
  const ordered = [...redeemers].sort((left, right) => {
    if (left.tag !== right.tag) {
      return left.tag - right.tag;
    }
    return left.index < right.index ? -1 : left.index > right.index ? 1 : 0;
  });
  return encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: ordered.map((redeemer) => {
      const key = `${redeemer.tag}:${redeemer.index.toString(10)}`;
      if (seen.has(key)) {
        failEncode("duplicate redeemer pointer", key);
      }
      seen.add(key);
      return {
        purpose: midgardRedeemerPurposeFromTag(redeemer.tag),
        index: redeemer.index,
        redeemerCbor: redeemer.dataCbor,
        executionUnits: {
          memory: redeemer.exUnits.memory,
          steps: redeemer.exUnits.steps,
        },
      };
    }),
  });
};

export const expectedRequiresPlutusEvaluation = (
  tx: Pick<
    MidgardLedgerTx,
    "plutusScriptHashes" | "redeemers" | "scriptIntegrityHash"
  >,
): boolean =>
  tx.plutusScriptHashes.length > 0 ||
  tx.redeemers.length > 0 ||
  !Buffer.from(tx.scriptIntegrityHash).equals(EMPTY_NULL_ROOT);

const assertRequiresPlutusEvaluation = (tx: MidgardLedgerTx): void => {
  const expected = expectedRequiresPlutusEvaluation(tx);
  if (tx.requiresPlutusEvaluation !== expected) {
    failEncode(
      "requiresPlutusEvaluation mismatch",
      `expected=${expected} actual=${tx.requiresPlutusEvaluation}`,
    );
  }
};

export const toNativeTx = (tx: MidgardLedgerTx): MidgardNativeTxFull => {
  if (tx.validity === undefined)
    throw new Error(
      "A forced execution view cannot be encoded as a normal transaction",
    );
  assertRequiresPlutusEvaluation(tx);
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: encodeOutRefList(tx.spendInputs, "spendInputs"),
    referenceInputsPreimageCbor: encodeOutRefList(
      tx.referenceInputs,
      "referenceInputs",
    ),
    outputsPreimageCbor: encodeOutputs(tx.outputs),
    fee: tx.fee,
    validityIntervalStart: encodeOptionalPosixTime(tx.validityIntervalStart),
    validityIntervalEnd: encodeOptionalPosixTime(tx.validityIntervalEnd),
    requiredObserversPreimageCbor: encodeHashList(
      tx.requiredObserverHashes,
      "requiredObserverHashes",
    ),
    requiredSignersPreimageCbor: encodeHashList(
      tx.requiredSignerHashes,
      "requiredSignerHashes",
    ),
    mintPreimageCbor: encodeMint(tx.mint),
    scriptIntegrityHash: assertHash32(
      tx.scriptIntegrityHash,
      "scriptIntegrityHash",
    ),
    auxiliaryDataHash: assertHash32(tx.auxiliaryDataHash, "auxiliaryDataHash"),
    networkId: encodeOptionalNetworkId(tx.networkId),
  };
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: encodeVKeyWitnesses(tx),
    scriptTxWitsPreimageCbor: encodeScriptWitnesses(tx),
    redeemerTxWitsPreimageCbor: encodeRedeemers(tx.redeemers),
  };
  const nativeTx: MidgardNativeTxFull = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: tx.validity,
    body,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, tx.validity),
  };
  const computedTxId = computeMidgardNativeTxId(nativeTx);
  assertBufferEquals("txId", tx.txId, computedTxId);
  return nativeTx;
};

const decodeMidgardLedgerTxFromNativeTx = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): MidgardLedgerTx => {
  const vkeyWitnesses = decodeVKeyWitnesses(
    nativeTx.witnessSet.addrTxWitsPreimageCbor,
  );
  const scriptWitnesses = decodeScriptWitnesses(
    nativeTx.witnessSet.scriptTxWitsPreimageCbor,
  );
  const redeemers = decodeRedeemers(
    nativeTx.witnessSet.redeemerTxWitsPreimageCbor,
  );
  const tx: MidgardLedgerTx = {
    txId: computeMidgardNativeTxId(nativeTx.compact) as MidgardTxId,
    ...("validity" in nativeTx ? { validity: nativeTx.validity } : {}),
    fee: nativeTx.body.fee,
    networkId: optionalNetworkId(nativeTx.body.networkId),
    validityIntervalStart: optionalPosixTime(
      nativeTx.body.validityIntervalStart,
    ),
    validityIntervalEnd: optionalPosixTime(nativeTx.body.validityIntervalEnd),
    auxiliaryDataHash: copyBuffer(nativeTx.body.auxiliaryDataHash),
    scriptIntegrityHash: copyBuffer(nativeTx.body.scriptIntegrityHash),
    spendInputs: decodeOutRefList(
      nativeTx.body.spendInputsPreimageCbor,
      "native.spend_inputs",
    ),
    referenceInputs: decodeOutRefList(
      nativeTx.body.referenceInputsPreimageCbor,
      "native.reference_inputs",
    ),
    outputs: decodeOutputs(nativeTx.body.outputsPreimageCbor),
    requiredSignerHashes: decodeHashList(
      nativeTx.body.requiredSignersPreimageCbor,
      "native.required_signers",
    ),
    requiredObserverHashes: decodeObserverHashes(
      nativeTx.body.requiredObserversPreimageCbor,
    ),
    vkeyWitnesses: vkeyWitnesses.vkeyWitnesses,
    witnessKeyHashes: vkeyWitnesses.witnessKeyHashes,
    scriptWitnesses: scriptWitnesses.scriptWitnesses,
    nativeScriptHashes: scriptWitnesses.nativeScriptHashes,
    plutusScriptHashes: scriptWitnesses.plutusScriptHashes,
    redeemers,
    mint: decodeMint(nativeTx.body.mintPreimageCbor),
    requiresPlutusEvaluation: expectedRequiresPlutusEvaluation({
      plutusScriptHashes: scriptWitnesses.plutusScriptHashes,
      redeemers,
      scriptIntegrityHash: nativeTx.body.scriptIntegrityHash,
    }),
  };
  return tx;
};

const envelopeFromNativeTx = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
  txCbor: Uint8Array,
  sourceKind: "normal" | "forced",
): MidgardSubmittedTx => {
  const witnessSetCompact = copyNativeWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  );
  return {
    txCbor: Buffer.from(txCbor),
    sourceKind,
    ledgerTx: decodeMidgardLedgerTxFromNativeTx(nativeTx),
    commitments: {
      transactionCompact: copyNativeTxCompact(nativeTx.compact),
      witnessSetCompact,
      redeemerWitnessHash: copyBuffer(witnessSetCompact.redeemerTxWitsHash),
    },
  };
};

export const decodeMidgardSubmittedTxFromCanonicalCbor = (
  txCbor: Uint8Array,
  sourceKind: "normal" | "forced" = "normal",
): MidgardSubmittedTx => {
  let nativeTx: MidgardNativeTxFull | MidgardForcedTxFull;
  try {
    nativeTx = (
      sourceKind === "forced"
        ? decodeMidgardForcedTxFullFromCanonicalCbor
        : decodeMidgardNativeTxFullFromCanonicalCbor
    )(txCbor);
  } catch (e) {
    throw new MidgardLedgerTxDecodeError("canonical-cbor", e);
  }

  try {
    return envelopeFromNativeTx(nativeTx, txCbor, sourceKind);
  } catch (e) {
    throw new MidgardLedgerTxDecodeError(
      "ledger",
      e instanceof MidgardLedgerOutputDecodeError ? e.causeValue : e,
      e instanceof MidgardLedgerOutputDecodeError,
      e instanceof MidgardLedgerOutputDecodeError ? e.outputIndex : undefined,
    );
  }
};
