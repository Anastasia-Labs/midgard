import {
  computeMidgardNativeTxIdV1,
  decodeMidgardMintFieldPreimageV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardSpendInputItemV1,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  deriveMidgardNativeTxCompactV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_NULL_ROOT,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardFieldPreimageForFieldV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardHash28ItemV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_MAX_OUTPUT_INDEX_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxWitnessSetCanonicalV1,
  midgardRedeemerPurposeFromTagV1,
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
  type MidgardTxOutput,
  sortMidgardMintItemsV1,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import {
  decodeMidgardRedeemers,
  redeemerDataFromCborHex,
} from "../midgard-redeemers.js";
import { plutusDataToCborHex } from "../plutus-data.js";
import type {
  MidgardAssetName,
  MidgardCredentialHash,
  MidgardLedgerMint,
  MidgardLedgerMintAsset,
  MidgardLedgerOutput,
  MidgardLedgerRedeemer,
  MidgardLedgerScriptWitness,
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
  MidgardOutRef,
  MidgardPolicyId,
  MidgardScriptHash,
  MidgardSubmittedTx,
  MidgardTxId,
} from "./types.js";

const HASH32_LENGTH = 32;
const HASH28_LENGTH = 28;
const MAX_ASSET_NAME_LENGTH = 32;

const failDecode = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const failEncode = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.CborEncode,
    message,
    detail,
  );
};

export type MidgardLedgerTxDecodeStage = "canonical-cbor" | "ledger";

export class MidgardLedgerTxDecodeError extends Error {
  readonly stage: MidgardLedgerTxDecodeStage;
  readonly causeValue: unknown;
  readonly invalidOutput: boolean;

  constructor(
    stage: MidgardLedgerTxDecodeStage,
    causeValue: unknown,
    invalidOutput = false,
  ) {
    super(`failed to decode Midgard ledger tx at ${stage} stage`);
    this.name = "MidgardLedgerTxDecodeError";
    this.stage = stage;
    this.causeValue = causeValue;
    this.invalidOutput = invalidOutput;
  }
}

class MidgardLedgerOutputDecodeError extends Error {
  readonly causeValue: unknown;

  constructor(causeValue: unknown) {
    super("failed to decode Midgard ledger output");
    this.name = "MidgardLedgerOutputDecodeError";
    this.causeValue = causeValue;
  }
}

const copyBuffer = (value: Uint8Array): Buffer => Buffer.from(value);

const optionalPosixTime = (value: bigint): bigint | undefined =>
  value === MIDGARD_POSIX_TIME_NONE ? undefined : value;

const encodeOptionalPosixTime = (value: bigint | undefined): bigint =>
  value ?? MIDGARD_POSIX_TIME_NONE;

const optionalNetworkId = (value: bigint): bigint | undefined =>
  value === MIDGARD_NATIVE_NETWORK_ID_NONE ? undefined : value;

const encodeOptionalNetworkId = (value: bigint | undefined): bigint =>
  value ?? MIDGARD_NATIVE_NETWORK_ID_NONE;

const assertByteLength = (
  value: Uint8Array,
  length: number,
  fieldName: string,
): Buffer => {
  const bytes = copyBuffer(value);
  if (bytes.length !== length) {
    failEncode(
      `${fieldName} must be ${length} bytes`,
      `length=${bytes.length}`,
    );
  }
  return bytes;
};

const assertHash32 = (value: Uint8Array, fieldName: string): Buffer =>
  assertByteLength(value, HASH32_LENGTH, fieldName);

const assertHash28 = (value: Uint8Array, fieldName: string): Buffer =>
  assertByteLength(value, HASH28_LENGTH, fieldName);

const assertBufferEquals = (
  fieldName: string,
  actual: Uint8Array,
  expected: Uint8Array,
): void => {
  const actualBuffer = copyBuffer(actual);
  const expectedBuffer = copyBuffer(expected);
  if (!actualBuffer.equals(expectedBuffer)) {
    failEncode(
      `${fieldName} mismatch`,
      `expected=${expectedBuffer.toString("hex")} actual=${actualBuffer.toString("hex")}`,
    );
  }
};

const assertBufferArrayEquals = (
  fieldName: string,
  actual: readonly Uint8Array[],
  expected: readonly Uint8Array[],
): void => {
  if (actual.length !== expected.length) {
    failEncode(
      `${fieldName} length mismatch`,
      `expected=${expected.length} actual=${actual.length}`,
    );
  }
  for (let i = 0; i < actual.length; i += 1) {
    assertBufferEquals(`${fieldName}[${i}]`, actual[i], expected[i]);
  }
};

const copyNativeTxCompact = (
  compact: MidgardNativeTxFullV1["compact"],
): MidgardNativeTxFullV1["compact"] => ({
  version: compact.version,
  transactionBody: {
    spendInputsHash: copyBuffer(compact.transactionBody.spendInputsHash),
    referenceInputsHash: copyBuffer(
      compact.transactionBody.referenceInputsHash,
    ),
    outputsHash: copyBuffer(compact.transactionBody.outputsHash),
    fee: compact.transactionBody.fee,
    validityIntervalStart: compact.transactionBody.validityIntervalStart,
    validityIntervalEnd: compact.transactionBody.validityIntervalEnd,
    requiredObserversHash: copyBuffer(
      compact.transactionBody.requiredObserversHash,
    ),
    requiredSignersHash: copyBuffer(
      compact.transactionBody.requiredSignersHash,
    ),
    mintHash: copyBuffer(compact.transactionBody.mintHash),
    scriptIntegrityHash: copyBuffer(
      compact.transactionBody.scriptIntegrityHash,
    ),
    auxiliaryDataHash: copyBuffer(compact.transactionBody.auxiliaryDataHash),
    networkId: compact.transactionBody.networkId,
  },
  transactionWitnessSetHash: copyBuffer(compact.transactionWitnessSetHash),
  validity: compact.validity,
});

const copyNativeWitnessSetCompact = (
  witnessSet: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompactV1>,
): ReturnType<typeof deriveMidgardNativeTxWitnessSetCompactV1> => ({
  addrTxWitsHash: copyBuffer(witnessSet.addrTxWitsHash),
  scriptTxWitsHash: copyBuffer(witnessSet.scriptTxWitsHash),
  redeemerTxWitsHash: copyBuffer(witnessSet.redeemerTxWitsHash),
});

/**
 * The §5.1 envelope over `enc_i` bytes the caller already has. Routed through
 * `midgard-core`'s one §5.1 encoder so the producer shares its width rules with
 * `decodeMidgardNativeByteListPreimage`, which is what reads these back.
 */
const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeMidgardFieldPreimageV1(items);

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes.
 *
 * A Midgard out-ref has exactly one byte spelling, and this is it — ledger MPF
 * trie keys, field-0/1 preimage items and transition-effect source keys are all
 * these same 38 bytes, so this is the inverse of `encodeOutRef` below and of
 * on-chain `decode_midgard_tx_input_cbor`. Every other shape throws: a
 * development ledger written under any other spelling must be reset, not
 * migrated (`docs/spec/midgard-tx.md` §5.3). Tolerating a second shape here
 * would be worse than useless — a stale 36-byte key would decode and re-encode
 * to different bytes, silently re-keying the row instead of failing.
 */
export const decodeMidgardOutRefBytes = (
  inputBytes: Uint8Array,
): MidgardOutRef => {
  const decoded = decodeMidgardSpendInputItemV1(inputBytes);
  return {
    txId: Buffer.from(decoded.txId) as MidgardTxId,
    index: BigInt(decoded.outputIndex),
  };
};

/**
 * §5.3 fields 0/1: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes.
 *
 * This must be the same encoder that produces the ledger MPF trie key
 * (`midgardOutRefToCbor`), because `toNativeTx` re-encodes a decoded ledger
 * transaction and asserts the recomputed tx id: a minimal output index here and
 * a fixed one there would make every round trip fail. On-chain the two are
 * literally one function — `ledger_outref_key` calls `encode_midgard_tx_input`,
 * the field-0/1 item encoder. See `docs/spec/midgard-tx.md` §5.3.
 */
const encodeOutRef = (outRef: MidgardOutRef, fieldName: string): Buffer => {
  if (outRef.index < 0n || outRef.index > BigInt(MIDGARD_MAX_OUTPUT_INDEX_V1)) {
    failEncode(
      `${fieldName}.index must be 0..65,535 (§5.3 fixed uint16 output index)`,
      `index=${outRef.index.toString()}`,
    );
  }
  return encodeMidgardSpendInputItemV1({
    txId: copyBuffer(assertHash32(outRef.txId, `${fieldName}.txId`)),
    outputIndex: Number(outRef.index),
  });
};

const decodeOutRefList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): MidgardOutRef[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    decodeMidgardOutRefBytes,
  );

const encodeOutRefList = (
  outRefs: readonly MidgardOutRef[],
  fieldName: string,
): Buffer =>
  encodeByteList(
    outRefs.map((outRef, index) =>
      encodeOutRef(outRef, `${fieldName}[${index}]`),
    ),
  );

const toLedgerOutput = (output: MidgardTxOutput): MidgardLedgerOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.script_ref === undefined ? {} : { scriptRef: output.script_ref }),
});

const toCodecOutput = (output: MidgardLedgerOutput): MidgardTxOutput => ({
  address: output.address,
  value: output.value,
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.scriptRef === undefined ? {} : { script_ref: output.scriptRef }),
});

const decodeOutputs = (preimageCbor: Uint8Array): MidgardLedgerOutput[] => {
  try {
    return decodeMidgardNativeByteListPreimage(
      preimageCbor,
      "native.outputs",
    ).map((outputBytes) => toLedgerOutput(decodeMidgardTxOutput(outputBytes)));
  } catch (e) {
    throw new MidgardLedgerOutputDecodeError(e);
  }
};

const encodeOutputs = (outputs: readonly MidgardLedgerOutput[]): Buffer =>
  encodeByteList(
    outputs.map((output) => encodeMidgardTxOutput(toCodecOutput(output))),
  );

const decodeHashList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): Buffer[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    (bytes, index) => {
      if (bytes.length !== HASH28_LENGTH) {
        failDecode(
          `${fieldName}[${index}] must be ${HASH28_LENGTH} bytes`,
          `length=${bytes.length}`,
        );
      }
      return copyBuffer(bytes);
    },
  );

/**
 * §5.3 fields 3/4: the item *is* the raw 28-byte hash. `encodeMidgardHash28ItemV1`
 * is the §5.3 encoder; `assertHash28` stays in front of it only to name the field
 * and index in the diagnostic, which the grammar-level encoder cannot.
 */
const encodeHashList = (
  hashes: readonly Uint8Array[],
  fieldName: string,
): Buffer =>
  encodeByteList(
    hashes.map((hash, index) =>
      encodeMidgardHash28ItemV1(assertHash28(hash, `${fieldName}[${index}]`)),
    ),
  );

const decodeObserverHashes = (preimageCbor: Uint8Array): MidgardScriptHash[] =>
  decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.required_observers",
  ).map((observerBytes, index) => {
    if (observerBytes.length === HASH28_LENGTH) {
      return copyBuffer(observerBytes);
    }
    const credential = CML.Credential.from_cbor_bytes(observerBytes);
    if (credential.kind() !== CML.CredentialKind.Script) {
      failDecode(
        "required observer credential must be a script credential",
        `native.required_observers[${index}]`,
      );
    }
    return Buffer.from(credential.as_script()!.to_raw_bytes());
  });

const decodeVKeyWitnesses = (
  preimageCbor: Uint8Array,
): {
  readonly vkeyWitnesses: readonly MidgardLedgerVKeyWitness[];
  readonly witnessKeyHashes: readonly MidgardCredentialHash[];
} => {
  const witnessBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.addr_tx_wits",
  );
  const vkeyWitnesses: MidgardLedgerVKeyWitness[] = [];
  const witnessKeyHashes: MidgardCredentialHash[] = [];
  const seenKeyHashes = new Set<string>();

  for (let index = 0; index < witnessBytes.length; index += 1) {
    const decoded = decodeVKeyWitnessWithCml(witnessBytes[index], index);
    const keyHash = decoded.keyHash;
    const keyHashHex = keyHash.toString("hex");
    if (!seenKeyHashes.has(keyHashHex)) {
      seenKeyHashes.add(keyHashHex);
      witnessKeyHashes.push(keyHash);
    }
    vkeyWitnesses.push(decoded);
  }

  return { vkeyWitnesses, witnessKeyHashes };
};

const decodeVKeyWitnessWithCml = (
  witnessBytes: Uint8Array,
  index: number,
): MidgardLedgerVKeyWitness => {
  const witness = CML.Vkeywitness.from_cbor_bytes(witnessBytes);
  try {
    const vkey = witness.vkey();
    try {
      const keyHash = vkey.hash();
      try {
        const signature = witness.ed25519_signature();
        try {
          return {
            index,
            keyHash: Buffer.from(keyHash.to_raw_bytes()),
            vkey: Buffer.from(vkey.to_raw_bytes()),
            signature: Buffer.from(signature.to_raw_bytes()),
          };
        } finally {
          signature.free();
        }
      } finally {
        keyHash.free();
      }
    } finally {
      vkey.free();
    }
  } finally {
    witness.free();
  }
};

const orderByIndex = <T extends { readonly index: number }>(
  values: readonly T[],
  fieldName: string,
): readonly T[] => {
  const ordered = [...values].sort((left, right) => left.index - right.index);
  for (let i = 0; i < ordered.length; i += 1) {
    if (ordered[i].index !== i) {
      failEncode(`${fieldName} must have contiguous zero-based indexes`);
    }
  }
  return ordered;
};

const encodeVKeyWitnesses = (
  tx: Pick<MidgardLedgerTx, "vkeyWitnesses" | "witnessKeyHashes">,
): Buffer => {
  const orderedWitnesses = orderByIndex(tx.vkeyWitnesses, "vkeyWitnesses");
  const derivedWitnessKeyHashes: MidgardCredentialHash[] = [];
  const seenKeyHashes = new Set<string>();
  const witnessBytes = orderedWitnesses.map((witness) => {
    const publicKey = CML.PublicKey.from_bytes(witness.vkey);
    const derivedKeyHash = Buffer.from(publicKey.hash().to_raw_bytes());
    assertBufferEquals(
      "vkeyWitnesses.keyHash",
      witness.keyHash,
      derivedKeyHash,
    );
    const keyHashHex = derivedKeyHash.toString("hex");
    if (!seenKeyHashes.has(keyHashHex)) {
      seenKeyHashes.add(keyHashHex);
      derivedWitnessKeyHashes.push(derivedKeyHash);
    }
    // §5.3 field 7 is `82 ‖ 58 20 vkey ‖ 58 40 signature`, a Midgard grammar rule
    // — not "whatever CML serializes a Vkeywitness to". They agree today, which is
    // exactly why the dependency was invisible; `encodeMidgardAddressWitnessItemV1`
    // is the encoder that owes the on-chain reader its 101-byte width.
    return encodeMidgardAddressWitnessItemV1({
      verificationKey: Buffer.from(publicKey.to_raw_bytes()),
      signature: witness.signature,
    });
  });

  assertBufferArrayEquals(
    "witnessKeyHashes",
    tx.witnessKeyHashes,
    derivedWitnessKeyHashes,
  );
  return encodeByteList(witnessBytes);
};

const scriptHash = (script: MidgardLedgerScriptWitness["script"]): Buffer =>
  Buffer.from(hashMidgardVersionedScript(script), "hex");

const decodeScriptWitnesses = (
  preimageCbor: Uint8Array,
): {
  readonly scriptWitnesses: readonly MidgardLedgerScriptWitness[];
  readonly nativeScriptHashes: readonly MidgardScriptHash[];
  readonly plutusScriptHashes: readonly MidgardScriptHash[];
} => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
    "native.script_tx_wits",
  );
  const scriptWitnesses: MidgardLedgerScriptWitness[] = [];
  const nativeScriptHashes: MidgardScriptHash[] = [];
  const plutusScriptHashes: MidgardScriptHash[] = [];
  for (let index = 0; index < scripts.length; index += 1) {
    const script = scripts[index];
    const hash = scriptHash(script);
    scriptWitnesses.push({ index, hash, script });
    if (script.language === "NativeCardano") {
      nativeScriptHashes.push(hash);
    } else {
      plutusScriptHashes.push(hash);
    }
  }
  return { scriptWitnesses, nativeScriptHashes, plutusScriptHashes };
};

const encodeScriptWitnesses = (
  tx: Pick<
    MidgardLedgerTx,
    "scriptWitnesses" | "nativeScriptHashes" | "plutusScriptHashes"
  >,
): Buffer => {
  const orderedWitnesses = orderByIndex(tx.scriptWitnesses, "scriptWitnesses");
  const nativeScriptHashes: MidgardScriptHash[] = [];
  const plutusScriptHashes: MidgardScriptHash[] = [];
  const scripts = orderedWitnesses.map((witness) => {
    const derivedHash = scriptHash(witness.script);
    assertBufferEquals("scriptWitnesses.hash", witness.hash, derivedHash);
    if (witness.script.language === "NativeCardano") {
      nativeScriptHashes.push(derivedHash);
    } else {
      plutusScriptHashes.push(derivedHash);
    }
    return witness.script;
  });

  assertBufferArrayEquals(
    "nativeScriptHashes",
    tx.nativeScriptHashes,
    nativeScriptHashes,
  );
  assertBufferArrayEquals(
    "plutusScriptHashes",
    tx.plutusScriptHashes,
    plutusScriptHashes,
  );
  return encodeMidgardVersionedScriptListPreimage(scripts);
};

/**
 * §5.6: field 5 is the enveloped list of per-policy items. The §5.3 decoder
 * enforces policy-id and asset-name ordering, rejects duplicates, an assetless
 * policy and a zero quantity, and requires the 28-byte policy id — so the checks
 * this function used to spell against a raw CBOR map now have exactly one home,
 * shared with the producer's twin. An empty mint is `80`, like every other field.
 */
const decodeMint = (preimageCbor: Uint8Array): MidgardLedgerMint => {
  let items;
  try {
    items = decodeMidgardMintFieldPreimageV1(preimageCbor);
  } catch (e) {
    return failDecode(
      "native.mint is not a canonical \u00a75.6 mint field preimage",
      String(e),
    );
  }
  const assets: MidgardLedgerMintAsset[] = [];
  for (const policy of items) {
    for (const asset of policy.assets) {
      assets.push({
        policyId: copyBuffer(policy.policyId),
        assetName: copyBuffer(asset.assetName),
        quantity: asset.quantity,
      });
    }
  }
  return { assets };
};

const ensureAssetName = (
  value: Uint8Array,
  fieldName: string,
): MidgardAssetName => {
  const assetName = copyBuffer(value);
  if (assetName.length > MAX_ASSET_NAME_LENGTH) {
    failEncode(
      `${fieldName} must be at most ${MAX_ASSET_NAME_LENGTH} bytes`,
      `length=${assetName.length}`,
    );
  }
  return assetName;
};

/**
 * §5.6: `82 \u2016 58 1C policy_id \u2016 map(k) \u2016 asset entries` per policy item, inside
 * the §5.1 envelope. The retired raw-map form is prohibited.
 *
 * The flat asset list is grouped by policy here — that grouping and its
 * duplicate check are this module's own invariant about `MidgardLedgerMint`'s
 * shape, and they have no counterpart in the byte grammar. Ordering is then
 * *enforced* by `encodeMidgardFieldPreimageForFieldV1` at both levels rather
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

  return encodeMidgardFieldPreimageForFieldV1({
    fieldIndex: 5,
    items: sortMidgardMintItemsV1(
      [...policies.values()].map((policy) => ({
        policyId: policy.policyId,
        assets: [...policy.assets.values()],
      })),
    ),
  });
};

const decodeRedeemers = (preimageCbor: Uint8Array): MidgardLedgerRedeemer[] =>
  decodeMidgardRedeemers(preimageCbor).map((redeemer) => ({
    tag: redeemer.tag,
    index: redeemer.index,
    data: redeemerDataFromCborHex(redeemer.dataCborHex),
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
  return encodeMidgardFieldPreimageForFieldV1({
    fieldIndex: 8,
    items: ordered.map((redeemer) => {
      const key = `${redeemer.tag}:${redeemer.index.toString(10)}`;
      if (seen.has(key)) {
        failEncode("duplicate redeemer pointer", key);
      }
      seen.add(key);
      return {
        purpose: midgardRedeemerPurposeFromTagV1(redeemer.tag),
        index: redeemer.index,
        redeemerCbor: Buffer.from(
          plutusDataToCborHex(redeemer.data, { canonical: true }),
          "hex",
        ),
        executionUnits: {
          memory: redeemer.exUnits.memory,
          steps: redeemer.exUnits.steps,
        },
      };
    }),
  });
};

const expectedRequiresPlutusEvaluation = (
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

const toNativeTx = (tx: MidgardLedgerTx): MidgardNativeTxFullV1 => {
  assertRequiresPlutusEvaluation(tx);
  const body: MidgardNativeTxBodyCanonicalV1 = {
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
  const witnessSet: MidgardNativeTxWitnessSetCanonicalV1 = {
    addrTxWitsPreimageCbor: encodeVKeyWitnesses(tx),
    scriptTxWitsPreimageCbor: encodeScriptWitnesses(tx),
    redeemerTxWitsPreimageCbor: encodeRedeemers(tx.redeemers),
  };
  const nativeTx: MidgardNativeTxFullV1 = {
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: tx.validity,
    body,
    witnessSet,
    compact: deriveMidgardNativeTxCompactV1(body, witnessSet, tx.validity),
  };
  const computedTxId = computeMidgardNativeTxIdV1(nativeTx);
  assertBufferEquals("txId", tx.txId, computedTxId);
  return nativeTx;
};

const decodeMidgardLedgerTxFromNativeTx = (
  nativeTx: MidgardNativeTxFullV1,
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
    txId: computeMidgardNativeTxIdV1(nativeTx) as MidgardTxId,
    validity: nativeTx.validity,
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
  nativeTx: MidgardNativeTxFullV1,
  txCbor: Uint8Array,
): MidgardSubmittedTx => {
  const witnessSetCompact = copyNativeWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet),
  );
  return {
    txCbor: Buffer.from(txCbor),
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
): MidgardSubmittedTx => {
  let nativeTx: MidgardNativeTxFullV1;
  try {
    nativeTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
  } catch (e) {
    throw new MidgardLedgerTxDecodeError("canonical-cbor", e);
  }

  try {
    return envelopeFromNativeTx(nativeTx, txCbor);
  } catch (e) {
    throw new MidgardLedgerTxDecodeError(
      "ledger",
      e instanceof MidgardLedgerOutputDecodeError ? e.causeValue : e,
      e instanceof MidgardLedgerOutputDecodeError,
    );
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
): MidgardLedgerTx =>
  decodeMidgardSubmittedTxFromCanonicalCbor(txCbor).ledgerTx;

export const encodeMidgardLedgerTxToCanonicalCbor = (
  tx: MidgardLedgerTx,
): Buffer => encodeMidgardNativeTxCanonicalV1(toNativeTx(tx));
