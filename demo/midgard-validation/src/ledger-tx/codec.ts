import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  asBytes,
  asMap,
  decodeSingleCbor,
  encodeCbor,
  encodeCborBytes,
  encodeCborInteger,
  encodeCborMapRaw,
} from "@al-ft/midgard-core/codec/cbor";
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

const compareBytes = (left: Uint8Array, right: Uint8Array): number => {
  const limit = Math.min(left.length, right.length);
  for (let i = 0; i < limit; i += 1) {
    const diff = left[i] - right[i];
    if (diff !== 0) {
      return diff;
    }
  }
  return left.length - right.length;
};

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
  compact: MidgardNativeTxFull["compact"],
): MidgardNativeTxFull["compact"] => ({
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
  witnessSet: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact>,
): ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact> => ({
  addrTxWitsHash: copyBuffer(witnessSet.addrTxWitsHash),
  scriptTxWitsHash: copyBuffer(witnessSet.scriptTxWitsHash),
  redeemerTxWitsHash: copyBuffer(witnessSet.redeemerTxWitsHash),
});

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => copyBuffer(item)));

const decodeOutRef = (inputBytes: Uint8Array): MidgardOutRef => {
  const input = CML.TransactionInput.from_cbor_bytes(inputBytes);
  return {
    txId: Buffer.from(input.transaction_id().to_raw_bytes()),
    index: BigInt(input.index()),
  };
};

const encodeOutRef = (outRef: MidgardOutRef, fieldName: string): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(
        assertHash32(outRef.txId, `${fieldName}.txId`),
      ),
      outRef.index,
    ).to_cbor_bytes(),
  );

const decodeOutRefList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): MidgardOutRef[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    decodeOutRef,
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

const encodeHashList = (
  hashes: readonly Uint8Array[],
  fieldName: string,
): Buffer =>
  encodeByteList(
    hashes.map((hash, index) => assertHash28(hash, `${fieldName}[${index}]`)),
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
    const witness = CML.Vkeywitness.from_cbor_bytes(witnessBytes[index]);
    const vkey = witness.vkey();
    const keyHash = Buffer.from(vkey.hash().to_raw_bytes());
    const keyHashHex = keyHash.toString("hex");
    if (!seenKeyHashes.has(keyHashHex)) {
      seenKeyHashes.add(keyHashHex);
      witnessKeyHashes.push(keyHash);
    }
    vkeyWitnesses.push({
      index,
      keyHash,
      vkey: Buffer.from(vkey.to_raw_bytes()),
      signature: Buffer.from(witness.ed25519_signature().to_raw_bytes()),
    });
  }

  return { vkeyWitnesses, witnessKeyHashes };
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
    return Buffer.from(
      CML.Vkeywitness.new(
        publicKey,
        CML.Ed25519Signature.from_raw_bytes(witness.signature),
      ).to_cbor_bytes(),
    );
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

const signedBigInt = (value: unknown, fieldName: string): bigint => {
  if (typeof value === "bigint") {
    return value;
  }
  if (typeof value === "number" && Number.isInteger(value)) {
    return BigInt(value);
  }
  return failDecode(`${fieldName} must be an integer`);
};

const decodeMint = (preimageCbor: Uint8Array): MidgardLedgerMint => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    if (decoded.length === 0) {
      return { assets: [] };
    }
    failDecode("native.mint must be an empty array or a CBOR map");
  }

  const policies = asMap(decoded, "native.mint");
  if (policies.size === 0) {
    failDecode("Midgard mint map cannot be empty");
  }
  const assets: MidgardLedgerMintAsset[] = [];
  let policyIndex = 0;
  for (const [policyValue, assetsValue] of policies.entries()) {
    const policyId = asBytes(policyValue, `native.mint[${policyIndex}].policy`);
    if (policyId.length !== HASH28_LENGTH) {
      failDecode(
        "mint policy id must be 28 bytes",
        `native.mint[${policyIndex}].policy length=${policyId.length}`,
      );
    }
    const assetMap = asMap(assetsValue, `native.mint[${policyIndex}].assets`);
    if (assetMap.size === 0) {
      failDecode("Mint policy asset map cannot be empty");
    }
    let assetIndex = 0;
    for (const [assetNameValue, quantityValue] of assetMap.entries()) {
      const assetName = asBytes(
        assetNameValue,
        `native.mint[${policyIndex}].assets[${assetIndex}].name`,
      );
      const quantity = signedBigInt(
        quantityValue,
        `native.mint[${policyIndex}].assets[${assetIndex}].quantity`,
      );
      if (quantity === 0n) {
        failDecode("mint quantity cannot be zero");
      }
      assets.push({
        policyId: copyBuffer(policyId),
        assetName: copyBuffer(assetName),
        quantity,
      });
      assetIndex += 1;
    }
    policyIndex += 1;
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

const encodeMint = (mint: MidgardLedgerMint): Buffer => {
  if (mint.assets.length === 0) {
    return Buffer.from(EMPTY_CBOR_LIST);
  }

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

  const policyEntries = [...policies.values()]
    .map((policy) => {
      const policyKey = encodeCborBytes(policy.policyId);
      const assetEntries = [...policy.assets.values()]
        .map(
          (asset) =>
            [
              encodeCborBytes(asset.assetName),
              encodeCborInteger(asset.quantity),
            ] as const,
        )
        .sort(([left], [right]) => compareBytes(left, right));
      return [policyKey, encodeCborMapRaw(assetEntries)] as const;
    })
    .sort(([left], [right]) => compareBytes(left, right));

  return encodeCborMapRaw(policyEntries);
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

const encodeRedeemers = (
  redeemers: readonly MidgardLedgerRedeemer[],
): Buffer => {
  if (redeemers.length === 0) {
    return Buffer.from(EMPTY_CBOR_LIST);
  }
  const seen = new Set<string>();
  const ordered = [...redeemers].sort((left, right) => {
    if (left.tag !== right.tag) {
      return left.tag - right.tag;
    }
    return left.index < right.index ? -1 : left.index > right.index ? 1 : 0;
  });
  return encodeCbor(
    ordered.map((redeemer) => {
      const key = `${redeemer.tag}:${redeemer.index.toString(10)}`;
      if (seen.has(key)) {
        failEncode("duplicate redeemer pointer", key);
      }
      seen.add(key);
      return [
        BigInt(redeemer.tag),
        redeemer.index,
        Buffer.from(
          plutusDataToCborHex(redeemer.data, { canonical: true }),
          "hex",
        ),
        [redeemer.exUnits.memory, redeemer.exUnits.steps],
      ];
    }),
  );
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

const toNativeTx = (tx: MidgardLedgerTx): MidgardNativeTxFull => {
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
  nativeTx: MidgardNativeTxFull,
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
    txId: computeMidgardNativeTxId(nativeTx) as MidgardTxId,
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
  nativeTx: MidgardNativeTxFull,
  txCbor: Uint8Array,
): MidgardSubmittedTx => {
  const witnessSetCompact = copyNativeWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
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
  let nativeTx: MidgardNativeTxFull;
  try {
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(txCbor);
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
): Buffer => encodeMidgardNativeTxCanonical(toNativeTx(tx));
