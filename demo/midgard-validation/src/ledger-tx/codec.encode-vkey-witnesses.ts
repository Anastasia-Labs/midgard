import {
  decodeMidgardMintFieldPreimage,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardAddressWitnessItem,
  encodeMidgardHash28Item,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import {
  assertBufferArrayEquals,
  assertBufferEquals,
  assertHash28,
  copyBuffer,
  encodeByteList,
  failDecode,
  failEncode,
  HASH28_LENGTH,
  MAX_ASSET_NAME_LENGTH,
} from "./codec.copy-native-tx-compact.js";
import type {
  MidgardAssetName,
  MidgardCredentialHash,
  MidgardLedgerMint,
  MidgardLedgerMintAsset,
  MidgardLedgerScriptWitness,
  MidgardLedgerTx,
  MidgardLedgerVKeyWitness,
  MidgardScriptHash,
} from "./types.js";

/**
 * §5.3 fields 3/4: the item *is* the raw 28-byte hash. `encodeMidgardHash28Item`
 * is the §5.3 encoder; `assertHash28` stays in front of it only to name the field
 * and index in the diagnostic, which the grammar-level encoder cannot.
 */
export const encodeHashList = (
  hashes: readonly Uint8Array[],
  fieldName: string,
): Buffer =>
  encodeByteList(
    hashes.map((hash, index) =>
      encodeMidgardHash28Item(assertHash28(hash, `${fieldName}[${index}]`)),
    ),
  );

export const decodeObserverHashes = (
  preimageCbor: Uint8Array,
): MidgardScriptHash[] =>
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

export const decodeVKeyWitnesses = (
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

export const encodeVKeyWitnesses = (
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
    // exactly why the dependency was invisible; `encodeMidgardAddressWitnessItem`
    // is the encoder that owes the on-chain reader its 101-byte width.
    return encodeMidgardAddressWitnessItem({
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

export const decodeScriptWitnesses = (
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

export const encodeScriptWitnesses = (
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
export const decodeMint = (preimageCbor: Uint8Array): MidgardLedgerMint => {
  let items;
  try {
    items = decodeMidgardMintFieldPreimage(preimageCbor);
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

export const ensureAssetName = (
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
