import {
  EMPTY_CBOR_LIST,
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldPreimage,
  encodeMidgardMintPolicyItem,
  encodeMidgardNativeScript,
  encodeMidgardSpendInputItem,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  midgardFieldCarriageBounds,
  type MidgardMintPolicyItem,
  type MidgardNativeScript,
  type MidgardNativeTxFull,
  sortMidgardMintItems,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

// ---------------------------------------------------------------------------
// §5.6 mint items and §5.5 native scripts
// ---------------------------------------------------------------------------

/** One §5.6 single-asset mint policy item's canonical bytes, hex. */
export const mintItemCborV1 = ({
  policyId,
  assetName,
  quantity = 1n,
}: {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity?: bigint;
}): string =>
  Buffer.from(
    encodeMidgardMintPolicyItem({
      policyId,
      assets: [{ assetName, quantity }],
    }),
  ).toString("hex");

/** A readable 28-byte policy id: `byte` repeated. */
export const policyIdByte = (byte: number): Buffer => Buffer.alloc(28, byte);

/** A distinct ascending 28-byte policy id: `index` in the trailing four bytes. */
const ascendingPolicyId = (index: number): Buffer => {
  const buffer = Buffer.alloc(28);
  buffer.writeUInt32BE(index >>> 0, 24);
  return buffer;
};

/**
 * The direction-B subject: a native `sig` timelock whose key hash signs no
 * committed witness, so its machine-twin evaluation over the (empty) field-7
 * signer set is unsatisfied. Returns the policy id it hashes to, the mint
 * item that spends under it, and the canonical payload bytes step-03 pins.
 */
export const directionBNativeScript = (
  keyByte = 0xcd,
): {
  readonly script: MidgardNativeScript;
  readonly scriptBytesHex: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
} => {
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, keyByte),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const policyIdHex = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript: script,
  });
  return {
    script,
    scriptBytesHex: scriptBytes.toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
  };
};

/**
 * A direction-B native `sig` whose key hash IS a committed signer — the honest
 * false-fault: its machine-twin evaluates SATISFIED, so no fault exists. Used
 * to drive step-03's EvaluateUnsatisfied local refusal.
 */
export const directionBSatisfiedNativeScript = (): {
  readonly scriptBytesHex: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
  readonly addrWitnessItemCbor: string;
} => {
  const verificationKey = Buffer.alloc(32, 0x07);
  const keyHashHex = Effect.runSync(
    SDK.hashHexWithBlake2b(verificationKey.toString("hex"), 28),
  );
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.from(keyHashHex, "hex"),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const policyIdHex = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript: script,
  });
  return {
    scriptBytesHex: scriptBytes.toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
    addrWitnessItemCbor: Buffer.from(
      encodeMidgardAddressWitnessItem({
        verificationKey,
        signature: Buffer.alloc(64, 0x09),
      }),
    ).toString("hex"),
  };
};

/**
 * A field-6 versioned-script item whose hash IS a claimed policy id — the
 * honest false-fault for direction A: a script the operator DID consult, so
 * the absence claim is false. Returns the item bytes and the policy id.
 */
export const directionAPresentScript = (): {
  readonly scriptWitnessItemCbor: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
} => {
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, 0x5e),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const versionedScript = {
    language: "NativeCardano" as const,
    scriptBytes,
    nativeScript: script,
  };
  const policyIdHex = hashMidgardVersionedScript(versionedScript);
  return {
    scriptWitnessItemCbor:
      encodeMidgardVersionedScript(versionedScript).toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
  };
};

/** A small single-policy tier-1 mint field: one absent policy, one asset. */
export const smallMintItemCbors = (): readonly string[] => [
  mintItemCborV1({
    policyId: policyIdByte(0xab),
    assetName: Buffer.from("beef", "hex"),
  }),
];

/**
 * A mint field whose §5.1 preimage lands in the tier-2 RawUtxo window
 * `(maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes]`, forced by
 * item count alone: distinct ascending single-asset policies, 32-byte asset
 * names, computed at build time from the encoded bytes rather than hard-coded.
 * Every policy is absent, so the whole field is direction-A convictable.
 */
export const largeMintItemCbors = (): {
  readonly itemCbors: readonly string[];
  readonly preimageByteLength: number;
} => {
  const { maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes } =
    midgardFieldCarriageBounds;
  const assetName = Buffer.alloc(32, 0x11);
  const items: MidgardMintPolicyItem[] = [];
  let count = 0;
  let preimageByteLength = 0;
  // Grow the field one policy at a time until its own preimage crosses the
  // tier-1 ceiling; the loop stops the instant tier-2 is selected by size.
  for (;;) {
    items.push({
      policyId: ascendingPolicyId(count),
      assets: [{ assetName, quantity: 1n }],
    });
    count += 1;
    const sorted = sortMidgardMintItems(items);
    const itemBuffers = sorted.map((item) =>
      Buffer.from(encodeMidgardMintPolicyItem(item)),
    );
    preimageByteLength = encodeMidgardFieldPreimage(itemBuffers).length;
    if (preimageByteLength > maxTier1RedeemerPreimageBytes) {
      if (preimageByteLength > maxPublishableCarriageBytes) {
        throw new Error(
          `large mint field overshot the single-publication window: ${preimageByteLength.toString()} bytes`,
        );
      }
      return {
        itemCbors: itemBuffers.map((item) => item.toString("hex")),
        preimageByteLength,
      };
    }
  }
};

/** N §5.3 address-witness items (fixed 101 bytes each), distinct decoy vkeys. */
export const addressWitnessItemCbors = (count: number): readonly string[] =>
  Array.from({ length: count }, (_unused, index) => {
    const verificationKey = Buffer.alloc(32);
    verificationKey.writeUInt32BE(index + 1, 28);
    return Buffer.from(
      encodeMidgardAddressWitnessItem({
        verificationKey,
        signature: Buffer.alloc(64, (index % 254) + 1),
      }),
    ).toString("hex");
  });

/**
 * A field-7 address-witness set whose §5.1 preimage crosses the tier-2 window
 * (103-byte per-item stride), forced by count alone. None of the decoy signers
 * matches a direction-B `sig` key hash, so the policy stays unsatisfied.
 */
export const largeAddressWitnessItemCbors = (): {
  readonly itemCbors: readonly string[];
  readonly preimageByteLength: number;
} => {
  const { maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes } =
    midgardFieldCarriageBounds;
  let count = 0;
  for (;;) {
    count += 1;
    const itemCbors = addressWitnessItemCbors(count);
    const preimageByteLength = encodeMidgardFieldPreimage(
      itemCbors.map((hex) => Buffer.from(hex, "hex")),
    ).length;
    if (preimageByteLength > maxTier1RedeemerPreimageBytes) {
      if (preimageByteLength > maxPublishableCarriageBytes) {
        throw new Error(
          `large address-witness field overshot the single-publication window: ${preimageByteLength.toString()} bytes`,
        );
      }
      return { itemCbors, preimageByteLength };
    }
  }
};

/** One §5.3 reference-input out-ref item's canonical bytes, hex. */
export const referenceInputItemCbor = ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): string =>
  Buffer.from(
    encodeMidgardSpendInputItem({
      txId: Buffer.from(txIdHex, "hex"),
      outputIndex,
    }),
  ).toString("hex");

// ---------------------------------------------------------------------------
// The committed transaction and its block fixture
// ---------------------------------------------------------------------------

export const fieldPreimageOf = (itemCbors: readonly string[]): Buffer =>
  itemCbors.length === 0
    ? Buffer.from(EMPTY_CBOR_LIST)
    : encodeMidgardFieldPreimage(
        itemCbors.map((hex) => Buffer.from(hex, "hex")),
      );

export type MintAuthorizationSubject = {
  readonly nativeTx: MidgardNativeTxFull;
  readonly witnessSetCompact: SDK.NativeTxWitnessSetCompact;
  readonly mintItemCbors: readonly string[];
  readonly scriptWitnessItemCbors: readonly string[];
  readonly addrWitnessItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
};
