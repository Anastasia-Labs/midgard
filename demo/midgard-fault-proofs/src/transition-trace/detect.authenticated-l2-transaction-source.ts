import { midgardFieldCommitmentFromItems } from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core/codec";
import {
  encodeCborBytes,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  exactHexBytes,
  type Tag4Output,
} from "./detect.mpf-proof-from-witness.js";
import { decodeTag4Output, encodeTag4Value } from "./detect.tag4-value-at.js";
import {
  TransitionTraceChallengerError,
  transitionTraceError,
} from "./errors.js";

const encodeTag4Output = (output: Tag4Output): Buffer => {
  const entryCount =
    2 +
    (output.datumCbor === undefined ? 0 : 1) +
    (output.scriptRef === undefined ? 0 : 1);
  return Buffer.concat([
    Buffer.from([0xa0 + entryCount, 0]),
    encodeCborBytes(output.address),
    Buffer.from([1]),
    encodeTag4Value(output),
    ...(output.datumCbor === undefined
      ? []
      : [Buffer.from([2]), encodeCborBytes(output.datumCbor)]),
    ...(output.scriptRef === undefined
      ? []
      : [
          Buffer.from([3, 0x82]),
          encodeCborUnsigned(output.scriptRef.language),
          encodeCborBytes(output.scriptRef.bytes),
        ]),
  ]);
};

export const canonicalNativeOutput = (bytes: Buffer, label: string): Buffer => {
  try {
    return encodeTag4Output(decodeTag4Output(bytes));
  } catch (cause) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not an Aiken V1 MidgardTxOutput.`,
      cause,
    );
  }
};

export const nativePreimageItems = (
  preimageHex: string,
  label: string,
): readonly Buffer[] => {
  try {
    return decodeMidgardNativeByteListPreimage(
      exactHexBytes(preimageHex, label),
      label,
    );
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a canonical native byte-list preimage.`,
      cause,
    );
  }
};

export const authenticatedL2TransactionSource = (
  sourceMembership: SDK.L2TransactionSourceMembershipProof,
): {
  readonly compact: ReturnType<typeof decodeMidgardNativeTxCompact>;
  readonly txId: Buffer;
} => {
  try {
    const txId = exactHexBytes(
      sourceMembership.key,
      "L2 transaction source key",
    );
    if (txId.length !== 32) {
      throw new Error("source key must contain exactly 32 bytes");
    }
    const source = Data.from(
      exactHexBytes(
        sourceMembership.value,
        "L2 transaction source value",
      ).toString("hex"),
      SDK.L2TransactionSource,
    );
    const sourceTxId = exactHexBytes(
      source.tx_id,
      "L2 transaction source value.tx_id",
    );
    if (!sourceTxId.equals(txId)) {
      throw new Error("source value tx_id does not match its membership key");
    }
    const compact = decodeMidgardNativeTxCompact(
      exactHexBytes(
        source.source.compact_cbor,
        "L2 transaction source compact",
      ),
    );
    if (compact.validity !== "TxIsValid") {
      throw new Error("source compact is not classified as TxIsValid");
    }
    return { compact, txId };
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      "L2 transaction source membership does not contain an authenticated canonical native transaction compact.",
      cause,
    );
  }
};

export const requireCanonicalFieldCommitment = ({
  items,
  expectedCommitment,
  label,
}: {
  readonly items: readonly Buffer[];
  readonly expectedCommitment: Buffer;
  readonly label: string;
}): void => {
  // §4: the field commits to a flat `blake2b_256` of its §5.1 preimage, so the
  // check is over the assembled bytes rather than a counted per-item root — and
  // the field index is no longer an input, because §4 puts no field index in the
  // hash. Which field this *is* comes from `expectedCommitment`: the caller reads
  // it out of the authenticated compact structure, which is what §4's
  // positional-identity invariant requires and the only thing separating fields
  // 0 and 2 for identical content.
  const actualCommitment = midgardFieldCommitmentFromItems(items);
  if (!actualCommitment.equals(expectedCommitment)) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} canonical field commitment does not match the authenticated transaction compact.`,
    );
  }
};
