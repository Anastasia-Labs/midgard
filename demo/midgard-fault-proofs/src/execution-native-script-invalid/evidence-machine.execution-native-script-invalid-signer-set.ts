import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardValidationMerkleMembershipIndex,
  computeHash32,
  decodeMidgardAddressWitnessItem,
  emptyMidgardValidationMerkleFrontier,
  hashMidgardSignerLeaf,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  type FrontierPeak,
  missingSignatureFieldWalkCheckpoint,
  missingSignatureVkeyHash,
  type NativeScriptPushdownFrame,
  type SignerSetProof,
} from "@al-ft/midgard-sdk";

export const EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT = 28;

export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH = 16;

export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH = 16;

export const EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_FINALIZE_BATCH = 16;

export const EXECUTION_NATIVE_SCRIPT_INVALID_NODE_BATCH = 16;

export const executionNativeScriptInvalidUsesDirectRoute = ({
  signerCount,
  scriptBytes,
}: {
  readonly signerCount: number;
  readonly scriptBytes: number;
}): boolean =>
  signerCount <= EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT &&
  scriptBytes <= 1_024;

export const assertExecutionNativeScriptInvalidDirectRoute = (
  signerCount: number,
) => {
  if (signerCount > EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT) {
    throw new Error(
      `execution-native-script-invalid: direct signer limit is ${EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_SIGNER_LIMIT.toString()}; use the staged route`,
    );
  }
};

export const hash32 = (bytes: Uint8Array): Buffer => computeHash32(bytes);

export const u24 = (value: number, label: string): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xff_ffff) {
    throw new Error(`${label} must fit an unsigned 24-bit word`);
  }
  const result = Buffer.alloc(3);
  result.writeUIntBE(value, 0, 3);
  return result;
};

const exactSignerHashes = (
  addressWitnessItems: readonly Uint8Array[],
): readonly Buffer[] => {
  const hashes: Buffer[] = [];
  for (const item of addressWitnessItems) {
    const witness = decodeMidgardAddressWitnessItem(item);
    const hash = Buffer.from(
      missingSignatureVkeyHash(
        Buffer.from(witness.verificationKey).toString("hex"),
      ),
      "hex",
    );
    const previous = hashes.at(-1);
    if (previous !== undefined && Buffer.compare(previous, hash) > 0) {
      throw new Error(
        "execution-native-script-invalid: address-witness signer hashes are not canonical",
      );
    }
    if (previous === undefined || !previous.equals(hash)) hashes.push(hash);
  }
  return hashes;
};

const frontierWire = (
  frontier: MidgardValidationMerkleFrontier,
): FrontierPeak[] =>
  frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: Buffer.from(peak.hash).toString("hex"),
  }));

export type ExecutionNativeScriptInvalidSignerScanState = Readonly<{
  checkpointBytes: string;
  checkpointHash: string;
  previousSignerHash: string;
  signerCount: bigint;
  signerPeaks: readonly FrontierPeak[];
  nextItemIndex: number;
  complete: boolean;
}>;

export const resolveExecutionNativeScriptInvalidSignerCheckpoint = ({
  txId,
  itemCount,
  totalLength,
  committedHash,
}: {
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly committedHash: string;
}) => {
  missingSignatureFieldWalkCheckpoint({
    txId,
    itemCount,
    totalLength,
    nextItemIndex: 0,
  });
  if (committedHash === "") return null;
  if (!/^[0-9a-f]{64}$/u.test(committedHash)) {
    throw new Error(
      "execution-native-script-invalid checkpoint commitment must be 32-byte lowercase hex",
    );
  }
  for (
    let cursor = EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_START_BATCH;
    cursor < itemCount;
    cursor += EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH
  ) {
    const candidate = missingSignatureFieldWalkCheckpoint({
      txId,
      itemCount,
      totalLength,
      nextItemIndex: cursor,
    });
    if (candidate.checkpointHash === committedHash) return candidate;
  }
  throw new Error(
    "execution-native-script-invalid checkpoint commitment is not reachable by the deterministic signer scan schedule",
  );
};

export const executionNativeScriptInvalidSignerScanState = ({
  txId,
  addressWitnessItems,
  totalLength,
  committedCheckpointHash = "",
  batchSize = EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH,
}: {
  readonly txId: string;
  readonly addressWitnessItems: readonly Uint8Array[];
  readonly totalLength: number;
  readonly committedCheckpointHash?: string;
  readonly batchSize?: number;
}): ExecutionNativeScriptInvalidSignerScanState => {
  if (
    !Number.isSafeInteger(batchSize) ||
    batchSize <= 0 ||
    batchSize > EXECUTION_NATIVE_SCRIPT_INVALID_SIGNER_RESUME_BATCH
  ) {
    throw new Error(
      "execution-native-script-invalid: signer batch size must be 1..16",
    );
  }
  const current = resolveExecutionNativeScriptInvalidSignerCheckpoint({
    txId,
    itemCount: addressWitnessItems.length,
    totalLength,
    committedHash: committedCheckpointHash,
  });
  const currentIndex = current?.nextItemIndex ?? 0;
  const nextItemIndex = Math.min(
    addressWitnessItems.length,
    currentIndex + batchSize,
  );
  const signerHashes = exactSignerHashes(
    addressWitnessItems.slice(0, nextItemIndex),
  );
  const frontier = signerHashes.reduce(
    (currentFrontier, signerHash) =>
      appendMidgardValidationMerkleLeaf(
        currentFrontier,
        hashMidgardSignerLeaf(signerHash),
      ),
    emptyMidgardValidationMerkleFrontier(),
  );
  const checkpoint = missingSignatureFieldWalkCheckpoint({
    txId,
    itemCount: addressWitnessItems.length,
    totalLength,
    nextItemIndex,
  });
  return {
    checkpointBytes: checkpoint.checkpointCbor,
    checkpointHash: checkpoint.checkpointHash,
    previousSignerHash: signerHashes.at(-1)?.toString("hex") ?? "",
    signerCount: BigInt(signerHashes.length),
    signerPeaks: frontierWire(frontier),
    nextItemIndex,
    complete: nextItemIndex === addressWitnessItems.length,
  };
};

export type ExecutionNativeScriptInvalidSignerSet = Readonly<{
  hashes: readonly Buffer[];
  frontier: MidgardValidationMerkleFrontier;
  proofFor: (signerHash: Uint8Array) => SignerSetProof;
}>;

export const executionNativeScriptInvalidSignerSet = (
  addressWitnessItems: readonly Uint8Array[],
): ExecutionNativeScriptInvalidSignerSet => {
  const hashes = exactSignerHashes(addressWitnessItems);
  const leafHashes = hashes.map(hashMidgardSignerLeaf);
  const membership = buildMidgardValidationMerkleMembershipIndex(leafHashes);
  const peaks = frontierWire(membership.frontier);
  const proofFor = (raw: Uint8Array): SignerSetProof => {
    const signerHash = Buffer.from(raw);
    if (signerHash.length !== 28) {
      throw new Error(
        "execution-native-script-invalid: signer query must be 28 bytes",
      );
    }
    const insertionIndex = hashes.findIndex(
      (candidate) => Buffer.compare(candidate, signerHash) >= 0,
    );
    if (insertionIndex >= 0 && hashes[insertionIndex]!.equals(signerHash)) {
      const exact = membership.membershipAt(insertionIndex);
      return {
        SignerMembershipProof: {
          peaks,
          signer_index: BigInt(insertionIndex),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    if (hashes.length === 0) return { EmptySignerSetProof: { peaks } };
    if (insertionIndex === 0) {
      const exact = membership.membershipAt(0);
      return {
        SignerBelowFirstProof: {
          peaks,
          first_signer_hash: hashes[0]!.toString("hex"),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    if (insertionIndex === -1) {
      const lastIndex = hashes.length - 1;
      const exact = membership.membershipAt(lastIndex);
      return {
        SignerAboveLastProof: {
          peaks,
          last_signer_hash: hashes[lastIndex]!.toString("hex"),
          siblings: exact.siblings.map((value) =>
            Buffer.from(value).toString("hex"),
          ),
        },
      };
    }
    const lowerIndex = insertionIndex - 1;
    const lower = membership.membershipAt(lowerIndex);
    const upper = membership.membershipAt(insertionIndex);
    return {
      SignerBetweenProof: {
        peaks,
        lower_index: BigInt(lowerIndex),
        lower_signer_hash: hashes[lowerIndex]!.toString("hex"),
        lower_siblings: lower.siblings.map((value) =>
          Buffer.from(value).toString("hex"),
        ),
        upper_signer_hash: hashes[insertionIndex]!.toString("hex"),
        upper_siblings: upper.siblings.map((value) =>
          Buffer.from(value).toString("hex"),
        ),
      },
    };
  };
  return { hashes, frontier: membership.frontier, proofFor };
};

export const SCRIPT_CURSOR_DOMAIN = Buffer.from(
  "MidgardNativeScriptWalkV1",
  "ascii",
);

export const SCRIPT_FRAME_DOMAIN = Buffer.from(
  "MidgardNativeScriptFrameV1",
  "ascii",
);

export const MAX_NODES = 32;

export const MAX_FRAMES = 15;

export const UNSATISFIABLE_REQUIRED = MAX_NODES + 1;

export type PushdownState = {
  readonly scriptDigest: Buffer;
  readonly scriptLength: number;
  readonly offset: number;
  readonly frames: readonly NativeScriptPushdownFrame[];
  readonly nodesVisited: number;
  readonly pending: 0 | 1 | 2;
};

const encodeFrame = (frame: NativeScriptPushdownFrame): Buffer =>
  Buffer.concat([
    Buffer.from([Number(frame.kind)]),
    u24(Number(frame.remaining), "native script frame remaining"),
    u24(Number(frame.satisfied), "native script frame satisfied"),
    u24(Number(frame.required), "native script frame required"),
  ]);

const chainFrame = (below: Buffer, frame: NativeScriptPushdownFrame): Buffer =>
  hash32(Buffer.concat([SCRIPT_FRAME_DOMAIN, below, encodeFrame(frame)]));

export const frameRoots = (
  frames: readonly NativeScriptPushdownFrame[],
): readonly Buffer[] => {
  const roots: Buffer[] = new Array(frames.length);
  let below = hash32(SCRIPT_FRAME_DOMAIN);
  for (let index = frames.length - 1; index >= 0; index -= 1) {
    below = chainFrame(below, frames[index]!);
    roots[index] = below;
  }
  return roots;
};
