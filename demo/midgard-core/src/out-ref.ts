import { normalizeHex } from "./hex.js";

export type OutRefLike = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export const TX_HASH_BYTE_LENGTH = 32;

export const normalizeTxHash = (txHash: string, fieldName = "txHash"): string =>
  normalizeHex(txHash, {
    fieldName,
    byteLength: TX_HASH_BYTE_LENGTH,
  });

export const normalizeOutRef = (outRef: OutRefLike): OutRefLike => {
  if (!Number.isSafeInteger(outRef.outputIndex) || outRef.outputIndex < 0) {
    throw new Error("outputIndex must be a non-negative safe integer");
  }
  return {
    txHash: normalizeTxHash(outRef.txHash),
    outputIndex: outRef.outputIndex,
  };
};

export const outRefLabel = (outRef: OutRefLike): string => {
  const normalized = normalizeOutRef(outRef);
  return `${normalized.txHash}#${normalized.outputIndex.toString()}`;
};

export const parseOutRefLabel = (label: string): OutRefLike => {
  const [txHash, outputIndexRaw, extra] = label.trim().split("#");
  if (
    txHash === undefined ||
    outputIndexRaw === undefined ||
    extra !== undefined ||
    !/^(0|[1-9][0-9]*)$/.test(outputIndexRaw)
  ) {
    throw new Error("out-ref label must use <txHash>#<outputIndex>");
  }
  return normalizeOutRef({
    txHash,
    outputIndex: Number(outputIndexRaw),
  });
};

export const compareOutRefs = (left: OutRefLike, right: OutRefLike): number => {
  const normalizedLeft = normalizeOutRef(left);
  const normalizedRight = normalizeOutRef(right);
  if (normalizedLeft.txHash !== normalizedRight.txHash) {
    return normalizedLeft.txHash < normalizedRight.txHash ? -1 : 1;
  }
  return normalizedLeft.outputIndex - normalizedRight.outputIndex;
};

export const outRefsEqual = (left: OutRefLike, right: OutRefLike): boolean => {
  return outRefLabel(left) === outRefLabel(right);
};

export const findOutRefIndex = (
  orderedOutRefs: readonly OutRefLike[],
  target: OutRefLike,
): number | undefined => {
  const index = orderedOutRefs.findIndex((candidate) =>
    outRefsEqual(candidate, target),
  );
  return index < 0 ? undefined : index;
};
