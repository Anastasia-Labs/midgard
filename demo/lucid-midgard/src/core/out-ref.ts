import { BuilderInvariantError } from "./errors.js";

export type OutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

const TX_HASH_HEX_LENGTH = 64;

export const normalizeTxHash = (txHash: string): string => {
  const normalized = txHash.trim().toLowerCase();
  if (
    normalized.length !== TX_HASH_HEX_LENGTH ||
    !/^[0-9a-f]+$/.test(normalized)
  ) {
    throw new BuilderInvariantError(
      "Invalid transaction hash",
      `txHash=${txHash}`,
    );
  }
  return normalized;
};

export const normalizeOutRef = (outRef: OutRef): OutRef => {
  if (
    !Number.isSafeInteger(outRef.outputIndex) ||
    outRef.outputIndex < 0
  ) {
    throw new BuilderInvariantError(
      "Invalid output index",
      `outputIndex=${outRef.outputIndex.toString()}`,
    );
  }
  return {
    txHash: normalizeTxHash(outRef.txHash),
    outputIndex: outRef.outputIndex,
  };
};

export const outRefLabel = (outRef: OutRef): string => {
  const normalized = normalizeOutRef(outRef);
  return `${normalized.txHash}#${normalized.outputIndex.toString()}`;
};

export const parseOutRefLabel = (label: string): OutRef => {
  const [txHash, outputIndex, extra] = label.trim().split("#");
  if (
    txHash === undefined ||
    outputIndex === undefined ||
    extra !== undefined ||
    !/^(0|[1-9][0-9]*)$/.test(outputIndex)
  ) {
    throw new BuilderInvariantError("Invalid outref label", label);
  }
  const parsedIndex = Number(outputIndex);
  return normalizeOutRef({ txHash, outputIndex: parsedIndex });
};

export const compareOutRefs = (left: OutRef, right: OutRef): number => {
  const normalizedLeft = normalizeOutRef(left);
  const normalizedRight = normalizeOutRef(right);
  const hashComparison = Buffer.from(normalizedLeft.txHash, "hex").compare(
    Buffer.from(normalizedRight.txHash, "hex"),
  );
  if (hashComparison !== 0) {
    return hashComparison;
  }
  return normalizedLeft.outputIndex - normalizedRight.outputIndex;
};

export const dedupeOutRefs = <T extends OutRef>(outRefs: readonly T[]): T[] => {
  const byLabel = new Map<string, T>();
  for (const outRef of outRefs) {
    const label = outRefLabel(outRef);
    if (!byLabel.has(label)) {
      byLabel.set(label, outRef);
    }
  }
  return [...byLabel.values()];
};
