import {
  compareOutRefs as compareCoreOutRefs,
  normalizeOutRef as normalizeCoreOutRef,
  normalizeTxHash as normalizeCoreTxHash,
  outRefLabel as coreOutRefLabel,
  type OutRefLike,
  parseOutRefLabel as parseCoreOutRefLabel,
} from "@al-ft/midgard-core/out-ref";

import { BuilderInvariantError } from "./errors.js";

export type OutRef = OutRefLike;

const toBuilderInvariant = <T>(
  message: string,
  detail: string | null,
  run: () => T,
): T => {
  try {
    return run();
  } catch (cause) {
    throw new BuilderInvariantError(
      message,
      cause instanceof Error
        ? detail === null
          ? cause.message
          : `${detail}: ${cause.message}`
        : detail,
    );
  }
};

export const normalizeTxHash = (txHash: string): string => {
  try {
    return normalizeCoreTxHash(txHash);
  } catch {
    throw new BuilderInvariantError(
      "Invalid transaction hash",
      `txHash=${txHash}`,
    );
  }
};

export const normalizeOutRef = (outRef: OutRef): OutRef =>
  toBuilderInvariant(
    "Invalid output reference",
    `txHash=${outRef.txHash},outputIndex=${outRef.outputIndex.toString()}`,
    () => normalizeCoreOutRef(outRef),
  );

export const outRefLabel = (outRef: OutRef): string =>
  toBuilderInvariant("Invalid output reference", String(outRef.txHash), () =>
    coreOutRefLabel(outRef),
  );

export const parseOutRefLabel = (label: string): OutRef =>
  toBuilderInvariant("Invalid outref label", label, () =>
    parseCoreOutRefLabel(label),
  );

export const compareOutRefs = (left: OutRef, right: OutRef): number =>
  toBuilderInvariant("Invalid output reference", null, () =>
    compareCoreOutRefs(left, right),
  );
