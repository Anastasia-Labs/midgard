import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type {
  FraudProofL1ObservationDepth,
  FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";

export const LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE =
  "midgard-local-kupmios-fraud-proof-raw-source-v1" as const;

/** The unpublished capture must restart when its pinned provider head changes. */
export class LocalKupmiosCheckpointChangedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "LocalKupmiosCheckpointChangedError";
  }
}

/**
 * Provider transport owned by the local node runtime. Every response remains
 * untrusted: this package parses the pages, binds both providers to one pinned
 * point, and admits the final canonical bytes.
 */
export interface LocalKupmiosFraudProofRawSource {
  /** Concrete local history lookup, including transactions whose outputs were spent. */
  resolveTransactionInclusion?(input: {
    readonly txHash: string;
  }): Promise<unknown>;
  pinBoundaryAtPoint?(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
  readOutRefsAtPoint?(input: {
    readonly point: FraudProofRawL1Point;
    readonly outRefs: readonly string[];
  }): Promise<unknown>;
  readonly sourceVersion: typeof LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE;
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosWebSocketUrl: string;
  readBoundary(input?: {
    readonly observationDepth?: FraudProofL1ObservationDepth;
  }): Promise<unknown>;
  /** Exact ordered raw block capture for independently authenticated readers. */
  readBlockAtPoint(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
  scanAddressPage(input: {
    readonly address: string;
    readonly throughPoint: FraudProofRawL1Point;
    readonly after: string | null;
  }): Promise<unknown>;
  scanUnitHistoryPage(input: {
    readonly unit: string;
    readonly fromGenesis: true;
    readonly throughPoint: FraudProofRawL1Point;
    readonly after: string | null;
  }): Promise<unknown>;
  readTransaction(input: {
    readonly txHash: string;
    readonly expectedInclusionPoint: FraudProofRawL1Point;
  }): Promise<unknown>;
  confirmCanonicalPoint(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
}

const sourceCaptures = new WeakMap<
  LocalKupmiosFraudProofRawSource,
  Promise<void>
>();

/** Own the source's mutable boundary through the final read and admission.
 * Call only at the outer capture boundary; nested reads use the held source.
 */
export const withLocalKupmiosSourceCapture = <T>(
  source: LocalKupmiosFraudProofRawSource,
  capture: () => Promise<T>,
  scope?: DaAvailabilityReadScope,
): Promise<T> => {
  const previous = sourceCaptures.get(source) ?? Promise.resolve();
  let release!: () => void;
  const held = new Promise<void>((resolve) => {
    release = resolve;
  });
  sourceCaptures.set(source, held);
  // Caller cancellation fences its result. Queue ownership follows completion
  // of the actual callback, not the outer cancellation race: a late read may
  // never mutate this source alongside the next capture.
  const completion = previous
    .then(async () => {
      scope?.assertCurrent();
      const result = await capture();
      scope?.assertCurrent();
      return result;
    })
    .finally(() => {
      release();
      if (sourceCaptures.get(source) === held) sourceCaptures.delete(source);
    });
  // An already-expired caller can reject before read() attaches its race.
  // Observe the owned completion without replacing its original error/result.
  void completion.catch(() => {});
  return scope === undefined ? completion : scope.read(() => completion);
};

/** A failed branch must not leave sibling reads using a released source. */
export const settleLocalKupmiosReads = async <T extends readonly unknown[]>(
  reads: T,
): Promise<{ -readonly [K in keyof T]: Awaited<T[K]> }> => {
  await Promise.allSettled(reads);
  return await Promise.all(reads);
};
