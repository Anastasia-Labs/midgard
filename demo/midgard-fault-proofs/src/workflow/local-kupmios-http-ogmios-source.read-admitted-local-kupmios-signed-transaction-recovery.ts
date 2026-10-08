import JSONBig from "json-bigint";

import {
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  withLocalKupmiosSourceCapture,
} from "./local-kupmios-raw-l1-authority.js";
import {
  type FraudProofL1ObservationDepth,
  type FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

export const OGMIOS_RAW_TRANSACTION_CBOR_FLAG =
  "--include-transaction-cbor" as const;

export const LOCAL_KUPMIOS_HTTP_OGMIOS_SOURCE =
  "midgard-local-kupo-http-ogmios-ws-source-v1" as const;

export const LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT =
  "midgard-local-kupmios-raw-block-at-point-v1" as const;

/** A transport failure leaves the canonical chain and any signed attempt unknown.
 * Retrying must begin with observation/reconciliation, never a replacement send. */
export class LocalKupmiosTransportUnavailableError extends Error {
  readonly cause: unknown;
  constructor(message: string, options?: Readonly<{ cause?: unknown }>) {
    super(message);
    this.cause = options?.cause;
    this.name = "LocalKupmiosTransportUnavailableError";
  }
}

/** Exact Kupo canonical-chain refusal; safe for bounded rollback-prefix search. */
export class LocalKupmiosExactPointNotCanonicalError extends Error {
  constructor(
    message: string,
    /**
     * Present when the exact checkpoint read found Kupo behind the requested
     * slot, so the mismatch may be indexing lag rather than a divergent chain.
     */
    readonly kupoLag?: Readonly<{
      requestedSlot: number;
      checkpointSlot: number;
      kupoHeadSlot: number;
    }>,
  ) {
    super(message);
    this.name = "LocalKupmiosExactPointNotCanonicalV1Error";
  }
}

/**
 * True when an exact point read failed because Kupo's checkpoint at the
 * requested slot is still an earlier block, so Kupo has not indexed a block
 * at that slot yet. The caller may wait for Kupo to catch up and read again.
 * A checkpoint at the requested slot that still differs is a real divergence
 * and is never reported as lag. Kupo's advertised head is deliberately not
 * consulted: it comes from a separate response header and can already name
 * the requested block while the checkpoint query still resolves to its
 * predecessor.
 */
export const isLocalKupmiosPointBehindKupoHead = (error: unknown): boolean =>
  error instanceof LocalKupmiosExactPointNotCanonicalError &&
  error.kupoLag !== undefined &&
  error.kupoLag.checkpointSlot < error.kupoLag.requestedSlot;

export type LocalKupmiosRawBlockAtPoint = Readonly<{
  schemaVersion: typeof LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT;
  sourceId: string;
  point: FraudProofRawL1Point;
  parentBlockHash: string | null;
  kupoCheckpoint: Readonly<{ slot: number; blockHash: string }>;
  transactions: readonly Readonly<{
    txHash: string;
    transactionCbor: string;
  }>[];
}>;

export type LocalKupmiosReferenceBodiesAtPoint = Readonly<{
  targetBlock: LocalKupmiosRawBlockAtPoint;
  creatingTransactionBodies: readonly string[];
}>;

export const admittedReferenceBodyReaders = new WeakMap<
  object,
  (point: FraudProofRawL1Point) => Promise<LocalKupmiosReferenceBodiesAtPoint>
>();

export const admittedHistoricalPageReaders = new WeakMap<
  LocalKupmiosFraudProofRawSource,
  Readonly<{
    address: LocalKupmiosFraudProofRawSource["scanAddressPage"];
    history: LocalKupmiosFraudProofRawSource["scanUnitHistoryPage"];
  }>
>();

export type LocalKupmiosAdmittedPredecessorPoint = Readonly<{
  sourceId: string;
  point: FraudProofRawL1Point;
  predecessorPoint: FraudProofRawL1Point;
}>;

export const admittedPredecessorReaders = new WeakMap<
  object,
  (point: FraudProofRawL1Point) => Promise<LocalKupmiosAdmittedPredecessorPoint>
>();

export type LocalKupmiosAdmittedBoundary = Readonly<{
  kupoCheckpoint: FraudProofRawL1Point;
  ogmiosTip: FraudProofRawL1Point;
  confirmationDepth: number;
}>;

export type LocalKupmiosAdmittedUnitHistory = Readonly<{
  checkpoint: FraudProofRawL1Point;
  transactions: readonly Readonly<{
    txHash: string;
    inclusionPoint: FraudProofRawL1Point;
  }>[];
}>;

export const admittedHttpOgmiosSources = new WeakSet<object>();

export const signedTransactionRecoveryReaders = new WeakMap<
  object,
  (
    input: SignedWorkflowTransaction,
  ) => Promise<SignedTransactionRecoveryObservation>
>();

export const signedTransactionRebroadcasters = new WeakMap<
  object,
  (
    input: SignedWorkflowTransaction,
    authorize: (input: SignedWorkflowTransaction) => Promise<void>,
  ) => Promise<string>
>();

/** Concrete local source admission; copied or structural provider objects cannot authorize recovery. */
export const readAdmittedLocalKupmiosSignedTransactionRecovery = async (
  input: SignedWorkflowTransaction & {
    readonly source: LocalKupmiosFraudProofRawSource;
  },
): Promise<SignedTransactionRecoveryObservation> => {
  const read = signedTransactionRecoveryReaders.get(input.source);
  if (read === undefined)
    throw new Error(
      "Signed recovery requires admitted local Kupo/Ogmios authority",
    );
  inspectSignedWorkflowTransaction(input);
  return withLocalKupmiosSourceCapture(input.source, async () => {
    for (let attempt = 0; ; attempt += 1) {
      try {
        return await read(input);
      } catch (cause) {
        if (
          !(cause instanceof LocalKupmiosCheckpointChangedError) ||
          attempt >= 2
        )
          throw cause;
        // readBoundary resets all capture caches; no partial input or inclusion
        // evidence crosses into the next independently pinned attempt.
      }
    }
  });
};

/** Replay recorded witnesses and body without rebuilding, evaluating, or signing a replacement. */
export const rebroadcastAdmittedLocalKupmiosSignedTransaction = async (
  input: SignedWorkflowTransaction & {
    readonly source: LocalKupmiosFraudProofRawSource;
    readonly authorizeResubmission: (
      input: SignedWorkflowTransaction,
    ) => Promise<void>;
  },
): Promise<string> => {
  const submit = signedTransactionRebroadcasters.get(input.source);
  if (submit === undefined)
    throw new Error(
      "Signed rebroadcast requires admitted local Ogmios authority",
    );
  inspectSignedWorkflowTransaction(input);
  return submit(input, input.authorizeResubmission);
};

export type LocalKupmiosHttpOgmiosRawSourceDetails = Readonly<{
  sourceId: string;
  kupoHttpUrl: string;
  ogmiosUrl: string;
  deploymentIdentityDigest: string;
  blueprintHash: string;
  finalityPolicyDigest: string;
  observationDepth: FraudProofL1ObservationDepth;
  confirmationDepth: number;
  automaticRecoveryMaxDepth: 2160;
}>;

export const admittedHttpOgmiosSourceDetails = new WeakMap<
  object,
  LocalKupmiosHttpOgmiosRawSourceDetails
>();

/**
 * Returns the immutable authority binding captured by the concrete loopback
 * constructor. Structural source copies deliberately have no details.
 */
export const localKupmiosHttpOgmiosRawSourceDetails = (
  source: LocalKupmiosFraudProofRawSource,
): LocalKupmiosHttpOgmiosRawSourceDetails | null =>
  admittedHttpOgmiosSourceDetails.get(source) ?? null;

export type FraudProofRawL1Fetch = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type FraudProofRawL1WebSocketLike = {
  send(data: string): void;
  close(code?: number, reason?: string): void;
  addEventListener(
    type: string,
    listener: (event: never) => void,
    options?: { once?: boolean },
  ): void;
  removeEventListener(type: string, listener: (event: never) => void): void;
};

export type FraudProofRawL1WebSocketFactory = (
  url: string,
) => FraudProofRawL1WebSocketLike;

export type LocalKupmiosHttpOgmiosSourceConfig = {
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosUrl: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly observationDepth?: FraudProofL1ObservationDepth;
  readonly fetchImpl?: FraudProofRawL1Fetch;
  readonly webSocketFactory?: FraudProofRawL1WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
  readonly signal?: AbortSignal;
  readonly maxResponseBytes?: number;
};

export type KupoPoint = {
  readonly slot: number;
  readonly blockHash: string;
};

export type KupoSpentPoint = KupoPoint & {
  readonly txHash: string;
};

export type KupoMatch = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly createdAt: KupoPoint;
  readonly spentAt: KupoSpentPoint | null;
  readonly datumHash: string | null;
  readonly datumType: "hash" | "inline" | null;
  readonly datum: string | null;
  readonly scriptHash: string | null;
  readonly script: unknown;
};

export type OgmiosTip = {
  readonly slot: number;
  readonly blockHash: string;
  readonly blockNo: number;
};

export type OgmiosRawTransactionAtPoint = {
  readonly txHash: string;
  readonly transactionCbor: string;
  readonly point: FraudProofRawL1Point;
};

export const losslessJson = JSONBig({ useNativeBigInt: true, strict: true });

export const DEFAULT_TIMEOUT_MS = 20_000;

export const DEFAULT_BLOCK_SCAN_LIMIT = 2_000;

export const MAX_MATCHES = 100_000;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

export const NATURAL = /^(0|[1-9][0-9]*)$/u;
