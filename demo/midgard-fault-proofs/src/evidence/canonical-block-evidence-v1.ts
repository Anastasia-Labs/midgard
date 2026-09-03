/**
 * Canonical evidence-source API — transport and block-evidence layer (`Q03`).
 *
 * `GOAL_SPEC.md` §9.2 acceptance:
 *
 * > Builders consume verified `DaPayloadV1`/proof bundles and authenticated L1
 * > observations, not operator-private REST/DB/files except labelled
 * > diagnostics.
 *
 * Every function here produces evidence from exactly two security inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. a `DaPayloadEnvelopeV1` retrieved over the public retained-DA protocol
 *    (`public_or_permissionless_da`),
 *
 * which are cross-checked by `reconstructDaPayload`: it re-derives every
 * counted root and count from the payload and requires them to equal the
 * L1-committed header. Operator REST/DB/file inputs are not reachable from this
 * module; the labelled-diagnostic escape hatch lives in
 * `./diagnostic-evidence-v1.ts` and cannot be admitted as security evidence.
 */
import {
  computeDaSha256Hash,
  type DaRequestResponseProtocol,
} from "@al-ft/midgard-core/da-transport";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  admitEvidenceProvenance,
  assertSecurityGradeEvidence,
  type AuthenticatedStateQueueHeaderObservation,
  CanonicalEvidenceRejection,
  combineEvidenceGrade,
  type EvidenceGrade,
  type EvidenceProvenance,
  type Header,
  type TransactionsInclusionRootAuthentication,
} from "@al-ft/midgard-sdk";

import type { NodeTransactionPayload } from "../prepare-double-spend.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaFetchAttempt,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";

export const CANONICAL_BLOCK_EVIDENCE_SCHEMA_VERSION =
  "midgard-canonical-block-evidence-v1" as const;

/**
 * One transaction of a canonically verified block: the exact canonical native
 * CBOR carried by the payload's `transaction_preimages`, already authenticated
 * by `reconstructDaPayload` against the payload's `transactions` entry and
 * therefore against the L1-committed `transactions_root`.
 */
export type CanonicalBlockTransaction = NodeTransactionPayload & {
  /** Canonical Data(L2TransactionSourceV1) bytes committed by the header. */
  readonly l2TransactionSourceCbor: string;
};

export type CanonicalBlockEvidenceProvenance = {
  readonly l1: EvidenceProvenance;
  readonly da: EvidenceProvenance;
};

export type CanonicalBlockEvidence = {
  readonly schemaVersion: typeof CANONICAL_BLOCK_EVIDENCE_SCHEMA_VERSION;
  readonly grade: EvidenceGrade;
  readonly provenance: CanonicalBlockEvidenceProvenance;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly headerHash: string;
  readonly header: Header;
  /** SHA-256 of the exact public `DaPayloadEnvelopeV1` bytes. */
  readonly payloadEnvelopeSha256: string;
  /** SHA-256 of the unwrapped canonical `DaPayloadV1` bytes. */
  readonly payloadSha256: string;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly transactions: readonly CanonicalBlockTransaction[];
  readonly inclusionRootAuthentication: TransactionsInclusionRootAuthentication;
};

export type CanonicalBlockEvidenceFetchAttempt = {
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly protocol: DaRequestResponseProtocol;
  readonly status: RetainedDaFetchAttempt["status"];
};

/** Re-derives the one normative transaction-source leaf convention. */
export const authenticateTransactionsInclusionRoots = async ({
  header,
  reconstruction,
}: {
  readonly header: Header;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly transactions: readonly CanonicalBlockTransaction[];
}): Promise<TransactionsInclusionRootAuthentication> => {
  const source = reconstruction.rootData.transactions;
  return {
    headerTransactionsRoot: header.transactionsRoot,
    l2TransactionCount: header.l2TransactionCount,
    sourceValuePhasRoot: source.phasRoot,
    sourceValueCountedRoot: source.root,
    sourceValueCount: source.count,
    sourceInclusionAuthenticated:
      source.root === header.transactionsRoot &&
      source.count === header.l2TransactionCount,
  };
};

/**
 * Builds canonical block evidence from bytes the caller already holds: an
 * authenticated L1 header observation plus the exact public
 * `DaPayloadEnvelopeV1` bytes (for example from the watcher's hash-addressed
 * canonical store). No transport is involved, so this is the reusable core for
 * both the retained-DA client and offline deterministic replay.
 */
export const canonicalBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<CanonicalBlockEvidence> => {
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservation({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const admittedDa = assertSecurityGradeEvidence(daProvenance);
  if (admittedDa.trustClass !== "public_or_permissionless_da") {
    throw new CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      `expected=public_or_permissionless_da actual=${admittedDa.trustClass}`,
    );
  }

  // The cross-check: every counted root and count of the public payload must
  // equal the L1-committed header, and the embedded header must hash to the
  // observed header hash.
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor,
    expectedHeaderHash: admittedObservation.headerHash,
    committedHeader: admittedObservation.header,
  });

  const transactions: readonly CanonicalBlockTransaction[] =
    reconstruction.transactions.map((entry) => ({
      nodeTxId: entry.txId,
      txCbor: entry.fullTransactionCbor.toString("hex"),
      l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
    }));
  const inclusionRootAuthentication =
    await authenticateTransactionsInclusionRoots({
      header: admittedObservation.header,
      reconstruction,
      transactions,
    });

  return {
    schemaVersion: CANONICAL_BLOCK_EVIDENCE_SCHEMA_VERSION,
    grade: combineEvidenceGrade([admittedObservation.provenance, admittedDa]),
    provenance: { l1: admittedObservation.provenance, da: admittedDa },
    observation: admittedObservation,
    headerHash: admittedObservation.headerHash,
    header: admittedObservation.header,
    payloadEnvelopeSha256: computeDaSha256Hash(
      Buffer.from(payloadEnvelopeCbor),
    ).toString("hex"),
    payloadSha256: computeDaSha256Hash(reconstruction.payloadCbor).toString(
      "hex",
    ),
    reconstruction,
    transactions,
    inclusionRootAuthentication,
  };
};

export type FetchCanonicalBlockEvidenceOptions = {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  /** Public retained-DA sources, e.g. `DaLibp2pRetainedDaSource`. */
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
};

/**
 * The canonical evidence-source entry point: authenticated L1 header
 * observation + public retained-DA payload -> verified block evidence.
 */
export const fetchCanonicalBlockEvidence = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
}: FetchCanonicalBlockEvidenceOptions): Promise<CanonicalBlockEvidence> => {
  if (sources.length === 0) {
    throw new CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservation({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admittedObservation.headerHash,
    sources,
    ...(retries === undefined ? {} : { retries }),
  });
  const daProvenance = assertSecurityGradeEvidence(
    admitEvidenceProvenance({ provenance: fetched.provenance }),
  );
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
};

/**
 * Authenticated per-transaction material for detection and classification.
 * Safe to use before the inclusion-root gate, because it carries no membership
 * proof: every byte is already bound to the L1-committed `transactions_root`
 * through the payload reconstruction.
 */
export const blockTransactionsFromCanonicalEvidence = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalBlockTransaction[] => {
  assertSecurityGradeEvidence(evidence.provenance.da);
  assertSecurityGradeEvidence(evidence.provenance.l1);
  return evidence.transactions;
};
