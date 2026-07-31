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
 * which are cross-checked by `reconstructDaPayloadV1`: it re-derives every
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
  admitAuthenticatedStateQueueHeaderObservationV1,
  admitEvidenceProvenanceV1,
  assertSecurityGradeEvidenceV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  CanonicalEvidenceRejectionV1,
  combineEvidenceGradeV1,
  type EvidenceGradeV1,
  type EvidenceProvenanceV1,
  type HeaderV1,
  ROOT_DOMAINS,
  type TransactionsInclusionRootAuthenticationV1,
} from "@al-ft/midgard-sdk";

import {
  decodeTransactionMaterial,
  nativeTrieItem,
  type NodeTransactionPayload,
} from "../prepare-double-spend.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaFetchAttempt,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import {
  reconstructDaPayloadV1,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";

export const CANONICAL_BLOCK_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-canonical-block-evidence-v1" as const;

/**
 * One transaction of a canonically verified block: the exact canonical native
 * CBOR carried by the payload's `transaction_preimages`, already authenticated
 * by `reconstructDaPayloadV1` against the payload's `transactions` entry and
 * therefore against the L1-committed `transactions_root`.
 */
export type CanonicalBlockTransactionV1 = NodeTransactionPayload;

export type CanonicalBlockEvidenceProvenanceV1 = {
  readonly l1: EvidenceProvenanceV1;
  readonly da: EvidenceProvenanceV1;
};

export type CanonicalBlockEvidenceV1 = {
  readonly schemaVersion: typeof CANONICAL_BLOCK_EVIDENCE_V1_SCHEMA_VERSION;
  readonly grade: EvidenceGradeV1;
  readonly provenance: CanonicalBlockEvidenceProvenanceV1;
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly headerHash: string;
  readonly header: HeaderV1;
  /** SHA-256 of the exact public `DaPayloadEnvelopeV1` bytes. */
  readonly payloadEnvelopeSha256: string;
  /** SHA-256 of the unwrapped canonical `DaPayloadV1` bytes. */
  readonly payloadSha256: string;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly transactions: readonly CanonicalBlockTransactionV1[];
  readonly inclusionRootAuthentication: TransactionsInclusionRootAuthenticationV1;
};

export type CanonicalBlockEvidenceFetchAttemptV1 = {
  readonly sourceId: string;
  readonly sourcePeerId: string;
  readonly protocol: DaRequestResponseProtocol;
  readonly status: RetainedDaFetchAttempt["status"];
};

/**
 * Re-derives both transactions-root leaf conventions and reports which one the
 * L1-committed header actually authenticates. See
 * `TransactionsInclusionRootAuthenticationV1` for why two exist.
 */
export const authenticateTransactionsInclusionRootsV1 = async ({
  header,
  reconstruction,
  transactions,
}: {
  readonly header: HeaderV1;
  readonly reconstruction: TransitionTraceReconstruction;
  readonly transactions: readonly CanonicalBlockTransactionV1[];
}): Promise<TransactionsInclusionRootAuthenticationV1> => {
  const decoded = await Promise.all(transactions.map(decodeTransactionMaterial));
  const nativeCompactPhas = await keyValuePhasRootWithCount(
    decoded.map(nativeTrieItem),
  );
  const nativeCompactCountedRoot = await commitCountedRoot({
    domain: ROOT_DOMAINS.transactionsV1,
    phasRoot: nativeCompactPhas.root,
    count: nativeCompactPhas.count,
  });
  const payloadSource = reconstruction.rootData.transactions;
  return {
    headerTransactionsRoot: header.transactionsRoot,
    l2TransactionCount: header.l2TransactionCount,
    payloadSourcePhasRoot: payloadSource.phasRoot,
    payloadSourceCountedRoot: payloadSource.root,
    nativeCompactPhasRoot: nativeCompactPhas.root,
    nativeCompactCountedRoot,
    nativeInclusionAuthenticated:
      nativeCompactCountedRoot === header.transactionsRoot &&
      nativeCompactPhas.count === header.l2TransactionCount,
    payloadSourceAuthenticated: payloadSource.root === header.transactionsRoot,
  };
};

/**
 * Builds canonical block evidence from bytes the caller already holds: an
 * authenticated L1 header observation plus the exact public
 * `DaPayloadEnvelopeV1` bytes (for example from the watcher's hash-addressed
 * canonical store). No transport is involved, so this is the reusable core for
 * both the retained-DA client and offline deterministic replay.
 */
export const canonicalBlockEvidenceFromVerifiedPayloadV1 = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<CanonicalBlockEvidenceV1> => {
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservationV1({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const admittedDa = assertSecurityGradeEvidenceV1(daProvenance);
  if (admittedDa.trustClass !== "public_or_permissionless_da") {
    throw new CanonicalEvidenceRejectionV1(
      "da_evidence_wrong_trust_class",
      `expected=public_or_permissionless_da actual=${admittedDa.trustClass}`,
    );
  }

  // The cross-check: every counted root and count of the public payload must
  // equal the L1-committed header, and the embedded header must hash to the
  // observed header hash.
  const reconstruction = await reconstructDaPayloadV1({
    payloadEnvelopeCbor,
    expectedHeaderHash: admittedObservation.headerHash,
    committedHeader: admittedObservation.header,
  });

  const transactions: readonly CanonicalBlockTransactionV1[] =
    reconstruction.transactions.map((entry) => ({
      nodeTxId: entry.txId,
      txCbor: entry.fullTransactionCbor.toString("hex"),
    }));
  const inclusionRootAuthentication =
    await authenticateTransactionsInclusionRootsV1({
      header: admittedObservation.header,
      reconstruction,
      transactions,
    });

  return {
    schemaVersion: CANONICAL_BLOCK_EVIDENCE_V1_SCHEMA_VERSION,
    grade: combineEvidenceGradeV1([admittedObservation.provenance, admittedDa]),
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

export type FetchCanonicalBlockEvidenceOptionsV1 = {
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  /** Public retained-DA sources, e.g. `DaLibp2pRetainedDaSource`. */
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
};

/**
 * The canonical evidence-source entry point: authenticated L1 header
 * observation + public retained-DA payload -> verified block evidence.
 */
export const fetchCanonicalBlockEvidenceV1 = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
}: FetchCanonicalBlockEvidenceOptionsV1): Promise<CanonicalBlockEvidenceV1> => {
  if (sources.length === 0) {
    throw new CanonicalEvidenceRejectionV1(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservationV1({
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
  const daProvenance = admitEvidenceProvenanceV1({
    provenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: `${fetched.sourceId}/${fetched.sourcePeerId}`,
      grade: "security",
    },
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
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
export const blockTransactionsFromCanonicalEvidenceV1 = (
  evidence: CanonicalBlockEvidenceV1,
): readonly CanonicalBlockTransactionV1[] => {
  assertSecurityGradeEvidenceV1(evidence.provenance.da);
  assertSecurityGradeEvidenceV1(evidence.provenance.l1);
  return evidence.transactions;
};
