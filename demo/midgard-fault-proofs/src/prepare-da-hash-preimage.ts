/**
 * `da-hash-preimage` DA-first evidence builder (Goal task `Q44`, §9.1 output 7).
 *
 * The fault this family proves is a committed `transactions_root` leaf whose
 * key is not the canonical native-V1 transaction id of its own value. Such a
 * block **cannot** be reconstructed by
 * `reconstructDaPayloadV1` — that routine fails closed with `malformedPayload`
 * exactly when `expectedTxId !== txId`
 * (`src/transition-trace/reconstruct.ts:476-491`) — so this builder cannot use
 * `fetchCanonicalBlockEvidenceV1`. It instead performs the one authentication
 * the proof actually needs, directly against the same two security inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * cross-checked by rebuilding the **raw** `(key, value)` MPF from the payload's
 * `transactions` entries, committing it under the counted
 * `TransactionsV1RootDomain`, and requiring it to equal the L1-committed
 * `transactions_root`. After that check every committed leaf is exactly as
 * trustworthy as the header, which is the same guarantee the L1 verifier
 * re-establishes in `verify_committed_transactions_leaf_in_state_queue_node`.
 *
 * No operator REST/DB/file input is reachable from this module.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { stringifyJson } from "./json-file.js";
import { buildTrieView, requireProof } from "./prepare-double-spend.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";
import {
  commitCountedRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

export const DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION =
  "midgard-da-hash-preimage-evidence-v1" as const;

export type DaHashPreimageRejectionCode =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "transactions_root_mismatch"
  | "no_violating_leaf"
  | "leaf_not_committed";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class DaHashPreimageRejection extends Error {
  readonly code: DaHashPreimageRejectionCode;

  constructor(code: DaHashPreimageRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "DaHashPreimageRejectionV1";
    this.code = code;
  }
}

/** One committed `transactions_root` leaf, classified by the Q44 rule. */
export type CommittedTransactionsLeaf = {
  readonly index: number;
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  readonly verdict: SDK.DaHashPreimageVerdict;
  readonly embeddedTxId: string | null;
  readonly derivedTxId: string | null;
  readonly committedLeafByteCount: number;
  readonly isViolation: boolean;
};

/** Prover arguments for `fraud_proofs/da_hash_preimage/step_01`. */
export type PreparedDaHashPreimageInclusionJson = {
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  /** Raw transactions MPF root the membership proof opens. */
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

/** Exactly the step-02 state the on-chain step-01 validator will derive. */
export type PreparedDaHashPreimageStateJson = {
  readonly verdict: SDK.DaHashPreimageVerdict;
};

export type PreparedDaHashPreimageOutput = {
  readonly schemaVersion: typeof DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION;
  readonly violationId: typeof SDK.DA_HASH_PREIMAGE_VIOLATION_ID;
  readonly headerHash: string;
  readonly l2TransactionCount: number;
  /** Raw MPF root opened by the leaf membership proof. */
  readonly transactionsPhasRoot: string;
  /** Counted, domain-separated root the header commits. */
  readonly committedTransactionsRoot: string;
  readonly leaves: readonly CommittedTransactionsLeaf[];
  readonly violation: CommittedTransactionsLeaf;
  readonly txInclusion: PreparedDaHashPreimageInclusionJson;
  readonly step02State: PreparedDaHashPreimageStateJson;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly planPath: string;
  };
};

const hexOf = (value: string, label: string): Buffer => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})*$/u.test(normalized)) {
    throw new DaHashPreimageRejection(
      "malformed_da_payload",
      `${label} is not even-length hexadecimal`,
    );
  }
  return Buffer.from(normalized, "hex");
};

/**
 * Classifies every committed leaf of an already-authenticated transactions
 * root. Pure and total: it never decodes a leaf value.
 */
export const classifyCommittedTransactionsLeaves = (
  entries: readonly (readonly [string, string])[],
): readonly CommittedTransactionsLeaf[] =>
  entries.map(([keyHex, valueHex], index) => {
    const key = hexOf(keyHex, `transactions[${index.toString()}].key`);
    const value = hexOf(valueHex, `transactions[${index.toString()}].value`);
    const evidence = SDK.daHashPreimageEvidenceFromCommittedLeaf({
      committedTxId: key.toString("hex"),
      committedLeafValue: value,
    });
    return {
      index,
      committedTxId: evidence.committedTxId,
      committedLeafValueCbor: evidence.committedLeafValueCbor,
      verdict: evidence.verdict,
      embeddedTxId: evidence.embeddedTxId,
      derivedTxId: evidence.derivedTxId,
      committedLeafByteCount: value.length,
      isViolation: evidence.isViolation,
    };
  });

export type PrepareDaHashPreimageFromCommittedLeavesOptions = {
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly entries: readonly (readonly [string, string])[];
  /** Pin a specific committed key; otherwise the first violating leaf is used. */
  readonly committedTxId?: string;
  readonly outputDir?: string;
};

/**
 * Core builder: authenticates the raw committed leaves against the header's
 * counted `transactions_root`, then emits the membership proof and the exact
 * step-02 state for one violating leaf.
 */
export const prepareDaHashPreimageFromCommittedLeaves = async ({
  headerHash,
  committedTransactionsRoot,
  l2TransactionCount,
  entries,
  committedTxId,
  outputDir,
}: PrepareDaHashPreimageFromCommittedLeavesOptions): Promise<PreparedDaHashPreimageOutput> => {
  const phasEntries = entries.map(([keyHex, valueHex], index) => ({
    key: hexOf(keyHex, `transactions[${index.toString()}].key`),
    value: hexOf(valueHex, `transactions[${index.toString()}].value`),
  }));
  const phas = await keyValuePhasRootWithCount(phasEntries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.transactionsV1,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== committedTransactionsRoot.toLowerCase() ||
    phas.count !== l2TransactionCount
  ) {
    throw new DaHashPreimageRejection(
      "transactions_root_mismatch",
      `header_transactions_root=${committedTransactionsRoot.toLowerCase()} derived=${countedRoot} header_count=${l2TransactionCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = classifyCommittedTransactionsLeaves(entries);
  const violation =
    committedTxId === undefined
      ? leaves.find((leaf) => leaf.isViolation)
      : leaves.find(
          (leaf) => leaf.committedTxId === committedTxId.toLowerCase(),
        );
  if (violation === undefined) {
    throw new DaHashPreimageRejection(
      committedTxId === undefined ? "no_violating_leaf" : "leaf_not_committed",
      `header_hash=${headerHash} leaf_count=${leaves.length.toString()}`,
    );
  }
  if (!violation.isViolation) {
    throw new DaHashPreimageRejection(
      "no_violating_leaf",
      `committed_tx_id=${violation.committedTxId} derives its own key; a valid block cannot be challenged`,
    );
  }

  const trie = await buildTrieView(phasEntries);
  const key = Buffer.from(violation.committedTxId, "hex");
  const txInclusion: PreparedDaHashPreimageInclusionJson = {
    committedTxId: violation.committedTxId,
    committedLeafValueCbor: violation.committedLeafValueCbor,
    transactionsPhasRoot: phas.root,
    txMembershipProofCbor: requireProof(trie, key, "committed leaf"),
  };
  const output: PreparedDaHashPreimageOutput = {
    schemaVersion: DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.DA_HASH_PREIMAGE_VIOLATION_ID,
    headerHash: headerHash.toLowerCase(),
    l2TransactionCount: Number(l2TransactionCount),
    transactionsPhasRoot: phas.root,
    committedTransactionsRoot: countedRoot,
    leaves,
    violation,
    txInclusion,
    step02State: {
      verdict: violation.verdict,
    },
  };
  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    txInclusionPath: join(outputDir, "tx-inclusion.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.txInclusionPath, stringifyJson(output.txInclusion)),
    writeFile(
      paths.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        headerHash: output.headerHash,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
        l2TransactionCount: output.l2TransactionCount,
        violation: output.violation,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

export type DaHashPreimageBlockEvidence = {
  readonly grade: SDK.EvidenceGrade;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenance;
    readonly da: SDK.EvidenceProvenance;
  };
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly l1ChainPoint: {
    readonly blockHash: string;
    readonly slot: bigint;
  };
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly entries: readonly (readonly [string, string])[];
};

/**
 * Extracts the committed `transactions` leaves from public retained-DA bytes
 * without requiring the block to be well-formed. Only the payload envelope,
 * canonical `DaPayload` framing and the embedded-header identity are
 * enforced here; leaf correctness is what the family adjudicates.
 */
export const daHashPreimageBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<DaHashPreimageBlockEvidence> => {
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservation({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const admittedDa = SDK.assertSecurityGradeEvidence(daProvenance);
  if (admittedDa.trustClass !== "public_or_permissionless_da") {
    throw new SDK.CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      `expected=public_or_permissionless_da actual=${admittedDa.trustClass}`,
    );
  }

  let payloadCbor: Buffer;
  try {
    payloadCbor = Buffer.from(
      (
        await unwrapDaPayload(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes,
    );
  } catch (cause) {
    throw new DaHashPreimageRejection(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(payloadCbor);
  } catch (cause) {
    throw new DaHashPreimageRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new DaHashPreimageRejection(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw new DaHashPreimageRejection(
      "wrong_da_payload_version",
      `expected=${SDK.DA_PAYLOAD_VERSION.toString()} actual=${payload.version.toString()}`,
    );
  }
  const body = payload.block_body;
  const embeddedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeader(body.header),
  );
  if (
    embeddedHeaderHash !== body.header_hash.toLowerCase() ||
    embeddedHeaderHash !== admittedObservation.headerHash
  ) {
    throw new DaHashPreimageRejection(
      "header_hash_mismatch",
      `embedded=${embeddedHeaderHash} payload=${body.header_hash.toLowerCase()} observed=${admittedObservation.headerHash}`,
    );
  }

  return {
    grade: SDK.combineEvidenceGrade([
      admittedObservation.provenance,
      admittedDa,
    ]),
    provenance: { l1: admittedObservation.provenance, da: admittedDa },
    headerHash: admittedObservation.headerHash,
    payloadEnvelopeSha256: computeDaSha256Hash(
      Buffer.from(payloadEnvelopeCbor),
    ).toString("hex"),
    payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
    l1ChainPoint: admittedObservation.chainPoint,
    committedTransactionsRoot: admittedObservation.header.transactionsRoot,
    l2TransactionCount: admittedObservation.header.l2TransactionCount,
    entries: body.transactions.map(
      ([keyHex, valueHex]) => [keyHex, valueHex] as const,
    ),
  };
};

/**
 * The security-grade entry point: authenticated L1 header observation + public
 * retained-DA payload -> a submittable `da-hash-preimage` proof plan.
 */
export const prepareDaHashPreimageFromRetainedDa = async ({
  observation,
  sources,
  retries,
  minimumConfirmationDepth,
  committedTxId,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedTxId?: string;
  readonly outputDir?: string;
}): Promise<PreparedDaHashPreimageOutput> => {
  if (sources.length === 0) {
    throw new SDK.CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservation({
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
  const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidence(
      SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return await prepareDaHashPreimageFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedTransactionsRoot: evidence.committedTransactionsRoot,
    l2TransactionCount: evidence.l2TransactionCount,
    entries: evidence.entries,
    ...(committedTxId === undefined ? {} : { committedTxId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
