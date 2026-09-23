/**
 * `fabricated-withdrawal` DA-first evidence builder (Goal task `Q40`, §9.1
 * output 7).
 *
 * The fault this family proves is a committed `withdrawals_root` leaf that is not
 * the authentic L1 withdrawal event pair: either no withdrawal event with the
 * committed `WithdrawalId` was ever authenticated, or the authentic event exists
 * and was due for the block but its `(body, signature)` content is not the
 * committed one. The committed `validity` verdict is not compared: the operator
 * owns it (decision 0007,
 * `docs/fault-proofs/decisions/0007-operator-owned-event-validity.md`), and a
 * verdict the chain contradicts is `withdrawalMistag`'s fault.
 *
 * Such a block cannot be reconstructed by `reconstructDaPayloadV1` — a whole block
 * whose withdrawal source set disagrees with L1 fails reconstruction long before
 * the leaf in question is reached — so, like `prepare-fabricated-deposit`, this
 * builder decodes the retained-DA envelope itself and performs exactly the
 * authentication the proof needs, against two security-graded inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * cross-checked by rebuilding the **raw** `(WithdrawalId, WithdrawalInfo)` MPF
 * from the payload's `withdrawals` entries, committing it under the counted
 * `WithdrawalsRootDomain`, and requiring **both** that the counted root equals the
 * L1-committed `withdrawals_root` **and** that the rebuilt cardinality equals the
 * header's `withdrawal_count`. After that check every committed withdrawal leaf is
 * exactly as trustworthy as the header itself.
 *
 * `assertNativeInclusionRootAuthenticatedV1` is deliberately **not** used: it
 * authenticates the native-compact *transaction* leaf convention against
 * `transactions_root` and has no bearing on `withdrawals_root`, which has a single
 * leaf convention. Requiring it would refuse a legitimate fabricated-withdrawal
 * proof whenever the block's unrelated transaction leaves use the payload-source
 * convention. The same reasoning is recorded at
 * `src/evidence/prepare-from-evidence.ts:145-150`.
 *
 * The L1 side is a hub-authenticated sorted-list gap/filler or Order. Large
 * payloads must be supplied by the actual retained-data reference output. The
 * preparation persists a complete payload/Value opening for the later capture
 * commitment; no operator archive or live nonce establishes list membership.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  authenticateFabricatedHistoryWitness,
  type FabricatedHistoryWitness,
} from "./fabricated-history-witness.js";
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

export const FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION =
  "midgard-fabricated-withdrawal-evidence-v1" as const;

export type FabricatedWithdrawalRejectionCode =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "withdrawals_root_mismatch"
  | "no_committed_withdrawal_leaf"
  | "leaf_not_committed"
  | "authentic_content_matches_commitment"
  | "history_witness_invalid";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class FabricatedWithdrawalRejection extends Error {
  readonly code: FabricatedWithdrawalRejectionCode;

  constructor(code: FabricatedWithdrawalRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "FabricatedWithdrawalRejectionV1";
    this.code = code;
  }
}

const hexOf = (value: string, label: string): Buffer => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})*$/u.test(normalized)) {
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `${label} is not even-length hexadecimal`,
    );
  }
  return Buffer.from(normalized, "hex");
};

/** One committed `withdrawals_root` leaf, decoded and committed to. */
export type CommittedWithdrawalLeaf = {
  readonly index: number;
  /** Canonical CBOR of the leaf key — a `WithdrawalId` output reference. */
  readonly committedWithdrawalIdCbor: string;
  /** Canonical CBOR of the leaf value — the committed `WithdrawalInfo`. */
  readonly committedWithdrawalInfoCbor: string;
  readonly committedWithdrawalId: SDK.OutputReference;
  readonly committedWithdrawalInfo: SDK.WithdrawalInfo;
  /** Blake2b-256 of the committed `WithdrawalInfo`'s canonical bytes. */
  readonly committedWithdrawalContentHash: string;
  readonly committedLeafByteCount: number;
};

const decodeCommittedWithdrawalLeaf = async (
  keyHex: string,
  valueHex: string,
  index: number,
): Promise<CommittedWithdrawalLeaf> => {
  const label = `withdrawals[${index.toString()}]`;
  const key = hexOf(keyHex, `${label}.key`);
  const value = hexOf(valueHex, `${label}.value`);
  const committedWithdrawalIdCbor = key.toString("hex");
  const committedWithdrawalInfoCbor = value.toString("hex");
  let committedWithdrawalId: SDK.OutputReference;
  let committedWithdrawalInfo: SDK.WithdrawalInfo;
  try {
    committedWithdrawalId = Data.from(
      committedWithdrawalIdCbor,
      SDK.OutputReference,
    );
    committedWithdrawalInfo = Data.from(
      committedWithdrawalInfoCbor,
      SDK.WithdrawalInfo,
    );
  } catch (cause) {
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `${label} does not decode as (WithdrawalId, WithdrawalInfo): ${String(cause)}`,
    );
  }
  if (
    SDK.committedWithdrawalKeyBytes(committedWithdrawalId) !==
      committedWithdrawalIdCbor ||
    SDK.committedWithdrawalValueBytes(committedWithdrawalInfo) !==
      committedWithdrawalInfoCbor
  ) {
    throw new FabricatedWithdrawalRejection(
      "non_canonical_da_payload",
      `${label} leaf bytes are not canonical for (WithdrawalId, WithdrawalInfo)`,
    );
  }
  const committedWithdrawalContentHash = await Effect.runPromise(
    SDK.withdrawalContentCommitment(committedWithdrawalInfo),
  );
  return {
    index,
    committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor,
    committedWithdrawalId,
    committedWithdrawalInfo,
    committedWithdrawalContentHash,
    committedLeafByteCount: value.length,
  };
};

/** Public-L1 hub, authenticated list anchor and optional retained-data output. */
export type FabricatedWithdrawalL1Witness = FabricatedHistoryWitness;

export type ClassifiedFabricatedWithdrawalFault = {
  readonly verdict: SDK.FabricatedWithdrawalEvidenceVerdict;
  readonly fault: SDK.FabricatedWithdrawalFault;
  readonly stateQueuePolicyId: string;
  readonly openingCbor: string | null;
  readonly authenticWithdrawalContentHash?: string;
  readonly eventInclusionTime?: bigint;
};

/** Current authenticated list absence or immutable Order facts. No live-nonce
 * fallback or operator archive can establish an event's existence/absence. */
export const classifyFabricatedWithdrawalFault = async ({
  leaf,
  headerStartTime,
  headerEndTime,
  witness,
  minimumConfirmationDepth,
}: {
  readonly leaf: CommittedWithdrawalLeaf;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly witness: FabricatedWithdrawalL1Witness;
  readonly minimumConfirmationDepth?: number;
}): Promise<ClassifiedFabricatedWithdrawalFault> => {
  let authenticated: Awaited<
    ReturnType<typeof authenticateFabricatedHistoryWitness>
  >;
  try {
    authenticated = await authenticateFabricatedHistoryWitness(
      witness,
      "Withdrawal",
      leaf.committedWithdrawalId,
      minimumConfirmationDepth,
    );
  } catch (cause) {
    throw new FabricatedWithdrawalRejection(
      "history_witness_invalid",
      String(cause),
    );
  }
  const { captured, stateQueuePolicyId } = authenticated;
  if (captured === undefined)
    return {
      verdict: "WithdrawalIdentityAbsent",
      fault: "NonexistentWithdrawalIdentity",
      stateQueuePolicyId,
      openingCbor: null,
    };
  const { commitment, payload, originalAssets } = captured;
  if (!("WithdrawalPayload" in payload))
    throw new FabricatedWithdrawalRejection(
      "history_witness_invalid",
      "Wrong authenticated event kind",
    );
  const authenticWithdrawalContentHash = await Effect.runPromise(
    SDK.withdrawalContentCommitment(payload.WithdrawalPayload.event.info),
  );
  const inclusionTime = commitment.inclusion_time;
  const eligible =
    headerStartTime < inclusionTime && inclusionTime <= headerEndTime;
  if (
    eligible &&
    authenticWithdrawalContentHash === leaf.committedWithdrawalContentHash
  )
    throw new FabricatedWithdrawalRejection(
      "authentic_content_matches_commitment",
      "The eligible event content matches the header; no fabrication is established",
    );
  return {
    verdict: { WithdrawalEventObserved: { commitment } },
    fault: eligible
      ? {
          MismatchedWithdrawalContent: {
            committed_withdrawal_content_hash:
              leaf.committedWithdrawalContentHash,
            authentic_withdrawal_content_hash: authenticWithdrawalContentHash,
            event_inclusion_time: inclusionTime,
          },
        }
      : { IneligibleWithdrawalEvent: { event_inclusion_time: inclusionTime } },
    stateQueuePolicyId,
    openingCbor: Data.to(
      { RetainedEventData: { payload, original_assets: originalAssets } },
      SDK.FabricatedWithdrawalAuthenticContentOpening,
    ),
    authenticWithdrawalContentHash,
    eventInclusionTime: inclusionTime,
  };
};

/** Prover arguments for `fraud_proofs/fabricated_withdrawal/step_01`. */
export type PreparedFabricatedWithdrawalInclusionJson = {
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalInfoCbor: string;
  /** Raw withdrawals MPF root the membership proof opens. */
  readonly withdrawalsPhasRoot: string;
  readonly withdrawalMembershipProofCbor: string;
};

/** The retained L1 opening `fabricated_withdrawal/step_03` re-hashes. */
export type PreparedFabricatedWithdrawalContentJson = {
  readonly openingCbor: string | null;
};

/** Exactly the step-02 state the on-chain step-01 validator will derive. */
export type PreparedFabricatedWithdrawalStateJson = {
  readonly stateQueuePolicyId: string;
  readonly challengedHeaderHash: string;
  readonly headerStartTime: string;
  readonly headerEndTime: string;
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalContentHash: string;
};

export type PreparedFabricatedWithdrawalOutput = {
  readonly schemaVersion: typeof FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION;
  readonly violationId: typeof SDK.FABRICATED_WITHDRAWAL_VIOLATION_ID;
  readonly fraudCategoryId: typeof SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID;
  readonly headerHash: string;
  readonly threadTokenAssetName: string;
  readonly withdrawalCount: number;
  /** Raw MPF root opened by the leaf membership proof. */
  readonly withdrawalsPhasRoot: string;
  /** Counted, domain-separated root the header commits. */
  readonly committedWithdrawalsRoot: string;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly leaves: readonly CommittedWithdrawalLeaf[];
  readonly challengedLeaf: CommittedWithdrawalLeaf;
  readonly classification: ClassifiedFabricatedWithdrawalFault;
  readonly withdrawalInclusion: PreparedFabricatedWithdrawalInclusionJson;
  readonly authenticContent: PreparedFabricatedWithdrawalContentJson;
  readonly step02State: PreparedFabricatedWithdrawalStateJson;
  readonly files?: {
    readonly withdrawalInclusionPath: string;
    readonly authenticContentPath: string;
    readonly planPath: string;
  };
};

export type PrepareFabricatedWithdrawalFromCommittedLeavesOptions = {
  readonly headerHash: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
  readonly witness: FabricatedWithdrawalL1Witness;
  /** Pin a specific committed leaf key; otherwise the sole leaf is used. */
  readonly committedWithdrawalIdCbor?: string;
  readonly minimumConfirmationDepth?: number;
  readonly outputDir?: string;
};

/**
 * Core builder: authenticates the raw committed withdrawal leaves against the
 * header's counted `withdrawals_root` **and** `withdrawal_count`, classifies one
 * leaf against the authenticated L1 witness, then emits the membership proof, the
 * retained content opening, and the exact step-02 state.
 */
export const prepareFabricatedWithdrawalFromCommittedLeaves = async ({
  headerHash,
  committedWithdrawalsRoot,
  withdrawalCount,
  headerStartTime,
  headerEndTime,
  entries,
  witness,
  committedWithdrawalIdCbor,
  minimumConfirmationDepth,
  outputDir,
}: PrepareFabricatedWithdrawalFromCommittedLeavesOptions): Promise<PreparedFabricatedWithdrawalOutput> => {
  const phasEntries = entries.map(([keyHex, valueHex], index) => ({
    key: hexOf(keyHex, `withdrawals[${index.toString()}].key`),
    value: hexOf(valueHex, `withdrawals[${index.toString()}].value`),
  }));
  const phas = await keyValuePhasRootWithCount(phasEntries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.withdrawals,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== committedWithdrawalsRoot.toLowerCase() ||
    phas.count !== withdrawalCount
  ) {
    throw new FabricatedWithdrawalRejection(
      "withdrawals_root_mismatch",
      `header_withdrawals_root=${committedWithdrawalsRoot.toLowerCase()} derived=${countedRoot} header_count=${withdrawalCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = await Promise.all(
    entries.map(async ([keyHex, valueHex], index) =>
      decodeCommittedWithdrawalLeaf(keyHex, valueHex, index),
    ),
  );
  if (leaves.length === 0) {
    throw new FabricatedWithdrawalRejection(
      "no_committed_withdrawal_leaf",
      `header_hash=${headerHash.toLowerCase()} commits an empty withdrawal source set`,
    );
  }
  const challengedLeaf =
    committedWithdrawalIdCbor === undefined
      ? leaves[0]
      : leaves.find(
          (leaf) =>
            leaf.committedWithdrawalIdCbor ===
            committedWithdrawalIdCbor.toLowerCase(),
        );
  if (challengedLeaf === undefined) {
    throw new FabricatedWithdrawalRejection(
      "leaf_not_committed",
      `committed_withdrawal_id=${committedWithdrawalIdCbor?.toLowerCase() ?? ""} is not a committed leaf of header_hash=${headerHash.toLowerCase()} (leaf_count=${leaves.length.toString()})`,
    );
  }

  const classification = await classifyFabricatedWithdrawalFault({
    leaf: challengedLeaf,
    headerStartTime,
    headerEndTime,
    witness,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });

  const trie = await buildTrieView(phasEntries);
  const withdrawalInclusion: PreparedFabricatedWithdrawalInclusionJson = {
    committedWithdrawalIdCbor: challengedLeaf.committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor: challengedLeaf.committedWithdrawalInfoCbor,
    withdrawalsPhasRoot: phas.root,
    withdrawalMembershipProofCbor: requireProof(
      trie,
      Buffer.from(challengedLeaf.committedWithdrawalIdCbor, "hex"),
      "committed withdrawal leaf",
    ),
  };
  const output: PreparedFabricatedWithdrawalOutput = {
    schemaVersion: FABRICATED_WITHDRAWAL_EVIDENCE_SCHEMA_VERSION,
    violationId: SDK.FABRICATED_WITHDRAWAL_VIOLATION_ID,
    fraudCategoryId: SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
    headerHash: headerHash.toLowerCase(),
    threadTokenAssetName: SDK.fabricatedWithdrawalThreadTokenAssetName(
      headerHash.toLowerCase(),
    ),
    withdrawalCount: Number(withdrawalCount),
    withdrawalsPhasRoot: phas.root,
    committedWithdrawalsRoot: countedRoot,
    headerStartTime,
    headerEndTime,
    leaves,
    challengedLeaf,
    classification,
    withdrawalInclusion,
    authenticContent: {
      openingCbor: classification.openingCbor,
    },
    step02State: {
      stateQueuePolicyId: classification.stateQueuePolicyId,
      challengedHeaderHash: headerHash.toLowerCase(),
      headerStartTime: headerStartTime.toString(),
      headerEndTime: headerEndTime.toString(),
      committedWithdrawalIdCbor: challengedLeaf.committedWithdrawalIdCbor,
      committedWithdrawalContentHash:
        challengedLeaf.committedWithdrawalContentHash,
    },
  };
  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    withdrawalInclusionPath: join(outputDir, "withdrawal-inclusion.json"),
    authenticContentPath: join(outputDir, "authentic-content.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(
      paths.withdrawalInclusionPath,
      stringifyJson(output.withdrawalInclusion),
    ),
    writeFile(
      paths.authenticContentPath,
      stringifyJson(output.authenticContent),
    ),
    writeFile(
      paths.planPath,
      stringifyJson({
        schemaVersion: output.schemaVersion,
        violationId: output.violationId,
        fraudCategoryId: output.fraudCategoryId,
        headerHash: output.headerHash,
        threadTokenAssetName: output.threadTokenAssetName,
        withdrawalsPhasRoot: output.withdrawalsPhasRoot,
        committedWithdrawalsRoot: output.committedWithdrawalsRoot,
        withdrawalCount: output.withdrawalCount,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

export type FabricatedWithdrawalBlockEvidence = {
  readonly grade: SDK.EvidenceGrade;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenance;
    readonly da: SDK.EvidenceProvenance;
  };
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
};

/**
 * Extracts the committed `withdrawals` leaves from public retained-DA bytes
 * without requiring the block to be well-formed. Only the payload envelope,
 * canonical `DaPayload` framing and the embedded-header identity are enforced
 * here; leaf authenticity is what the family adjudicates.
 */
export const fabricatedWithdrawalBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<FabricatedWithdrawalBlockEvidence> => {
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
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayload;
  try {
    payload = SDK.decodeDaPayload(payloadCbor);
  } catch (cause) {
    throw new FabricatedWithdrawalRejection(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor)) {
    throw new FabricatedWithdrawalRejection(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_VERSION) {
    throw new FabricatedWithdrawalRejection(
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
    throw new FabricatedWithdrawalRejection(
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
    committedWithdrawalsRoot: admittedObservation.header.withdrawalsRoot,
    withdrawalCount: admittedObservation.header.withdrawalCount,
    headerStartTime: admittedObservation.header.startTime,
    headerEndTime: admittedObservation.header.endTime,
    entries: body.withdrawals.map(
      ([keyHex, valueHex]) => [keyHex, valueHex] as const,
    ),
  };
};

/**
 * The security-grade entry point: authenticated L1 header observation + public
 * retained-DA payload + an authenticated L1 withdrawal-identity witness -> a
 * submittable `fabricated-withdrawal` proof plan.
 */
export const prepareFabricatedWithdrawalFromRetainedDa = async ({
  observation,
  sources,
  witness,
  retries,
  minimumConfirmationDepth,
  committedWithdrawalIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly witness: FabricatedWithdrawalL1Witness;
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedWithdrawalIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedFabricatedWithdrawalOutput> => {
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
  const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidence(
      SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return await prepareFabricatedWithdrawalFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
    withdrawalCount: evidence.withdrawalCount,
    headerStartTime: evidence.headerStartTime,
    headerEndTime: evidence.headerEndTime,
    entries: evidence.entries,
    witness,
    ...(committedWithdrawalIdCbor === undefined
      ? {}
      : { committedWithdrawalIdCbor }),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
