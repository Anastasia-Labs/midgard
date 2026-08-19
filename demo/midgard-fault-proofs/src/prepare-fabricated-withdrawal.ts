/**
 * `fabricated-withdrawal` DA-first evidence builder (Goal task `Q40`, §9.1
 * output 7).
 *
 * The fault this family proves is a committed `withdrawals_root` leaf that is not
 * the authentic L1 withdrawal event pair: either no withdrawal event with the
 * committed `WithdrawalId` was ever authenticated, or the authentic event exists
 * and was due for the block but its `WithdrawalInfo` — its body, its signature or
 * its validity verdict — is not the committed one.
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
 * `src/evidence/prepare-from-evidence-v1.ts:145-150`.
 *
 * The L1 side of the argument is authenticated, never asserted: absence of a
 * withdrawal identity is established by exhibiting the committed `WithdrawalId` in
 * an authenticated **live** output-reference set (an unspent outref cannot have
 * been consumed by `authenticate_new_event`), and presence is established by the
 * withdrawal event NFT asset name derived from the committed identity. No operator
 * REST/DB/file input is reachable from this module, and there is no consumed-live-
 * UTxO fallback: both arms fail closed.
 *
 * Leaf bytes are compared in `serialiseData` form throughout — see the note on
 * definite versus indefinite Plutus maps in
 * `midgard-sdk/src/fraud-proof/fabricated-withdrawal-v1.ts`. A withdrawal leaf
 * value embeds a `Value` map, so this matters here in a way it did not for
 * deposits.
 */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
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

export const FABRICATED_WITHDRAWAL_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-fabricated-withdrawal-evidence-v1" as const;

export type FabricatedWithdrawalRejectionCodeV1 =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "withdrawals_root_mismatch"
  | "no_committed_withdrawal_leaf"
  | "leaf_not_committed"
  | "consumed_live_utxo_fallback_refused"
  | "withdrawal_identity_observation_mismatch"
  | "event_datum_not_canonical"
  | "event_identity_mismatch"
  | "authentic_content_matches_commitment"
  | "event_not_due_for_block";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class FabricatedWithdrawalRejectionV1 extends Error {
  readonly code: FabricatedWithdrawalRejectionCodeV1;

  constructor(code: FabricatedWithdrawalRejectionCodeV1, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "FabricatedWithdrawalRejectionV1";
    this.code = code;
  }
}

const hexOf = (value: string, label: string): Buffer => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})*$/u.test(normalized)) {
    throw new FabricatedWithdrawalRejectionV1(
      "malformed_da_payload",
      `${label} is not even-length hexadecimal`,
    );
  }
  return Buffer.from(normalized, "hex");
};

/** One committed `withdrawals_root` leaf, decoded and committed to. */
export type CommittedWithdrawalLeafV1 = {
  readonly index: number;
  /** Canonical CBOR of the leaf key — a `WithdrawalId` output reference. */
  readonly committedWithdrawalIdCbor: string;
  /** Canonical CBOR of the leaf value — the committed `WithdrawalInfo`. */
  readonly committedWithdrawalInfoCbor: string;
  readonly committedWithdrawalId: SDK.OutputReference;
  readonly committedWithdrawalInfo: SDK.WithdrawalInfo;
  /** Blake2b-256 of the committed `WithdrawalInfo`'s canonical bytes. */
  readonly committedWithdrawalInfoHash: string;
  readonly committedLeafByteCount: number;
};

const decodeCommittedWithdrawalLeafV1 = async (
  keyHex: string,
  valueHex: string,
  index: number,
): Promise<CommittedWithdrawalLeafV1> => {
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
    throw new FabricatedWithdrawalRejectionV1(
      "malformed_da_payload",
      `${label} does not decode as (WithdrawalId, WithdrawalInfo): ${String(cause)}`,
    );
  }
  if (
    SDK.committedWithdrawalKeyBytesV1(committedWithdrawalId) !==
      committedWithdrawalIdCbor ||
    SDK.committedWithdrawalValueBytesV1(committedWithdrawalInfo) !==
      committedWithdrawalInfoCbor
  ) {
    throw new FabricatedWithdrawalRejectionV1(
      "non_canonical_da_payload",
      `${label} leaf bytes are not canonical for (WithdrawalId, WithdrawalInfo)`,
    );
  }
  const committedWithdrawalInfoHash = await Effect.runPromise(
    SDK.withdrawalInfoCommitmentV1(committedWithdrawalInfo),
  );
  return {
    index,
    committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor,
    committedWithdrawalId,
    committedWithdrawalInfo,
    committedWithdrawalInfoHash,
    committedLeafByteCount: value.length,
  };
};

/** A live (unspent) L1 output reference at an authenticated chain point. */
export type LiveOutputReferenceV1 = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

/**
 * The prover's authenticated L1 witness about a committed withdrawal identity.
 *
 * `absent_identity` carries the live output-reference set observed at the
 * authenticated chain point. Because `authenticate_new_event` rule 4 requires a
 * withdrawal event's id to be an outref the authenticating transaction **spent**,
 * a still-live outref positively proves no event with that identity was ever
 * authenticated. Membership in that set is checked here; a prover cannot simply
 * declare absence, and a consumed outref is refused rather than treated as absent.
 *
 * `present_event` carries the retained withdrawal event datum plus the withdrawal
 * policy and the asset name observed carrying the event, so the observation is
 * bound to the committed identity by `out_ref_to_nonce` rather than by trust.
 */
export type FabricatedWithdrawalL1WitnessV1 =
  | {
      readonly kind: "absent_identity";
      readonly observation: SDK.AuthenticatedL1ObservationV1;
      readonly liveOutputReferences: readonly LiveOutputReferenceV1[];
    }
  | {
      readonly kind: "present_event";
      readonly observation: SDK.AuthenticatedL1ObservationV1;
      /**
       * Withdrawal event NFT policy id, read from the authentic hub oracle's
       * `withdrawal` field — not its `deposit` field, which registers a different
       * event family.
       */
      readonly withdrawalEventPolicyId: string;
      /** Asset name observed carrying the withdrawal event. */
      readonly observedEventAssetName: string;
      /** Canonical CBOR of the retained withdrawal event datum. */
      readonly eventDatumCbor: string;
    };

/** The classified fault, plus the two authenticated intermediates it rests on. */
export type ClassifiedFabricatedWithdrawalFaultV1 = {
  readonly verdict: SDK.FabricatedWithdrawalEvidenceVerdictV1;
  readonly fault: SDK.FabricatedWithdrawalFaultV1;
  /** Present only for the content-mismatch shape. */
  readonly authenticWithdrawalInfoHash?: string;
  /** Present only for the content-mismatch shape. */
  readonly eventInclusionTime?: bigint;
  /** Present only for the content-mismatch shape. */
  readonly eventDatumHash?: string;
};

const admitWitnessObservationV1 = ({
  witness,
  minimumConfirmationDepth,
}: {
  readonly witness: FabricatedWithdrawalL1WitnessV1;
  readonly minimumConfirmationDepth?: number;
}): SDK.EvidenceProvenanceV1 => {
  const admitted = SDK.admitAuthenticatedL1ObservationV1({
    observation: witness.observation,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return SDK.assertSecurityGradeEvidenceV1(admitted.provenance);
};

/**
 * Classifies one committed withdrawal leaf against an authenticated L1 witness.
 *
 * Both arms are decided by a check, never by the prover's claim: absence by
 * live-set membership of the committed identity, presence by the event NFT asset
 * name the committed identity derives, and mismatch by comparing two commitments
 * over canonical bytes inside the block's own event window. The content comparison
 * is a single 32-byte inequality over the whole `WithdrawalInfo`, so a diverted
 * body, a forged signature and an overridden validity are all caught by it.
 */
export const classifyFabricatedWithdrawalFaultV1 = async ({
  leaf,
  headerStartTime,
  headerEndTime,
  witness,
  minimumConfirmationDepth,
}: {
  readonly leaf: CommittedWithdrawalLeafV1;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly witness: FabricatedWithdrawalL1WitnessV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<ClassifiedFabricatedWithdrawalFaultV1> => {
  admitWitnessObservationV1({
    witness,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  if (witness.kind === "absent_identity") {
    const live = witness.liveOutputReferences.some(
      (candidate) =>
        candidate.transactionId.toLowerCase() ===
          leaf.committedWithdrawalId.transactionId.toLowerCase() &&
        candidate.outputIndex === leaf.committedWithdrawalId.outputIndex,
    );
    if (!live) {
      throw new FabricatedWithdrawalRejectionV1(
        "consumed_live_utxo_fallback_refused",
        `committed_withdrawal_id=${leaf.committedWithdrawalIdCbor} is not in the authenticated live output-reference set, so its absence cannot be established from a consumed UTxO`,
      );
    }
    return {
      verdict: "WithdrawalIdentityAbsent",
      fault: "NonexistentWithdrawalIdentity",
    };
  }

  const expectedAssetName = await Effect.runPromise(
    SDK.withdrawalEventNonceV1(leaf.committedWithdrawalId),
  );
  if (witness.observedEventAssetName.toLowerCase() !== expectedAssetName) {
    throw new FabricatedWithdrawalRejectionV1(
      "withdrawal_identity_observation_mismatch",
      `observed_asset_name=${witness.observedEventAssetName.toLowerCase()} expected=${expectedAssetName} policy=${witness.withdrawalEventPolicyId.toLowerCase()}`,
    );
  }
  const eventDatumCbor = hexOf(
    witness.eventDatumCbor,
    "witness.eventDatumCbor",
  );
  let eventDatum: SDK.WithdrawalOrderDatum;
  try {
    eventDatum = Data.from(
      eventDatumCbor.toString("hex"),
      SDK.WithdrawalOrderDatum,
    );
  } catch (cause) {
    throw new FabricatedWithdrawalRejectionV1(
      "event_datum_not_canonical",
      `witness.eventDatumCbor does not decode as a withdrawal event datum: ${String(cause)}`,
    );
  }
  if (
    SDK.withdrawalEventDatumBytesV1(eventDatum) !==
    eventDatumCbor.toString("hex")
  ) {
    throw new FabricatedWithdrawalRejectionV1(
      "event_datum_not_canonical",
      "witness.eventDatumCbor is not canonical for a withdrawal event datum",
    );
  }
  if (
    SDK.committedWithdrawalKeyBytesV1(eventDatum.event.id) !==
    leaf.committedWithdrawalIdCbor
  ) {
    throw new FabricatedWithdrawalRejectionV1(
      "event_identity_mismatch",
      `event_id=${SDK.committedWithdrawalKeyBytesV1(eventDatum.event.id)} committed_withdrawal_id=${leaf.committedWithdrawalIdCbor}`,
    );
  }
  const [authenticWithdrawalInfoHash, eventDatumHash] = await Promise.all([
    Effect.runPromise(SDK.withdrawalInfoCommitmentV1(eventDatum.event.info)),
    Effect.runPromise(SDK.withdrawalEventDatumCommitmentV1(eventDatum)),
  ]);
  if (authenticWithdrawalInfoHash === leaf.committedWithdrawalInfoHash) {
    throw new FabricatedWithdrawalRejectionV1(
      "authentic_content_matches_commitment",
      `committed_withdrawal_info_hash=${leaf.committedWithdrawalInfoHash} equals the authentic event's content; a valid block cannot be challenged`,
    );
  }
  const inclusionTime = eventDatum.inclusion_time;
  if (!(headerStartTime < inclusionTime && inclusionTime <= headerEndTime)) {
    throw new FabricatedWithdrawalRejectionV1(
      "event_not_due_for_block",
      `inclusion_time=${inclusionTime.toString()} is outside the challenged block's window (${headerStartTime.toString()}, ${headerEndTime.toString()}]`,
    );
  }
  return {
    verdict: {
      WithdrawalEventObserved: {
        event_datum_hash: eventDatumHash,
        event_inclusion_time: inclusionTime,
      },
    },
    fault: {
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: leaf.committedWithdrawalInfoHash,
        authentic_withdrawal_info_hash: authenticWithdrawalInfoHash,
        event_inclusion_time: inclusionTime,
      },
    },
    authenticWithdrawalInfoHash,
    eventInclusionTime: inclusionTime,
    eventDatumHash,
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
  readonly eventDatumCbor: string | null;
};

/** Exactly the step-02 state the on-chain step-01 validator will derive. */
export type PreparedFabricatedWithdrawalStateJson = {
  readonly challengedHeaderHash: string;
  readonly headerStartTime: string;
  readonly headerEndTime: string;
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalInfoHash: string;
};

export type PreparedFabricatedWithdrawalOutput = {
  readonly schemaVersion: typeof FABRICATED_WITHDRAWAL_EVIDENCE_V1_SCHEMA_VERSION;
  readonly violationId: typeof SDK.FABRICATED_WITHDRAWAL_VIOLATION_ID_V1;
  readonly fraudCategoryId: typeof SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1;
  readonly headerHash: string;
  readonly threadTokenAssetName: string;
  readonly withdrawalCount: number;
  /** Raw MPF root opened by the leaf membership proof. */
  readonly withdrawalsPhasRoot: string;
  /** Counted, domain-separated root the header commits. */
  readonly committedWithdrawalsRoot: string;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly leaves: readonly CommittedWithdrawalLeafV1[];
  readonly challengedLeaf: CommittedWithdrawalLeafV1;
  readonly classification: ClassifiedFabricatedWithdrawalFaultV1;
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
  readonly witness: FabricatedWithdrawalL1WitnessV1;
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
export const prepareFabricatedWithdrawalFromCommittedLeavesV1 = async ({
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
    throw new FabricatedWithdrawalRejectionV1(
      "withdrawals_root_mismatch",
      `header_withdrawals_root=${committedWithdrawalsRoot.toLowerCase()} derived=${countedRoot} header_count=${withdrawalCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = await Promise.all(
    entries.map(async ([keyHex, valueHex], index) =>
      decodeCommittedWithdrawalLeafV1(keyHex, valueHex, index),
    ),
  );
  if (leaves.length === 0) {
    throw new FabricatedWithdrawalRejectionV1(
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
    throw new FabricatedWithdrawalRejectionV1(
      "leaf_not_committed",
      `committed_withdrawal_id=${committedWithdrawalIdCbor?.toLowerCase() ?? ""} is not a committed leaf of header_hash=${headerHash.toLowerCase()} (leaf_count=${leaves.length.toString()})`,
    );
  }

  const classification = await classifyFabricatedWithdrawalFaultV1({
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
    schemaVersion: FABRICATED_WITHDRAWAL_EVIDENCE_V1_SCHEMA_VERSION,
    violationId: SDK.FABRICATED_WITHDRAWAL_VIOLATION_ID_V1,
    fraudCategoryId: SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
    headerHash: headerHash.toLowerCase(),
    threadTokenAssetName: SDK.fabricatedWithdrawalThreadTokenAssetNameV1(
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
      eventDatumCbor:
        witness.kind === "present_event"
          ? witness.eventDatumCbor.toLowerCase()
          : null,
    },
    step02State: {
      challengedHeaderHash: headerHash.toLowerCase(),
      headerStartTime: headerStartTime.toString(),
      headerEndTime: headerEndTime.toString(),
      committedWithdrawalIdCbor: challengedLeaf.committedWithdrawalIdCbor,
      committedWithdrawalInfoHash: challengedLeaf.committedWithdrawalInfoHash,
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

export type FabricatedWithdrawalBlockEvidenceV1 = {
  readonly grade: SDK.EvidenceGradeV1;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenanceV1;
    readonly da: SDK.EvidenceProvenanceV1;
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
 * canonical `DaPayloadV1` framing and the embedded-header identity are enforced
 * here; leaf authenticity is what the family adjudicates.
 */
export const fabricatedWithdrawalBlockEvidenceFromVerifiedPayloadV1 = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<FabricatedWithdrawalBlockEvidenceV1> => {
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservationV1({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const admittedDa = SDK.assertSecurityGradeEvidenceV1(daProvenance);
  if (admittedDa.trustClass !== "public_or_permissionless_da") {
    throw new SDK.CanonicalEvidenceRejectionV1(
      "da_evidence_wrong_trust_class",
      `expected=public_or_permissionless_da actual=${admittedDa.trustClass}`,
    );
  }

  let payloadCbor: Buffer;
  try {
    payloadCbor = Buffer.from(
      (
        await unwrapDaPayloadV1(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        })
      ).innerBytes,
    );
  } catch (cause) {
    throw new FabricatedWithdrawalRejectionV1(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayloadV1;
  try {
    payload = SDK.decodeDaPayloadV1(payloadCbor);
  } catch (cause) {
    throw new FabricatedWithdrawalRejectionV1(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor)) {
    throw new FabricatedWithdrawalRejectionV1(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_V1_VERSION) {
    throw new FabricatedWithdrawalRejectionV1(
      "wrong_da_payload_version",
      `expected=${SDK.DA_PAYLOAD_V1_VERSION.toString()} actual=${payload.version.toString()}`,
    );
  }
  const body = payload.block_body;
  const embeddedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeaderV1(body.header),
  );
  if (
    embeddedHeaderHash !== body.header_hash.toLowerCase() ||
    embeddedHeaderHash !== admittedObservation.headerHash
  ) {
    throw new FabricatedWithdrawalRejectionV1(
      "header_hash_mismatch",
      `embedded=${embeddedHeaderHash} payload=${body.header_hash.toLowerCase()} observed=${admittedObservation.headerHash}`,
    );
  }

  return {
    grade: SDK.combineEvidenceGradeV1([
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
export const prepareFabricatedWithdrawalFromRetainedDaV1 = async ({
  observation,
  sources,
  witness,
  retries,
  minimumConfirmationDepth,
  committedWithdrawalIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly witness: FabricatedWithdrawalL1WitnessV1;
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedWithdrawalIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedFabricatedWithdrawalOutput> => {
  if (sources.length === 0) {
    throw new SDK.CanonicalEvidenceRejectionV1(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservationV1({
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
  const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayloadV1(
    {
      observation: admittedObservation,
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      daProvenance: SDK.assertSecurityGradeEvidenceV1(
        SDK.admitEvidenceProvenanceV1({ provenance: fetched.provenance }),
      ),
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    },
  );
  return await prepareFabricatedWithdrawalFromCommittedLeavesV1({
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
