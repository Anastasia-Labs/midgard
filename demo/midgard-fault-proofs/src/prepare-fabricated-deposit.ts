/**
 * `fabricated-deposit` DA-first evidence builder (Goal task `Q39`, §9.1 output 7).
 *
 * The fault this family proves is a committed `deposits_root` leaf that is not
 * the authentic L1 deposit event pair: either no deposit event with the
 * committed `DepositId` was ever authenticated, or the authentic event exists
 * and was due for the block but its `DepositInfo` is not the committed one.
 *
 * Such a block cannot be reconstructed by `reconstructDaPayloadV1` — a whole
 * block whose deposit source set disagrees with L1 fails reconstruction long
 * before the leaf in question is reached — so, like `prepare-da-hash-preimage`,
 * this builder decodes the retained-DA envelope itself and performs exactly the
 * authentication the proof needs, against two security-graded inputs:
 *
 * 1. an authenticated L1 observation of the committed state-queue header
 *    (`authenticated_cardano_l1`), and
 * 2. the exact `DaPayloadEnvelopeV1` bytes retrieved over the public retained-DA
 *    protocol (`public_or_permissionless_da`),
 *
 * cross-checked by rebuilding the **raw** `(DepositId, DepositInfo)` MPF from the
 * payload's `deposits` entries, committing it under the counted
 * `DepositsRootDomain`, and requiring **both** that the counted root equals the
 * L1-committed `deposits_root` **and** that the rebuilt cardinality equals the
 * header's `deposit_count`. After that check every committed deposit leaf is
 * exactly as trustworthy as the header itself.
 *
 * `assertNativeInclusionRootAuthenticatedV1` is deliberately **not** used: it
 * authenticates the native-compact *transaction* leaf convention against
 * `transactions_root` and has no bearing on `deposits_root`, which has a single
 * leaf convention. Requiring it would refuse a legitimate fabricated-deposit
 * proof whenever the block's unrelated transaction leaves use the payload-source
 * convention. The same reasoning is recorded at
 * `src/evidence/prepare-from-evidence-v1.ts:145-150`.
 *
 * The L1 side of the argument is authenticated, never asserted: absence of a
 * deposit identity is established by exhibiting the committed `DepositId` in an
 * authenticated **live** output-reference set (an unspent outref cannot have been
 * consumed by `authenticate_new_event`), and presence is established by the
 * deposit event NFT asset name derived from the committed identity. No operator
 * REST/DB/file input is reachable from this module, and there is no
 * consumed-live-UTxO fallback: both arms fail closed.
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

export const FABRICATED_DEPOSIT_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-fabricated-deposit-evidence-v1" as const;

export type FabricatedDepositRejectionCodeV1 =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "deposits_root_mismatch"
  | "no_committed_deposit_leaf"
  | "leaf_not_committed"
  | "consumed_live_utxo_fallback_refused"
  | "deposit_identity_observation_mismatch"
  | "event_datum_not_canonical"
  | "event_identity_mismatch"
  | "authentic_content_matches_commitment"
  | "event_not_due_for_block";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class FabricatedDepositRejectionV1 extends Error {
  readonly code: FabricatedDepositRejectionCodeV1;

  constructor(code: FabricatedDepositRejectionCodeV1, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "FabricatedDepositRejectionV1";
    this.code = code;
  }
}

const hexOf = (value: string, label: string): Buffer => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})*$/u.test(normalized)) {
    throw new FabricatedDepositRejectionV1(
      "malformed_da_payload",
      `${label} is not even-length hexadecimal`,
    );
  }
  return Buffer.from(normalized, "hex");
};

/** One committed `deposits_root` leaf, decoded and committed to. */
export type CommittedDepositLeafV1 = {
  readonly index: number;
  /** Canonical CBOR of the leaf key — a `DepositId` output reference. */
  readonly committedDepositIdCbor: string;
  /** Canonical CBOR of the leaf value — the committed `DepositInfo`. */
  readonly committedDepositInfoCbor: string;
  readonly committedDepositId: SDK.OutputReference;
  readonly committedDepositInfo: SDK.DepositInfo;
  /** Blake2b-256 of the committed `DepositInfo`'s canonical bytes. */
  readonly committedDepositInfoHash: string;
  readonly committedLeafByteCount: number;
};

const decodeCommittedDepositLeafV1 = async (
  keyHex: string,
  valueHex: string,
  index: number,
): Promise<CommittedDepositLeafV1> => {
  const label = `deposits[${index.toString()}]`;
  const key = hexOf(keyHex, `${label}.key`);
  const value = hexOf(valueHex, `${label}.value`);
  const committedDepositIdCbor = key.toString("hex");
  const committedDepositInfoCbor = value.toString("hex");
  let committedDepositId: SDK.OutputReference;
  let committedDepositInfo: SDK.DepositInfo;
  try {
    committedDepositId = Data.from(committedDepositIdCbor, SDK.OutputReference);
    committedDepositInfo = Data.from(committedDepositInfoCbor, SDK.DepositInfo);
  } catch (cause) {
    throw new FabricatedDepositRejectionV1(
      "malformed_da_payload",
      `${label} does not decode as (DepositId, DepositInfo): ${String(cause)}`,
    );
  }
  if (
    SDK.committedDepositKeyBytesV1(committedDepositId) !==
      committedDepositIdCbor ||
    SDK.committedDepositValueBytesV1(committedDepositInfo) !==
      committedDepositInfoCbor
  ) {
    throw new FabricatedDepositRejectionV1(
      "non_canonical_da_payload",
      `${label} leaf bytes are not canonical for (DepositId, DepositInfo)`,
    );
  }
  const committedDepositInfoHash = await Effect.runPromise(
    SDK.depositInfoCommitmentV1(committedDepositInfo),
  );
  return {
    index,
    committedDepositIdCbor,
    committedDepositInfoCbor,
    committedDepositId,
    committedDepositInfo,
    committedDepositInfoHash,
    committedLeafByteCount: value.length,
  };
};

/** A live (unspent) L1 output reference at an authenticated chain point. */
export type LiveOutputReferenceV1 = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

/**
 * The prover's authenticated L1 witness about a committed deposit identity.
 *
 * `absent_identity` carries the live output-reference set observed at the
 * authenticated chain point. Because `authenticate_new_event` rule 4 requires a
 * deposit event's id to be an outref the authenticating transaction **spent**,
 * a still-live outref positively proves no event with that identity was ever
 * authenticated. Membership in that set is checked here; a prover cannot simply
 * declare absence, and a consumed outref is refused rather than treated as
 * absent.
 *
 * `present_event` carries the retained deposit event datum plus the deposit
 * policy and the asset name observed carrying the event, so the observation is
 * bound to the committed identity by `out_ref_to_nonce` rather than by trust.
 */
export type FabricatedDepositL1WitnessV1 =
  | {
      readonly kind: "absent_identity";
      readonly observation: SDK.AuthenticatedL1ObservationV1;
      readonly liveOutputReferences: readonly LiveOutputReferenceV1[];
    }
  | {
      readonly kind: "present_event";
      readonly observation: SDK.AuthenticatedL1ObservationV1;
      /** Deposit event NFT policy id, read from the authentic hub oracle. */
      readonly depositEventPolicyId: string;
      /** Asset name observed carrying the deposit event. */
      readonly observedEventAssetName: string;
      /** Canonical CBOR of the retained `DepositDatum`. */
      readonly eventDatumCbor: string;
    };

/** The classified fault, plus the two authenticated intermediates it rests on. */
export type ClassifiedFabricatedDepositFaultV1 = {
  readonly verdict: SDK.FabricatedDepositEvidenceVerdictV1;
  readonly fault: SDK.FabricatedDepositFaultV1;
  /** Present only for the content-mismatch shape. */
  readonly authenticDepositInfoHash?: string;
  /** Present only for the content-mismatch shape. */
  readonly eventInclusionTime?: bigint;
  /** Present only for the content-mismatch shape. */
  readonly eventDatumHash?: string;
};

const admitWitnessObservationV1 = ({
  witness,
  minimumConfirmationDepth,
}: {
  readonly witness: FabricatedDepositL1WitnessV1;
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
 * Classifies one committed deposit leaf against an authenticated L1 witness.
 *
 * Both arms are decided by a check, never by the prover's claim: absence by
 * live-set membership of the committed identity, presence by the event NFT
 * asset name the committed identity derives, and mismatch by comparing two
 * commitments over canonical bytes inside the block's own event window.
 */
export const classifyFabricatedDepositFaultV1 = async ({
  leaf,
  headerStartTime,
  headerEndTime,
  witness,
  minimumConfirmationDepth,
}: {
  readonly leaf: CommittedDepositLeafV1;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly witness: FabricatedDepositL1WitnessV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<ClassifiedFabricatedDepositFaultV1> => {
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
          leaf.committedDepositId.transactionId.toLowerCase() &&
        candidate.outputIndex === leaf.committedDepositId.outputIndex,
    );
    if (!live) {
      throw new FabricatedDepositRejectionV1(
        "consumed_live_utxo_fallback_refused",
        `committed_deposit_id=${leaf.committedDepositIdCbor} is not in the authenticated live output-reference set, so its absence cannot be established from a consumed UTxO`,
      );
    }
    return {
      verdict: "DepositIdentityAbsent",
      fault: "NonexistentDepositIdentity",
    };
  }

  const expectedAssetName = await Effect.runPromise(
    SDK.depositEventNonceV1(leaf.committedDepositId),
  );
  if (witness.observedEventAssetName.toLowerCase() !== expectedAssetName) {
    throw new FabricatedDepositRejectionV1(
      "deposit_identity_observation_mismatch",
      `observed_asset_name=${witness.observedEventAssetName.toLowerCase()} expected=${expectedAssetName} policy=${witness.depositEventPolicyId.toLowerCase()}`,
    );
  }
  const eventDatumCbor = hexOf(
    witness.eventDatumCbor,
    "witness.eventDatumCbor",
  );
  let eventDatum: SDK.DepositDatum;
  try {
    eventDatum = Data.from(eventDatumCbor.toString("hex"), SDK.DepositDatum);
  } catch (cause) {
    throw new FabricatedDepositRejectionV1(
      "event_datum_not_canonical",
      `witness.eventDatumCbor does not decode as DepositDatum: ${String(cause)}`,
    );
  }
  if (
    Data.to(eventDatum, SDK.DepositDatum) !== eventDatumCbor.toString("hex")
  ) {
    throw new FabricatedDepositRejectionV1(
      "event_datum_not_canonical",
      "witness.eventDatumCbor is not canonical for DepositDatum",
    );
  }
  if (
    SDK.committedDepositKeyBytesV1(eventDatum.event.id) !==
    leaf.committedDepositIdCbor
  ) {
    throw new FabricatedDepositRejectionV1(
      "event_identity_mismatch",
      `event_id=${SDK.committedDepositKeyBytesV1(eventDatum.event.id)} committed_deposit_id=${leaf.committedDepositIdCbor}`,
    );
  }
  const [authenticDepositInfoHash, eventDatumHash] = await Promise.all([
    Effect.runPromise(SDK.depositInfoCommitmentV1(eventDatum.event.info)),
    Effect.runPromise(SDK.depositEventDatumCommitmentV1(eventDatum)),
  ]);
  if (authenticDepositInfoHash === leaf.committedDepositInfoHash) {
    throw new FabricatedDepositRejectionV1(
      "authentic_content_matches_commitment",
      `committed_deposit_info_hash=${leaf.committedDepositInfoHash} equals the authentic event's content; a valid block cannot be challenged`,
    );
  }
  const inclusionTime = eventDatum.inclusion_time;
  if (!(headerStartTime < inclusionTime && inclusionTime <= headerEndTime)) {
    throw new FabricatedDepositRejectionV1(
      "event_not_due_for_block",
      `inclusion_time=${inclusionTime.toString()} is outside the challenged block's window (${headerStartTime.toString()}, ${headerEndTime.toString()}]`,
    );
  }
  return {
    verdict: {
      DepositEventObserved: {
        event_datum_hash: eventDatumHash,
        event_inclusion_time: inclusionTime,
      },
    },
    fault: {
      MismatchedDepositContent: {
        committed_deposit_info_hash: leaf.committedDepositInfoHash,
        authentic_deposit_info_hash: authenticDepositInfoHash,
        event_inclusion_time: inclusionTime,
      },
    },
    authenticDepositInfoHash,
    eventInclusionTime: inclusionTime,
    eventDatumHash,
  };
};

/** Prover arguments for `fraud_proofs/fabricated_deposit/step_01`. */
export type PreparedFabricatedDepositInclusionJson = {
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoCbor: string;
  /** Raw deposits MPF root the membership proof opens. */
  readonly depositsPhasRoot: string;
  readonly depositMembershipProofCbor: string;
};

/** The retained L1 opening `fabricated_deposit/step_03` re-hashes. */
export type PreparedFabricatedDepositContentJson = {
  readonly eventDatumCbor: string | null;
};

/** Exactly the step-02 state the on-chain step-01 validator will derive. */
export type PreparedFabricatedDepositStateJson = {
  readonly challengedHeaderHash: string;
  readonly headerStartTime: string;
  readonly headerEndTime: string;
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoHash: string;
};

export type PreparedFabricatedDepositOutput = {
  readonly schemaVersion: typeof FABRICATED_DEPOSIT_EVIDENCE_V1_SCHEMA_VERSION;
  readonly violationId: typeof SDK.FABRICATED_DEPOSIT_VIOLATION_ID_V1;
  readonly fraudCategoryId: typeof SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1;
  readonly headerHash: string;
  readonly threadTokenAssetName: string;
  readonly depositCount: number;
  /** Raw MPF root opened by the leaf membership proof. */
  readonly depositsPhasRoot: string;
  /** Counted, domain-separated root the header commits. */
  readonly committedDepositsRoot: string;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly leaves: readonly CommittedDepositLeafV1[];
  readonly challengedLeaf: CommittedDepositLeafV1;
  readonly classification: ClassifiedFabricatedDepositFaultV1;
  readonly depositInclusion: PreparedFabricatedDepositInclusionJson;
  readonly authenticContent: PreparedFabricatedDepositContentJson;
  readonly step02State: PreparedFabricatedDepositStateJson;
  readonly files?: {
    readonly depositInclusionPath: string;
    readonly authenticContentPath: string;
    readonly planPath: string;
  };
};

export type PrepareFabricatedDepositFromCommittedLeavesOptions = {
  readonly headerHash: string;
  readonly committedDepositsRoot: string;
  readonly depositCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
  readonly witness: FabricatedDepositL1WitnessV1;
  /** Pin a specific committed leaf key; otherwise the sole leaf is used. */
  readonly committedDepositIdCbor?: string;
  readonly minimumConfirmationDepth?: number;
  readonly outputDir?: string;
};

/**
 * Core builder: authenticates the raw committed deposit leaves against the
 * header's counted `deposits_root` **and** `deposit_count`, classifies one leaf
 * against the authenticated L1 witness, then emits the membership proof, the
 * retained content opening, and the exact step-02 state.
 */
export const prepareFabricatedDepositFromCommittedLeavesV1 = async ({
  headerHash,
  committedDepositsRoot,
  depositCount,
  headerStartTime,
  headerEndTime,
  entries,
  witness,
  committedDepositIdCbor,
  minimumConfirmationDepth,
  outputDir,
}: PrepareFabricatedDepositFromCommittedLeavesOptions): Promise<PreparedFabricatedDepositOutput> => {
  const phasEntries = entries.map(([keyHex, valueHex], index) => ({
    key: hexOf(keyHex, `deposits[${index.toString()}].key`),
    value: hexOf(valueHex, `deposits[${index.toString()}].value`),
  }));
  const phas = await keyValuePhasRootWithCount(phasEntries);
  const countedRoot = await commitCountedRoot({
    domain: SDK.ROOT_DOMAINS.deposits,
    phasRoot: phas.root,
    count: phas.count,
  });
  if (
    countedRoot !== committedDepositsRoot.toLowerCase() ||
    phas.count !== depositCount
  ) {
    throw new FabricatedDepositRejectionV1(
      "deposits_root_mismatch",
      `header_deposits_root=${committedDepositsRoot.toLowerCase()} derived=${countedRoot} header_count=${depositCount.toString()} derived_count=${phas.count.toString()}`,
    );
  }

  const leaves = await Promise.all(
    entries.map(async ([keyHex, valueHex], index) =>
      decodeCommittedDepositLeafV1(keyHex, valueHex, index),
    ),
  );
  if (leaves.length === 0) {
    throw new FabricatedDepositRejectionV1(
      "no_committed_deposit_leaf",
      `header_hash=${headerHash.toLowerCase()} commits an empty deposit source set`,
    );
  }
  const challengedLeaf =
    committedDepositIdCbor === undefined
      ? leaves[0]
      : leaves.find(
          (leaf) =>
            leaf.committedDepositIdCbor ===
            committedDepositIdCbor.toLowerCase(),
        );
  if (challengedLeaf === undefined) {
    throw new FabricatedDepositRejectionV1(
      "leaf_not_committed",
      `committed_deposit_id=${committedDepositIdCbor?.toLowerCase() ?? ""} is not a committed leaf of header_hash=${headerHash.toLowerCase()} (leaf_count=${leaves.length.toString()})`,
    );
  }

  const classification = await classifyFabricatedDepositFaultV1({
    leaf: challengedLeaf,
    headerStartTime,
    headerEndTime,
    witness,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });

  const trie = await buildTrieView(phasEntries);
  const depositInclusion: PreparedFabricatedDepositInclusionJson = {
    committedDepositIdCbor: challengedLeaf.committedDepositIdCbor,
    committedDepositInfoCbor: challengedLeaf.committedDepositInfoCbor,
    depositsPhasRoot: phas.root,
    depositMembershipProofCbor: requireProof(
      trie,
      Buffer.from(challengedLeaf.committedDepositIdCbor, "hex"),
      "committed deposit leaf",
    ),
  };
  const output: PreparedFabricatedDepositOutput = {
    schemaVersion: FABRICATED_DEPOSIT_EVIDENCE_V1_SCHEMA_VERSION,
    violationId: SDK.FABRICATED_DEPOSIT_VIOLATION_ID_V1,
    fraudCategoryId: SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
    headerHash: headerHash.toLowerCase(),
    threadTokenAssetName: SDK.fabricatedDepositThreadTokenAssetNameV1(
      headerHash.toLowerCase(),
    ),
    depositCount: Number(depositCount),
    depositsPhasRoot: phas.root,
    committedDepositsRoot: countedRoot,
    headerStartTime,
    headerEndTime,
    leaves,
    challengedLeaf,
    classification,
    depositInclusion,
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
      committedDepositIdCbor: challengedLeaf.committedDepositIdCbor,
      committedDepositInfoHash: challengedLeaf.committedDepositInfoHash,
    },
  };
  if (outputDir === undefined) {
    return output;
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    depositInclusionPath: join(outputDir, "deposit-inclusion.json"),
    authenticContentPath: join(outputDir, "authentic-content.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(
      paths.depositInclusionPath,
      stringifyJson(output.depositInclusion),
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
        depositsPhasRoot: output.depositsPhasRoot,
        committedDepositsRoot: output.committedDepositsRoot,
        depositCount: output.depositCount,
        step02State: output.step02State,
      }),
    ),
  ]);
  return { ...output, files: paths };
};

export type FabricatedDepositBlockEvidenceV1 = {
  readonly grade: SDK.EvidenceGradeV1;
  readonly provenance: {
    readonly l1: SDK.EvidenceProvenanceV1;
    readonly da: SDK.EvidenceProvenanceV1;
  };
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly committedDepositsRoot: string;
  readonly depositCount: bigint;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly entries: readonly (readonly [string, string])[];
};

/**
 * Extracts the committed `deposits` leaves from public retained-DA bytes without
 * requiring the block to be well-formed. Only the payload envelope, canonical
 * `DaPayloadV1` framing and the embedded-header identity are enforced here; leaf
 * authenticity is what the family adjudicates.
 */
export const fabricatedDepositBlockEvidenceFromVerifiedPayloadV1 = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<FabricatedDepositBlockEvidenceV1> => {
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
    throw new FabricatedDepositRejectionV1(
      "malformed_da_payload",
      `failed to decode the mandatory DaPayloadEnvelopeV1: ${String(cause)}`,
    );
  }
  let payload: SDK.DaPayloadV1;
  try {
    payload = SDK.decodeDaPayloadV1(payloadCbor);
  } catch (cause) {
    throw new FabricatedDepositRejectionV1(
      "malformed_da_payload",
      `failed to decode DaPayloadV1 canonical CBOR: ${String(cause)}`,
    );
  }
  if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor)) {
    throw new FabricatedDepositRejectionV1(
      "non_canonical_da_payload",
      "DA payload CBOR is not canonical for DaPayloadV1",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_V1_VERSION) {
    throw new FabricatedDepositRejectionV1(
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
    throw new FabricatedDepositRejectionV1(
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
    committedDepositsRoot: admittedObservation.header.depositsRoot,
    depositCount: admittedObservation.header.depositCount,
    headerStartTime: admittedObservation.header.startTime,
    headerEndTime: admittedObservation.header.endTime,
    entries: body.deposits.map(
      ([keyHex, valueHex]) => [keyHex, valueHex] as const,
    ),
  };
};

/**
 * The security-grade entry point: authenticated L1 header observation + public
 * retained-DA payload + an authenticated L1 deposit-identity witness -> a
 * submittable `fabricated-deposit` proof plan.
 */
export const prepareFabricatedDepositFromRetainedDaV1 = async ({
  observation,
  sources,
  witness,
  retries,
  minimumConfirmationDepth,
  committedDepositIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly witness: FabricatedDepositL1WitnessV1;
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedDepositIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedFabricatedDepositOutput> => {
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
  const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayloadV1({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidenceV1(
      SDK.admitEvidenceProvenanceV1({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return await prepareFabricatedDepositFromCommittedLeavesV1({
    headerHash: evidence.headerHash,
    committedDepositsRoot: evidence.committedDepositsRoot,
    depositCount: evidence.depositCount,
    headerStartTime: evidence.headerStartTime,
    headerEndTime: evidence.headerEndTime,
    entries: evidence.entries,
    witness,
    ...(committedDepositIdCbor === undefined ? {} : { committedDepositIdCbor }),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
