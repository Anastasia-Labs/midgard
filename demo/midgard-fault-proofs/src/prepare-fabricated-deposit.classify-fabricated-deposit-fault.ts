import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  authenticateFabricatedHistoryWitness,
  fabricatedHistoryOpeningCbor,
  type FabricatedHistoryWitness,
} from "./fabricated-history-witness.js";

export const FABRICATED_DEPOSIT_EVIDENCE_SCHEMA_VERSION =
  "midgard-fabricated-deposit-evidence-v1" as const;

export type FabricatedDepositRejectionCode =
  | "malformed_da_payload"
  | "non_canonical_da_payload"
  | "wrong_da_payload_version"
  | "header_hash_mismatch"
  | "deposits_root_mismatch"
  | "no_committed_deposit_leaf"
  | "leaf_not_committed"
  | "authentic_content_matches_commitment"
  | "history_witness_invalid";

/** Deterministic, value-free rejection; `detail` carries only public data. */
export class FabricatedDepositRejection extends Error {
  readonly code: FabricatedDepositRejectionCode;

  constructor(code: FabricatedDepositRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "FabricatedDepositRejectionV1";
    this.code = code;
  }
}

export const hexOf = (value: string, label: string): Buffer => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})*$/u.test(normalized)) {
    throw new FabricatedDepositRejection(
      "malformed_da_payload",
      `${label} is not even-length hexadecimal`,
    );
  }
  return Buffer.from(normalized, "hex");
};

/** One committed `deposits_root` leaf, decoded and committed to. */
export type CommittedDepositLeaf = {
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

export const decodeCommittedDepositLeaf = async (
  keyHex: string,
  valueHex: string,
  index: number,
): Promise<CommittedDepositLeaf> => {
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
    throw new FabricatedDepositRejection(
      "malformed_da_payload",
      `${label} does not decode as (DepositId, DepositInfo): ${String(cause)}`,
    );
  }
  if (
    SDK.committedDepositKeyBytes(committedDepositId) !==
      committedDepositIdCbor ||
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      committedDepositInfoCbor,
    ) !== committedDepositInfoCbor
  ) {
    throw new FabricatedDepositRejection(
      "non_canonical_da_payload",
      `${label} leaf bytes are not canonical for (DepositId, DepositInfo)`,
    );
  }
  const committedDepositInfoHash = await Effect.runPromise(
    SDK.depositInfoCommitmentCbor(committedDepositInfoCbor),
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

/** Public-L1 hub, authenticated list anchor and optional retained-data output. */
export type FabricatedDepositL1Witness = FabricatedHistoryWitness;

export type ClassifiedFabricatedDepositFault = {
  readonly verdict: SDK.FabricatedDepositEvidenceVerdict;
  readonly fault: SDK.FabricatedDepositFault;
  readonly stateQueuePolicyId: string;
  readonly openingCbor: string | null;
  readonly authenticDepositInfoHash?: string;
  readonly eventInclusionTime?: bigint;
};

/** Current authenticated list absence or immutable Order facts. No live-nonce
 * fallback or operator archive can establish an event's existence/absence. */
export const classifyFabricatedDepositFault = async ({
  leaf,
  headerStartTime,
  headerEndTime,
  witness,
  minimumConfirmationDepth,
}: {
  readonly leaf: CommittedDepositLeaf;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly witness: FabricatedDepositL1Witness;
  readonly minimumConfirmationDepth?: number;
}): Promise<ClassifiedFabricatedDepositFault> => {
  let authenticated: Awaited<
    ReturnType<typeof authenticateFabricatedHistoryWitness>
  >;
  try {
    authenticated = await authenticateFabricatedHistoryWitness(
      witness,
      "Deposit",
      leaf.committedDepositId,
      minimumConfirmationDepth,
    );
  } catch (cause) {
    throw new FabricatedDepositRejection(
      "history_witness_invalid",
      String(cause),
    );
  }
  const { captured, stateQueuePolicyId } = authenticated;
  if (captured === undefined)
    return {
      verdict: "DepositIdentityAbsent",
      fault: "NonexistentDepositIdentity",
      stateQueuePolicyId,
      openingCbor: null,
    };
  const { commitment, payload } = captured;
  if (!("DepositPayload" in payload))
    throw new FabricatedDepositRejection(
      "history_witness_invalid",
      "Wrong authenticated event kind",
    );
  const authenticDepositInfoHash = await Effect.runPromise(
    SDK.depositInfoCommitmentCbor(
      plutusConstrFieldCbor(captured.payloadCbor, [0, 1]),
    ),
  );
  const inclusionTime = commitment.inclusion_time;
  const eligible =
    headerStartTime < inclusionTime && inclusionTime <= headerEndTime;
  if (eligible && authenticDepositInfoHash === leaf.committedDepositInfoHash)
    throw new FabricatedDepositRejection(
      "authentic_content_matches_commitment",
      "The eligible event content matches the header; no fabrication is established",
    );
  return {
    verdict: { DepositEventObserved: { commitment } },
    fault: eligible
      ? {
          MismatchedDepositContent: {
            committed_deposit_info_hash: leaf.committedDepositInfoHash,
            authentic_deposit_info_hash: authenticDepositInfoHash,
            event_inclusion_time: inclusionTime,
          },
        }
      : { IneligibleDepositEvent: { event_inclusion_time: inclusionTime } },
    stateQueuePolicyId,
    openingCbor: fabricatedHistoryOpeningCbor(captured),
    authenticDepositInfoHash,
    eventInclusionTime: inclusionTime,
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
  readonly openingCbor: string | null;
};

/** Exactly the step-02 state the on-chain step-01 validator will derive. */
export type PreparedFabricatedDepositStateJson = {
  readonly stateQueuePolicyId: string;
  readonly challengedHeaderHash: string;
  readonly headerStartTime: string;
  readonly headerEndTime: string;
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoHash: string;
};

export type PreparedFabricatedDepositOutput = {
  readonly schemaVersion: typeof FABRICATED_DEPOSIT_EVIDENCE_SCHEMA_VERSION;
  readonly violationId: typeof SDK.FABRICATED_DEPOSIT_VIOLATION_ID;
  readonly fraudCategoryId: typeof SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID;
  readonly headerHash: string;
  readonly threadTokenAssetName: string;
  readonly depositCount: number;
  /** Raw MPF root opened by the leaf membership proof. */
  readonly depositsPhasRoot: string;
  /** Counted, domain-separated root the header commits. */
  readonly committedDepositsRoot: string;
  readonly headerStartTime: bigint;
  readonly headerEndTime: bigint;
  readonly leaves: readonly CommittedDepositLeaf[];
  readonly challengedLeaf: CommittedDepositLeaf;
  readonly classification: ClassifiedFabricatedDepositFault;
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
  readonly witness: FabricatedDepositL1Witness;
  /** Pin a specific committed leaf key; otherwise the sole leaf is used. */
  readonly committedDepositIdCbor?: string;
  readonly minimumConfirmationDepth?: number;
  readonly outputDir?: string;
};
