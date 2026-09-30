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

export const hexOf = (value: string, label: string): Buffer => {
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

export const decodeCommittedWithdrawalLeaf = async (
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
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      committedWithdrawalInfoCbor,
    ) !== committedWithdrawalInfoCbor
  ) {
    throw new FabricatedWithdrawalRejection(
      "non_canonical_da_payload",
      `${label} leaf bytes are not canonical for (WithdrawalId, WithdrawalInfo)`,
    );
  }
  const committedWithdrawalContentHash = await Effect.runPromise(
    SDK.withdrawalContentCommitmentCbor(committedWithdrawalInfoCbor),
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
  const { commitment, payload } = captured;
  if (!("WithdrawalPayload" in payload))
    throw new FabricatedWithdrawalRejection(
      "history_witness_invalid",
      "Wrong authenticated event kind",
    );
  const authenticWithdrawalContentHash = await Effect.runPromise(
    SDK.withdrawalContentCommitmentCbor(
      plutusConstrFieldCbor(captured.payloadCbor, [0, 1]),
    ),
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
    openingCbor: fabricatedHistoryOpeningCbor(captured),
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
