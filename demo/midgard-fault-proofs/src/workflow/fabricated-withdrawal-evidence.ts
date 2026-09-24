import { createHash } from "node:crypto";

import {
  type AuthenticatedStateQueueHeaderObservation,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  OutputReference,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  authenticateFabricatedHistoryWitness,
  type FabricatedHistoryEnvironment,
  fabricatedHistoryOpeningCbor,
  fetchCurrentHistory,
  fetchFabricatedHistoryWitness,
} from "../fabricated-history-witness.js";
import {
  type FabricatedWithdrawalL1Witness,
  FabricatedWithdrawalRejection,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../prepare-fabricated-withdrawal.js";
import type { CanonicalViolationDetection } from "./classification.js";

export const FABRICATED_WITHDRAWAL_EVIDENCE_AUTHORITY =
  "midgard-production-fabricated-withdrawal-evidence-authority-v1" as const;
export const FABRICATED_WITHDRAWAL_ARTIFACT =
  "midgard-production-fabricated-withdrawal-artifact-v1" as const;

export type FabricatedWithdrawalArtifact = Readonly<{
  schemaVersion: typeof FABRICATED_WITHDRAWAL_ARTIFACT;
  headerHash: string;
  owner: string;
  withdrawalIndex: number;
  withdrawalInclusion: Readonly<{
    committedWithdrawalIdCbor: string;
    committedWithdrawalInfoCbor: string;
    withdrawalsPhasRoot: string;
    withdrawalMembershipProofCbor: string;
  }>;
  authenticContent: Readonly<{ openingCbor: string | null }>;
  l1Evidence: Readonly<{
    kind: "absent_identity" | "present_event";
    historyOutRef: string;
    retainedDataOutRef: string | null;
  }>;
  artifactDigest: string;
}>;

export type FabricatedWithdrawalDetection = Readonly<{
  detection: CanonicalViolationDetection;
  artifact: FabricatedWithdrawalArtifact;
}>;

export interface FabricatedWithdrawalEvidenceAuthority {
  readonly authorityVersion: typeof FABRICATED_WITHDRAWAL_EVIDENCE_AUTHORITY;
  detect(
    evidence: CanonicalBlockEvidence,
    owner: string,
  ): Promise<readonly FabricatedWithdrawalDetection[]>;
  prepare(
    evidence: CanonicalBlockEvidence,
    owner: string,
    withdrawalIndex: number,
  ): Promise<FabricatedWithdrawalArtifact>;
  /** Re-authenticates a journal-restored artifact against current public L1. */
  readmit(value: unknown): Promise<FabricatedWithdrawalArtifact>;
  /** Integrity-only journal admission after capture. The stage submitter must
   * authenticate the opening against its actual L1 computation-thread state. */
  readmitRetained(value: unknown): FabricatedWithdrawalArtifact;
}

const admittedAuthorities = new WeakSet<object>();
const admittedArtifacts = new WeakSet<object>();
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const plainRecord = (
  value: unknown,
  expectedKeys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length ||
    Object.keys(value).sort().join(",") !== [...expectedKeys].sort().join(",")
  ) {
    throw new Error(`${label} has unknown, missing, or non-string fields`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const artifactDigest = (
  value: Omit<FabricatedWithdrawalArtifact, "artifactDigest">,
): string =>
  createHash("sha256")
    .update(FABRICATED_WITHDRAWAL_ARTIFACT)
    .update("\0")
    .update(value.headerHash)
    .update("\0")
    .update(value.owner)
    .update("\0")
    .update(value.withdrawalIndex.toString())
    .update("\0")
    // Journal normalization reorders object keys; bind the schema fields in
    // their original digest order on both preparation and re-admission.
    .update(
      JSON.stringify({
        committedWithdrawalIdCbor:
          value.withdrawalInclusion.committedWithdrawalIdCbor,
        committedWithdrawalInfoCbor:
          value.withdrawalInclusion.committedWithdrawalInfoCbor,
        withdrawalsPhasRoot: value.withdrawalInclusion.withdrawalsPhasRoot,
        withdrawalMembershipProofCbor:
          value.withdrawalInclusion.withdrawalMembershipProofCbor,
      }),
    )
    .update("\0")
    .update(JSON.stringify(value.authenticContent))
    .update("\0")
    .update(
      JSON.stringify({
        kind: value.l1Evidence.kind,
        historyOutRef: value.l1Evidence.historyOutRef,
        retainedDataOutRef: value.l1Evidence.retainedDataOutRef,
      }),
    )
    .digest("hex");

const outRef = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const discoverWitness = async ({
  lucid,
  network,
  hubOraclePolicyId,
  history,
  observation,
  withdrawalId,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly withdrawalId: OutputReference;
}): Promise<{
  readonly witness: FabricatedWithdrawalL1Witness;
  readonly l1Evidence: FabricatedWithdrawalArtifact["l1Evidence"];
}> => {
  const witness = await fetchFabricatedHistoryWitness({
    lucid,
    network,
    hubOraclePolicyId,
    history,
    observation,
    kind: "Withdrawal",
    id: withdrawalId,
  });
  const authenticated = await authenticateFabricatedHistoryWitness(
    witness,
    "Withdrawal",
    withdrawalId,
  );
  return {
    witness,
    l1Evidence: {
      kind:
        authenticated.witness.kind === "Present"
          ? "present_event"
          : "absent_identity",
      historyOutRef: outRef(witness.anchor),
      retainedDataOutRef: witness.retainedDataUtxo
        ? outRef(witness.retainedDataUtxo)
        : null,
    },
  };
};

const prepareAt = async ({
  lucid,
  network,
  hubOraclePolicyId,
  history,
  minimumConfirmationDepth,
  evidence,
  owner,
  withdrawalIndex,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly minimumConfirmationDepth: number;
  readonly evidence: CanonicalBlockEvidence;
  readonly owner: string;
  readonly withdrawalIndex: number;
}): Promise<FabricatedWithdrawalArtifact> => {
  if (
    !HEX_28.test(owner) ||
    !Number.isSafeInteger(withdrawalIndex) ||
    withdrawalIndex < 0
  ) {
    throw new Error("fabricated-withdrawal artifact identity is invalid");
  }
  const selected = evidence.reconstruction.withdrawals[withdrawalIndex];
  if (selected === undefined) {
    throw new Error(
      "fabricated-withdrawal selected leaf is outside the committed set",
    );
  }
  const { witness, l1Evidence } = await discoverWitness({
    lucid,
    network,
    hubOraclePolicyId,
    history,
    observation: evidence.observation,
    withdrawalId: selected.key,
  });
  const prepared = await prepareFabricatedWithdrawalFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: evidence.header.withdrawalCount,
    headerStartTime: evidence.header.startTime,
    headerEndTime: evidence.header.endTime,
    entries: evidence.reconstruction.withdrawals.map(
      (entry) =>
        [
          entry.keyBytes.toString("hex"),
          entry.valueBytes.toString("hex"),
        ] as const,
    ),
    witness,
    committedWithdrawalIdCbor: selected.keyBytes.toString("hex"),
    minimumConfirmationDepth,
  });
  const body = {
    schemaVersion: FABRICATED_WITHDRAWAL_ARTIFACT,
    headerHash: prepared.headerHash,
    owner,
    withdrawalIndex,
    withdrawalInclusion: Object.freeze({ ...prepared.withdrawalInclusion }),
    authenticContent: Object.freeze({ ...prepared.authenticContent }),
    l1Evidence: Object.freeze({ ...l1Evidence }),
  } as const;
  const artifact = Object.freeze({
    ...body,
    artifactDigest: artifactDigest(body),
  });
  admittedArtifacts.add(artifact);
  return artifact;
};

/**
 * Concrete production authority. Candidate discovery is not trusted: step 02
 * re-authenticates the current hub-bound list witness and the family
 * adapter captures a locally evaluated transaction before any submit.
 */
export const createFabricatedWithdrawalEvidenceAuthority = ({
  lucid,
  network,
  hubOraclePolicyId,
  history,
  minimumConfirmationDepth,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly minimumConfirmationDepth: number;
}): FabricatedWithdrawalEvidenceAuthority => {
  if (
    !HEX_28.test(hubOraclePolicyId) ||
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth < 1
  ) {
    throw new Error(
      "fabricated-withdrawal evidence authority config is invalid",
    );
  }
  const authority: FabricatedWithdrawalEvidenceAuthority = {
    authorityVersion: FABRICATED_WITHDRAWAL_EVIDENCE_AUTHORITY,
    prepare: async (evidence, owner, withdrawalIndex) =>
      await prepareAt({
        lucid,
        network,
        hubOraclePolicyId,
        history,
        minimumConfirmationDepth,
        evidence,
        owner,
        withdrawalIndex,
      }),
    detect: async (evidence, owner) => {
      const detections: FabricatedWithdrawalDetection[] = [];
      for (
        let index = 0;
        index < evidence.reconstruction.withdrawals.length;
        index += 1
      ) {
        try {
          const artifact = await prepareAt({
            lucid,
            network,
            hubOraclePolicyId,
            history,
            minimumConfirmationDepth,
            evidence,
            owner,
            withdrawalIndex: index,
          });
          detections.push(
            Object.freeze({
              artifact,
              detection: Object.freeze({
                detectionId: `${FABRICATED_WITHDRAWAL_VIOLATION_ID}:${index.toString()}:${artifact.withdrawalInclusion.committedWithdrawalIdCbor}`,
                headerHash: evidence.headerHash,
                violationId: FABRICATED_WITHDRAWAL_VIOLATION_ID,
                position: BigInt(index),
                diagnostic: `committed withdrawal ${index.toString()} is absent from authentic L1 or differs from its authentic event`,
              }),
            }),
          );
        } catch (cause) {
          if (
            cause instanceof FabricatedWithdrawalRejection &&
            cause.code === "authentic_content_matches_commitment"
          ) {
            continue;
          }
          throw cause;
        }
      }
      return Object.freeze(detections);
    },
    readmit: async (value) => {
      const artifact = parseArtifact(value);
      const id = Data.from(
        artifact.withdrawalInclusion.committedWithdrawalIdCbor,
        OutputReference,
      );
      // Resolve current L1 anchors afresh: stored output references are hints,
      // and an unchanged Order may have any number of pointer continuations.
      const current = await fetchCurrentHistory({
        lucid,
        network,
        hubOraclePolicyId,
        history,
        kind: "Withdrawal",
        id,
      });
      const captured = current.captured;
      const currentOpening = captured
        ? fabricatedHistoryOpeningCbor(captured)
        : null;
      const currentKind = captured ? "present_event" : "absent_identity";
      if (
        currentKind !== artifact.l1Evidence.kind ||
        currentOpening !== artifact.authenticContent.openingCbor
      )
        throw new Error(
          "History facts changed before capture; reprepare the proof artifact",
        );
      admittedArtifacts.add(artifact);
      return artifact;
    },
    readmitRetained: (value) => {
      const artifact = parseArtifact(value);
      admittedArtifacts.add(artifact);
      return artifact;
    },
  };
  admittedAuthorities.add(authority);
  return Object.freeze(authority);
};

export const requireFabricatedWithdrawalEvidenceAuthority = (
  authority: FabricatedWithdrawalEvidenceAuthority,
): FabricatedWithdrawalEvidenceAuthority => {
  if (
    !admittedAuthorities.has(authority) ||
    authority.authorityVersion !== FABRICATED_WITHDRAWAL_EVIDENCE_AUTHORITY
  ) {
    throw new Error(
      "fabricated-withdrawal production evidence authority is not admitted",
    );
  }
  return authority;
};

const parseArtifact = (value: unknown): FabricatedWithdrawalArtifact => {
  const outer = plainRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "owner",
      "withdrawalIndex",
      "withdrawalInclusion",
      "authenticContent",
      "l1Evidence",
      "artifactDigest",
    ],
    "fabricated-withdrawal production artifact",
  );
  const inclusion = plainRecord(
    outer.withdrawalInclusion,
    [
      "committedWithdrawalIdCbor",
      "committedWithdrawalInfoCbor",
      "withdrawalsPhasRoot",
      "withdrawalMembershipProofCbor",
    ],
    "fabricated-withdrawal inclusion",
  );
  const authentic = plainRecord(
    outer.authenticContent,
    ["openingCbor"],
    "fabricated-withdrawal authentic content",
  );
  const l1 = plainRecord(
    outer.l1Evidence,
    ["kind", "historyOutRef", "retainedDataOutRef"],
    "fabricated-withdrawal L1 evidence",
  );
  const artifact = {
    schemaVersion: outer.schemaVersion,
    headerHash: outer.headerHash,
    owner: outer.owner,
    withdrawalIndex: outer.withdrawalIndex,
    withdrawalInclusion: inclusion,
    authenticContent: authentic,
    l1Evidence: l1,
    artifactDigest: outer.artifactDigest,
  } as unknown as FabricatedWithdrawalArtifact;
  if (
    artifact.schemaVersion !== FABRICATED_WITHDRAWAL_ARTIFACT ||
    !HEX_28.test(artifact.owner) ||
    !HEX_28.test(artifact.headerHash) ||
    !HEX_32.test(artifact.artifactDigest) ||
    !Number.isSafeInteger(artifact.withdrawalIndex) ||
    artifact.withdrawalIndex < 0 ||
    !EVEN_HEX.test(artifact.withdrawalInclusion.committedWithdrawalIdCbor) ||
    !EVEN_HEX.test(artifact.withdrawalInclusion.committedWithdrawalInfoCbor) ||
    !HEX_32.test(artifact.withdrawalInclusion.withdrawalsPhasRoot) ||
    !EVEN_HEX.test(
      artifact.withdrawalInclusion.withdrawalMembershipProofCbor,
    ) ||
    (artifact.authenticContent.openingCbor !== null &&
      !EVEN_HEX.test(artifact.authenticContent.openingCbor)) ||
    !["absent_identity", "present_event"].includes(artifact.l1Evidence.kind) ||
    !OUT_REF.test(artifact.l1Evidence.historyOutRef) ||
    (artifact.l1Evidence.retainedDataOutRef !== null &&
      !OUT_REF.test(artifact.l1Evidence.retainedDataOutRef))
  ) {
    throw new Error("fabricated-withdrawal production artifact is malformed");
  }
  const { artifactDigest: supplied, ...body } = artifact;
  if (artifactDigest(body) !== supplied) {
    throw new Error(
      "fabricated-withdrawal production artifact digest mismatch",
    );
  }
  return Object.freeze({ ...artifact });
};

export const requireFabricatedWithdrawalArtifact = (
  value: unknown,
  owner: string,
  headerHash: string,
): FabricatedWithdrawalArtifact => {
  const artifact = value as FabricatedWithdrawalArtifact;
  if (
    !admittedArtifacts.has(artifact) ||
    artifact.owner !== owner ||
    artifact.headerHash !== headerHash
  ) {
    throw new Error(
      "fabricated-withdrawal production artifact was not re-authenticated for this workflow",
    );
  }
  return artifact;
};
