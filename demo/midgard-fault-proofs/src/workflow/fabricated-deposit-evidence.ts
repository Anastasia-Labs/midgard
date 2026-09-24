import { createHash } from "node:crypto";

import {
  type AuthenticatedStateQueueHeaderObservation,
  FABRICATED_DEPOSIT_VIOLATION_ID,
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
  type FabricatedDepositL1Witness,
  FabricatedDepositRejection,
  prepareFabricatedDepositFromCommittedLeaves,
} from "../prepare-fabricated-deposit.js";
import type { CanonicalViolationDetection } from "./classification.js";

export const FABRICATED_DEPOSIT_EVIDENCE_AUTHORITY =
  "midgard-production-fabricated-deposit-evidence-authority-v1" as const;
export const FABRICATED_DEPOSIT_ARTIFACT =
  "midgard-production-fabricated-deposit-artifact-v1" as const;

export type FabricatedDepositArtifact = Readonly<{
  schemaVersion: typeof FABRICATED_DEPOSIT_ARTIFACT;
  headerHash: string;
  owner: string;
  depositIndex: number;
  depositInclusion: Readonly<{
    committedDepositIdCbor: string;
    committedDepositInfoCbor: string;
    depositsPhasRoot: string;
    depositMembershipProofCbor: string;
  }>;
  authenticContent: Readonly<{ openingCbor: string | null }>;
  l1Evidence: Readonly<{
    kind: "absent_identity" | "present_event";
    historyOutRef: string;
    retainedDataOutRef: string | null;
  }>;
  artifactDigest: string;
}>;

export type FabricatedDepositDetection = Readonly<{
  detection: CanonicalViolationDetection;
  artifact: FabricatedDepositArtifact;
}>;

export interface FabricatedDepositEvidenceAuthority {
  readonly authorityVersion: typeof FABRICATED_DEPOSIT_EVIDENCE_AUTHORITY;
  detect(
    evidence: CanonicalBlockEvidence,
    owner: string,
  ): Promise<readonly FabricatedDepositDetection[]>;
  prepare(
    evidence: CanonicalBlockEvidence,
    owner: string,
    depositIndex: number,
  ): Promise<FabricatedDepositArtifact>;
  /** Re-authenticates a journal-restored artifact against current public L1. */
  readmit(value: unknown): Promise<FabricatedDepositArtifact>;
  /** Integrity-only journal admission after capture. The stage submitter must
   * authenticate the opening against its actual L1 computation-thread state. */
  readmitRetained(value: unknown): FabricatedDepositArtifact;
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
  value: Omit<FabricatedDepositArtifact, "artifactDigest">,
): string =>
  createHash("sha256")
    .update(FABRICATED_DEPOSIT_ARTIFACT)
    .update("\0")
    .update(value.headerHash)
    .update("\0")
    .update(value.owner)
    .update("\0")
    .update(value.depositIndex.toString())
    .update("\0")
    // Journal normalization reorders object keys; bind the schema fields in
    // their original digest order on both preparation and re-admission.
    .update(
      JSON.stringify({
        committedDepositIdCbor: value.depositInclusion.committedDepositIdCbor,
        committedDepositInfoCbor:
          value.depositInclusion.committedDepositInfoCbor,
        depositsPhasRoot: value.depositInclusion.depositsPhasRoot,
        depositMembershipProofCbor:
          value.depositInclusion.depositMembershipProofCbor,
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
  depositId,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly depositId: OutputReference;
}): Promise<{
  readonly witness: FabricatedDepositL1Witness;
  readonly l1Evidence: FabricatedDepositArtifact["l1Evidence"];
}> => {
  const witness = await fetchFabricatedHistoryWitness({
    lucid,
    network,
    hubOraclePolicyId,
    history,
    observation,
    kind: "Deposit",
    id: depositId,
  });
  const authenticated = await authenticateFabricatedHistoryWitness(
    witness,
    "Deposit",
    depositId,
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
  depositIndex,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly minimumConfirmationDepth: number;
  readonly evidence: CanonicalBlockEvidence;
  readonly owner: string;
  readonly depositIndex: number;
}): Promise<FabricatedDepositArtifact> => {
  if (
    !HEX_28.test(owner) ||
    !Number.isSafeInteger(depositIndex) ||
    depositIndex < 0
  ) {
    throw new Error("fabricated-deposit artifact identity is invalid");
  }
  const selected = evidence.reconstruction.deposits[depositIndex];
  if (selected === undefined) {
    throw new Error(
      "fabricated-deposit selected leaf is outside the committed set",
    );
  }
  const { witness, l1Evidence } = await discoverWitness({
    lucid,
    network,
    hubOraclePolicyId,
    history,
    observation: evidence.observation,
    depositId: selected.key,
  });
  const prepared = await prepareFabricatedDepositFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedDepositsRoot: evidence.header.depositsRoot,
    depositCount: evidence.header.depositCount,
    headerStartTime: evidence.header.startTime,
    headerEndTime: evidence.header.endTime,
    entries: evidence.reconstruction.deposits.map(
      (entry) =>
        [
          entry.keyBytes.toString("hex"),
          entry.valueBytes.toString("hex"),
        ] as const,
    ),
    witness,
    committedDepositIdCbor: selected.keyBytes.toString("hex"),
    minimumConfirmationDepth,
  });
  const body = {
    schemaVersion: FABRICATED_DEPOSIT_ARTIFACT,
    headerHash: prepared.headerHash,
    owner,
    depositIndex,
    depositInclusion: Object.freeze({ ...prepared.depositInclusion }),
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
export const createFabricatedDepositEvidenceAuthority = ({
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
}): FabricatedDepositEvidenceAuthority => {
  if (
    !HEX_28.test(hubOraclePolicyId) ||
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth < 1
  ) {
    throw new Error("fabricated-deposit evidence authority config is invalid");
  }
  const authority: FabricatedDepositEvidenceAuthority = {
    authorityVersion: FABRICATED_DEPOSIT_EVIDENCE_AUTHORITY,
    prepare: async (evidence, owner, depositIndex) =>
      await prepareAt({
        lucid,
        network,
        hubOraclePolicyId,
        history,
        minimumConfirmationDepth,
        evidence,
        owner,
        depositIndex,
      }),
    detect: async (evidence, owner) => {
      const detections: FabricatedDepositDetection[] = [];
      for (
        let index = 0;
        index < evidence.reconstruction.deposits.length;
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
            depositIndex: index,
          });
          detections.push(
            Object.freeze({
              artifact,
              detection: Object.freeze({
                detectionId: `${FABRICATED_DEPOSIT_VIOLATION_ID}:${index.toString()}:${artifact.depositInclusion.committedDepositIdCbor}`,
                headerHash: evidence.headerHash,
                violationId: FABRICATED_DEPOSIT_VIOLATION_ID,
                position: BigInt(index),
                diagnostic: `committed deposit ${index.toString()} is absent from authentic L1 or differs from its authentic event`,
              }),
            }),
          );
        } catch (cause) {
          if (
            cause instanceof FabricatedDepositRejection &&
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
        artifact.depositInclusion.committedDepositIdCbor,
        OutputReference,
      );
      // Resolve current L1 anchors afresh: stored output references are hints,
      // and an unchanged Order may have any number of pointer continuations.
      const current = await fetchCurrentHistory({
        lucid,
        network,
        hubOraclePolicyId,
        history,
        kind: "Deposit",
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

export const requireFabricatedDepositEvidenceAuthority = (
  authority: FabricatedDepositEvidenceAuthority,
): FabricatedDepositEvidenceAuthority => {
  if (
    !admittedAuthorities.has(authority) ||
    authority.authorityVersion !== FABRICATED_DEPOSIT_EVIDENCE_AUTHORITY
  ) {
    throw new Error(
      "fabricated-deposit production evidence authority is not admitted",
    );
  }
  return authority;
};

const parseArtifact = (value: unknown): FabricatedDepositArtifact => {
  const outer = plainRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "owner",
      "depositIndex",
      "depositInclusion",
      "authenticContent",
      "l1Evidence",
      "artifactDigest",
    ],
    "fabricated-deposit production artifact",
  );
  const inclusion = plainRecord(
    outer.depositInclusion,
    [
      "committedDepositIdCbor",
      "committedDepositInfoCbor",
      "depositsPhasRoot",
      "depositMembershipProofCbor",
    ],
    "fabricated-deposit inclusion",
  );
  const authentic = plainRecord(
    outer.authenticContent,
    ["openingCbor"],
    "fabricated-deposit authentic content",
  );
  const l1 = plainRecord(
    outer.l1Evidence,
    ["kind", "historyOutRef", "retainedDataOutRef"],
    "fabricated-deposit L1 evidence",
  );
  const artifact = {
    schemaVersion: outer.schemaVersion,
    headerHash: outer.headerHash,
    owner: outer.owner,
    depositIndex: outer.depositIndex,
    depositInclusion: inclusion,
    authenticContent: authentic,
    l1Evidence: l1,
    artifactDigest: outer.artifactDigest,
  } as unknown as FabricatedDepositArtifact;
  if (
    artifact.schemaVersion !== FABRICATED_DEPOSIT_ARTIFACT ||
    !HEX_28.test(artifact.owner) ||
    !HEX_28.test(artifact.headerHash) ||
    !HEX_32.test(artifact.artifactDigest) ||
    !Number.isSafeInteger(artifact.depositIndex) ||
    artifact.depositIndex < 0 ||
    !EVEN_HEX.test(artifact.depositInclusion.committedDepositIdCbor) ||
    !EVEN_HEX.test(artifact.depositInclusion.committedDepositInfoCbor) ||
    !HEX_32.test(artifact.depositInclusion.depositsPhasRoot) ||
    !EVEN_HEX.test(artifact.depositInclusion.depositMembershipProofCbor) ||
    (artifact.authenticContent.openingCbor !== null &&
      !EVEN_HEX.test(artifact.authenticContent.openingCbor)) ||
    !["absent_identity", "present_event"].includes(artifact.l1Evidence.kind) ||
    !OUT_REF.test(artifact.l1Evidence.historyOutRef) ||
    (artifact.l1Evidence.retainedDataOutRef !== null &&
      !OUT_REF.test(artifact.l1Evidence.retainedDataOutRef))
  ) {
    throw new Error("fabricated-deposit production artifact is malformed");
  }
  const { artifactDigest: supplied, ...body } = artifact;
  if (artifactDigest(body) !== supplied) {
    throw new Error("fabricated-deposit production artifact digest mismatch");
  }
  return Object.freeze({ ...artifact });
};

export const requireFabricatedDepositArtifact = (
  value: unknown,
  owner: string,
  headerHash: string,
): FabricatedDepositArtifact => {
  const artifact = value as FabricatedDepositArtifact;
  if (
    !admittedArtifacts.has(artifact) ||
    artifact.owner !== owner ||
    artifact.headerHash !== headerHash
  ) {
    throw new Error(
      "fabricated-deposit production artifact was not re-authenticated for this workflow",
    );
  }
  return artifact;
};
