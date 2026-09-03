import { createHash } from "node:crypto";

import {
  type AuthenticatedStateQueueHeaderObservation,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  OutputReference,
  withdrawalEventNonce,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FabricatedWithdrawalL1Witness,
  FabricatedWithdrawalRejection,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../prepare-fabricated-withdrawal.js";
import { requireSingletonUtxo } from "../runtime.js";
import type { CanonicalViolationDetection } from "./classification-v1.js";

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
  authenticContent: Readonly<{ eventDatumCbor: string | null }>;
  l1Evidence:
    | Readonly<{ kind: "absent_identity"; unspentOutRef: string }>
    | Readonly<{ kind: "present_event"; eventOutRef: string }>;
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
    .update(JSON.stringify(value.withdrawalInclusion))
    .update("\0")
    .update(JSON.stringify(value.authenticContent))
    .update("\0")
    .update(JSON.stringify(value.l1Evidence))
    .digest("hex");

const outRef = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const exactOne = <T>(values: readonly T[], label: string): T => {
  if (values.length !== 1) {
    throw new Error(`${label} requires exactly one current L1 output`);
  }
  return values[0]!;
};

const discoverWitness = async ({
  lucid,
  network,
  hubOraclePolicyId,
  observation,
  withdrawalId,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly withdrawalId: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }>;
}): Promise<{
  readonly witness: FabricatedWithdrawalL1Witness;
  readonly l1Evidence: FabricatedWithdrawalArtifact["l1Evidence"];
}> => {
  const candidateOutRef = `${withdrawalId.transactionId}#${withdrawalId.outputIndex.toString()}`;
  const live = await lucid.utxosByOutRef([
    {
      txHash: withdrawalId.transactionId,
      outputIndex: Number(withdrawalId.outputIndex),
    },
  ]);
  if (live.length > 1) {
    throw new Error(
      "fabricated-withdrawal L1 lookup returned duplicate outrefs",
    );
  }
  if (live.length === 1) {
    return {
      witness: {
        kind: "absent_identity",
        observation,
        liveOutputReferences: [withdrawalId],
      },
      l1Evidence: { kind: "absent_identity", unspentOutRef: candidateOutRef },
    };
  }

  const hubOracleAddress = credentialToAddress(
    network,
    scriptHashToCredential(hubOraclePolicyId),
  );
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: hubOracleAddress,
    unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "fabricated-withdrawal hub oracle",
  });
  if (hubOracleUtxo.datum == null) {
    throw new Error("fabricated-withdrawal hub oracle has no inline datum");
  }
  const hub = Data.from(hubOracleUtxo.datum, HubOracleDatum);
  const nonce = await Effect.runPromise(withdrawalEventNonce(withdrawalId));
  const eventUnit = toUnit(hub.withdrawal, nonce);
  const withdrawalAddress = credentialToAddress(
    network,
    scriptHashToCredential(hub.withdrawal),
  );
  const eventUtxo = exactOne(
    await lucid.utxosAtWithUnit(withdrawalAddress, eventUnit),
    "fabricated-withdrawal event lookup",
  );
  if (eventUtxo.datum == null) {
    throw new Error("fabricated-withdrawal event output has no inline datum");
  }
  return {
    witness: {
      kind: "present_event",
      observation,
      withdrawalEventPolicyId: hub.withdrawal,
      observedEventAssetName: nonce,
      eventDatumCbor: eventUtxo.datum,
    },
    l1Evidence: { kind: "present_event", eventOutRef: outRef(eventUtxo) },
  };
};

const prepareAt = async ({
  lucid,
  network,
  hubOraclePolicyId,
  minimumConfirmationDepth,
  evidence,
  owner,
  withdrawalIndex,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
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
 * re-authenticates the exact live outref or hub-bound event NFT and the family
 * adapter captures a locally evaluated transaction before any submit.
 */
export const createFabricatedWithdrawalEvidenceAuthority = ({
  lucid,
  network,
  hubOraclePolicyId,
  minimumConfirmationDepth,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
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
            (cause.code === "authentic_content_matches_commitment" ||
              cause.code === "event_not_due_for_block")
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
      const withdrawalId = Data.from(
        artifact.withdrawalInclusion.committedWithdrawalIdCbor,
        // This is the same exact V1 key decoded by step 01.
        OutputReference,
      );
      const current = await lucid.utxosByOutRef([
        {
          txHash: withdrawalId.transactionId,
          outputIndex: Number(withdrawalId.outputIndex),
        },
      ]);
      if (current.length > 1) {
        throw new Error(
          "fabricated-withdrawal artifact re-admission found duplicate original outrefs",
        );
      }
      if (artifact.l1Evidence.kind === "absent_identity") {
        const expected = `${withdrawalId.transactionId}#${withdrawalId.outputIndex.toString()}`;
        if (
          current.length !== 1 ||
          artifact.l1Evidence.unspentOutRef !== expected
        ) {
          throw new Error(
            "fabricated-withdrawal absence artifact is no longer authenticated by current L1",
          );
        }
      } else {
        if (current.length !== 0) {
          throw new Error(
            "fabricated-withdrawal event artifact conflicts with a live original outref",
          );
        }
        const hubOracleAddress = credentialToAddress(
          network,
          scriptHashToCredential(hubOraclePolicyId),
        );
        const hubOracleUtxo = await requireSingletonUtxo({
          lucid,
          address: hubOracleAddress,
          unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
          label: "fabricated-withdrawal hub oracle",
        });
        if (hubOracleUtxo.datum == null) {
          throw new Error(
            "fabricated-withdrawal hub oracle has no inline datum",
          );
        }
        const hub = Data.from(hubOracleUtxo.datum, HubOracleDatum);
        const nonce = await Effect.runPromise(
          withdrawalEventNonce(withdrawalId),
        );
        const eventUtxos = await lucid.utxosAtWithUnit(
          credentialToAddress(network, scriptHashToCredential(hub.withdrawal)),
          toUnit(hub.withdrawal, nonce),
        );
        const event = exactOne(
          eventUtxos,
          "fabricated-withdrawal event lookup",
        );
        if (
          event.datum == null ||
          outRef(event) !== artifact.l1Evidence.eventOutRef ||
          event.datum !== artifact.authenticContent.eventDatumCbor
        ) {
          throw new Error(
            "fabricated-withdrawal event artifact changed its authenticated L1 outref or datum",
          );
        }
      }
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
    ["eventDatumCbor"],
    "fabricated-withdrawal authentic content",
  );
  const l1 = plainRecord(
    outer.l1Evidence,
    (outer.l1Evidence as { readonly kind?: unknown })?.kind ===
      "absent_identity"
      ? ["kind", "unspentOutRef"]
      : ["kind", "eventOutRef"],
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
    (artifact.authenticContent.eventDatumCbor !== null &&
      !EVEN_HEX.test(artifact.authenticContent.eventDatumCbor)) ||
    !["absent_identity", "present_event"].includes(artifact.l1Evidence.kind) ||
    !OUT_REF.test(
      artifact.l1Evidence.kind === "absent_identity"
        ? artifact.l1Evidence.unspentOutRef
        : artifact.l1Evidence.eventOutRef,
    )
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
