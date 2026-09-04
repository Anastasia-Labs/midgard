import { createHash } from "node:crypto";

import {
  type AuthenticatedStateQueueHeaderObservation,
  depositEventNonce,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  OutputReference,
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

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FabricatedDepositL1Witness,
  FabricatedDepositRejection,
  prepareFabricatedDepositFromCommittedLeaves,
} from "../prepare-fabricated-deposit.js";
import { requireSingletonUtxo } from "../runtime.js";
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
  authenticContent: Readonly<{ eventDatumCbor: string | null }>;
  l1Evidence:
    | Readonly<{ kind: "absent_identity"; unspentOutRef: string }>
    | Readonly<{ kind: "present_event"; eventOutRef: string }>;
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
    .update(JSON.stringify(value.depositInclusion))
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
  depositId,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly depositId: Readonly<{
    transactionId: string;
    outputIndex: bigint;
  }>;
}): Promise<{
  readonly witness: FabricatedDepositL1Witness;
  readonly l1Evidence: FabricatedDepositArtifact["l1Evidence"];
}> => {
  const candidateOutRef = `${depositId.transactionId}#${depositId.outputIndex.toString()}`;
  const live = await lucid.utxosByOutRef([
    {
      txHash: depositId.transactionId,
      outputIndex: Number(depositId.outputIndex),
    },
  ]);
  if (live.length > 1) {
    throw new Error("fabricated-deposit L1 lookup returned duplicate outrefs");
  }
  if (live.length === 1) {
    return {
      witness: {
        kind: "absent_identity",
        observation,
        liveOutputReferences: [depositId],
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
    label: "fabricated-deposit hub oracle",
  });
  if (hubOracleUtxo.datum == null) {
    throw new Error("fabricated-deposit hub oracle has no inline datum");
  }
  const hub = Data.from(hubOracleUtxo.datum, HubOracleDatum);
  const nonce = await Effect.runPromise(depositEventNonce(depositId));
  const eventUnit = toUnit(hub.deposit, nonce);
  const depositAddress = credentialToAddress(
    network,
    scriptHashToCredential(hub.deposit),
  );
  const eventUtxo = exactOne(
    await lucid.utxosAtWithUnit(depositAddress, eventUnit),
    "fabricated-deposit event lookup",
  );
  if (eventUtxo.datum == null) {
    throw new Error("fabricated-deposit event output has no inline datum");
  }
  return {
    witness: {
      kind: "present_event",
      observation,
      depositEventPolicyId: hub.deposit,
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
  depositIndex,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
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
 * re-authenticates the exact live outref or hub-bound event NFT and the family
 * adapter captures a locally evaluated transaction before any submit.
 */
export const createFabricatedDepositEvidenceAuthority = ({
  lucid,
  network,
  hubOraclePolicyId,
  minimumConfirmationDepth,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
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
      const depositId = Data.from(
        artifact.depositInclusion.committedDepositIdCbor,
        // This is the same exact V1 key decoded by step 01.
        OutputReference,
      );
      const current = await lucid.utxosByOutRef([
        {
          txHash: depositId.transactionId,
          outputIndex: Number(depositId.outputIndex),
        },
      ]);
      if (current.length > 1) {
        throw new Error(
          "fabricated-deposit artifact re-admission found duplicate original outrefs",
        );
      }
      if (artifact.l1Evidence.kind === "absent_identity") {
        const expected = `${depositId.transactionId}#${depositId.outputIndex.toString()}`;
        if (
          current.length !== 1 ||
          artifact.l1Evidence.unspentOutRef !== expected
        ) {
          throw new Error(
            "fabricated-deposit absence artifact is no longer authenticated by current L1",
          );
        }
      } else {
        if (current.length !== 0) {
          throw new Error(
            "fabricated-deposit event artifact conflicts with a live original outref",
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
          label: "fabricated-deposit hub oracle",
        });
        if (hubOracleUtxo.datum == null) {
          throw new Error("fabricated-deposit hub oracle has no inline datum");
        }
        const hub = Data.from(hubOracleUtxo.datum, HubOracleDatum);
        const nonce = await Effect.runPromise(depositEventNonce(depositId));
        const eventUtxos = await lucid.utxosAtWithUnit(
          credentialToAddress(network, scriptHashToCredential(hub.deposit)),
          toUnit(hub.deposit, nonce),
        );
        const event = exactOne(eventUtxos, "fabricated-deposit event lookup");
        if (
          event.datum == null ||
          outRef(event) !== artifact.l1Evidence.eventOutRef ||
          event.datum !== artifact.authenticContent.eventDatumCbor
        ) {
          throw new Error(
            "fabricated-deposit event artifact changed its authenticated L1 outref or datum",
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
    ["eventDatumCbor"],
    "fabricated-deposit authentic content",
  );
  const l1 = plainRecord(
    outer.l1Evidence,
    (outer.l1Evidence as { readonly kind?: unknown })?.kind ===
      "absent_identity"
      ? ["kind", "unspentOutRef"]
      : ["kind", "eventOutRef"],
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
    (artifact.authenticContent.eventDatumCbor !== null &&
      !EVEN_HEX.test(artifact.authenticContent.eventDatumCbor)) ||
    !["absent_identity", "present_event"].includes(artifact.l1Evidence.kind) ||
    !OUT_REF.test(
      artifact.l1Evidence.kind === "absent_identity"
        ? artifact.l1Evidence.unspentOutRef
        : artifact.l1Evidence.eventOutRef,
    )
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
