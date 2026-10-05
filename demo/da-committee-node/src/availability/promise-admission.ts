import {
  type AvailabilityResponseAdmissionBoundary,
  type AvailabilityResponseAdmissionDecision,
  type AvailabilityResponseAdmissionInput,
  type AvailabilityResponseObligation,
  availabilityResponsePublicationCount,
} from "@al-ft/midgard-core";
import type { AvailabilityOperationActorSnapshot } from "@al-ft/midgard-core/availability-operation-journal";
import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeConfig } from "../config.js";
import type { StateQueueHeaderRecord, ValidationSummary } from "../domain.js";
import { hashBlockHeader } from "../l1/state-queue-scanner.js";
import { validateDaSignatureRecord } from "../peer/signatures.js";
import type { DaSignerValidation } from "../signer.js";
import type { CommitteeStore } from "../store.js";
import {
  committeePromiseAdmissionDecision,
  committeePromisePolicyEnvelope,
} from "./promise-admission-decision.js";
import { committeeAdmissionReads } from "./promise-admission-reads.js";
import { committeePromiseOwnedRead } from "./promise-owned-read.js";
import type { CommitteePromiseSigningBoundaryIdentity } from "./promise-retirement-admission.js";
import type { CommitteePromiseRuntimePolicyAuthority } from "./promise-runtime-policy.js";
import type { AvailabilityResponderChallenge } from "./responder.js";
import { retainedAvailabilityPayload } from "./retained-payload.js";

export type CommitteePromiseAdmissionCandidate = Readonly<{
  record: StateQueueHeaderRecord;
  commitment: SDK.DaAvailabilityCommitment;
  commitmentDigest: string;
  /** Supplied by production verification before the fresh-signing gate. */
  verifiedPayload?: Readonly<{
    payloadHash: string;
    validation: ValidationSummary;
  }>;
}>;

export type CommitteePromiseAdmissionBoundary =
  AvailabilityResponseAdmissionBoundary &
    CommitteePromiseSigningBoundaryIdentity;

/** One complete canonical discovery, including actor-wide durable blocking. */
export type CommitteePromiseAdmissionSnapshot = Readonly<{
  boundary: CommitteePromiseAdmissionBoundary;
  canonicalTimeMs: number;
  /** Computed from durable commitment-bound cutoff certificates at this exact boundary. */
  retiredCommitmentDigests?: ReadonlySet<string>;
  /** Fresh canonical timing set only; excluded members retain full protected resources. */
  currentSchedulingCommitmentDigests?: ReadonlySet<string>;
  challenges: readonly AvailabilityResponderChallenge[];
  retainedAttempts?: AvailabilityOperationActorSnapshot["retainedAttempts"];
  complete: boolean;
  blocking: AvailabilityResponseAdmissionInput["blocking"];
  resourceWorkload?: Readonly<{
    walletInputs: number;
    journalEntries: number;
    challengeRecords: number;
    storeRecords: number;
    storeEncodedBytes: number;
  }>;
}>;

export type CommitteePromiseAdmissionSource = Readonly<{
  /** Absent until runtime bindings and conditional calibration are adopted. */
  policyAuthority?: CommitteePromiseRuntimePolicyAuthority;
  /** One aggregate scope retained through the final pre-signing fence. */
  openReadScope?: () => SDK.DaAvailabilityReadScope;
  drainReadResources?: (scope?: SDK.DaAvailabilityReadScope) => Promise<void>;
  readResourceWorkload?: (scope?: SDK.DaAvailabilityReadScope) => Promise<
    Readonly<{
      walletInputs: number;
      journalEntries: number;
      storeRecords: number;
      storeEncodedBytes: number;
    }>
  >;
  projectResources?: (
    candidate: CommitteePromiseAdmissionCandidate,
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<Readonly<{ storeRecords: number; storeEncodedBytes: number }>>;
  readSnapshot: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<CommitteePromiseAdmissionSnapshot>;
  assertCurrent: (
    boundary: CommitteePromiseAdmissionBoundary,
    scope?: SDK.DaAvailabilityReadScope,
    candidate?: CommitteePromiseAdmissionCandidate,
  ) => Promise<void>;
}>;

export type CommitteePromiseAdmissionResult = Readonly<{
  decision: AvailabilityResponseAdmissionDecision;
  /** Recheck the exact point and generation immediately before signing. */
  assertCurrent?: () => Promise<void>;
}>;

export type CommitteePromiseAdmission = Readonly<{
  /** Advisory policy health, never a capacity reservation for a candidate. */
  policyStatus: () => CommitteePromiseAdmissionPolicyStatus;
  check: (
    candidate: CommitteePromiseAdmissionCandidate,
  ) => Promise<CommitteePromiseAdmissionResult>;
}>;

export type CommitteePromiseAdmissionPolicyStatus = Readonly<
  | { status: "unavailable"; reason: string }
  | { status: "conditional"; policyId: string; envelopeId: string }
>;

const scalar = (value: bigint): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error("admission scalar is out of range");
  return Number(value);
};
const digest = (commitment: SDK.DaAvailabilityCommitment): string =>
  computeDaSha256Hash(
    Buffer.from(SDK.encodeDaAvailabilityCommitment(commitment), "hex"),
  ).toString("hex");

const potential = (
  commitment: SDK.DaAvailabilityCommitment,
  config: CommitteeConfig,
): AvailabilityResponseObligation => {
  const classes = config.availabilityChallenge.responseClasses;
  const bytes = scalar(commitment.payload_byte_length);
  if (bytes > classes.fullPayloadMaxBytes)
    throw new Error("promise exceeds authenticated response classes");
  return {
    headerHash: commitment.header_hash,
    commitmentDigest: digest(commitment),
    kind: "potential",
    remainingPublications: availabilityResponsePublicationCount(
      commitment.tranche_descriptors.map((item) => scalar(item.byte_length)),
      scalar(commitment.response_geometry.chunk_byte_length),
    ),
    remainingSettlements: commitment.tranche_descriptors.length,
    remainingCloses: 1,
    responseWindowMs:
      bytes <= classes.smallPayloadMaxBytes
        ? classes.smallResponseWindowMs
        : classes.fullResponseWindowMs,
  };
};

const active = (
  challenge: AvailabilityResponderChallenge,
  config: CommitteeConfig,
  currentProgress = false,
): AvailabilityResponseObligation => {
  const commitment = challenge.record.datum.commitment;
  // Discovery authenticates these datums against the exact live queue. Do not
  // turn missing or timed-out state into zero remaining interference.
  SDK.parseDaAvailabilityCommitmentCbor(
    SDK.encodeDaAvailabilityCommitment(commitment),
    SDK.availabilityResponseGeometry(
      config.availabilityChallenge.responseGeometry,
    ),
  );
  const terminal = challenge.terminal.datum;
  const next = scalar(terminal.next_tranche_index);
  if (
    terminal.has_timed_out_tranche ||
    next > commitment.tranche_descriptors.length
  )
    throw new Error("unresolved terminal response state");
  const remaining: number[] = [];
  for (const descriptor of commitment.tranche_descriptors.slice(next)) {
    const matches = challenge.tranches.filter(
      ({ datum }) =>
        ("Active" in datum ? datum.Active : datum.Receipt).descriptor
          .tranche_index === descriptor.tranche_index,
    );
    if (matches.length !== 1)
      throw new Error("response tranche evidence is incomplete");
    const datum = matches[0]!.datum;
    const fields = "Active" in datum ? datum.Active : datum.Receipt;
    if (
      Object.entries(descriptor).some(
        ([key, value]) =>
          fields.descriptor[key as keyof typeof descriptor] !== value,
      )
    )
      throw new Error("response descriptor differs from signed commitment");
    if ("Active" in datum) {
      const end = descriptor.start_offset + descriptor.byte_length;
      const offset = datum.Active.next_offset;
      if (
        offset < descriptor.start_offset ||
        offset > end ||
        (offset < end &&
          (offset - descriptor.start_offset) %
            commitment.response_geometry.chunk_byte_length !==
            0n)
      )
        throw new Error("response offset is not a signed chunk boundary");
      remaining.push(scalar(end - offset));
    }
  }
  const publications = availabilityResponsePublicationCount(
    remaining,
    scalar(commitment.response_geometry.chunk_byte_length),
  );
  if (currentProgress)
    return {
      ...potential(commitment, config),
      kind: publications === 0 ? "completion" : "active",
      remainingPublications: publications,
      remainingSettlements: commitment.tranche_descriptors.length - next,
      remainingCloses: 1,
      responseDeadlineMs: scalar(challenge.record.datum.response_deadline),
    };
  // Shallow publications/settlements can roll back. Reserve their complete
  // signed demand until a k-safe terminal or cutoff retires the promise.
  return {
    ...potential(commitment, config),
    kind: publications === 0 ? "potential" : "active",
    ...(publications === 0
      ? {}
      : {
          responseDeadlineMs: scalar(challenge.record.datum.response_deadline),
        }),
  };
};

/** Rebuild liabilities from durable signatures on every check and restart. */
export const committeePromiseAdmission = (args: {
  config: CommitteeConfig;
  store: CommitteeStore;
  signerValidation: DaSignerValidation;
  source: CommitteePromiseAdmissionSource;
  now?: () => number;
}): CommitteePromiseAdmission => ({
  policyStatus: () =>
    args.source.policyAuthority?.status() ?? {
      status: "unavailable",
      reason: "finite_runtime_policy_unavailable",
    },
  check: async (candidate) => {
    const base = {
      deploymentId: args.config.deploymentFingerprint,
      candidateHeaderHash: candidate.record.headerHash,
      candidateCommitmentDigest: candidate.commitmentDigest,
    };
    let scope: SDK.DaAvailabilityReadScope | undefined;
    const reads = committeeAdmissionReads(args.source);
    const incomplete = reads.incomplete(base, () => scope);
    const authority = args.source.policyAuthority;
    if (authority?.status().status !== "conditional")
      return incomplete("finite_runtime_policy_unavailable");
    if (
      authority.binding?.deploymentFingerprint !==
        args.config.deploymentFingerprint ||
      authority.binding?.contractManifestId !==
        args.config.contractDeploymentInfo.manifestId
    )
      return incomplete("runtime_policy_deployment_binding_mismatch");
    const read = <T>(callback: () => Promise<T>) =>
      committeePromiseOwnedRead(scope)(callback);
    try {
      scope = args.source.openReadScope?.();
      // Snapshot may persist conservative cutoff evidence; never race that write.
      const snapshot = await args.source.readSnapshot(scope);
      scope?.assertCurrent();
      const obligations: AvailabilityResponseObligation[] = [];
      const seen = new Set<string>();
      let retainedPayloadBytes = 0;
      for (const record of await read(() => args.store.listDaSignatures())) {
        const witnessIndex = Number.parseInt(
          record.signatureWitness.slice(0, 2),
          16,
        );
        if (
          record.signerIndex !== args.signerValidation.signerIndex &&
          witnessIndex !== args.signerValidation.signerIndex
        )
          continue;
        const commitment = SDK.parseDaAvailabilityCommitmentCbor(
          record.availabilityCommitmentCbor,
          SDK.availabilityResponseGeometry(
            args.config.availabilityChallenge.responseGeometry,
          ),
        );
        const payload = await read(() =>
          args.store.getDaPayload(record.headerHash),
        );
        if (
          record.signerIndex !== args.signerValidation.signerIndex ||
          commitment.header_hash !== record.headerHash ||
          commitment.deployment_identity !== args.config.hubOraclePolicyId ||
          digest(commitment) !== record.availabilityCommitmentDigest ||
          validateDaSignatureRecord({
            body: record,
            headerHash: record.headerHash,
            deploymentFingerprint: args.config.deploymentFingerprint,
            signerValidation: args.signerValidation,
            verifiedPayload: payload,
          }) !== undefined
        )
          throw new Error(
            "durable own-signer promise is invalid or belongs to another deployment",
          );
        const header = await read(() =>
          args.store.getStateQueueHeader(record.headerHash),
        );
        if (
          header === undefined ||
          header.deploymentFingerprint !== args.config.deploymentFingerprint ||
          hashBlockHeader(header.header) !== record.headerHash
        )
          throw new Error("signed header evidence is unavailable");
        const challenges = snapshot.challenges.filter(
          (item) =>
            digest(item.record.datum.commitment) ===
            record.availabilityCommitmentDigest,
        );
        // Source re-verifies persisted protected floors at this exact boundary.
        if (
          challenges.length === 0 &&
          snapshot.complete &&
          snapshot.retiredCommitmentDigests?.has(
            record.availabilityCommitmentDigest,
          )
        )
          continue;
        if (
          (await retainedAvailabilityPayload({
            store: {
              getDaPayload: (hash) => read(() => args.store.getDaPayload(hash)),
            },
            deploymentFingerprint: args.config.deploymentFingerprint,
            deploymentIdentity: args.config.hubOraclePolicyId,
            commitment,
          })) === undefined
        )
          throw new Error("signed promise bytes are unavailable");
        if (!seen.has(record.availabilityCommitmentDigest)) {
          obligations.push(potential(commitment, args.config));
          retainedPayloadBytes += scalar(commitment.payload_byte_length);
        }
        seen.add(record.availabilityCommitmentDigest);
      }
      for (const challenge of snapshot.challenges) {
        const commitment = challenge.record.datum.commitment;
        if (
          commitment.deployment_identity !== args.config.hubOraclePolicyId ||
          (await retainedAvailabilityPayload({
            store: {
              getDaPayload: (hash) => read(() => args.store.getDaPayload(hash)),
            },
            deploymentFingerprint: args.config.deploymentFingerprint,
            deploymentIdentity: args.config.hubOraclePolicyId,
            commitment,
          })) === undefined
        )
          throw new Error(
            "active response deployment or retained bytes are unavailable",
          );
        if (!seen.has(digest(commitment)))
          retainedPayloadBytes += scalar(commitment.payload_byte_length);
        seen.add(digest(commitment));
        obligations.push(active(challenge, args.config));
      }
      if (
        candidate.commitment.header_hash !== candidate.record.headerHash ||
        candidate.commitment.deployment_identity !==
          args.config.hubOraclePolicyId ||
        digest(candidate.commitment) !== candidate.commitmentDigest ||
        hashBlockHeader(candidate.record.header) !== candidate.record.headerHash
      )
        throw new Error("candidate commitment binding is invalid");
      const candidateObligation = potential(candidate.commitment, args.config);
      const unique = new Map(
        obligations.map((item) => [item.commitmentDigest, item]),
      );
      if (!unique.has(candidate.commitmentDigest))
        retainedPayloadBytes += scalar(
          candidate.commitment.payload_byte_length,
        );
      unique.set(candidate.commitmentDigest, candidateObligation);
      if (snapshot.resourceWorkload === undefined)
        return incomplete("runtime_workload_evidence_unavailable");
      const reserve = await args.source.projectResources?.(candidate, scope);
      scope?.assertCurrent();
      const workload = {
        ...snapshot.resourceWorkload,
        storeRecords:
          snapshot.resourceWorkload.storeRecords + (reserve?.storeRecords ?? 0),
        storeEncodedBytes:
          snapshot.resourceWorkload.storeEncodedBytes +
          (reserve?.storeEncodedBytes ?? 0),
        retainedPayloadBytes,
        outstandingPromises: unique.size,
        tranches: [...unique.values()].reduce(
          (sum, item) => sum + item.remainingSettlements,
          0,
        ),
        publications: [...unique.values()].reduce(
          (sum, item) => sum + item.remainingPublications,
          0,
        ),
      };
      if (args.source.projectResources) {
        const futureRows = authority.futureIntentRows?.(workload);
        if (futureRows === undefined)
          return incomplete("prospective_actor_intent_growth_unknown");
        workload.journalEntries += futureRows;
      }
      const policy = committeePromisePolicyEnvelope(authority, workload);
      if (policy === undefined)
        return incomplete("runtime_policy_domain_exceeded_or_unavailable");
      const decide = (currentWorkload = workload) =>
        committeePromiseAdmissionDecision({
          authority,
          workload: currentWorkload,
          snapshot,
          deploymentId: base.deploymentId,
          nowMs: (args.now ?? Date.now)(),
          obligations,
          candidate: candidateObligation,
          currentProgress: snapshot.challenges.map((challenge) =>
            active(challenge, args.config, true),
          ),
        });
      const decision = decide();
      if (decision.status !== "admitted") await reads.refuse(scope);
      return {
        decision,
        ...(decision.status === "admitted"
          ? {
              assertCurrent: () =>
                reads.final(scope, async () => {
                  let currentWorkload = workload;
                  if (args.source.readResourceWorkload) {
                    const resources =
                      await args.source.readResourceWorkload(scope);
                    const reserve = await args.source.projectResources?.(
                      candidate,
                      scope,
                    );
                    currentWorkload = {
                      ...workload,
                      ...resources,
                      storeRecords:
                        resources.storeRecords + (reserve?.storeRecords ?? 0),
                      storeEncodedBytes:
                        resources.storeEncodedBytes +
                        (reserve?.storeEncodedBytes ?? 0),
                    };
                    const rows =
                      args.source.policyAuthority?.futureIntentRows?.(
                        currentWorkload,
                      );
                    if (rows === undefined)
                      throw new Error(
                        "Final actor intent growth is unavailable",
                      );
                    currentWorkload.journalEntries += rows;
                  }
                  await args.source.assertCurrent(
                    snapshot.boundary,
                    scope,
                    candidate,
                  );
                  scope?.assertCurrent();
                  if (
                    args.source.policyAuthority?.causal &&
                    decide(currentWorkload).status !== "admitted"
                  )
                    throw new Error(
                      "Causal response prefix changed before signing",
                    );
                  if (
                    (
                      args.source.policyAuthority &&
                      committeePromisePolicyEnvelope(
                        args.source.policyAuthority,
                        currentWorkload,
                      )
                    )?.envelopeId !== policy.envelopeId
                  )
                    throw new Error(
                      "Promise runtime policy changed before signing",
                    );
                }),
            }
          : {}),
      };
    } catch (error) {
      return incomplete(
        `canonical_admission_evidence_unavailable: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  },
});
