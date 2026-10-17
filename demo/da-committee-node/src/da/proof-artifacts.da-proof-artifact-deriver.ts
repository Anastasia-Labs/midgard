import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  daDeploymentFingerprintFromHex,
  type DaEventToStepByEventRequest,
  type DaEventToStepByEventResponse,
  type DaProofBundleByHeaderRequest,
  type DaProofBundleByHeaderResponse,
  type DaTraceStepByIndexRequest,
  type DaTraceStepByIndexResponse,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import type { DaPayloadRecord, StateQueueHeaderRecord } from "../domain.js";
import { headerHashOf } from "../l1/follower/queue-derivation.js";
import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  computeDaPayloadRoots,
  type DaPayloadRootSet,
  daPayloadSha256,
  decodeDaPayloadStrict,
} from "./payload.js";
import {
  type DaProofArtifactDerivation,
  type DaProofArtifactDeriverOptions,
  type DaProofArtifactReasonCode,
  type DaProofArtifactStore,
  decodeCanonicalEventKey,
  encodeProofBundle,
  eventKeyFingerprint,
  type ProofRootSetInput,
  rejectedProofBundleResponse,
  type VerifiedPayloadResolution,
} from "./proof-artifacts.da-proof-artifact-reason-code.js";
import {
  buildEventToStepMembershipProof,
  buildEventToStepNonMembershipProof,
  countMismatches,
  encodeData,
  headerCborHex,
  headerCounts,
  headerRoots,
  membershipProof,
  normalizeRootSet,
  reconstructTraceProofs,
  rootMismatches,
} from "./proof-artifacts.reconstruct-trace-proofs.js";

export class DaProofArtifactDeriver {
  private readonly deploymentFingerprint: string;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly store: DaProofArtifactStore;

  constructor(options: DaProofArtifactDeriverOptions) {
    this.deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
      options.deploymentFingerprint,
    );
    this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
      this.deploymentFingerprint,
    );
    this.store = options.store;
  }

  async proofBundleByHeader(
    request: DaProofBundleByHeaderRequest,
  ): Promise<DaProofArtifactDerivation<DaProofBundleByHeaderResponse>> {
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: rejectedProofBundleResponse(
          headerHash,
          "deployment_fingerprint_mismatch",
        ),
      };
    }
    const resolution = await this.resolveVerifiedPayload(headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: {
          status: "not_found",
          headerHash,
          proofBundleHash: null,
          proofBundleBytes: null,
          chunkManifest: null,
          reasonCode: "stored_payload_not_found",
        },
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: rejectedProofBundleResponse(
          headerHash,
          resolution.reasonCode,
        ),
      };
    }
    const proofBundleBytes = encodeProofBundle(resolution.reconstruction);
    const proofBundleHash = computeDaSha256Hash(proofBundleBytes);
    if (proofBundleBytes.length > request.maxInlineBytes) {
      return {
        reasonCode: "proof_bundle_too_large_for_inline_response",
        response: {
          status: "rejected",
          headerHash,
          proofBundleHash,
          proofBundleBytes: null,
          chunkManifest: null,
          reasonCode: "proof_bundle_too_large_for_inline_response",
        },
      };
    }
    return {
      reasonCode: null,
      response: {
        status: "found_inline",
        headerHash,
        proofBundleHash,
        proofBundleBytes,
        chunkManifest: null,
        reasonCode: null,
      },
    };
  }

  async traceStepByIndex(
    request: DaTraceStepByIndexRequest,
  ): Promise<DaProofArtifactDerivation<DaTraceStepByIndexResponse>> {
    const absentResponse = (
      status: DaTraceStepByIndexResponse["status"],
    ): DaTraceStepByIndexResponse => ({
      status,
      headerHash: request.headerHash,
      stepIndex: request.stepIndex,
      transitionStepBytes: null,
      membershipProofBytes: null,
    });
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: absentResponse("rejected"),
      };
    }
    const resolution = await this.resolveVerifiedPayload(request.headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: absentResponse("not_found"),
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: absentResponse("rejected"),
      };
    }
    const entry = resolution.reconstruction.transitionTrace.get(
      BigInt(request.stepIndex),
    );
    if (entry === undefined) {
      return {
        reasonCode: "trace_step_not_found",
        response: absentResponse("not_found"),
      };
    }
    try {
      const proof = await membershipProof(
        resolution.reconstruction.rootData.transitionTrace,
        entry,
      );
      return {
        reasonCode: null,
        response: {
          status: "found",
          headerHash: request.headerHash,
          stepIndex: request.stepIndex,
          transitionStepBytes: entry.valueBytes,
          membershipProofBytes: encodeData(
            proof,
            SDK.TransitionTraceMembershipProofSchema,
          ),
        },
      };
    } catch {
      return {
        reasonCode: "witness_construction_failed",
        response: absentResponse("rejected"),
      };
    }
  }

  async eventToStepByEvent(
    request: DaEventToStepByEventRequest,
  ): Promise<DaProofArtifactDerivation<DaEventToStepByEventResponse>> {
    const absentResponse = (
      status: DaEventToStepByEventResponse["status"],
    ): DaEventToStepByEventResponse => ({
      status,
      headerHash: request.headerHash,
      eventKey: request.eventKey,
      eventToStepEntryBytes: null,
      membershipOrNonmembershipProofBytes: null,
    });
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: absentResponse("rejected"),
      };
    }
    const eventKey = decodeCanonicalEventKey(request.eventKey);
    if (eventKey === null) {
      return {
        reasonCode: "event_key_malformed",
        response: absentResponse("rejected"),
      };
    }
    const resolution = await this.resolveVerifiedPayload(request.headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: absentResponse("not_found"),
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: absentResponse("rejected"),
      };
    }
    const entry = resolution.reconstruction.eventToStep.get(
      eventKeyFingerprint(eventKey),
    );
    try {
      const proof =
        entry === undefined
          ? await buildEventToStepNonMembershipProof(
              resolution.reconstruction.rootData.eventToStep,
              eventKey,
            )
          : await buildEventToStepMembershipProof(
              resolution.reconstruction.rootData.eventToStep,
              entry,
            );
      return {
        reasonCode: entry === undefined ? "event_to_step_not_found" : null,
        response: {
          status: "found",
          headerHash: request.headerHash,
          eventKey: request.eventKey,
          eventToStepEntryBytes: entry?.valueBytes ?? null,
          membershipOrNonmembershipProofBytes: encodeData(
            proof,
            SDK.EventToStepProofSchema,
          ),
        },
      };
    } catch {
      return {
        reasonCode: "witness_construction_failed",
        response: absentResponse("rejected"),
      };
    }
  }

  private async resolveVerifiedPayload(
    headerHash: Buffer,
  ): Promise<VerifiedPayloadResolution> {
    const headerHashHex = headerHash.toString("hex");
    const record = await this.store.getDaPayload(headerHashHex);
    if (record === undefined) {
      return { kind: "missing" };
    }
    const recordCheck = this.validateRecordEnvelope(record, headerHashHex);
    if (recordCheck !== null) {
      return { kind: "rejected", reasonCode: recordCheck };
    }
    const committedHeader = await this.store.getStateQueueHeader(headerHashHex);
    if (committedHeader === undefined) {
      return { kind: "rejected", reasonCode: "committed_header_missing" };
    }
    const headerCheck = this.validateCommittedHeader(
      committedHeader,
      headerHashHex,
    );
    if (headerCheck !== null) {
      return { kind: "rejected", reasonCode: headerCheck };
    }
    let payloadBytes: Buffer;
    try {
      payloadBytes = hexToBytes(record.payloadCborHex, "stored payload CBOR");
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_bytes_malformed" };
    }
    if (payloadBytes.length === 0) {
      return { kind: "rejected", reasonCode: "stored_payload_bytes_malformed" };
    }
    let storedHash: string;
    try {
      storedHash = normalizeHex(record.payloadSha256, {
        fieldName: "stored payload sha256",
        byteLength: 32,
      });
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_hash_malformed" };
    }
    if (storedHash !== daPayloadSha256(payloadBytes)) {
      return { kind: "rejected", reasonCode: "stored_payload_hash_mismatch" };
    }
    if (record.payloadSchemaVersion !== 1) {
      return { kind: "rejected", reasonCode: "stored_payload_malformed" };
    }
    let payload: SDK.DaPayload;
    try {
      const unwrapped = await unwrapDaPayload(payloadBytes, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      });
      payload = decodeDaPayloadStrict(unwrapped.innerBytes);
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_malformed" };
    }
    if (
      payload.block_body.header_hash !== headerHashHex ||
      headerCborHex(payload.block_body.header) !==
        headerCborHex(committedHeader.header)
    ) {
      return { kind: "rejected", reasonCode: "payload_header_hash_mismatch" };
    }
    let roots: DaPayloadRootSet;
    try {
      roots = await computeDaPayloadRoots(payload);
    } catch {
      return {
        kind: "rejected",
        reasonCode: "payload_root_derivation_failed",
      };
    }
    if (
      rootMismatches(record.rootSummary as ProofRootSetInput, roots).length > 0
    ) {
      return { kind: "rejected", reasonCode: "stored_root_summary_mismatch" };
    }
    if (rootMismatches(headerRoots(committedHeader.header), roots).length > 0) {
      return { kind: "rejected", reasonCode: "committed_header_root_mismatch" };
    }
    if (
      countMismatches(
        headerCounts(committedHeader.header),
        payload.block_body.counts,
      ).length > 0
    ) {
      return {
        kind: "rejected",
        reasonCode: "committed_header_count_mismatch",
      };
    }
    try {
      return {
        kind: "found",
        reconstruction: await reconstructTraceProofs(payload, {
          headerHash: Buffer.from(headerHashHex, "hex"),
          payloadHash: Buffer.from(storedHash, "hex"),
          rootSummary: roots,
          countSummary: payload.block_body.counts,
        }),
      };
    } catch {
      return { kind: "rejected", reasonCode: "witness_construction_failed" };
    }
  }

  private validateRecordEnvelope(
    record: DaPayloadRecord,
    headerHashHex: string,
  ): DaProofArtifactReasonCode | null {
    if (record.validationStatus !== "verified") {
      return "stored_payload_not_verified";
    }
    if (record.rootSummary === undefined) {
      return "stored_root_summary_missing";
    }
    try {
      if (
        normalizeDaDeploymentFingerprintHex(record.deploymentFingerprint) !==
        this.deploymentFingerprint
      ) {
        return "record_deployment_fingerprint_mismatch";
      }
      if (
        normalizeHex(record.headerHash, {
          fieldName: "stored header hash",
          byteLength: 28,
        }) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
    } catch {
      return "record_header_hash_mismatch";
    }
    try {
      normalizeRootSet(
        record.rootSummary as ProofRootSetInput,
        "stored root summary",
      );
    } catch {
      return "stored_root_summary_malformed";
    }
    try {
      normalizeHex(record.payloadSha256, {
        fieldName: "stored payload sha256",
        byteLength: 32,
      });
    } catch {
      return "stored_payload_hash_malformed";
    }
    return null;
  }

  private validateCommittedHeader(
    record: StateQueueHeaderRecord,
    headerHashHex: string,
  ): DaProofArtifactReasonCode | null {
    try {
      if (
        normalizeDaDeploymentFingerprintHex(record.deploymentFingerprint) !==
        this.deploymentFingerprint
      ) {
        return "record_deployment_fingerprint_mismatch";
      }
      if (
        normalizeHex(record.headerHash, {
          fieldName: "committed header hash",
          byteLength: 28,
        }) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
      if (
        normalizeHex(record.computedHeaderHash, {
          fieldName: "computed header hash",
          byteLength: 28,
        }) !== headerHashHex ||
        headerHashOf(record.header) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
    } catch {
      return "record_header_hash_mismatch";
    }
    return null;
  }

  private matchesDeployment(value: Buffer): boolean {
    return value.equals(this.deploymentFingerprintBytes);
  }
}
