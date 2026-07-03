import * as SDK from "@al-ft/midgard-sdk";

import type {
  DaPayloadRecord,
  DaSignatureRecord,
  PayloadCountSet,
  PayloadRootSet,
  StateQueueHeaderRecord,
  ValidationSummary,
} from "../domain.js";
import {
  classifyDaAttestationMarker,
  formatUnexpectedDaAttestationMarker,
} from "../l1/attestation-marker.js";
import { validateDaSignatureRecord } from "../peer/signatures.js";
import type { DaCommitteeValidation } from "../signer.js";
import type { WatcherStore } from "../store.js";
import type {
  DaAttestationContext,
  ReconcileAttestationArgs,
} from "./on-chain.js";

export type SubmitterReconcileStatus = "reconciled" | "post_failed" | "skipped";

export type SubmitterReconcileResult = {
  readonly status: SubmitterReconcileStatus;
  readonly reason?: string;
};

export type SubmitterReconcilerDeps = {
  readonly deploymentFingerprint: string;
  readonly committeeValidation: DaCommitteeValidation;
  readonly daAttestationPolicyId: string;
  readonly store: Pick<WatcherStore, "getDaPayload" | "listDaSignatures">;
  readonly coordinator: {
    readonly reconcileAttestation: (
      args: ReconcileAttestationArgs,
    ) => Promise<"posted" | "post_failed">;
    readonly lastPublishError?: (
      record: Pick<DaSignatureRecord, "headerHash">,
    ) => string | undefined;
  };
  readonly peerPoller?: {
    readonly pollPeerSignatures: (headerHash: string) => Promise<void>;
  };
  readonly submitterId?: string;
};

export class SubmitterReconciler {
  private readonly deps: SubmitterReconcilerDeps;

  constructor(deps: SubmitterReconcilerDeps) {
    this.deps = deps;
  }

  async reconcileHeader(
    header: StateQueueHeaderRecord,
  ): Promise<SubmitterReconcileResult> {
    const marker = classifyDaAttestationMarker(
      header.daAttestation,
      this.deps.daAttestationPolicyId,
    );
    if (
      header.finalized &&
      header.status === "attested" &&
      marker.kind === "already_attested_expected"
    ) {
      return { status: "reconciled", reason: "already attested" };
    }
    if (
      header.finalized &&
      (marker.kind === "already_attested_foreign" || marker.kind === "invalid")
    ) {
      return {
        status: "post_failed",
        reason: formatUnexpectedDaAttestationMarker(header.headerHash, marker),
      };
    }
    if (!isReconcilableHeader(header)) {
      return { status: "skipped", reason: "header is not in submitter scope" };
    }
    const payload = await this.deps.store.getDaPayload(header.headerHash);
    if (!isVerifiedPayload(payload)) {
      return { status: "skipped", reason: "verified payload is not available" };
    }
    await this.deps.peerPoller?.pollPeerSignatures(header.headerHash);
    const witnessHexes = await this.validWitnessHexes(
      header.headerHash,
      payload,
    );
    const context = contextFromHeader({
      deploymentFingerprint: this.deps.deploymentFingerprint,
      committeeSignersHash: this.deps.committeeValidation.committeeSignersHash,
      header,
      payload,
    });
    const result = await this.deps.coordinator.reconcileAttestation({
      context,
      witnessHexes,
      requireThresholdWitnesses: true,
      submitterId: this.deps.submitterId,
    });
    return result === "posted"
      ? { status: "reconciled" }
      : {
          status: "post_failed",
          reason:
            this.deps.coordinator.lastPublishError?.({
              headerHash: header.headerHash,
            }) ?? "L1 reconciliation failed",
        };
  }

  private async validWitnessHexes(
    headerHash: string,
    payload: DaPayloadRecord,
  ): Promise<readonly string[]> {
    const selected = new Map<number, string>();
    for (const signature of await this.deps.store.listDaSignatures(
      headerHash,
    )) {
      const validationError = validateDaSignatureRecord({
        body: signature,
        headerHash,
        deploymentFingerprint: this.deps.deploymentFingerprint,
        signerValidation: this.deps.committeeValidation,
        verifiedPayload: payload,
      });
      if (validationError !== undefined) {
        continue;
      }
      if (!selected.has(signature.signerIndex)) {
        selected.set(signature.signerIndex, signature.signatureWitness);
      }
    }
    return [...selected.values()];
  }
}

const isReconcilableHeader = (header: StateQueueHeaderRecord): boolean =>
  header.finalized &&
  (header.status === "unattested" || header.status === "attesting");

const isVerifiedPayload = (
  payload: DaPayloadRecord | undefined,
): payload is DaPayloadRecord =>
  payload !== undefined &&
  payload.validationStatus === "verified" &&
  payload.payloadSha256.length > 0;

const contextFromHeader = ({
  deploymentFingerprint,
  committeeSignersHash,
  header,
  payload,
}: {
  readonly deploymentFingerprint: string;
  readonly committeeSignersHash: string;
  readonly header: StateQueueHeaderRecord;
  readonly payload: DaPayloadRecord;
}): DaAttestationContext => ({
  deploymentFingerprint,
  headerHash: header.headerHash,
  payloadHash: payload.payloadSha256,
  committeeSignersHash,
  l1ChainPoint: header.observedChainPoint,
  validation: validationSummaryFromHeader(
    header,
    payload.rootSummary ?? rootSummaryFromHeader(header),
  ),
});

const validationSummaryFromHeader = (
  header: StateQueueHeaderRecord,
  rootSummary: PayloadRootSet,
): ValidationSummary => ({
  payloadVersion: Number(SDK.DA_PAYLOAD_V2_VERSION),
  rootsMatch: true,
  stateQueueOutRef: header.stateQueueOutRef,
  headerHash: header.headerHash,
  rootSummary,
  countSummary: countSummaryFromHeader(header),
  l1Header: {
    startTime: header.header.startTime.toString(),
    endTime: header.header.endTime.toString(),
    operatorVkey: header.header.operatorVkey,
    prevHeaderHash: header.header.prevHeaderHash,
    protocolVersion: header.header.protocolVersion.toString(),
  },
});

const rootSummaryFromHeader = (
  header: StateQueueHeaderRecord,
): PayloadRootSet => ({
  utxosRoot: header.header.utxosRoot,
  transactionsRoot: header.header.transactionsRoot,
  depositsRoot: header.header.depositsRoot,
  withdrawalsRoot: header.header.withdrawalsRoot,
  forcedTransactionsRoot: header.header.forcedTransactionsRoot,
  transitionTraceRoot: header.header.transitionTraceRoot,
  eventToStepRoot: header.header.eventToStepRoot,
});

const countSummaryFromHeader = (
  header: StateQueueHeaderRecord,
): PayloadCountSet => ({
  withdrawalCount: header.header.withdrawalCount,
  forcedTransactionCount: header.header.forcedTransactionCount,
  l2TransactionCount: header.header.l2TransactionCount,
  depositCount: header.header.depositCount,
  totalEventCount: header.header.totalEventCount,
  transitionStepCount: header.header.transitionStepCount,
});
