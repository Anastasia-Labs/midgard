import type { WatcherConfig } from "./config.js";
import type { AttestationCoordinator } from "./coordinator/coordinator.js";
import type { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import { DaPayloadClient } from "./da/client.js";
import {
  daPayloadSha256,
  DaPayloadValidationError,
  type TransactionRootValueProjector,
  type VerifiedDaPayload,
  verifyDaPayloadAgainstHeader,
} from "./da/payload.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import type { DaAttestationChainReader } from "./l1/da-attestation-reader.js";
import {
  scanStateQueue,
  type StateQueueProvider,
} from "./l1/state-queue-scanner.js";
import {
  type DaSigner,
  type DaSignerValidation,
  signDaAttestation,
} from "./signer.js";
import type { WatcherStore } from "./store.js";

export type WatcherServiceDeps = {
  readonly config: WatcherConfig;
  readonly store: WatcherStore;
  readonly stateQueueProvider: StateQueueProvider;
  readonly payloadClient: DaPayloadClient;
  readonly signer?: DaSigner;
  readonly signerValidation?: DaSignerValidation;
  readonly coordinator?: AttestationCoordinator;
  readonly submitterReconciler?: Pick<SubmitterReconciler, "reconcileHeader">;
  readonly daChainReader?: DaAttestationChainReader;
  readonly transactionProjector?: TransactionRootValueProjector;
};

export type WatcherTickResult = {
  readonly scannedHeaders: number;
  readonly signedHeaders: number;
  readonly reconciledHeaders: number;
  readonly skippedHeaders: number;
  readonly errors: readonly string[];
};

type SignedHeaderResult = {
  readonly signature: DaSignatureRecord;
  readonly publishError?: string;
};

type CoordinatorPublishResult = {
  readonly broadcastStatus: DaSignatureRecord["broadcastStatus"];
  readonly error?: string;
};

export class WatcherService {
  private readonly deps: WatcherServiceDeps;
  private tickInFlight?: Promise<WatcherTickResult>;

  constructor(deps: WatcherServiceDeps) {
    this.deps = deps;
  }

  async initialize(): Promise<void> {
    await this.deps.store.initDeployment({
      fingerprint: this.deps.config.deploymentFingerprint,
      manifestSha256: this.deps.config.deploymentManifestSha256,
      manifestRaw: this.deps.config.deploymentManifestRaw,
    });
    if (this.deps.daChainReader !== undefined) {
      const daParams = await this.deps.daChainReader.fetchDaParams();
      if (daParams.committeeHex !== this.deps.config.daParams.committeeHex) {
        throw new Error("on-chain DA committee does not match watcher config");
      }
      if (
        daParams.committeeSignersHash !==
        this.deps.config.daParams.committeeSignersHash
      ) {
        throw new Error(
          "on-chain DA committee_signers_hash does not match watcher config",
        );
      }
      if (daParams.threshold !== this.deps.config.daParams.threshold) {
        throw new Error("on-chain DA threshold does not match watcher config");
      }
    }
  }

  async tick(): Promise<WatcherTickResult> {
    if (this.tickInFlight !== undefined) {
      return this.tickInFlight;
    }
    const tickPromise = this.tickOnce().finally(() => {
      if (this.tickInFlight === tickPromise) {
        this.tickInFlight = undefined;
      }
    });
    this.tickInFlight = tickPromise;
    return tickPromise;
  }

  private async tickOnce(): Promise<WatcherTickResult> {
    const records = await scanStateQueue(this.deps.stateQueueProvider, {
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      daAttestationPolicyId: this.deps.config.daAttestationPolicyId,
      finalityDepth: this.deps.config.finalityDepth,
    });
    const errors: string[] = [];
    let signedHeaders = 0;
    let reconciledHeaders = 0;
    let skippedHeaders = 0;
    for (const record of records) {
      await this.deps.store.upsertStateQueueHeader(record);
      if (
        this.deps.signer === undefined ||
        this.deps.signerValidation === undefined ||
        this.deps.config.signerIndex === undefined
      ) {
        await this.ensurePayloadForSubmitter(record, errors);
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      const existingSignature = await this.deps.store.getDaSignature({
        headerHash: record.headerHash,
        signerIndex: this.deps.config.signerIndex,
      });
      if (existingSignature !== undefined) {
        if (
          this.shouldRepublishExistingSignatureForHeader(
            existingSignature,
            record.status,
          )
        ) {
          const published = await this.publishSignature(existingSignature);
          if (published.broadcastStatus !== existingSignature.broadcastStatus) {
            await this.deps.store.saveDaSignature({
              ...existingSignature,
              broadcastStatus: published.broadcastStatus,
            });
          }
          if (published.broadcastStatus === "post_failed") {
            errors.push(
              published.error ??
                this.coordinatorPostFailedMessage(existingSignature),
            );
          }
        }
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      if (record.status !== "unattested" || !record.finalized) {
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      try {
        const { signature, publishError } =
          await this.fetchVerifyAndSign(record);
        await this.deps.store.saveDaSignature(signature);
        signedHeaders += 1;
        if (signature.broadcastStatus === "post_failed") {
          errors.push(
            publishError ?? this.coordinatorPostFailedMessage(signature),
          );
        }
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
      } catch (error) {
        skippedHeaders += 1;
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }
    return {
      scannedHeaders: records.length,
      signedHeaders,
      reconciledHeaders,
      skippedHeaders,
      errors,
    };
  }

  private async fetchVerifyAndSign(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
  ): Promise<SignedHeaderResult> {
    const verified = await this.fetchVerifyPayload(record);
    if (
      this.deps.signer === undefined ||
      this.deps.signerValidation === undefined ||
      this.deps.config.signerIndex === undefined
    ) {
      throw new Error("DA signer is not configured");
    }
    const signatureWitness = signDaAttestation({
      signer: this.deps.signer,
      signerIndex: this.deps.config.signerIndex,
      headerHash: record.headerHash,
    });
    const signature: DaSignatureRecord = {
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      signerIndex: this.deps.config.signerIndex,
      signatureWitness,
      payloadHash: verified.payloadSha256,
      committeeSignersHash: this.deps.signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "local",
      source: "local",
      verifiedAt: new Date().toISOString(),
      l1ChainPoint: record.observedChainPoint,
      validation: verified.validation,
    };
    const published = await this.publishSignature(signature);
    return {
      signature: {
        ...signature,
        broadcastStatus: published.broadcastStatus,
      },
      ...(published.error === undefined
        ? {}
        : { publishError: published.error }),
    };
  }

  private async fetchVerifyPayload(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
  ): Promise<VerifiedDaPayload> {
    const fetched = await this.deps.payloadClient.fetchPayloadCandidates(
      record.headerHash,
    );
    if (!fetched.ok) {
      await this.deps.store.saveDaPayload({
        deploymentFingerprint: this.deps.config.deploymentFingerprint,
        headerHash: record.headerHash,
        payloadCborHex: "",
        payloadSha256: "",
        sourceEndpoint: "",
        fetchedAt: new Date().toISOString(),
        validationStatus: "missing_da",
        validationError: fetched.attempts
          .map((attempt) => `${attempt.endpoint}:${attempt.status}`)
          .join(","),
      });
      throw new Error(`missing DA payload for ${record.headerHash}`);
    }
    const selectedPayload = await this.selectUnambiguousPayload(
      record.headerHash,
      fetched.candidates,
    );
    const payloadRecord = await this.deps.store.saveDaPayload({
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      payloadCborHex: selectedPayload.payloadCbor.toString("hex"),
      payloadSha256: daPayloadSha256(selectedPayload.payloadCbor),
      sourceEndpoint: selectedPayload.endpoint,
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
      conflictStatus: "none",
    });
    if (payloadRecord.validationStatus === "conflicted") {
      throw new Error(payloadRecord.validationError ?? "payload conflict");
    }
    const verified = await this.verifyAndStorePayload(
      selectedPayload.payloadCbor,
      record,
      payloadRecord,
    );
    return verified;
  }

  private async selectUnambiguousPayload(
    headerHash: string,
    candidates: readonly {
      readonly endpoint: string;
      readonly payloadCbor: Buffer;
    }[],
  ): Promise<{ readonly endpoint: string; readonly payloadCbor: Buffer }> {
    const byHash = new Map<
      string,
      { readonly payloadCbor: Buffer; readonly endpoints: string[] }
    >();
    for (const candidate of candidates) {
      const hash = daPayloadSha256(candidate.payloadCbor);
      const existing = byHash.get(hash);
      if (existing === undefined) {
        byHash.set(hash, {
          payloadCbor: candidate.payloadCbor,
          endpoints: [candidate.endpoint],
        });
      } else {
        existing.endpoints.push(candidate.endpoint);
      }
    }
    if (byHash.size === 1) {
      const [, entry] = [...byHash.entries()][0]!;
      return {
        endpoint: entry.endpoints[0]!,
        payloadCbor: entry.payloadCbor,
      };
    }
    const conflictDetail = [...byHash.entries()]
      .map(([hash, entry]) => `${hash}@${entry.endpoints.join("+")}`)
      .join(",");
    await this.deps.store.saveDaPayload({
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash,
      payloadCborHex: candidates[0]?.payloadCbor.toString("hex") ?? "",
      payloadSha256:
        candidates[0] === undefined
          ? ""
          : daPayloadSha256(candidates[0].payloadCbor),
      sourceEndpoint: candidates
        .map((candidate) => candidate.endpoint)
        .join(","),
      fetchedAt: new Date().toISOString(),
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      validationError: `conflicting DA payload bytes from endpoints: ${conflictDetail}`,
    });
    throw new Error(`conflicting DA payload bytes for ${headerHash}`);
  }

  private async verifyAndStorePayload(
    payloadCbor: Buffer,
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadRecord: DaPayloadRecord,
  ) {
    try {
      const verified = await verifyDaPayloadAgainstHeader(
        payloadCbor,
        record.headerHash,
        record.header,
        {
          stateQueueOutRef: record.stateQueueOutRef,
          transactionProjector: this.deps.transactionProjector,
        },
      );
      const verifiedPayloadRecord: DaPayloadRecord = {
        ...payloadRecord,
        verifiedAt: new Date().toISOString(),
        rootSummary: verified.roots,
        validationStatus: "verified",
      };
      await this.deps.store.saveDaPayload(verifiedPayloadRecord);
      return verified;
    } catch (error) {
      const status =
        error instanceof DaPayloadValidationError &&
        error.code === "root_mismatch"
          ? "root_mismatch"
          : "malformed_da";
      await this.deps.store.saveDaPayload({
        ...payloadRecord,
        validationStatus: status,
        validationError: error instanceof Error ? error.message : String(error),
      });
      throw error;
    }
  }

  private async ensurePayloadForSubmitter(
    record: StateQueueHeaderRecord,
    errors: string[],
  ): Promise<void> {
    if (
      this.deps.submitterReconciler === undefined ||
      !this.isSubmitterHeaderScope(record)
    ) {
      return;
    }
    const existing = await this.deps.store.getDaPayload(record.headerHash);
    if (existing?.validationStatus === "verified") {
      return;
    }
    try {
      await this.fetchVerifyPayload(record);
    } catch (error) {
      errors.push(error instanceof Error ? error.message : String(error));
    }
  }

  private async reconcileHeader(
    record: StateQueueHeaderRecord,
    errors: string[],
  ): Promise<boolean> {
    const reconciler = this.deps.submitterReconciler;
    if (reconciler === undefined) {
      return false;
    }
    const result = await reconciler.reconcileHeader(record);
    if (result.status === "post_failed") {
      errors.push(
        `failed to reconcile DA attestation for ${record.headerHash}${
          result.reason === undefined ? "" : `: ${result.reason}`
        }`,
      );
    }
    return result.status === "reconciled";
  }

  private isSubmitterHeaderScope(record: StateQueueHeaderRecord): boolean {
    return (
      record.finalized &&
      (record.status === "unattested" || record.status === "attesting")
    );
  }

  private async publishSignature(
    record: DaSignatureRecord,
  ): Promise<CoordinatorPublishResult> {
    const coordinator = this.deps.coordinator;
    if (coordinator === undefined) {
      return { broadcastStatus: "local" };
    }
    try {
      const broadcastStatus = await coordinator.publishSignature(record);
      return broadcastStatus === "post_failed"
        ? {
            broadcastStatus,
            error: this.coordinatorPostFailedMessage(record),
          }
        : { broadcastStatus };
    } catch (error) {
      return {
        broadcastStatus: "post_failed",
        error: `${this.coordinatorPostFailedMessage(record)}: ${
          error instanceof Error ? error.message : String(error)
        }`,
      };
    }
  }

  private coordinatorPostFailedMessage(
    record: Pick<DaSignatureRecord, "headerHash" | "signerIndex">,
  ): string {
    const coordinatorError = this.deps.coordinator?.lastPublishError?.(record);
    return `failed to publish DA signature for ${record.headerHash} signer ${record.signerIndex.toString()}${
      coordinatorError === undefined ? "" : `: ${coordinatorError}`
    }`;
  }

  private shouldRepublishExistingSignature(record: DaSignatureRecord): boolean {
    const coordinator = this.deps.coordinator;
    if (coordinator === undefined) {
      return false;
    }
    return (
      record.broadcastStatus !== "posted" ||
      coordinator.retryPublishedSignatures === true
    );
  }

  private shouldRepublishExistingSignatureForHeader(
    record: DaSignatureRecord,
    status: Awaited<ReturnType<typeof scanStateQueue>>[number]["status"],
  ): boolean {
    if (!this.shouldRepublishExistingSignature(record)) {
      return false;
    }
    if (status === "unattested" || status === "attesting") {
      return true;
    }
    return (
      status === "attested" &&
      this.deps.coordinator?.retryPublishedSignaturesForAttestedHeaders === true
    );
  }
}
