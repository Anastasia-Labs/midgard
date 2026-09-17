import { mkdir, realpath } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowActuationPermitIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  isWorkflowActuationRevokedError,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosTransportUnavailableError,
  type WorkflowActuationPermit,
  type WorkflowActuationRevokedError,
  type WorkflowAdapterRunner,
  type WorkflowFundingReservationPermit,
} from "@al-ft/midgard-fault-proofs";
import { type UTxO, utxoToCore } from "@lucid-evolution/lucid";

import {
  assertWatcherProverFundingAuthorityFactory,
  type WatcherProverFundingAuthorityFactory,
} from "../funding/prover-funding-authority.js";
import { WatcherProverFundingUnavailableError } from "../funding/prover-funding-reservation.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import type {
  WatcherCompletedFaultProofVerification,
  WatcherFaultProofApplication,
  WatcherInstalledWorkflowCategory,
} from "./fault-proof-application.js";
import type { WatcherFaultProofJob } from "./fault-proof-supervisor.js";
import { isWatcherPreflightStalledResult } from "./preflight-stall-retry.js";

/** Chosen by the supervisor after reloading and validating the durable journal. */
export type WatcherFaultProofExecutionAdmission = Readonly<{
  mode: "run" | "resume";
  funding: "create_or_resume" | "resume_only";
}>;

export type WatcherFaultProofExecutionOutcome =
  | Readonly<{ kind: "completed" | "terminal_included"; result: unknown }>
  | Readonly<{ kind: "pending"; resume: "await_observation"; reason: string }>
  | Readonly<{
      kind: "authority_revoked";
      error: WorkflowActuationRevokedError;
    }>
  | Readonly<{
      kind: "retryable";
      resume: "backoff";
      reason: string;
      retryAfterMs: number;
    }>;

export type WatcherFaultProofExecution = Readonly<{
  verifyCompleted(input: {
    readonly job: WatcherFaultProofJob;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
    readonly terminal: FraudProofWorkflowTerminal;
  }): Promise<
    | WatcherCompletedFaultProofVerification
    | Extract<WatcherFaultProofExecutionOutcome, { kind: "retryable" }>
  >;
  execute(input: {
    readonly job: WatcherFaultProofJob;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly admission: WatcherFaultProofExecutionAdmission;
  }): Promise<WatcherFaultProofExecutionOutcome>;
}>;

// Only failures from this adapter's read-only provider calls receive transport
// retry semantics. Arbitrary runner exceptions and authentication errors do not.
class FundingProviderTransportUnavailable extends Error {}
const readFundingProvider = async <T>(read: () => Promise<T>): Promise<T> => {
  try {
    return await read();
  } catch (error) {
    const cause = error instanceof TypeError ? error.cause : undefined;
    if (
      cause instanceof Error &&
      "code" in cause &&
      typeof cause.code === "string" &&
      [
        "UND_ERR_SOCKET",
        "UND_ERR_CONNECT_TIMEOUT",
        "ECONNRESET",
        "ECONNREFUSED",
        "EPIPE",
        "ETIMEDOUT",
      ].includes(cause.code)
    )
      throw new FundingProviderTransportUnavailable(
        "Local-node funding input query is temporarily unavailable",
        { cause: error },
      );
    throw error;
  }
};
const parseWatcherProverFundingOutRef = (
  outRef: string,
): Readonly<{ txHash: string; outputIndex: number }> => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(outRef);
  if (match === null) {
    throw new Error("prover funding output reference is not canonical");
  }
  return Object.freeze({ txHash: match[1]!, outputIndex: Number(match[2]!) });
};

export type WatcherProverFundingUtxoProvider = Readonly<{
  getUtxos(address: string): Promise<UTxO[]>;
  getUtxosByOutRef(
    outRefs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
  ): Promise<UTxO[]>;
}>;

/**
 * Application-supervisor permit mint. The supervisor binds the exact admitted
 * decision digest, actuation permit, and rollback generation to a fresh
 * atomically reserved slice of the live prover wallet; the fault-proof
 * application can neither mint nor substitute this authority. All wallet and
 * protocol-input resolution goes through the same local-node Kupo/Ogmios
 * authority the runners execute against.
 */
export const mintWatcherProverFundingReservationPermit = async (input: {
  readonly category: WatcherInstalledWorkflowCategory;
  readonly runner: WorkflowAdapterRunner;
  readonly factory: WatcherProverFundingAuthorityFactory;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly rollbackGeneration: string;
  readonly decisionDigest: string;
  readonly walletAddress: string;
  readonly provider: WatcherProverFundingUtxoProvider;
  readonly reservationMode?: "create_or_resume" | "resume_only";
}): Promise<WorkflowFundingReservationPermit> => {
  assertWatcherProverFundingAuthorityFactory(input.factory);
  return await input.factory.create({
    category: input.category,
    runner: input.runner,
    actuationPermit: input.actuationPermit,
    rollbackGeneration: input.rollbackGeneration,
    decisionDigest: input.decisionDigest,
    walletAddress: input.walletAddress,
    reservationMode: input.reservationMode,
    readWalletUtxos: async () =>
      await readFundingProvider(() =>
        input.provider.getUtxos(input.walletAddress),
      ),
    resolveInputs: async (outRefs) =>
      await readFundingProvider(() =>
        input.provider.getUtxosByOutRef(
          outRefs.map(parseWatcherProverFundingOutRef),
        ),
      ),
    resolveProtocolInputAuthority: async ({
      deploymentIdentity,
      outRef,
      semanticRole,
    }) => {
      const resolved = await readFundingProvider(() =>
        input.provider.getUtxosByOutRef([
          parseWatcherProverFundingOutRef(outRef),
        ]),
      );
      if (resolved.length !== 1) {
        throw new Error(
          "prover funding protocol input is not a unique live local-node output",
        );
      }
      return Object.freeze({
        deploymentFingerprint: deploymentIdentity.manifestId,
        outRef,
        semanticRole,
        resolvedOutputCborHex: utxoToCore(resolved[0]!)
          .output()
          .to_canonical_cbor_hex(),
      });
    },
  });
};

/** Executes one admitted turn. Scheduling, fresh journal admission, and retries
 * belong to the supervisor; this adapter owns funding, invocation and cleanup. */
export const createWatcherFaultProofExecution = (dependencies: {
  readonly application: Readonly<{
    runners: Partial<WatcherFaultProofApplication["runners"]>;
    runOrResume: WatcherFaultProofApplication["runOrResume"];
    verifyCompleted: WatcherFaultProofApplication["verifyCompleted"];
  }>;
  readonly fundingFactory: WatcherProverFundingAuthorityFactory;
  readonly walletAddress: string;
  readonly provider: WatcherProverFundingUtxoProvider;
  readonly journalRoot: string;
  readonly runtimeConfigPath: string;
  readonly deploymentFingerprint: string;
  readonly operationsSink: () => Pick<
    WatcherOperationsSink,
    "recordProofStep" | "setAlert"
  >;
}): WatcherFaultProofExecution =>
  Object.freeze({
    verifyCompleted: async ({ job, actuationPermit, entries, terminal }) => {
      const identity = assertWorkflowActuationPermitIdentity({
        permit: actuationPermit,
        category: job.category,
        rollbackGeneration: job.rollbackGeneration,
      });
      if (
        identity.headerHash !== job.headerHash ||
        identity.decisionDigest !== job.decisionDigest ||
        identity.deploymentFingerprint !== dependencies.deploymentFingerprint
      )
        throw new Error(
          "completed proof verification changed its admitted objective",
        );
      const executionIdentity = entries[0]?.identity;
      if (
        executionIdentity?.decisionDigest === undefined ||
        executionIdentity.deploymentFingerprint !==
          identity.deploymentFingerprint ||
        executionIdentity.category !== job.category ||
        executionIdentity.target.kind !== "state_queue_header" ||
        executionIdentity.target.headerHash !== job.headerHash
      )
        throw new Error(
          "completed proof verification changed its durable execution identity",
        );
      try {
        return await dependencies.application.verifyCompleted({
          runtimeConfigPath: dependencies.runtimeConfigPath,
          category: job.category,
          headerHash: job.headerHash,
          decisionDigest: executionIdentity.decisionDigest,
          entries,
          terminal,
        });
      } catch (cause) {
        if (cause instanceof LocalKupmiosTransportUnavailableError)
          return {
            kind: "retryable",
            resume: "backoff",
            reason: cause.message,
            retryAfterMs: 1_000,
          };
        throw cause;
      }
    },
    execute: async ({ job, actuationPermit, admission }) => {
      const { category, headerHash, decisionDigest } = job;
      const sink = dependencies.operationsSink();
      const actionIdentityDigest = watcherSha256CanonicalJson({
        category,
        headerHash,
        decisionDigest,
        rollbackGeneration: job.rollbackGeneration,
      });
      const record = (
        status: Parameters<
          WatcherOperationsSink["recordProofStep"]
        >[0]["status"],
        stage: "prepare" | "terminal" = "terminal",
      ) =>
        sink.recordProofStep({
          decisionDigest,
          stage,
          actionIdentityDigest,
          status,
          updatedAtMs: Date.now().toString(),
        });
      record("preflight", "prepare");
      try {
        const authority = assertWorkflowActuationPermitIdentity({
          permit: actuationPermit,
          category,
          rollbackGeneration: job.rollbackGeneration,
        });
        if (
          authority.deploymentFingerprint !==
            dependencies.deploymentFingerprint ||
          authority.headerHash !== headerHash ||
          authority.decisionDigest !== decisionDigest ||
          admission.mode !== job.mode ||
          (admission.funding === "resume_only" &&
            admission.mode !== "resume") ||
          (authority.authority === "reconciliation" &&
            admission.funding !== "resume_only")
        )
          throw new Error(
            "fault-proof execution admission changed its objective or authority",
          );
        const journalDirectory = join(
          dependencies.journalRoot,
          "fault-proofs",
          category,
          headerHash,
        );
        await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
        if ((await realpath(journalDirectory)) !== journalDirectory)
          throw new Error(
            "watcher workflow journal directory traverses a symlink",
          );
        const runner = dependencies.application.runners[category];
        if (runner === undefined)
          throw new Error(
            `fault-proof execution has no installed ${category} runner`,
          );
        const fundingReservationPermit =
          await mintWatcherProverFundingReservationPermit({
            category,
            runner,
            factory: dependencies.fundingFactory,
            actuationPermit,
            rollbackGeneration: job.rollbackGeneration,
            decisionDigest,
            walletAddress: dependencies.walletAddress,
            provider: dependencies.provider,
            reservationMode: admission.funding,
          });
        const result = await dependencies.application.runOrResume({
          mode: admission.mode,
          category,
          deploymentFingerprint: dependencies.deploymentFingerprint,
          headerHash,
          decisionDigest,
          actuationPermit,
          fundingReservationPermit,
          journalDirectory,
          runtimeConfigPath: dependencies.runtimeConfigPath,
        });
        if (typeof result === "object" && result !== null && "kind" in result) {
          if (
            result.kind === "completed" ||
            result.kind === "terminal_included"
          ) {
            record(result.kind === "completed" ? "completed" : "confirmed");
            return { kind: result.kind, result };
          }
          if (
            "reason" in result &&
            typeof result.reason === "string" &&
            (result.kind === "pending" ||
              isWatcherPreflightStalledResult(result))
          ) {
            record("reconciling");
            return {
              kind: "pending",
              resume: "await_observation",
              reason: result.reason,
            };
          }
        }
        const reason =
          typeof result === "object" &&
          result !== null &&
          "reason" in result &&
          typeof result.reason === "string"
            ? result.reason
            : "invalid execution outcome";
        throw new Error(
          `Watcher ${category} workflow did not progress: ${reason}`,
        );
      } catch (error) {
        if (isWorkflowActuationRevokedError(error)) {
          record("cancelled");
          return { kind: "authority_revoked", error };
        }
        if (
          error instanceof LocalKupmiosCheckpointChangedError ||
          error instanceof WatcherProverFundingUnavailableError
        ) {
          record("reconciling");
          return {
            kind: "pending",
            resume: "await_observation",
            reason: error.message,
          };
        }
        if (
          error instanceof FundingProviderTransportUnavailable ||
          error instanceof LocalKupmiosTransportUnavailableError
        ) {
          record("reconciling");
          return {
            kind: "retryable",
            resume: "backoff",
            reason: error.message,
            retryAfterMs: 1_000,
          };
        }
        record("failed");
        sink.setAlert({
          code: "proof_submission_failure",
          subjectDigest: decisionDigest,
          active: true,
          observedAtMs: Date.now().toString(),
        });
        throw error;
      } finally {
        await dependencies.fundingFactory.releaseUnused({ actuationPermit });
      }
    },
  });
