import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type Network } from "@lucid-evolution/lucid";

import {
  publishProofChunks,
  resolvePublishedProofChunks,
} from "../publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import { recoveryForTransaction } from "./proof-chunk-prerequisite.recovery-for-transaction.js";
import {
  parseProofCarriageRecovery,
  parseRecovery,
  requirementFromJournaledAction,
} from "./proof-chunk-prerequisite.requirement-from-journaled-action.js";
import {
  exact,
  PROOF_CHUNK_PREREQUISITE,
  type ProofChunkPrerequisitePort,
  type ProofChunkPublicationRecovery,
  type ProofChunkRequirement,
  publicationAction,
  record,
  requirementFor,
  sameJson,
  sha256,
  TX_HASH,
} from "./proof-chunk-prerequisite.route-action-identity.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";

/**
 * Concrete authenticated publication port. Lucid is used only to build/query
 * candidate UTxOs; canonical inclusion admission is always performed by the raw-L1
 * publication observer.
 */
export const createAuthenticatedProofChunkPrerequisitePort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  network,
  signer,
  publications,
  proofCborForAction,
  maximumTransactionBytes,
  transactionConfirmed,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly proofCborForAction: (input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }) => string | null | Promise<string | null>;
  /** Exact `cardanoProtocolParameters.snapshot.maxTxSize` from the manifest. */
  readonly maximumTransactionBytes?: string | number;
  readonly transactionConfirmed: (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => Promise<boolean>;
}): ProofChunkPrerequisitePort<Category> => {
  if (
    publications.observerVersion !==
    FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER
  ) {
    throw new Error(`${category} proof chunks require a raw-L1 observer`);
  }
  const releaseMaximumTransactionBytes =
    maximumTransactionBytes === undefined
      ? undefined
      : typeof maximumTransactionBytes === "string"
        ? Number(maximumTransactionBytes)
        : maximumTransactionBytes;
  if (
    releaseMaximumTransactionBytes !== undefined &&
    (!Number.isSafeInteger(releaseMaximumTransactionBytes) ||
      releaseMaximumTransactionBytes <= 0 ||
      String(releaseMaximumTransactionBytes) !==
        String(maximumTransactionBytes))
  ) {
    throw new Error(
      `${category} proof carriage requires canonical release maxTxSize`,
    );
  }
  const requirement = async ({
    headerHash,
    action,
    artifact,
  }: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<ProofChunkRequirement | null> => {
    const proofCbor = await proofCborForAction({
      headerHash,
      action,
      artifact,
    });
    return proofCbor === null
      ? null
      : requirementFor({ proofCbor, label: `${category} ${action.actionId}` });
  };
  const exactAction = async ({
    headerHash,
    action,
    artifact,
  }: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<{ readonly requirement: ProofChunkRequirement }> => {
    const input = exact(
      action.input,
      [
        "schemaVersion",
        "category",
        "stage",
        "forAction",
        "proofCborSha256",
        "chunkDatumSha256s",
      ],
      `${category} proof-chunk publication action`,
    );
    if (
      input.schemaVersion !== PROOF_CHUNK_PREREQUISITE ||
      input.category !== category ||
      input.stage !== "direct_or_publish_proof"
    ) {
      throw new Error(`${category} proof-chunk action changed identity`);
    }
    const parsedBaseAction = exact(
      input.forAction,
      ["actionId", "input"],
      `${category} proof-chunk base action`,
    );
    if (typeof parsedBaseAction.actionId !== "string") {
      throw new Error(`${category} proof-chunk base action changed identity`);
    }
    const baseAction: FraudProofWorkflowAction = {
      actionId: parsedBaseAction.actionId,
      input: record(
        parsedBaseAction.input,
        `${category} proof-chunk base action input`,
      ) as JournalJsonObject,
    };
    const required = await requirement({
      headerHash,
      action: baseAction,
      artifact,
    });
    if (required === null) {
      throw new Error(`${category} proof-chunk action is no longer required`);
    }
    const expected = publicationAction({
      category,
      baseAction,
      requirement: required,
    });
    if (!sameJson(action, expected)) {
      throw new Error(`${category} proof-chunk action changed its proof`);
    }
    return { requirement: required };
  };
  const observeOutputs = async ({
    headerHash,
    outputs,
  }: {
    readonly headerHash: string;
    readonly outputs: readonly Readonly<{
      outRef: string;
      datumCbor: string;
    }>[];
  }): Promise<readonly boolean[]> =>
    await Promise.all(
      outputs.map(
        async (output) =>
          (
            await publications.observeExact({
              headerHash,
              kind: "proof_chunk",
              address: signer.address,
              expectedOutRef: output.outRef,
              expectedDatumCbor: output.datumCbor,
            })
          ).kind === "confirmed",
      ),
    );
  const port: ProofChunkPrerequisitePort<Category> = {
    portVersion: PROOF_CHUNK_PREREQUISITE,
    category,
    classifyDirectCapacityFailure: (cause) => {
      const message = cause instanceof Error ? cause.message : String(cause);
      const matched =
        /Max transaction size of (\d+) exceeded\. Found: (\d+)/u.exec(message);
      if (matched === null) {
        throw cause instanceof Error
          ? cause
          : new Error(
              `${category} direct proof preflight failed for a non-capacity reason: ${message}`,
            );
      }
      const maximum = Number(matched[1]);
      const actual = Number(matched[2]);
      if (
        releaseMaximumTransactionBytes === undefined ||
        maximum !== releaseMaximumTransactionBytes ||
        !Number.isSafeInteger(actual) ||
        actual <= maximum
      ) {
        throw new Error(
          `${category} direct proof capacity failure does not match the release-bound maxTxSize`,
        );
      }
      return Object.freeze({
        kind: "max_tx_size" as const,
        maximumTransactionBytes: maximum,
        actualTransactionBytes: actual,
        errorSha256: sha256(message),
      });
    },
    inspect: async ({ headerHash, baseAction, artifact, entries }) => {
      const required = await requirement({
        headerHash,
        action: baseAction,
        artifact,
      });
      if (required === null || required.chunkDatums.length === 0) {
        return { kind: "not_required" };
      }
      const routeAction = publicationAction({
        category,
        baseAction,
        requirement: required,
      });
      const confirmed = entries.some(
        (entry) =>
          entry.event.kind === "confirmed" &&
          entry.event.actionId === routeAction.actionId,
      );
      const intent = [...entries]
        .reverse()
        .find(
          (entry) =>
            entry.event.kind === "submission_intent" &&
            entry.event.actionId === routeAction.actionId,
        );
      let publicationAuthorized = false;
      if (
        confirmed &&
        intent !== undefined &&
        intent.event.kind === "submission_intent"
      ) {
        const recovery = parseProofCarriageRecovery({
          value: intent.event.durableRecovery,
          requirement: required,
        });
        publicationAuthorized =
          recovery.route === "publication" &&
          sameJson(recovery.baseAction, baseAction);
      }
      if (!publicationAuthorized) {
        return { kind: "required", action: routeAction };
      }
      const chunks = await resolvePublishedProofChunks({
        lucid,
        address: signer.address,
        proofCbor: required.proofCbor,
      });
      if (chunks === undefined) {
        return { kind: "required", action: routeAction };
      }
      const outputsConfirmed = await observeOutputs({
        headerHash,
        outputs: chunks.map((chunk) => ({
          outRef: chunk.outRef,
          datumCbor: chunk.datumCbor,
        })),
      });
      return outputsConfirmed.every(Boolean)
        ? { kind: "satisfied" }
        : {
            kind: "pending",
            reason: `${category} exact proof chunks exist but are not authenticated on the current chain`,
          };
    },
    capture: async ({ headerHash, action, artifact }) => {
      const required = (await exactAction({ headerHash, action, artifact }))
        .requirement;
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await publishProofChunks({
            lucid,
            network,
            signer,
            proofCbor: required.proofCbor,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
        },
      );
      return {
        transaction,
        durableRecovery: recoveryForTransaction({
          transaction,
          requirement: required,
          address: signer.address,
        }),
      };
    },
    reconcile: async ({
      headerHash,
      action,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      authorizeResubmission,
    }) => {
      let required: ProofChunkRequirement;
      try {
        required = requirementFromJournaledAction({
          category,
          action,
          durableRecovery,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (txHash === undefined || !TX_HASH.test(txHash)) {
        return {
          kind: "conflict",
          reason: `${category} proof-chunk intent omitted its exact transaction hash`,
        };
      }
      let recovery: ProofChunkPublicationRecovery;
      try {
        recovery = parseRecovery({
          value: durableRecovery,
          requirement: required,
          txHash,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      const confirmed = await observeOutputs({
        headerHash,
        outputs: recovery.outputs,
      });
      if (confirmed.every(Boolean)) {
        return { kind: "confirmed", txHash };
      }
      const included = await transactionConfirmed({ headerHash, txHash });
      if (confirmed.some(Boolean) || included) {
        return {
          kind: "conflict",
          reason: `${category} proof-chunk transaction did not produce its exact complete output set`,
        };
      }
      // Current-chain absence does not establish rejection or expiry of a
      // submitted publication. Preserve its exact journaled output set.
      return signedTransactionCborHex === undefined ||
        publications.observeSignedTransaction === undefined
        ? { kind: "pending", txHash }
        : reconcileSignedWorkflowTransaction({
            transactionHash: txHash,
            signedTransactionCborHex,
            observe: publications.observeSignedTransaction,
            rebroadcast: publications.rebroadcastSignedTransaction,
            authorizeResubmission,
          });
    },
  };
  return Object.freeze(port);
};
