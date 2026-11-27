import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import {
  buildProofChunkPublication,
  resolvePublishedProofChunks,
} from "../publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type WorkflowJsonObject } from "./identity.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  exact,
  PROOF_CHUNK_PREREQUISITE,
  type ProofChunkPrerequisitePort,
  type ProofChunkRequirement,
  publicationAction,
  record,
  requirementFor,
  sameJson,
  sha256,
} from "./proof-chunk-prerequisite.route-action-identity.js";
import { proofMaterialsViewFor } from "./proof-materials-view.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "./raw-l1-publication-observation.js";

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
  signer,
  publications,
  proofCborForAction,
  maximumTransactionBytes,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly proofCborForAction: (input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowAction;
    readonly artifact: WorkflowJsonObject;
  }) => string | null | Promise<string | null>;
  /** Exact `cardanoProtocolParameters.snapshot.maxTxSize` from the manifest. */
  readonly maximumTransactionBytes?: string | number;
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
    readonly artifact: WorkflowJsonObject;
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
    readonly artifact: WorkflowJsonObject;
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
      ) as WorkflowJsonObject,
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
  const materials = () =>
    proofMaterialsViewFor({
      view: signer.proofMaterials,
      lucid,
      address: signer.proofMaterialsAddress,
    });
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
              address: signer.proofMaterialsAddress,
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
    inspect: async ({ headerHash, workflowId, baseAction, artifact }) => {
      const required = await requirement({
        headerHash,
        action: baseAction,
        artifact,
      });
      if (required === null || required.chunkDatums.length === 0) {
        return { kind: "not_required" };
      }
      const view = materials();
      const routeAction = publicationAction({
        category,
        baseAction,
        requirement: required,
      });
      // Found by content: an earlier publication of this workflow, of
      // another, or a copy anyone paid, as long as it is plainly shaped.
      const chunks = await resolvePublishedProofChunks({
        lucid,
        address: signer.proofMaterialsAddress,
        proofCbor: required.proofCbor,
        view,
        holder: { workflowId, objective: { category, headerHash } },
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
        ? { kind: "satisfied", chunks }
        : {
            kind: "pending",
            reason: `${category} exact proof chunks exist but are not authenticated on the current chain`,
          };
    },
    capture: async ({ headerHash, action, artifact }) => {
      const required = (await exactAction({ headerHash, action, artifact }))
        .requirement;
      const transaction = (
        await buildProofChunkPublication({
          lucid,
          signer,
          proofCbor: required.proofCbor,
        })
      ).transaction;
      return { transaction };
    },
  };
  return Object.freeze(port);
};
