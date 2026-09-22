import type { LucidEvolution, Network } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
  JournalJsonValue,
} from "../workflow/journal.js";
import { journalJsonDigest } from "../workflow/journal.js";
import { createAuthenticatedProofChunkPrerequisitePort } from "../workflow/proof-chunk-prerequisite.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import type {
  createDistinctAssetAccumulationActuator,
  DistinctAssetAccumulationActuationArtifact,
  DistinctAssetAccumulationActuatorAction,
} from "./actuator.js";
import {
  DistinctAssetStep01RedeemerSchema,
  DistinctAssetStep02RedeemerSchema,
  DistinctAssetStep03RedeemerSchema,
  DistinctAssetStep04RedeemerSchema,
  DistinctAssetStep05RedeemerSchema,
  DistinctAssetVerdictSubjectSchema,
} from "./schemas.js";

export const DISTINCT_ASSET_PROOF_PUBLICATION =
  "midgard-distinct-asset-proof-publication-v1" as const;

export const createDistinctAssetProofPrerequisite = ({
  lucid,
  network,
  signer,
  publications,
  transactionConfirmed,
  artifact,
  maximumTransactionBytes,
}: {
  readonly maximumTransactionBytes: string | number;
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
  readonly artifact: DistinctAssetAccumulationActuationArtifact;
}) => ({
  maximumTransactionBytes: Number(maximumTransactionBytes),
  ...createAuthenticatedProofChunkPrerequisitePort({
    category: "distinctAssetAccumulationLimit",
    lucid,
    network,
    signer,
    publications,
    transactionConfirmed,
    maximumTransactionBytes,
    proofCborForAction: ({ action }) =>
      action.input.stage === "step01"
        ? (artifact.accepted?.txInclusion.txMembershipProofCbor ?? null)
        : null,
  }),
});

/** Capture one durable turn; only an exact maxTxSize refusal selects publication. */
export const captureDistinctAssetActionWithProofCarriage = async ({
  actuator,
  action,
  artifact,
  entries,
  prerequisite,
  lucid,
  signer,
}: {
  readonly actuator: ReturnType<typeof createDistinctAssetAccumulationActuator>;
  readonly action: DistinctAssetAccumulationActuatorAction;
  readonly artifact: DistinctAssetAccumulationActuationArtifact;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly prerequisite: ReturnType<
    typeof createDistinctAssetProofPrerequisite
  >;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
}) => {
  const actionId = `distinctAssetAccumulationLimit:${action.stage}:${"threadOutRef" in action ? action.threadOutRef : "stepIndex" in action ? String(action.stepIndex) : "0"}`;
  const baseAction = {
    actionId,
    input: {
      schemaVersion: "midgard-production-cursor-family-action-v1",
      category: "distinctAssetAccumulationLimit",
      ...action,
    } as JournalJsonObject,
  };
  const inspected = await prerequisite.inspect({
    headerHash: artifact.headerHash,
    baseAction,
    artifact: {},
    entries: [],
  });
  if (inspected.kind === "required") {
    const intent = [...entries]
      .reverse()
      .find(
        (entry) =>
          entry.event.kind === "submission_intent" &&
          entry.event.actionId === inspected.action.actionId,
      );
    if (
      intent?.event.kind === "submission_intent" &&
      entries.some(
        (entry) =>
          entry.event.kind === "confirmed" &&
          entry.event.actionId === inspected.action.actionId,
      )
    ) {
      const recovery = intent.event.durableRecovery;
      const publication = distinctAssetProofPublicationRecovery(
        recovery,
        prerequisite.maximumTransactionBytes,
      );
      const result = await prerequisite.reconcile({
        headerHash: artifact.headerHash,
        action: inspected.action,
        artifact: {},
        txHash: intent.event.txHash,
        durableRecovery: publication,
      });
      if (result.kind !== "confirmed")
        throw new Error(
          "distinct-asset proof publication is not authenticated and final",
        );
      const chunks = await resolvePublishedProofChunks({
        lucid,
        address: signer.address,
        proofCbor: artifact.accepted!.txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error("distinct-asset published source proof disappeared");
      return {
        ...(await actuator.capture({
          action,
          artifact,
          publishedProofChunks: chunks,
        })),
        actionId,
        actionInput: baseAction.input,
        durableRecovery: undefined,
      };
    }
    try {
      return {
        ...(await actuator.capture({ action, artifact })),
        actionId,
        actionInput: baseAction.input,
        durableRecovery: undefined,
      };
    } catch (cause) {
      const directCapacityFailure =
        prerequisite.classifyDirectCapacityFailure(cause);
      const captured = await prerequisite.capture({
        headerHash: artifact.headerHash,
        action: inspected.action,
        artifact: {},
      });
      return {
        ...captured,
        mutationLease: undefined,
        actionId: inspected.action.actionId,
        actionInput: inspected.action.input,
        durableRecovery: {
          schemaVersion: DISTINCT_ASSET_PROOF_PUBLICATION,
          directCapacityFailure,
          publication: captured.durableRecovery,
        } as JournalJsonObject,
      };
    }
  }
  if (inspected.kind !== "not_required")
    throw new Error("distinct-asset source prerequisite changed");
  return {
    ...(await actuator.capture({ action, artifact })),
    actionId,
    actionInput: baseAction.input,
    durableRecovery: undefined,
  };
};

/** Exact public witness bytes rederived from retained DA before each journal turn. */
export const distinctAssetPreparedJournalArtifact = (
  artifact: DistinctAssetAccumulationActuationArtifact,
): JournalJsonObject => {
  const coordinate = artifact.finding.coordinate;
  const wireCoordinate =
    coordinate.kind === "input"
      ? {
          fold: 0n,
          primary_index: BigInt(coordinate.inputIndex),
          asset_index: BigInt(coordinate.assetIndex),
        }
      : coordinate.kind === "output"
        ? {
            fold: 1n,
            primary_index: BigInt(coordinate.outputIndex),
            asset_index: BigInt(coordinate.assetIndex),
          }
        : {
            fold: 2n,
            primary_index: BigInt(coordinate.mintIndex),
            asset_index: 0n,
          };
  const accepted = artifact.accepted?.txInclusion;
  const source: JournalJsonObject =
    accepted === undefined
      ? {
          forcedSourceCbor: Data.to(
            {
              Continue: [
                {
                  source: {
                    ForcedSource: {
                      ...artifact.forcedSource,
                      input_index: 0n,
                      output_index: 0n,
                    },
                  },
                  coordinate: wireCoordinate,
                },
              ],
            } as never,
            DistinctAssetStep01RedeemerSchema as never,
          ),
        }
      : {
          nativeTxId: accepted.nativeTxId,
          nativeTxCompactCbor: accepted.nativeTxCompactCbor,
          l2TransactionSourceCbor: accepted.l2TransactionSourceCbor,
          transactionsPhasRoot: accepted.transactionsPhasRoot,
          txMembershipProofCbor: accepted.txMembershipProofCbor,
        };
  const foldSchemas = [
    DistinctAssetStep03RedeemerSchema,
    DistinctAssetStep04RedeemerSchema,
    DistinctAssetStep05RedeemerSchema,
  ];
  return {
    schemaVersion: "midgard-distinct-asset-retained-artifact-v1",
    headerHash: artifact.headerHash,
    subjectCbor: Data.to(
      artifact.finding.subject as never,
      DistinctAssetVerdictSubjectSchema as never,
    ),
    coordinate: { ...coordinate },
    source,
    authenticationCbor: Data.to(
      {
        Continue: [
          { input_index: 0n, output_index: 0n, ...artifact.authentication },
        ],
      } as never,
      DistinctAssetStep02RedeemerSchema as never,
    ),
    foldCbors: artifact.folds.map((fold, index) =>
      Data.to(
        {
          Continue: [
            fold.kind === "skip"
              ? { Skip: { input_index: 0n, output_index: 0n } }
              : {
                  Authenticate: {
                    input_index: 0n,
                    output_index: 0n,
                    evidence: fold.evidence,
                  },
                },
          ],
        } as never,
        foldSchemas[index]! as never,
      ),
    ),
  };
};

const isJournalObject = (
  value: JournalJsonValue | undefined,
): value is JournalJsonObject =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/** Refuse journal edits before looking up any publication outputs. */
export const distinctAssetProofPublicationRecovery = (
  value: JournalJsonObject | undefined,
  maximumTransactionBytes: number,
): JournalJsonObject => {
  if (
    value === undefined ||
    Object.keys(value).sort().join(",") !==
      "directCapacityFailure,publication,schemaVersion" ||
    value.schemaVersion !== DISTINCT_ASSET_PROOF_PUBLICATION
  )
    throw new Error("distinct-asset proof publication recovery changed");
  const failure = value.directCapacityFailure;
  if (
    !isJournalObject(failure) ||
    Object.keys(failure).sort().join(",") !==
      "actualTransactionBytes,errorSha256,kind,maximumTransactionBytes" ||
    failure.kind !== "max_tx_size" ||
    failure.maximumTransactionBytes !== maximumTransactionBytes ||
    typeof failure.actualTransactionBytes !== "number" ||
    !Number.isSafeInteger(failure.actualTransactionBytes) ||
    failure.actualTransactionBytes <= maximumTransactionBytes ||
    typeof failure.errorSha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(failure.errorSha256)
  )
    throw new Error(
      "distinct-asset proof publication capacity evidence changed",
    );
  const publication = value.publication;
  if (!isJournalObject(publication))
    throw new Error("distinct-asset proof publication outputs changed");
  return publication;
};

export const requireDistinctAssetPreparedArtifact = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  artifact: DistinctAssetAccumulationActuationArtifact,
): void => {
  const prepared = entries[1]?.event;
  if (
    prepared?.kind !== "prepared" ||
    prepared.artifactDigest !==
      journalJsonDigest(distinctAssetPreparedJournalArtifact(artifact))
  )
    throw new Error(
      "distinct-asset retained artifact changed after preparation",
    );
};
