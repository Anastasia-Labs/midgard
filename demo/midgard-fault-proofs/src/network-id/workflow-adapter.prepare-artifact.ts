import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import {
  detectNetworkIdAcceptedForced,
  NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID,
  prepareNetworkIdAcceptedForced,
} from "./accepted-forced.js";
import { prepareNetworkIdFromCanonicalEvidence } from "./prepare.js";
import { networkIdAcceptedForcedArtifactFromPrepared } from "./workflow-adapter.accepted-forced-artifact.js";
import { networkIdForcedArtifactFromPrepared } from "./workflow-adapter.admit-network-id-forced-artifact.js";
import {
  artifactFromPrepared,
  CATEGORY,
} from "./workflow-adapter.prepared-from-artifact.js";
import {
  createNetworkIdWrongfulRejectionPlanner,
  detectNetworkIdWrongfulRejections,
  NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID,
} from "./wrongful-rejection.js";

export const prepareNetworkIdWorkflowArtifact = async ({
  evidence,
  classification,
  expectedNetworkId,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification?: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
  readonly expectedNetworkId: 0n | 1n;
}): Promise<JournalJsonObject> => {
  if (
    classification?.category === CATEGORY &&
    classification.selected.violationId ===
      NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID
  ) {
    const detections = detectNetworkIdAcceptedForced({
      block: evidence,
      expectedNetworkId,
    });
    if (
      !detections.some(
        (value) => value.detectionId === classification.selected.detectionId,
      )
    )
      throw new Error(
        "network-id classification does not select the the selected authenticated accepted forced fault",
      );
    return networkIdAcceptedForcedArtifactFromPrepared(
      await prepareNetworkIdAcceptedForced({
        block: evidence,
        expectedNetworkId,
        detectionId: classification.selected.detectionId,
      }),
    ) as unknown as JournalJsonObject;
  }
  if (
    classification?.category === CATEGORY &&
    classification.selected.violationId ===
      NETWORK_ID_WRONGFUL_REJECTION_VIOLATION_ID
  ) {
    const detections = detectNetworkIdWrongfulRejections({
      block: evidence,
      expectedNetworkId,
    });
    if (detections[0]?.detectionId !== classification.selected.detectionId)
      throw new Error(
        "network-id forced classification does not select the earliest authenticated wrongful rejection",
      );
    return networkIdForcedArtifactFromPrepared(
      await createNetworkIdWrongfulRejectionPlanner(expectedNetworkId)({
        block: evidence,
      }),
    ) as unknown as JournalJsonObject;
  }
  return artifactFromPrepared(
    await prepareNetworkIdFromCanonicalEvidence({
      evidence,
      expectedNetworkId,
    }),
  );
};
