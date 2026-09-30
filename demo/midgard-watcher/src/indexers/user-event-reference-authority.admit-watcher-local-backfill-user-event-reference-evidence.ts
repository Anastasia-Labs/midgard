import {
  readWatcherLocalBackfillFinalityObservation,
  type WatcherLocalBackfillFinalityReceipt,
} from "../l1/finality-engine.js";
import { type WatcherLocalBackfillObservationReceipt } from "../l1/l1-adapter.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { readWatcherUserEventReferenceEvidence } from "./user-event-reference-authority.create-body-reference-authority.js";
import {
  authorities,
  backfillAuthorityPairs,
  type WatcherUserEventReferenceAuthority,
  type WatcherUserEventReferenceEvidence,
} from "./user-event-reference-authority.create-watcher-local-user-event-reference-authority.js";

/** Public evidence equality is checked only after private identical pairing. */
export const admitWatcherLocalBackfillUserEventReferenceEvidence = ({
  evidence,
  deploymentIdentity,
  finality,
  observation,
  referenceAuthority,
}: Readonly<{
  evidence: unknown;
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>): WatcherUserEventReferenceEvidence | null => {
  try {
    const binding = backfillAuthorityPairs.get(referenceAuthority);
    const state = authorities.get(referenceAuthority);
    if (
      binding === undefined ||
      state === undefined ||
      binding.finality !== finality ||
      binding.observation !== observation
    )
      return null;
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    const pair = readWatcherLocalBackfillFinalityObservation({
      finality,
      observation,
    });
    const capture = pair.observation.capture;
    if (
      capture.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
      capture.blueprintHash !== deploymentIdentity.blueprintHash ||
      capture.network !== deploymentIdentity.network
    )
      return null;
    state.assertLive();
    if (!watcherSameCanonicalJson(state.evidence, evidence)) return null;
    readWatcherLocalBackfillFinalityObservation({ finality, observation });
    return readWatcherUserEventReferenceEvidence(referenceAuthority);
  } catch {
    return null;
  }
};
