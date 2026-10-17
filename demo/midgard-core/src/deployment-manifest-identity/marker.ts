import { requireDeploymentManifestId, requireRecord } from "./primitives.js";
import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "./types.js";

export const parseDeploymentMarker = (value: unknown): DeploymentMarker => {
  const candidate = requireRecord(value, "Deployment marker V1");
  const keys = Object.keys(candidate);
  if (
    keys.length !== 2 ||
    !Object.prototype.hasOwnProperty.call(candidate, "schemaVersion") ||
    !Object.prototype.hasOwnProperty.call(candidate, "manifestId")
  ) {
    throw new Error(
      "Deployment marker V1 must contain exactly schemaVersion and manifestId",
    );
  }
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION) {
    throw new Error(
      `Deployment marker V1 schemaVersion must be ${MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION}`,
    );
  }
  return {
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId: requireDeploymentManifestId(
      candidate.manifestId,
      "Deployment marker V1 manifestId",
    ),
  };
};

export const makeDeploymentMarker = (manifestId: string): DeploymentMarker =>
  parseDeploymentMarker({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId,
  });

export const assertDeploymentMarkerMatches = (
  expected: DeploymentMarker,
  actual: unknown,
  boundary = "deployment boundary",
): DeploymentMarker => {
  const canonicalExpected = parseDeploymentMarker(expected);
  const canonicalActual = parseDeploymentMarker(actual);
  if (canonicalActual.manifestId !== canonicalExpected.manifestId) {
    throw new Error(
      `${boundary} deployment marker mismatch: expected ${canonicalExpected.manifestId}, found ${canonicalActual.manifestId}`,
    );
  }
  return canonicalActual;
};
