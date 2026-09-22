import {
  arrayOf,
  booleanValue,
  exactRecord,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  nullable,
  nullableNonEmptyString,
  oneOf,
  positiveInteger,
} from "midgard-node/artifact-schema";
import {
  type DaProducerPeerResult,
  type DaProducerPublicationReport,
  publicationSatisfied,
} from "midgard-node/da/libp2p-producer";

export const E2E_DA_GATE_SCHEMA_VERSION = "midgard-e2e-da-gate-v1";

export type DaGateStatus = "satisfied" | "pending" | "blocked" | "failed";

type DaGateObservation = {
  readonly headerHash: string;
  readonly deploymentFingerprint?: string;
  readonly status: DaGateStatus;
  readonly nextSafeAction:
    | "continue"
    | "wait_for_da_payload_publication"
    | "inspect_da_publication"
    | "configure_da_libp2p";
  readonly checkedAt: string;
  readonly acceptedPeers: number;
  readonly threshold: number | null;
  readonly announcementTopic: string | null;
  readonly announcementRecipients: readonly string[];
  readonly peerResults: DaProducerPublicationReport["peerResults"];
  readonly reason: string;
};

export type DaGateProbeResult = DaGateObservation & {
  readonly schemaVersion: typeof E2E_DA_GATE_SCHEMA_VERSION;
  readonly kind: "probe";
};

export type WaitForDaGateResult = DaGateObservation & {
  readonly schemaVersion: typeof E2E_DA_GATE_SCHEMA_VERSION;
  readonly kind: "wait";
  readonly attempts: number;
  readonly timedOut: boolean;
};

export type DaGateResult = DaGateProbeResult | WaitForDaGateResult;

export type ProbeDaGateOptions = {
  readonly headerHash: string;
  readonly publicationReport: DaProducerPublicationReport;
  readonly now?: Date;
};

export type WaitForDaGateOptions = Omit<
  ProbeDaGateOptions,
  "publicationReport"
> & {
  readonly probePublication: () => Promise<DaProducerPublicationReport>;
  readonly timeoutMs?: number;
  readonly intervalMs?: number;
  readonly sleep?: (milliseconds: number) => Promise<void>;
};

const HEADER_HASH_REGEX = /^[0-9a-f]{56}$/i;
const DEFAULT_WAIT_TIMEOUT_MS = 120_000;
const DEFAULT_WAIT_INTERVAL_MS = 5_000;

const parseLowerHex = (
  value: unknown,
  label: string,
  byteLength: number,
): string => {
  const parsed = nonEmptyString(value, label);
  if (parsed.length !== byteLength * 2 || !/^[0-9a-f]+$/u.test(parsed)) {
    throw new Error(
      `${label} must be ${byteLength.toString()} bytes of lowercase hexadecimal`,
    );
  }
  return parsed;
};

const parseDaProducerPeerResult = (
  value: unknown,
  label: string,
): DaProducerPeerResult => {
  const input = exactRecord(
    value,
    label,
    ["peerId", "signerIndex", "protocolId", "status", "payloadHash"],
    ["error"],
  );
  const parsed: DaProducerPeerResult = {
    peerId: nonEmptyString(input.peerId, `${label}.peerId`),
    signerIndex: nonNegativeInteger(input.signerIndex, `${label}.signerIndex`),
    protocolId: nonEmptyString(input.protocolId, `${label}.protocolId`),
    status: oneOf(input.status, `${label}.status`, [
      "accepted",
      "duplicate",
      "conflict",
      "rejected",
      "deferred",
      "transport_error",
    ]),
    payloadHash: parseLowerHex(input.payloadHash, `${label}.payloadHash`, 32),
    ...(input.error === undefined
      ? {}
      : { error: nonEmptyString(input.error, `${label}.error`) }),
  };
  if ((parsed.status === "transport_error") !== (parsed.error !== undefined)) {
    throw new Error(`${label}.error/status binding is inconsistent`);
  }
  return parsed;
};

const parseDaGateResultWithKind = (
  value: unknown,
  expectedKind: DaGateResult["kind"],
  label: string,
): DaGateResult => {
  const commonKeys = [
    "schemaVersion",
    "kind",
    "headerHash",
    "status",
    "nextSafeAction",
    "checkedAt",
    "acceptedPeers",
    "threshold",
    "announcementTopic",
    "announcementRecipients",
    "peerResults",
    "reason",
  ] as const;
  const input = exactRecord(
    value,
    label,
    expectedKind === "wait"
      ? [...commonKeys, "attempts", "timedOut"]
      : commonKeys,
    ["deploymentFingerprint"],
  );
  if (input.schemaVersion !== E2E_DA_GATE_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_DA_GATE_SCHEMA_VERSION}`,
    );
  }
  if (input.kind !== expectedKind) {
    throw new Error(`${label}.kind must be ${expectedKind}`);
  }
  const observation: DaGateObservation = {
    headerHash: parseLowerHex(input.headerHash, `${label}.headerHash`, 28),
    ...(input.deploymentFingerprint === undefined
      ? {}
      : {
          deploymentFingerprint: parseLowerHex(
            input.deploymentFingerprint,
            `${label}.deploymentFingerprint`,
            32,
          ),
        }),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "pending",
      "blocked",
      "failed",
    ]),
    nextSafeAction: oneOf(input.nextSafeAction, `${label}.nextSafeAction`, [
      "continue",
      "wait_for_da_payload_publication",
      "inspect_da_publication",
      "configure_da_libp2p",
    ]),
    checkedAt: isoTimestamp(input.checkedAt, `${label}.checkedAt`),
    acceptedPeers: nonNegativeInteger(
      input.acceptedPeers,
      `${label}.acceptedPeers`,
    ),
    threshold: nullable(input.threshold, `${label}.threshold`, positiveInteger),
    announcementTopic: nullableNonEmptyString(
      input.announcementTopic,
      `${label}.announcementTopic`,
    ),
    announcementRecipients: arrayOf(
      input.announcementRecipients,
      `${label}.announcementRecipients`,
      nonEmptyString,
    ),
    peerResults: arrayOf(
      input.peerResults,
      `${label}.peerResults`,
      parseDaProducerPeerResult,
    ),
    reason: nonEmptyString(input.reason, `${label}.reason`),
  };
  const acceptedResultCount = observation.peerResults.filter(
    (result) => result.status === "accepted" || result.status === "duplicate",
  ).length;
  const peerIds = observation.peerResults.map((result) => result.peerId);
  const signerIndexes = observation.peerResults.map(
    (result) => result.signerIndex,
  );
  const payloadHashes = new Set(
    observation.peerResults.map((result) => result.payloadHash),
  );
  const announcementRecipients = observation.announcementRecipients;
  const publicationSatisfied =
    observation.announcementTopic !== null &&
    observation.threshold !== null &&
    observation.acceptedPeers >= observation.threshold;
  const expectedAction: Record<
    DaGateStatus,
    DaGateProbeResult["nextSafeAction"]
  > = {
    satisfied: "continue",
    pending: "wait_for_da_payload_publication",
    blocked: "configure_da_libp2p",
    failed: "inspect_da_publication",
  };
  if (
    observation.acceptedPeers !== acceptedResultCount ||
    new Set(peerIds).size !== peerIds.length ||
    new Set(signerIndexes).size !== signerIndexes.length ||
    payloadHashes.size > 1 ||
    new Set(announcementRecipients).size !== announcementRecipients.length ||
    (observation.announcementTopic === null &&
      announcementRecipients.length !== 0) ||
    observation.nextSafeAction !== expectedAction[observation.status] ||
    (observation.status === "satisfied" && !publicationSatisfied) ||
    (observation.status === "pending" && publicationSatisfied) ||
    (observation.deploymentFingerprint !== undefined &&
      observation.announcementTopic !== null &&
      observation.announcementTopic !==
        `/midgard/${observation.deploymentFingerprint}/da/payload-announcements/1`)
  ) {
    throw new Error(`${label} publication evidence is inconsistent`);
  }
  if (expectedKind === "probe") {
    return {
      schemaVersion: E2E_DA_GATE_SCHEMA_VERSION,
      kind: "probe",
      ...observation,
    };
  }
  const parsed: WaitForDaGateResult = {
    schemaVersion: E2E_DA_GATE_SCHEMA_VERSION,
    kind: "wait",
    ...observation,
    attempts: positiveInteger(input.attempts, `${label}.attempts`),
    timedOut: booleanValue(input.timedOut, `${label}.timedOut`),
  };
  if (parsed.timedOut && parsed.status !== "pending") {
    throw new Error(`${label} timeout/status binding is inconsistent`);
  }
  return parsed;
};

export const parseDaGateProbeResult = (value: unknown): DaGateProbeResult =>
  parseDaGateResultWithKind(
    value,
    "probe",
    "DA gate probe",
  ) as DaGateProbeResult;

export const parseWaitForDaGateResult = (value: unknown): WaitForDaGateResult =>
  parseDaGateResultWithKind(
    value,
    "wait",
    "DA gate wait",
  ) as WaitForDaGateResult;

export const parseDaGateResult = (value: unknown): DaGateResult => {
  const input = exactRecord(
    value,
    "DA gate result",
    ["kind"],
    [
      "schemaVersion",
      "headerHash",
      "deploymentFingerprint",
      "status",
      "nextSafeAction",
      "checkedAt",
      "acceptedPeers",
      "threshold",
      "announcementTopic",
      "announcementRecipients",
      "peerResults",
      "reason",
      "attempts",
      "timedOut",
    ],
  );
  return input.kind === "probe"
    ? parseDaGateProbeResult(value)
    : input.kind === "wait"
      ? parseWaitForDaGateResult(value)
      : (() => {
          throw new Error("DA gate result.kind must be probe or wait");
        })();
};

const classifyProbe = ({
  normalizedHeaderHash,
  publicationReport,
}: {
  readonly normalizedHeaderHash: string;
  readonly publicationReport: DaProducerPublicationReport;
}): Pick<DaGateProbeResult, "status" | "nextSafeAction" | "reason"> => {
  if (publicationReport.headerHash !== normalizedHeaderHash) {
    return {
      status: "failed",
      nextSafeAction: "inspect_da_publication",
      reason: `publication report header ${publicationReport.headerHash} does not match requested header ${normalizedHeaderHash}`,
    };
  }
  if (!publicationReport.configured) {
    return {
      status: "blocked",
      nextSafeAction: "configure_da_libp2p",
      reason:
        publicationReport.reason ?? "libp2p DA publication is not configured",
    };
  }
  if (publicationReport.announcement === undefined) {
    return {
      status: "pending",
      nextSafeAction: "wait_for_da_payload_publication",
      reason:
        "payload-submit ran but payload announcement has not been published",
    };
  }
  if (!publicationSatisfied(publicationReport)) {
    return {
      status: "pending",
      nextSafeAction: "wait_for_da_payload_publication",
      reason: `accepted peer count ${publicationReport.acceptedPeers.toString()} is below threshold ${publicationReport.threshold?.toString() ?? "unknown"}`,
    };
  }
  const failedPeerCount = publicationReport.peerResults.filter(
    (result) => result.status === "transport_error",
  ).length;
  return {
    status: "satisfied",
    nextSafeAction: "continue",
    reason:
      failedPeerCount === 0
        ? "DA payload was published over libp2p"
        : `DA payload reached threshold with ${failedPeerCount.toString()} peer failure(s)`,
  };
};

export const probeDaGate = async ({
  headerHash,
  publicationReport,
  now = new Date(),
}: ProbeDaGateOptions): Promise<DaGateProbeResult> => {
  const normalizedHeaderHash = headerHash.toLowerCase();
  if (!HEADER_HASH_REGEX.test(normalizedHeaderHash)) {
    throw new Error("headerHash must be a 56-character hex string.");
  }
  const classified = classifyProbe({
    normalizedHeaderHash,
    publicationReport,
  });
  return parseDaGateProbeResult({
    schemaVersion: E2E_DA_GATE_SCHEMA_VERSION,
    kind: "probe",
    headerHash: normalizedHeaderHash,
    ...(publicationReport.deploymentFingerprint === undefined
      ? {}
      : { deploymentFingerprint: publicationReport.deploymentFingerprint }),
    status: classified.status,
    nextSafeAction: classified.nextSafeAction,
    checkedAt: now.toISOString(),
    acceptedPeers: publicationReport.acceptedPeers,
    threshold: publicationReport.threshold ?? null,
    announcementTopic: publicationReport.announcement?.topic ?? null,
    announcementRecipients: publicationReport.announcement?.recipients ?? [],
    peerResults: publicationReport.peerResults,
    reason: classified.reason,
  });
};

export const waitForDaGate = async ({
  timeoutMs = DEFAULT_WAIT_TIMEOUT_MS,
  intervalMs = DEFAULT_WAIT_INTERVAL_MS,
  sleep = (milliseconds) =>
    new Promise((resolve) => setTimeout(resolve, milliseconds)),
  probePublication,
  ...options
}: WaitForDaGateOptions): Promise<WaitForDaGateResult> => {
  const startedAt = Date.now();
  let attempts = 0;
  for (;;) {
    attempts += 1;
    const latest = await probeDaGate({
      ...options,
      publicationReport: await probePublication(),
    });
    if (latest.status !== "pending") {
      return parseWaitForDaGateResult({
        ...latest,
        kind: "wait",
        attempts,
        timedOut: false,
      });
    }
    if (Date.now() - startedAt >= timeoutMs) {
      return parseWaitForDaGateResult({
        ...latest,
        kind: "wait",
        attempts,
        timedOut: true,
      });
    }
    await sleep(intervalMs);
  }
};
