import {
  type DaProducerPublicationReport,
  publicationSatisfied,
} from "@/da/libp2p-producer.js";

export const E2E_DA_GATE_SCHEMA_VERSION = "midgard-e2e-da-gate-v1";

export type DaGateStatus = "satisfied" | "pending" | "blocked" | "failed";

export type DaGateProbeResult = {
  readonly schemaVersion: typeof E2E_DA_GATE_SCHEMA_VERSION;
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

export type WaitForDaGateResult = DaGateProbeResult & {
  readonly attempts: number;
  readonly timedOut: boolean;
};

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
  return {
    schemaVersion: E2E_DA_GATE_SCHEMA_VERSION,
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
  };
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
      return { ...latest, attempts, timedOut: false };
    }
    if (Date.now() - startedAt >= timeoutMs) {
      return { ...latest, attempts, timedOut: true };
    }
    await sleep(intervalMs);
  }
};
