import { createHash } from "node:crypto";
import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock, JourneyCategory } from "./fixture.js";
import {
  type JourneyEvidenceDeployment,
  verifyJourneyResultEvidence,
} from "./readiness-evidence.js";
import { JOURNEY_VERIFIED_HEADERS_ARTIFACT } from "./verified-headers.js";

/**
 * The verifier's healthy-block check, on a synthetic run directory holding
 * the artifacts it reads before that check. Stubbed: the finalized-manifest
 * identity check, header hashing (a fake header carries its own hash), the
 * workflow journal validator (it returns the saved entries) and the
 * fault-decision journal audit (it returns one fault decision). An empty
 * native capture makes the verifier stop just after the check, so a run that
 * passes it is told apart by the next refusal.
 */

vi.mock(
  "@al-ft/midgard-core/deployment-manifest-identity",
  async (importOriginal) => ({
    ...(await importOriginal<object>()),
    verifyFinalizedDeploymentManifest: () => {},
  }),
);
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => ({
  ...(await importOriginal<object>()),
  hashBlockHeader: (header: { hash: string }) => Effect.succeed(header.hash),
}));
vi.mock("@al-ft/midgard-fault-proofs", async (importOriginal) => ({
  ...(await importOriginal<object>()),
  validateFraudProofWorkflowJournal: ({ entries }: { entries: unknown[] }) =>
    entries,
}));
vi.mock(
  "./readiness-evidence.read-journey-canonical-transactions.js",
  async (importOriginal) => ({
    ...(await importOriginal<object>()),
    readDecisions: async () => [FAULT],
  }),
);

const CATEGORY = "invalidOneStepTransition" as JourneyCategory;
const FINGERPRINT = "f0".repeat(32);
const DECISION_DIGEST = "d0".repeat(32);
const sha256 = (bytes: Buffer) =>
  createHash("sha256").update(bytes).digest("hex");

const block = (byte: string, prevHeaderHash = "00".repeat(28)) => {
  const headerHash = byte.repeat(28);
  return {
    header: { hash: headerHash, prevHeaderHash },
    headerHash,
    payloadEnvelopeCbor: Buffer.from(`envelope-${byte}`),
  } as unknown as JourneyBlock;
};

const PREDECESSOR = block("a1");
const CURRENT = block("b2", PREDECESSOR.headerHash);
// A resumed proof adopted a later head, so the successor's parent is not the
// staged predecessor.
const ADOPTED_HEAD = block("c3", PREDECESSOR.headerHash);
const SUCCESSOR = { ...block("d4", ADOPTED_HEAD.headerHash), commitTxHash: "" };
const FAULT = {
  headerHash: CURRENT.headerHash,
  decision: "fault_detected",
  category: CATEGORY,
  decisionDigest: DECISION_DIGEST,
  payloadEnvelopeSha256: sha256(CURRENT.payloadEnvelopeCbor),
};
const TERMINAL = { completion: "synthetic" };
const deployment = {
  manifest: { manifestId: FINGERPRINT },
} as unknown as JourneyEvidenceDeployment;

const verified = (value: JourneyBlock) => ({
  kind: "verification",
  headerHash: value.headerHash,
  outcome: "verified",
  payloadEnvelopeSha256: sha256(value.payloadEnvelopeCbor),
});

let runDirectory: string;
let directory: string;
beforeEach(async () => {
  runDirectory = await mkdtemp(join(tmpdir(), "journey-result-evidence-"));
  directory = join(runDirectory, "journey");
  await mkdir(directory);
  const artifact = (name: string, value: unknown) =>
    writeJourneyArtifact(join(directory, name), value);
  await artifact("result.json", {
    status: "passed",
    category: CATEGORY,
    deploymentFingerprint: FINGERPRINT,
    completion: TERMINAL,
    successor: SUCCESSOR.headerHash,
  });
  await artifact("staged.json", { predecessor: PREDECESSOR, current: CURRENT });
  await artifact("successor.json", SUCCESSOR);
  await artifact("successor-predecessor.json", ADOPTED_HEAD);
  await artifact("completed-workflow.json", [
    {
      workflowId: "synthetic-workflow",
      identity: {
        deploymentFingerprint: FINGERPRINT,
        category: CATEGORY,
        target: { kind: "state_queue_header", headerHash: CURRENT.headerHash },
        decisionDigest: DECISION_DIGEST,
      },
      event: { kind: "completed", terminal: TERMINAL },
    },
  ]);
  await writeFile(join(directory, "native-chain.ndjson"), "");
});
afterEach(async () => {
  await rm(runDirectory, { recursive: true, force: true });
});

const retainVerified = (blocks: readonly JourneyBlock[]) =>
  writeJourneyArtifact(
    join(directory, JOURNEY_VERIFIED_HEADERS_ARTIFACT),
    blocks.map(verified),
  );
const verify = () =>
  verifyJourneyResultEvidence(runDirectory, directory, CATEGORY, deployment);

describe("journey result evidence: healthy blocks", () => {
  it("passes the check with a verified record for each healthy block", async () => {
    await retainVerified([PREDECESSOR, ADOPTED_HEAD, SUCCESSOR]);
    await expect(verify()).rejects.toThrow(
      /No canonical native tip was captured/u,
    );
  });

  it("refuses a run directory without verified-header evidence", async () => {
    await expect(verify()).rejects.toThrow(
      /Healthy predecessor\/successor decision is missing/u,
    );
  });

  it("refuses evidence that lacks the successor's parent", async () => {
    await retainVerified([PREDECESSOR, SUCCESSOR]);
    await expect(verify()).rejects.toThrow(
      /Healthy predecessor\/successor decision is missing/u,
    );
  });
});
