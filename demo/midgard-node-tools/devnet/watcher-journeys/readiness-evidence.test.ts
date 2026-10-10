import { createHash } from "node:crypto";
import { mkdir, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofReleaseFinalityPolicyDigest } from "@al-ft/midgard-fault-proofs";
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
const native = vi.hoisted(() => ({ chain: undefined as unknown }));
vi.mock(
  "./readiness-evidence.read-journey-canonical-transactions.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("./readiness-evidence.read-journey-canonical-transactions.js")
      >();
    return {
      ...original,
      readDecisions: async () => [FAULT],
      readJourneyCanonicalTransactions: async (path: string) =>
        native.chain ?? original.readJourneyCanonicalTransactions(path),
    };
  },
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
  native.chain = undefined;
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

/**
 * A release-depth stamp anchors `terminal_included`; the verifier re-checks the
 * proof-token, removal and successor depths in the native capture (stubbed
 * here). Passing that check is told apart by the next refusal: the synthetic
 * capture holds no terminal confirmation block.
 */
describe("journey result evidence: release-depth anchor", () => {
  const depth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
  const [INIT_TX, PROOF_TX, REMOVAL_TX, SUCCESSOR_TX] = [
    "1a",
    "2b",
    "3c",
    "4d",
  ].map((byte) => byte.repeat(32));
  const included = {
    proofToken: { createdByTxHash: PROOF_TX },
    correction: { removalTxHash: REMOVAL_TX },
    observedAt: { blockHash: "5e".repeat(32), slot: "1", confirmationDepth: 1 },
  };
  const anchored = {
    manifest: {
      manifestId: FINGERPRINT,
      l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
      steps: { initProtocol: { txHash: INIT_TX } },
    },
    initialization: { txHash: INIT_TX },
  } as unknown as JourneyEvidenceDeployment;
  const journalEntry = (event: unknown) => ({
    workflowId: "synthetic-workflow",
    identity: {
      deploymentFingerprint: FINGERPRINT,
      category: CATEGORY,
      target: { kind: "state_queue_header", headerHash: CURRENT.headerHash },
      decisionDigest: DECISION_DIGEST,
    },
    event,
  });
  const includedEvent = { kind: "terminal_included", terminal: included };
  /** Native blocks holding each transaction, at the tip's release depth. */
  const capture = (shallow?: string) => {
    const tip = 100n + BigInt(depth) - 1n;
    native.chain = {
      tip: { hash: "6f".repeat(32), blockNo: tip },
      blocks: new Map(),
      transactions: new Map(
        [INIT_TX, PROOF_TX, REMOVAL_TX, SUCCESSOR_TX].map((hash) => [
          hash,
          { blockNo: hash === shallow ? 101n : 100n },
        ]),
      ),
    };
  };
  const stage = async ({
    terminalKind,
    finalized = [],
  }: {
    terminalKind?: string;
    finalized?: unknown[];
  }) => {
    const artifact = (name: string, value: unknown) =>
      writeJourneyArtifact(join(directory, name), value);
    const saved = [journalEntry(includedEvent)];
    await artifact("result.json", {
      status: "passed",
      executionPolicy: "authenticated-inclusion",
      category: CATEGORY,
      deploymentFingerprint: FINGERPRINT,
      completion: included,
      successor: SUCCESSOR.headerHash,
    });
    await artifact("successor.json", {
      ...SUCCESSOR,
      commitTxHash: SUCCESSOR_TX,
    });
    await artifact("completed-workflow.json", saved);
    await artifact("finalized-workflow.json", [...saved, ...finalized]);
    await artifact("finalized-evidence-stamp.json", {
      category: CATEGORY,
      headerHash: CURRENT.headerHash,
      deploymentFingerprint: FINGERPRINT,
      releaseFinalityPolicyDigest: computeFraudProofReleaseFinalityPolicyDigest(
        DEPLOYMENT_MANIFEST_L1_FINALITY,
      ),
      finalityDepth: depth,
      nativeEvidencePath: join(directory, "native-chain.ndjson"),
      terminalKind,
      terminal: included,
    });
    await retainVerified([PREDECESSOR, ADOPTED_HEAD, SUCCESSOR]);
  };
  const verifyAnchored = () =>
    verifyJourneyResultEvidence(runDirectory, directory, CATEGORY, anchored);

  it("accepts an included terminal whose effects are release-depth deep", async () => {
    await stage({ terminalKind: "terminal_included" });
    capture();
    await expect(verifyAnchored()).rejects.toThrow(
      /Terminal confirmation point is not canonical/u,
    );
  });

  it.each([PROOF_TX, REMOVAL_TX, SUCCESSOR_TX])(
    "refuses an included anchor with a transaction one block short of release depth (%s)",
    async (shallow) => {
      await stage({ terminalKind: "terminal_included" });
      capture(shallow);
      await expect(verifyAnchored()).rejects.toThrow(
        /insufficient finality depth/u,
      );
    },
  );

  it.each(["completed", undefined])(
    "keeps the completed anchor for a stamp of kind %s",
    async (terminalKind) => {
      await stage({
        terminalKind,
        finalized: [journalEntry({ kind: "completed", terminal: included })],
      });
      capture();
      await expect(verifyAnchored()).rejects.toThrow(
        /Terminal confirmation point is not canonical/u,
      );
    },
  );

  it("reads a stamp without a terminal kind as a completed anchor", async () => {
    await stage({});
    capture();
    await expect(verifyAnchored()).rejects.toThrow(
      /lacks the stamp's anchored terminal/u,
    );
  });

  it("refuses an included anchor over a journal that already completed", async () => {
    await stage({
      terminalKind: "terminal_included",
      finalized: [
        journalEntry({ kind: "completed", terminal: included }),
        journalEntry(includedEvent),
      ],
    });
    capture();
    await expect(verifyAnchored()).rejects.toThrow(
      /ignored a completed terminal/u,
    );
  });
});
