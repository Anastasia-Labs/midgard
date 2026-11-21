/**
 * A doubleSpend fault the production classifier detects, twice: `old` at
 * the first observed depth and `fresh` one block deeper, each journaled as
 * a live decision under a fresh journal root, with the block, its header
 * observation and the shared deployment authority the decisions bind.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import { CML } from "@lucid-evolution/lucid";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "./deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
import { testStorageRoot } from "./test-storage-root.js";
import { TEST_JOURNAL_KEY } from "./watcher-journal-fixture.js";

const directories: string[] = [];

/** Removes every journal root the fixture made. */
export const cleanupClassifiedFaultFixtures = async (): Promise<void> => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
};

export const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));

/** The prover wallet `key` signs for. */
export const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(key.to_public().hash()),
)
  .to_address()
  .to_bech32();

export const deploymentIdentity =
  makeWatcherDeploymentAuthorityFixture().result;

export const finality =
  watcherDeploymentReleaseFinalityAuthority(deploymentIdentity);

/** One security-grade DA source serving `payloadEnvelopeCbor`. */
export const sourcesFor = (payloadEnvelopeCbor: Buffer) => [
  {
    sourceId: "libp2p-test",
    fetchPayloadByHeaderHash: async () => ({
      ok: true as const,
      provenance: {
        trustClass: "public_or_permissionless_da" as const,
        sourceId: "libp2p-test/peer-a",
        grade: "security" as const,
      },
      sourceId: "libp2p-test",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    }),
  },
];

/** The block, its header observation, and the fault classified at depths 30
 * (`old`) and 31 (`fresh`), with nothing written anywhere. */
export const classifiedDoubleSpend = async ({
  headerEndTime = 20n,
}: { readonly headerEndTime?: bigint } = {}) => {
  const sharedInput = outRefCbor(91, 0n);
  const fixture = await buildCanonicalBlockFixture({
    endTime: headerEndTime,
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: finality,
  });
  const classify = async (confirmationDepth: number) => {
    const observed = { ...observation, confirmationDepth };
    const decision = await classifyHeader({
      classifier,
      observation: observed,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation: observed,
          minimumConfirmationDepth: 30,
        }),
      sources: sourcesFor(fixture.payloadEnvelopeCbor),
    });
    if (decision.decision !== "fault_detected")
      throw new Error("fixture must classify double spend");
    return decision;
  };
  return {
    fixture,
    observation,
    old: await classify(30),
    fresh: await classify(31),
  };
};

/** `classifiedDoubleSpend` with both decisions journaled under a fresh
 * journal root. */
export const setupClassifiedFaultFixture = async (
  options: { readonly headerEndTime?: bigint } = {},
) => {
  const journalRoot = await mkdtemp(
    join(testStorageRoot, ".watcher-classified-fault-"),
  );
  directories.push(journalRoot);
  const { fixture, observation, old, fresh } =
    await classifiedDoubleSpend(options);
  const decisionJournal = await openWatcherFaultDecisionJournal({
    directory: journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: installedLaunchScope(old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });
  await decisionJournal.appendLiveDecision(old);
  await decisionJournal.appendLiveDecision(fresh);
  return { journalRoot, fixture, observation, old, fresh, decisionJournal };
};
