import { deriveFraudProofRawL1CompletedTerminal } from "@al-ft/midgard-fault-proofs";
import { fixture as terminalFixture } from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";

import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import {
  deploymentIdentity,
  finality,
  key,
} from "../support/fault-proof-funding-fixture.sources-for.js";
import { type Fixture } from "./superseded-attempt-replacement.js";

/** The correction's terminal, created by `createdBy`, at `confirmationDepth`
 * (by default within the recovery window). */
export const correctionOn = async (
  fixture: Fixture,
  createdBy: string,
  confirmationDepth?: number,
) => {
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
    ...(confirmationDepth === undefined ? {} : { confirmationDepth }),
  });
  const derived = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (derived.kind !== "removed")
    throw new Error("raw fixture omitted its correction");
  const terminal = derived.terminal;
  return {
    policy: verifiedFinality.policy,
    terminal: {
      ...terminal,
      proofToken: { ...terminal.proofToken, createdByTxHash: createdBy },
    },
  };
};
