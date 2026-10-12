import { type Header } from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import { type UTxO } from "@lucid-evolution/lucid";

import { type ValidationTraceChallenge } from "../../src/workflow/challenge-authority.js";
import { type recordCrossBlockRawEmulator } from "./cross-block-raw-emulator.js";
import { type buildMinimalFaultProofContracts } from "./emulator/contracts.js";
import { type createValidationDisputeParties } from "./emulator/dispute-staging.js";

/** What the journey stages from a fixture. */
export type InstalledValidationJourneyFixture = Readonly<{
  header: Header;
  /**
   * A predecessor committed first, so the challenged header is the second
   * block on the state queue.
   */
  predecessorHeader?: Header;
  operatorTrace: Pick<DeterministicValidationMachineTrace, "tree">;
  evidence: Readonly<{
    oneStepArgument: Readonly<{
      resolverIndex: number;
      semanticResolverIndex: number;
    }>;
  }>;
}>;

/** The ledger a challenge supplier reads once every reference is staged. */
export type InstalledValidationJourneyStaged<
  Fixture extends InstalledValidationJourneyFixture,
> = Readonly<{
  emulator: Awaited<
    ReturnType<typeof createValidationDisputeParties>
  >["emulator"];
  recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  contracts: Awaited<ReturnType<typeof buildMinimalFaultProofContracts>>;
  nonceUtxo: UTxO;
  fixture: Fixture;
  setup: Readonly<{ headerHash: string; fraudulentBlockOutRef: string }>;
}>;

/**
 * The challenge the workflow runs, and the deployment its coordinate names
 * (the binding carries that fingerprint). Without a challenge the journey
 * stages the ledger and the workflow context only.
 */
export type InstalledValidationChallengeSupply = Readonly<{
  challenge?: ValidationTraceChallenge;
  deploymentFingerprint: string;
  decisionDigest?: string;
}>;
