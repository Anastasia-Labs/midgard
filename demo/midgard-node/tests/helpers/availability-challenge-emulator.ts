import "node:fs";
import "node:path";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "@lucid-evolution/scalus-uplc";
import "@lucid-evolution/uplc";
import "@noble/hashes/blake2.js";
import "effect";
import "vitest";
import "./availability-challenge.js";
import "./mainnet-protocol-parameters.js";
import "./real-midgard-contracts.js";
import "./availability-challenge-emulator.measure-availability-transaction.js";
import "./availability-challenge-emulator.availability-redeemer-script.js";
import "./availability-challenge-emulator.create-fixture.js";
import "./availability-challenge-emulator.open-availability.js";
import "./availability-challenge-emulator.attest-availability.js";
import "./availability-challenge-emulator.build-availability-publication.js";
import "./availability-challenge-emulator.build-availability-timeout.js";
import "./availability-challenge-emulator.commit-availability-block.js";

import { credentialToRewardAddress } from "@lucid-evolution/lucid";
export {
  attestAvailability,
  availabilityDeployment,
  reportAvailabilityScenario,
} from "./availability-challenge-emulator.attest-availability.js";
export {
  type AvailabilityFixtureOptions,
  availabilityRedeemerScript,
  availabilityReferenceScriptTargets,
} from "./availability-challenge-emulator.availability-redeemer-script.js";
export {
  buildAvailabilityClose,
  buildAvailabilityPublication,
  buildAvailabilitySettlement,
} from "./availability-challenge-emulator.build-availability-publication.js";
export {
  advanceAvailabilityDeadline,
  type AvailabilityCommitFixture,
  buildAvailabilityTimeout,
  createAvailabilityCommitFixture,
  liveAvailabilityTarget,
} from "./availability-challenge-emulator.build-availability-timeout.js";
export {
  type AvailabilityCommittedBlock,
  commitAvailabilityBlock,
  withLiveAvailabilityQueue,
} from "./availability-challenge-emulator.commit-availability-block.js";
export { assertAvailabilityRefusal } from "./availability-challenge-emulator.create-fixture.js";
export {
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  AVAILABILITY_COLLATERAL_COIN_LOVELACE,
  AVAILABILITY_DEFAULT_POOL_LOVELACE,
  AVAILABILITY_EMULATOR_PARAMETERS,
  AVAILABILITY_PROFILE,
  AVAILABILITY_QUEUE_NODE_LOVELACE,
  AVAILABILITY_REQUIRED_COLLATERAL_LOVELACE,
  AVAILABILITY_TIMING,
  type AvailabilityMeasurement,
  type AvailabilityRefusal,
  type AvailabilityRefusalExpectation,
  type AvailabilityRefusalPurpose,
  createAvailabilityEmulatorLucid,
  lastAvailabilityEvaluationFailure,
  measureAvailabilityTransaction,
  parseAvailabilityEvaluationFailure,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
export {
  type AvailabilityFixture,
  createAvailabilityFixture,
  type OpenAvailability,
  openAvailability,
} from "./availability-challenge-emulator.open-availability.js";

export { credentialToRewardAddress };
