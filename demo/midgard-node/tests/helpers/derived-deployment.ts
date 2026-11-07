/**
 * A derived deployment (no manifest) for a process under test: the run
 * state its contract bundle reads the reference-script auth policy from, and
 * the explicit environment a fresh bundle derives its scripts with (event
 * history bounds, history protection, the DA availability block). The bundle
 * loads both before its first L1 read. `stubEnvironment` points the process
 * at them.
 */
import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  createReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import {
  createDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "../../src/e2e/run-state.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./availability-challenge.js";

/** The publisher's key and the slot clock are all a policy needs. */
const PUBLISHER = {
  wallet: () => ({
    address: async () =>
      credentialToAddress("Preprod", { type: "Key", hash: "ab".repeat(28) }),
  }),
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
} as unknown as LucidEvolution;

const DERIVATION_ENVIRONMENT = {
  MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES: 512,
  MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES: 5_000,
  MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES: 512,
  MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS: 3_600_000,
  MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH:
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.chunkByteLength,
  MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH:
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.trancheByteLength,
  MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT:
    TEST_AVAILABILITY_CHALLENGE.responseGeometry.maxTrancheCount,
  MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.challengerBondLovelace,
  MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.maxOpenFeeLovelace,
  MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.maxPublicationFeeLovelace,
  MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.maxSettlementFeeLovelace,
  MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.maxCloseFeeLovelace,
  MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE:
    TEST_AVAILABILITY_CHALLENGE.maxTimeoutFeeLovelace,
} as const;

export type DerivedDeployment = Readonly<{
  /** Stubs the run state's path and the derivation environment. */
  stubEnvironment: () => void;
  remove: () => Promise<void>;
}>;

export const derivedDeployment = async (): Promise<DerivedDeployment> => {
  const directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-derived-deployment-")),
  );
  const runStatePath = join(directory, "midgard-run-state.json");
  const policy = referenceScriptAuthPolicyDeploymentInfo(
    await createReferenceScriptAuthPolicy(PUBLISHER, 1_000, 10_000),
  );
  await writeDeploymentRunStateAtomic(
    runStatePath,
    createDeploymentRunState({
      mode: "resume",
      identity: {
        referenceScriptAuthPolicyId: policy.policyId,
        referenceScriptAuthPolicy: {
          policyId: policy.policyId,
          nativeScript: policy.nativeScript,
        },
      },
    }),
  );
  return {
    stubEnvironment: () => {
      vi.stubEnv("MIDGARD_RUN_STATE_PATH", runStatePath);
      for (const [name, value] of Object.entries(DERIVATION_ENVIRONMENT))
        vi.stubEnv(name, String(value));
    },
    remove: () => rm(directory, { recursive: true, force: true }),
  };
};
