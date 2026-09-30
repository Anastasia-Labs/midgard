import { createHash } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { h32ForOrdinal } from "@al-ft/midgard-test-support/hex";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach } from "vitest";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
  type DeploymentManifestIdentityContext,
} from "../src/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
} from "../src/deployment-manifest.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";

export const PRODUCER_KEY = `seed:${"00".repeat(31)}01`;

export const COMMITTEE_KEY = `seed:${"00".repeat(31)}02`;

export const PUBLIC_RETAINED_DA_KEY = `seed:${"00".repeat(31)}03`;

export const DA_VKEY = "11".repeat(32);

export const PRODUCER_DA_VKEY = "22".repeat(32);

const CARDANO_PARAMETERS = TEST_CARDANO_PROTOCOL_PARAMETERS;

const MANIFEST_IDENTITY_CONTEXT: DeploymentManifestIdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  cardanoProtocolParameters: {
    snapshot: CARDANO_PARAMETERS,
    digest: computeDeploymentManifestJsonDigest(CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestJsonDigest(
      normalizeDeploymentManifestJsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [DA_VKEY],
    committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
      DA_VKEY,
    ]),
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
      envelopeEncoding: "identity" as const,
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS,
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    },
  },
  artifacts: {
    blueprintHash: "33".repeat(32),
  },
};

export const tempDirs: string[] = [];

afterEach(async () => {
  await Promise.all(tempDirs.map((dir) => rm(dir, { recursive: true })));
  tempDirs.length = 0;
});

export const writeFinalizedDeploymentInfo = async (
  mutate?: (manifest: Record<string, unknown>) => void,
): Promise<{
  readonly path: string;
  readonly manifestId: string;
  readonly sha256: string;
}> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-da-runtime-manifest-"));
  tempDirs.push(dir);
  const contracts = withRealEventHistoryForTest(
    await Effect.runPromise(
      AlwaysSucceedsContract.pipe(
        Effect.provide(AlwaysSucceedsContract.Default),
      ),
    ),
    { txHash: "ab".repeat(32), outputIndex: 0 },
  );
  const nativeScriptCbor = "820500";
  const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId: validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    }),
    nativeScript: {
      type: "Native",
      cborHex: nativeScriptCbor,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "test fixture" },
  };
  const referenceScriptOutRefs = new Map(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, index) => [
        contractName,
        {
          txHash: h32ForOrdinal(index + 1),
          outputIndex: 0,
        },
      ],
    ),
  );
  const fraudProofCatalogue = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      fraudProofsToIndexedValidators(contracts.fraudProofs),
    ),
  );
  const manifest = buildDeploymentManifest(
    buildContractDeploymentInfoFromContracts(
      contracts,
      referenceScriptAuthPolicy,
      referenceScriptOutRefs,
      fraudProofCatalogue,
    ),
    {
      network: "Preprod",
      ...MANIFEST_IDENTITY_CONTEXT,
      referenceScriptDeployAddress: "addr_test1reference",
      hubOracleOneShotTxHash: "ab".repeat(32),
      hubOracleOneShotOutputIndex: 0,
      hubOracleOneShotStatus: "consumed_by_init",
      steps: {
        initProtocol: { status: "complete" },
        availabilityRegistration: { status: "complete" },
      },
    },
  ) as unknown as Record<string, unknown>;
  mutate?.(manifest);
  delete manifest.manifestId;
  manifest.manifestId = computeDeploymentManifestId(
    manifest as unknown as Omit<DeploymentManifest, "manifestId">,
  );
  const raw = `${JSON.stringify(manifest, null, 2)}\n`;
  const path = join(dir, "contract-deployment-info.json");
  await writeFile(path, raw, "utf8");
  return {
    path,
    manifestId: String(manifest.manifestId),
    sha256: createHash("sha256").update(raw).digest("hex"),
  };
};
