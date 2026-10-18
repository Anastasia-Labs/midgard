import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { memoizeForRun } from "@al-ft/midgard-test-support/run-shared-fixtures";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildContractDeploymentInfoFromContracts } from "../../src/commands/contract-deployment-info.js";
import { loadRealBlueprintSha256 } from "../../src/services/midgard-contracts.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

/**
 * The DA committee's deployment input: every current applied contract with
 * synthetic reference outpoints, as JSON data.
 */
export type DaDeploymentInfo = {
  readonly schemaVersion: string;
  readonly fixtureKind: string;
  readonly manifestId: string;
  readonly consensusProfile: unknown;
  readonly artifacts: { readonly blueprintHash: string };
  readonly contracts: Readonly<Record<string, unknown>>;
};

const produceDaDeploymentInfo = async (): Promise<DaDeploymentInfo> => {
  const mintingScript = {
    type: "Native" as const,
    script: "820500",
  };
  const policyId = validatorToScriptHash(mintingScript);
  const auth: ReferenceScriptAuthPolicyDeploymentInfo = {
    policyId,
    nativeScript: {
      type: "Native",
      cborHex: mintingScript.script,
      expiresAtSlot: 0,
      expiresAtUnixTime: 0,
      timelockDurationMs: 1,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: { required: true, rule: "DA codec fixture" },
  };
  const contracts = await loadRealMidgardContractsForTest(
    { txHash: "ab".repeat(32), outputIndex: 0 },
    { policyId, mintingScript, mintingScriptCBOR: mintingScript.script },
  );
  // These deterministic references exercise manifest codecs only. Publication
  // acceptance belongs to the installed workflow tests, which submit and
  // confirm their reference transactions instead.
  const referenceNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const references = new Map(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.filter((name) =>
      referenceNames.has(name),
    ).map((name, index) => [
      name,
      { txHash: (index + 1).toString(16).padStart(64, "0"), outputIndex: 0 },
    ]),
  );
  const deployment = buildContractDeploymentInfoFromContracts(
    contracts,
    auth,
    references,
  );
  // A JSON round trip leaves the plain data a deployment file carries: no
  // undefined members, no class instances.
  return JSON.parse(
    JSON.stringify({
      schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
      fixtureKind: "codec fixture with synthetic reference outpoints",
      manifestId: "ab".repeat(32),
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      artifacts: {
        blueprintHash: await Effect.runPromise(loadRealBlueprintSha256()),
      },
      contracts: deployment.contracts,
    }),
  ) as DaDeploymentInfo;
};

/**
 * The DA deployment input derived from this run's blueprint, produced once
 * per run. Each call returns its own copy.
 */
export const deriveDaDeploymentInfo: () => Promise<DaDeploymentInfo> =
  memoizeForRun("da-deployment-info", produceDaDeploymentInfo);
