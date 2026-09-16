import { readFile, writeFile } from "node:fs/promises";

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
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { buildContractDeploymentInfoFromContracts } from "../src/commands/contract-deployment-info.js";
import { loadRealBlueprintSha256 } from "../src/services/midgard-contracts.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const fixtureUrl = new URL(
  "../../da-committee-node/tests/fixtures/da-contract-deployment-info.json",
  import.meta.url,
);

it("regenerates the DA codec fixture from every current applied contract", async () => {
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
  const fixture = {
    schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    fixtureKind: "codec fixture with synthetic reference outpoints",
    manifestId: "ab".repeat(32),
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    artifacts: {
      blueprintHash: await Effect.runPromise(loadRealBlueprintSha256()),
    },
    contracts: deployment.contracts,
  };
  expect(Object.keys(fixture.contracts).sort()).toEqual(
    [...DEPLOYMENT_MANIFEST_CONTRACT_NAMES].sort(),
  );
  if (process.env.MIDGARD_WRITE_DA_DEPLOYMENT_FIXTURE === "1") {
    await writeFile(fixtureUrl, `${JSON.stringify(fixture, null, 2)}\n`);
  }
  expect(JSON.parse(await readFile(fixtureUrl, "utf8"))).toEqual(fixture);
}, 120_000);
