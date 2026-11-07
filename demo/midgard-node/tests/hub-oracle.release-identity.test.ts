import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  scriptHashToCredential,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { collectScriptDescriptors } from "../src/commands/contract-deployment-info.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { buildRealHubOracleValidator } from "../src/services/midgard-contracts.build-real-hub-oracle-validator.js";
import { loadRealBlueprint } from "../src/services/midgard-contracts.load-reference-script-auth-validator.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";

const construct = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const base = yield* AlwaysSucceedsContract;
      const hubOracle = yield* buildRealHubOracleValidator("Preprod", {
        txHash: "00".repeat(32),
        outputIndex: 0,
      });
      const blueprint = yield* loadRealBlueprint();
      return { base, hubOracle, blueprint };
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

describe("real hub-oracle release identity", () => {
  it("describes the actual immutable mint program at its existing script credential", async () => {
    const { base, hubOracle } = await construct();
    expect(hubOracle.spendingScript).toEqual(hubOracle.mintingScript);
    expect(hubOracle.spendingScriptCBOR).toBe(hubOracle.mintingScriptCBOR);
    expect(validatorToScriptHash(hubOracle.spendingScript)).toBe(
      hubOracle.policyId,
    );
    expect(hubOracle.spendingScriptHash).toBe(hubOracle.policyId);
    expect(hubOracle.spendingScriptAddress).toBe(
      credentialToAddress(
        "Preprod",
        scriptHashToCredential(hubOracle.policyId),
      ),
    );
    expect(hubOracle.spendingScriptCBOR).not.toBe(
      base.hubOracle.spendingScriptCBOR,
    );
    const descriptor = collectScriptDescriptors(
      withRealEventHistoryForTest(
        { ...base, hubOracle },
        { txHash: "00".repeat(32), outputIndex: 0 },
      ),
    ).find((entry) => entry.name === "hubOracleMint");
    expect(descriptor?.script).toEqual(hubOracle.mintingScript);
    expect(descriptor?.scriptHash).toBe(hubOracle.policyId);
  });

  it("permits only the known hub credential's mint-program correspondence and refuses scaffold metadata", async () => {
    const { base, hubOracle, blueprint } = await construct();
    if (
      hubOracle.spendingScript.type !== "PlutusV3" ||
      base.hubOracle.spendingScript.type !== "PlutusV3"
    )
      throw new Error("Hub oracle probe requires actual Plutus V3 scripts");
    const actualScript = {
      type: "PlutusV3" as const,
      script: hubOracle.spendingScript.script,
    };
    const scaffoldScript = {
      type: "PlutusV3" as const,
      script: base.hubOracle.spendingScript.script,
    };
    const spend = {
      contract: "hubOracleSpend",
      purpose: "spend" as const,
      script: actualScript,
      scriptHash: hubOracle.spendingScriptHash,
      referenceScriptRole: null,
    };
    const nonDeployed = Object.fromEntries(
      blueprint.validators
        .filter((entry) => entry.title !== "hub_oracle.mint.mint")
        .map((entry) => [
          entry.title,
          "Outside this isolated constructor probe",
        ]),
    );
    const registry = SDK.buildBlueprintDeploymentRegistry(
      blueprint,
      [spend],
      nonDeployed,
    );
    expect(registry.contracts[0]?.application?.validatorTitle).toBe(
      "hub_oracle.mint.mint",
    );
    expect(registry.contracts[0]?.application?.parameters).toHaveLength(2);
    expect(() =>
      SDK.buildBlueprintDeploymentRegistry(
        blueprint,
        [{ ...spend, contract: "arbitrarySpend" }],
        nonDeployed,
      ),
    ).toThrow(/one complete blueprint parameter record/);
    expect(() =>
      SDK.buildBlueprintDeploymentRegistry(
        blueprint,
        [{ ...spend, script: scaffoldScript }],
        nonDeployed,
      ),
    ).toThrow(/hash differs from actual bytes/);
    expect(() =>
      SDK.buildBlueprintDeploymentRegistry(
        blueprint,
        [
          {
            ...spend,
            script: scaffoldScript,
            scriptHash: base.hubOracle.spendingScriptHash,
          },
        ],
        nonDeployed,
      ),
    ).toThrow(/one complete blueprint parameter record/);
  });
});
