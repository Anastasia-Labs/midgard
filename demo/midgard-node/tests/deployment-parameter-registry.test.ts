import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { makeGenuineDeploymentRegistryForTest } from "./helpers/deployment-parameter-registry.js";

it("reapplies the complete genuine deployment roster and refuses a newly unclassified raw validator", async () => {
  const { blueprint, registry } = await makeGenuineDeploymentRegistryForTest();
  expect(registry.contracts.map((entry) => entry.contract).sort()).toEqual(
    [...DEPLOYMENT_MANIFEST_CONTRACT_NAMES].sort(),
  );
  expect(registry.contracts.length).toBeGreaterThan(500);
  expect(registry.validators.length).toBe(blueprint.validators.length);
  expect(
    registry.contracts.some((entry) =>
      entry.contract.startsWith("escapeHatch"),
    ),
  ).toBe(false);
  expect(
    SDK.verifyBlueprintDeploymentRegistry(
      SDK.parseFaultProofBlueprint(blueprint),
      registry,
    ),
  ).toEqual(registry);
  const unknown = SDK.parseFaultProofBlueprint({
    ...blueprint,
    validators: [
      ...blueprint.validators,
      {
        ...blueprint.validators[0]!,
        title: "unclassified.release_control.spend",
      },
    ],
  });
  expect(() =>
    SDK.verifyBlueprintDeploymentRegistry(unknown, registry),
  ).toThrow(
    "Unclassified raw blueprint validator: unclassified.release_control.spend",
  );
}, 120_000);
