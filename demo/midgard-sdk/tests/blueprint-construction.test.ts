import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  applyBlueprintParams,
  blueprintApplicationRecords,
  buildBlueprintDeploymentRegistry,
  getUnappliedScript,
  parseFaultProofBlueprint,
  verifyBlueprintDeploymentRegistry,
} from "../src/index.js";

const entry = {
  title: "example.main.spend",
  compiledCode: "49480100002221200101",
};

describe("blueprint construction boundary", () => {
  it("refuses duplicate titles through both deployment doors", () => {
    const blueprint = parseFaultProofBlueprint({ validators: [entry, entry] });
    expect(() => getUnappliedScript(blueprint, entry.title)).toThrow(
      /exactly one/,
    );
    expect(() => applyBlueprintParams(blueprint, entry.title, [])).toThrow(
      /exactly one/,
    );
  });
});

it("preserves declared type metadata when a normalized blueprint crosses an adapter", () => {
  const raw = {
    validators: [
      {
        ...entry,
        parameters: [
          {
            title: "policy",
            schema: { $ref: "#/definitions/cardano~1assets~1PolicyId" },
          },
        ],
      },
    ],
  };
  const parsed = parseFaultProofBlueprint(raw);
  expect(parseFaultProofBlueprint(parsed)).toEqual(parsed);
  expect(() =>
    applyBlueprintParams(parseFaultProofBlueprint(parsed), entry.title, ["ab"]),
  ).toThrow(/28-byte hash/);
});

describe("complete deployment parameter registry", () => {
  const deployment = (script: string) => ({
    contract: "exampleSpend",
    purpose: "spend" as const,
    script: { type: "PlutusV3" as const, script },
    scriptHash: validatorToScriptHash({ type: "PlutusV3", script }),
    referenceScriptRole: null,
  });
  it("records actual applications including a cache hit and the zero-parameter door", () => {
    const blueprint = parseFaultProofBlueprint({ validators: [entry] });
    const first = getUnappliedScript(blueprint, entry.title);
    expect(getUnappliedScript(blueprint, entry.title)).toBe(first);
    expect(blueprintApplicationRecords(blueprint)).toHaveLength(1);
    const registry = buildBlueprintDeploymentRegistry(
      blueprint,
      [deployment(first)],
      {},
    );
    expect(registry.contracts[0]?.application?.parameters).toEqual([]);
    expect(registry.validators).toEqual([
      {
        title: entry.title,
        disposition: "deployed",
        contracts: ["exampleSpend"],
      },
    ]);
  });
  it("refuses an injected raw validator until its nondeployed purpose is explicit", () => {
    const extra = { ...entry, title: "unclassified.main.spend" };
    const blueprint = parseFaultProofBlueprint({ validators: [entry, extra] });
    const script = getUnappliedScript(blueprint, entry.title);
    expect(() =>
      buildBlueprintDeploymentRegistry(blueprint, [deployment(script)], {}),
    ).toThrow("Unclassified raw blueprint validator: unclassified.main.spend");
    const registry = buildBlueprintDeploymentRegistry(
      blueprint,
      [deployment(script)],
      { [extra.title]: "Explicit excluded test control" },
    );
    expect(registry.validators).toHaveLength(2);
    expect(registry.validators[1]).toMatchObject({
      disposition: "not_deployed",
      reason: "Explicit excluded test control",
    });
  });
  it("refuses unrecorded deployed bytes and ambiguous application identities", () => {
    const blueprint = parseFaultProofBlueprint({
      validators: [entry, { ...entry, title: "other.main.spend" }],
    });
    const script = getUnappliedScript(blueprint, entry.title);
    expect(() =>
      buildBlueprintDeploymentRegistry(
        blueprint,
        [
          {
            ...deployment(script),
            script: { type: "PlutusV3", script: "49480100002221200102" },
            scriptHash: validatorToScriptHash({
              type: "PlutusV3",
              script: "49480100002221200102",
            }),
          },
        ],
        {},
      ),
    ).toThrow("one complete blueprint parameter record");
    getUnappliedScript(blueprint, "other.main.spend");
    expect(() =>
      buildBlueprintDeploymentRegistry(blueprint, [deployment(script)], {}),
    ).toThrow("one complete blueprint parameter record");
  });
  it("does not classify a deployed validator as unused or accept arbitrary native contracts", () => {
    const blueprint = parseFaultProofBlueprint({ validators: [entry] });
    const script = getUnappliedScript(blueprint, entry.title);
    expect(() =>
      buildBlueprintDeploymentRegistry(blueprint, [deployment(script)], {
        [entry.title]: "unused",
      }),
    ).toThrow("Deployed validator is classified nondeployed");
    expect(() =>
      buildBlueprintDeploymentRegistry(
        blueprint,
        [
          {
            ...deployment(script),
            script: { type: "Native", script: "820501" },
            scriptHash: validatorToScriptHash({
              type: "Native",
              script: "820501",
            }),
          },
        ],
        {},
      ),
    ).toThrow("Unclassified native deployment contract");
  });
});

it("retains nonzero parameter CBOR on the real application/cache boundary and independently reapplies it", () => {
  const blueprint = parseFaultProofBlueprint({
    validators: [
      {
        ...entry,
        parameters: [
          { title: "minimum", schema: { $ref: "#/definitions/Int" } },
        ],
      },
    ],
  });
  const script = applyBlueprintParams(blueprint, entry.title, [4310n]);
  expect(applyBlueprintParams(blueprint, entry.title, [4310n])).toBe(script);
  const records = blueprintApplicationRecords(blueprint);
  expect(records).toHaveLength(1);
  expect(records[0]?.parameters).toEqual([
    { title: "minimum", schemaRef: "#/definitions/Int", cborHex: "1910d6" },
  ]);
  const registry = buildBlueprintDeploymentRegistry(
    blueprint,
    [
      {
        contract: "exampleSpend",
        purpose: "spend",
        script: { type: "PlutusV3", script },
        scriptHash: validatorToScriptHash({ type: "PlutusV3", script }),
        referenceScriptRole: null,
      },
    ],
    {},
  );
  expect(
    verifyBlueprintDeploymentRegistry(
      parseFaultProofBlueprint(blueprint),
      registry,
    ),
  ).toEqual(registry);
  const wrongParameter = JSON.parse(JSON.stringify(registry)) as {
    contracts: { application: { parameters: { cborHex: string }[] } }[];
  };
  wrongParameter.contracts[0]!.application!.parameters[0]!.cborHex = "01";
  expect(() =>
    verifyBlueprintDeploymentRegistry(
      parseFaultProofBlueprint(blueprint),
      wrongParameter,
    ),
  ).toThrow("Retained parameters do not produce the deployed script");
});
