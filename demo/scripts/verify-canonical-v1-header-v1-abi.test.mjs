import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { test } from "node:test";
import assert from "node:assert/strict";

import {
  assertHeaderV1AbiContract,
  extractHeaderV1Blueprint,
  extractHeaderV1SdkSchema,
  verifyHeaderV1Abi,
} from "./verify-canonical-v1-header-v1-abi.mjs";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const realBlueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
const realSdkModulePath = resolve(repoRoot, "demo/midgard-sdk/dist/index.js");

const registry = JSON.parse(
  readFileSync(
    new URL(
      "../../docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
      import.meta.url,
    ),
    "utf8",
  ),
);
const contract = registry.formats.find((row) => row.id === "L01")
  .canonicalForms[0];

const blueprintRef = (name) => `#/definitions/${name.replaceAll("/", "~1")}`;

const makeBlueprint = (source = contract) => {
  const definitions = {
    [source.blueprintDefinition]: {
      title: source.name,
      anyOf: [
        {
          title: source.name,
          dataType: "constructor",
          index: source.constructorTags[0],
          fields: source.fields.map((field) => ({
            title: field.name,
            $ref: blueprintRef(field.aikenRef),
          })),
        },
      ],
    },
  };
  for (const field of source.fields) {
    definitions[field.aikenRef] = {
      dataType: field.type.startsWith("bytes") ? "bytes" : "integer",
    };
  }
  return { definitions };
};

const makeSdkSchema = (source = contract) => ({
  anyOf: [
    {
      title: source.name,
      dataType: "constructor",
      index: source.constructorTags[0],
      fields: source.fields.map((field) => {
        if (field.type.startsWith("bytes")) {
          const length = Number(field.type.slice("bytes".length));
          return {
            title: field.sdkName,
            dataType: "bytes",
            minLength: length,
            maxLength: length,
          };
        }
        return { title: field.sdkName, dataType: "integer" };
      }),
    },
  ],
});

// #519 V-8: every other case in this file builds both "projections" out of the
// contract under test, so the suite could only ever prove that the contract
// agrees with itself. This case consumes the two projections it does NOT own —
// the blueprint the Aiken compiler generated and the Data schema the built SDK
// exports at runtime — and is the case the registry's L01 cross-language row
// cites. It fails closed when either artifact is absent: a missing build must
// redden the suite, not silently delete its only real comparison.
test("agrees with the real Aiken blueprint and the real SDK runtime schema", async () => {
  assert.ok(
    existsSync(realBlueprintPath),
    `the generated Aiken blueprint is required: ${realBlueprintPath}; run \`aiken build\` in onchain/aiken`,
  );
  assert.ok(
    existsSync(realSdkModulePath),
    `the built SDK runtime module is required: ${realSdkModulePath}; run \`pnpm --dir demo/midgard-sdk run build\``,
  );
  const realBlueprint = JSON.parse(readFileSync(realBlueprintPath, "utf8"));
  const realSdk = await import(pathToFileURL(realSdkModulePath).href);
  assert.ok(
    realSdk.HeaderSchema !== undefined,
    "the built SDK must export HeaderSchema",
  );

  assert.doesNotThrow(() =>
    verifyHeaderV1Abi({
      contract,
      blueprint: realBlueprint,
      sdkSchema: realSdk.HeaderSchema,
    }),
  );

  // Assert the comparison was not vacuous: both extractions must have produced
  // the full 25-field constructor 0, so a projection that degraded to an empty
  // field list could not pass the loop above by having nothing to compare.
  const blueprintProjection = extractHeaderV1Blueprint(realBlueprint);
  const sdkProjection = extractHeaderV1SdkSchema(realSdk.HeaderSchema);
  assert.equal(blueprintProjection.constructorTag, 0);
  assert.equal(blueprintProjection.fields.length, contract.fields.length);
  assert.equal(sdkProjection.constructorTag, 0);
  assert.equal(sdkProjection.fields.length, contract.fields.length);
});

test("accepts the canonical contract and matching SDK/Aiken projections", () => {
  assert.doesNotThrow(() =>
    verifyHeaderV1Abi({
      contract,
      blueprint: makeBlueprint(),
      sdkSchema: makeSdkSchema(),
    }),
  );
});

test("rejects reordered, missing, extra, type, group, tag, and arity mutations", () => {
  const mutations = [
    [
      "reordered fields",
      () => {
        const blueprint = makeBlueprint();
        const fields =
          blueprint.definitions[contract.blueprintDefinition].anyOf[0].fields;
        [fields[0], fields[1]] = [fields[1], fields[0]];
        return { contract, blueprint, sdkSchema: makeSdkSchema() };
      },
    ],
    [
      "missing field",
      () => {
        const sdkSchema = makeSdkSchema();
        sdkSchema.anyOf[0].fields.pop();
        return { contract, blueprint: makeBlueprint(), sdkSchema };
      },
    ],
    [
      "extra field",
      () => {
        const blueprint = makeBlueprint();
        blueprint.definitions[
          contract.blueprintDefinition
        ].anyOf[0].fields.push({
          title: "hostile_extra",
          $ref: blueprintRef("Int"),
        });
        blueprint.definitions.Int = { dataType: "integer" };
        return { contract, blueprint, sdkSchema: makeSdkSchema() };
      },
    ],
    [
      "type mutation",
      () => {
        const sdkSchema = makeSdkSchema();
        sdkSchema.anyOf[0].fields[0].dataType = "integer";
        delete sdkSchema.anyOf[0].fields[0].minLength;
        delete sdkSchema.anyOf[0].fields[0].maxLength;
        return { contract, blueprint: makeBlueprint(), sdkSchema };
      },
    ],
    [
      "Aiken type mutation",
      () => {
        const blueprint = makeBlueprint();
        blueprint.definitions[contract.fields[0].aikenRef].dataType = "integer";
        return { contract, blueprint, sdkSchema: makeSdkSchema() };
      },
    ],
    [
      "group mutation",
      () => {
        const mutatedContract = structuredClone(contract);
        mutatedContract.fields[0].group = "counts";
        return {
          contract: mutatedContract,
          blueprint: makeBlueprint(),
          sdkSchema: makeSdkSchema(),
        };
      },
    ],
    [
      "group descriptor mutation",
      () => {
        const mutatedContract = structuredClone(contract);
        mutatedContract.groups[0].start = 1;
        return {
          contract: mutatedContract,
          blueprint: makeBlueprint(),
          sdkSchema: makeSdkSchema(),
        };
      },
    ],
    [
      "exact field order mutation",
      () => {
        const mutatedContract = structuredClone(contract);
        [mutatedContract.exactFields[0], mutatedContract.exactFields[1]] = [
          mutatedContract.exactFields[1],
          mutatedContract.exactFields[0],
        ];
        return {
          contract: mutatedContract,
          blueprint: makeBlueprint(),
          sdkSchema: makeSdkSchema(),
        };
      },
    ],
    [
      "constructor arity mutation",
      () => {
        const mutatedContract = structuredClone(contract);
        mutatedContract.constructor.arity = 24;
        return {
          contract: mutatedContract,
          blueprint: makeBlueprint(),
          sdkSchema: makeSdkSchema(),
        };
      },
    ],
    [
      "tag mutation",
      () => {
        const blueprint = makeBlueprint();
        blueprint.definitions[contract.blueprintDefinition].anyOf[0].index = 1;
        return { contract, blueprint, sdkSchema: makeSdkSchema() };
      },
    ],
  ];

  for (const [label, makeMutation] of mutations) {
    assert.throws(
      () => verifyHeaderV1Abi(makeMutation()),
      undefined,
      `${label} must be rejected`,
    );
  }
  assert.doesNotThrow(() => assertHeaderV1AbiContract(contract));
});
