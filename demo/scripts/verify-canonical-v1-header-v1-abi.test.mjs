import { readFileSync } from "node:fs";
import { test } from "node:test";
import assert from "node:assert/strict";

import {
  assertHeaderV1AbiContract,
  verifyHeaderV1Abi,
} from "./verify-canonical-v1-header-v1-abi.mjs";

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
        const fields = blueprint.definitions[contract.blueprintDefinition]
          .anyOf[0].fields;
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
        blueprint.definitions[contract.blueprintDefinition].anyOf[0].fields.push(
          {
            title: "hostile_extra",
            $ref: blueprintRef("Int"),
          },
        );
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
        blueprint.definitions[contract.fields[0].aikenRef].dataType =
          "integer";
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
