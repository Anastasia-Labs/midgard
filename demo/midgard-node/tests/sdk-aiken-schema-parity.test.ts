import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeValidityCode,
  encodeValidityCode,
  type MidgardTxValidity as NativeMidgardTxValidity,
  MidgardTxValidityCodes,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

type DataSchema = {
  readonly $ref?: string;
  readonly anyOf?: readonly DataSchema[];
  readonly dataType?: string;
  readonly fields?: readonly DataSchema[];
  readonly index?: number;
  readonly items?: DataSchema;
  readonly keys?: DataSchema;
  readonly title?: string;
  readonly values?: DataSchema;
};

type Blueprint = {
  readonly definitions: Record<string, DataSchema>;
};

type NormalizedSchema =
  | { readonly type: string }
  | { readonly list: NormalizedSchema }
  | { readonly map: readonly [NormalizedSchema, NormalizedSchema] }
  | {
      readonly constructors: readonly {
        readonly index: number;
        readonly name?: string;
        readonly fields: readonly {
          readonly name?: string;
          readonly schema: NormalizedSchema;
        }[];
      }[];
    };

const testDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(testDir, "../../..");
const blueprint = JSON.parse(
  readFileSync(path.join(repoRoot, "onchain/aiken/plutus.json"), "utf8"),
) as Blueprint;

const jsonPointerName = (reference: string): string =>
  reference
    .replace("#/definitions/", "")
    .replaceAll("~1", "/")
    .replaceAll("~0", "~");

const snakeCase = (value: string): string =>
  value
    .replace(/([a-z0-9])([A-Z])/gu, "$1_$2")
    .replace(/([A-Z])([A-Z][a-z])/gu, "$1_$2")
    .toLowerCase();

const normalizeSchema = (
  schema: DataSchema,
  definitions?: Blueprint["definitions"],
  resolving: ReadonlySet<string> = new Set(),
): NormalizedSchema => {
  if (schema.$ref !== undefined) {
    expect(definitions, "a schema reference requires definitions").toBeDefined();
    const name = jsonPointerName(schema.$ref);
    expect(resolving.has(name), `recursive ABI schema ${name}`).toBe(false);
    const definition = definitions![name];
    expect(definition, `missing blueprint definition ${name}`).toBeDefined();
    return normalizeSchema(
      definition!,
      definitions,
      new Set([...resolving, name]),
    );
  }

  if (schema.anyOf !== undefined) {
    const includeNames = schema.anyOf.length > 1;
    return {
      constructors: schema.anyOf.map((constructor) => ({
        index: constructor.index!,
        ...(includeNames
          ? { name: constructor.title }
          : {}),
        fields: (constructor.fields ?? []).map((field) => ({
          ...(field.title === undefined
            ? {}
            : { name: snakeCase(field.title) }),
          schema: normalizeSchema(field, definitions, resolving),
        })),
      })),
    };
  }

  if (schema.dataType === "list" || schema.items !== undefined) {
    expect(schema.items, "list schema must name its item schema").toBeDefined();
    return { list: normalizeSchema(schema.items!, definitions, resolving) };
  }

  if (
    schema.dataType === "map" ||
    schema.keys !== undefined ||
    schema.values !== undefined
  ) {
    expect(schema.keys, "map schema must name its key schema").toBeDefined();
    expect(schema.values, "map schema must name its value schema").toBeDefined();
    return {
      map: [
        normalizeSchema(schema.keys!, definitions, resolving),
        normalizeSchema(schema.values!, definitions, resolving),
      ],
    };
  }

  expect(schema.dataType, "unclassified ABI schema").toBeDefined();
  return { type: schema.dataType! };
};

const ABI_MAPPINGS = [
  ["ProofStepSchema", "aiken/merkle_patricia_forestry/ProofStep"],
  ["NeighborSchema", "aiken/merkle_patricia_forestry/Neighbor"],
  ["HeaderV1Schema", "midgard/ledger_state/HeaderV1"],
  ["TransitionStepSchema", "midgard/ledger_state/TransitionStep"],
  ["EventKeySchema", "midgard/ledger_state/EventKey"],
  ["EventToStepValueSchema", "midgard/ledger_state/EventToStepValue"],
  ["ForcedInclusionTxV1Schema", "midgard/ledger_state/ForcedInclusionTxV1"],
  ["L2TransactionSourceV1Schema", "midgard/ledger_state/L2TransactionSourceV1"],
  ["NativeTxProofSourceV1Schema", "midgard/ledger_state/NativeTxProofSourceV1"],
  ["TxFieldPreimageV1Schema", "midgard/ledger_state/TxFieldPreimageV1"],
  ["TxFieldReceiptV1Schema", "midgard/ledger_state/TxFieldReceiptV1"],
  [
    "CekProgramMaterialDatumV1Schema",
    "midgard/ledger_state/CekProgramMaterialDatumV1",
  ],
  [
    "ValidationMachineStateV1Schema",
    "midgard/validation_trace_v1/ValidationMachineStateV1",
  ],
  [
    "ValidationTraceDescriptorV1Schema",
    "midgard/validation_trace_v1/ValidationTraceDescriptorV1",
  ],
  [
    "ValidationTraceProofV1Schema",
    "midgard/validation_trace_v1/ValidationTraceProof",
  ],
  [
    "ValidationDisputeV1Schema",
    "midgard/validation_dispute_v1/ValidationDisputeV1",
  ],
  [
    "ValidationSourceMembershipV1Schema",
    "midgard/validation_claim_v1/ValidationSourceMembershipV1",
  ],
  [
    "ValidationClaimWitnessV1Schema",
    "midgard/validation_claim_v1/ValidationClaimWitnessV1",
  ],
  ["MidgardTxValiditySchema", "midgard/ledger_state/MidgardTxValidity"],
] as const;

const VALIDITY_VECTORS = [
  ["TxIsValid", 0n, "d87980"],
  ["NonExistentInputUtxo", 1n, "d87a80"],
  ["InvalidSignature", 2n, "d87b80"],
  ["FailedScript", 3n, "d87c80"],
  ["FeeTooLow", 4n, "d87d80"],
  ["UnbalancedTx", 5n, "d87e80"],
] as const satisfies readonly [
  NativeMidgardTxValidity,
  bigint,
  string,
][];

describe("SDK/Aiken canonical V1 schema parity", () => {
  it.each(ABI_MAPPINGS)(
    "matches %s to %s recursively",
    (typescriptSchemaName, aikenDefinitionName) => {
      const typescriptSchema = SDK[typescriptSchemaName] as
        | DataSchema
        | undefined;
      const aikenSchema = blueprint.definitions[aikenDefinitionName];
      expect(
        typescriptSchema,
        `missing SDK schema ${typescriptSchemaName}`,
      ).toBeDefined();
      expect(
        aikenSchema,
        `missing Aiken blueprint definition ${aikenDefinitionName}`,
      ).toBeDefined();

      expect(normalizeSchema(typescriptSchema!)).toEqual(
        normalizeSchema(
          aikenSchema!,
          blueprint.definitions,
          new Set([aikenDefinitionName]),
        ),
      );
    },
  );

  it.each(VALIDITY_VECTORS)(
    "binds raw native validity %s code %s to its Plutus constructor",
    (name, code, plutusDataCbor) => {
      expect(MidgardTxValidityCodes[name]).toBe(code);
      expect(encodeValidityCode(name)).toBe(code);
      expect(decodeValidityCode(code, "validity")).toBe(name);
      expect(
        Data.to(name as never, SDK.MidgardTxValiditySchema as never),
      ).toBe(plutusDataCbor);
      expect(
        Data.from(plutusDataCbor, SDK.MidgardTxValiditySchema),
      ).toBe(name);
    },
  );

  it("rejects unknown raw and Plutus validity constructors", () => {
    expect(() => decodeValidityCode(6n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() =>
      Data.from("d87f80", SDK.MidgardTxValiditySchema),
    ).toThrow();
  });
});
