import { existsSync, readFileSync } from "node:fs";
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
        readonly fields: readonly {
          readonly schema: NormalizedSchema;
        }[];
      }[];
    };

const testDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(testDir, "../../..");
/**
 * KNOWN RED against the blueprint currently in the tree. #584 retired
 * `transaction_commitment` from the committed source leaves without regenerating
 * `plutus.json`, so four rows below compare a two- or three-field SDK schema
 * against a stale three- or four-field blueprint definition and fail on the field
 * count: `ForcedInclusionTxV1Schema`, `L2TransactionSourceV1Schema`,
 * `ValidationSourceMembershipV1Schema` and `ValidationClaimWitnessV1Schema`
 * (`4 failed | 28 passed`). Regenerating the blueprint is
 * [#579](https://github.com/Anastasia-Labs/midgard/issues/579)'s; these four are
 * rows 7-10 of the ten-test handoff set enumerated in
 * `demo/midgard-fault-proofs/tests/support/submit-init-emulator-shared.ts`, which
 * also owns the six emulator scenarios red for the same cause. Point
 * `MIDGARD_REAL_BLUEPRINT_PATH` at a regenerated blueprint to check the fix.
 */
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  path.join(repoRoot, "onchain/aiken/plutus.json");
const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as Blueprint;

const jsonPointerName = (reference: string): string =>
  reference
    .replace("#/definitions/", "")
    .replaceAll("~1", "/")
    .replaceAll("~0", "~");

const normalizeSchema = (
  schema: DataSchema,
  definitions?: Blueprint["definitions"],
  resolving: ReadonlySet<string> = new Set(),
): NormalizedSchema => {
  if (schema.$ref !== undefined) {
    expect(
      definitions,
      "a schema reference requires definitions",
    ).toBeDefined();
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
    return {
      constructors: schema.anyOf.map((constructor) => ({
        index: constructor.index!,
        fields: (constructor.fields ?? []).map((field) => ({
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
    expect(
      schema.values,
      "map schema must name its value schema",
    ).toBeDefined();
    return {
      map: [
        normalizeSchema(schema.keys!, definitions, resolving),
        normalizeSchema(schema.values!, definitions, resolving),
      ],
    };
  }

  if (schema.dataType === undefined) {
    return { type: "data" };
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
  ["TxOrderPayloadV1Schema", "midgard/ledger_state/TxOrderPayloadV1"],
  ["TxOrderEventV1Schema", "midgard/ledger_state/TxOrderEventV1"],
  ["TxFieldPreimageV1Schema", "midgard/ledger_state/TxFieldPreimageV1"],
  ["TxFieldReceiptV1Schema", "midgard/ledger_state/TxFieldReceiptV1"],
  [
    "CekProgramMaterialDatumV1Schema",
    "midgard/ledger_state/CekProgramMaterialDatumV1",
  ],
  ["TxOrderDatumV1Schema", "midgard/user_events/tx_order_v1/Datum"],
  [
    "TxOrderSpendRedeemerV1Schema",
    "midgard/user_events/tx_order_v1/SpendRedeemer",
  ],
  [
    "TxFieldReceiptMintRedeemerV1Schema",
    "midgard/user_events/tx_field_receipt_v1/MintRedeemer",
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
] as const satisfies readonly [NativeMidgardTxValidity, bigint, string][];

describe("SDK/Aiken canonical V1 schema parity", () => {
  it("has no retired transaction-order validator, export, or parser", () => {
    const retiredName =
      /(?:TxOrder(?:Datum|Event|Payload)|ForcedInclusionTx)V(?:2|3)/u;
    expect(Object.keys(SDK).filter((name) => retiredName.test(name))).toEqual(
      [],
    );

    const productionSources = [
      "demo/midgard-sdk/src/user-events/tx-order.ts",
      "demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.ts",
      "onchain/aiken/lib/midgard/user-events/tx-order-v1.ak",
      "onchain/aiken/validators/user-events/tx-order-v1.ak",
    ];
    for (const relativePath of productionSources) {
      expect(
        readFileSync(path.join(repoRoot, relativePath), "utf8"),
        relativePath,
      ).not.toMatch(retiredName);
    }

    for (const relativePath of [
      "onchain/aiken/lib/midgard/user-events/tx-order-v2.ak",
      "onchain/aiken/lib/midgard/user-events/tx-order-v3.ak",
      "onchain/aiken/validators/user-events/tx-order-v2.ak",
      "onchain/aiken/validators/user-events/tx-order-v3.ak",
    ]) {
      expect(existsSync(path.join(repoRoot, relativePath)), relativePath).toBe(
        false,
      );
    }
  });

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
      expect(Data.to(name as never, SDK.MidgardTxValiditySchema as never)).toBe(
        plutusDataCbor,
      );
      expect(Data.from(plutusDataCbor, SDK.MidgardTxValiditySchema)).toBe(name);
    },
  );

  it("rejects unknown raw and Plutus validity constructors", () => {
    expect(() => decodeValidityCode(6n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() => Data.from("d87f80", SDK.MidgardTxValiditySchema)).toThrow();
  });
});
