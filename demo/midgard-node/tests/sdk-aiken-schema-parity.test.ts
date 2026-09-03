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
 * KNOWN RED against the blueprint currently in the tree, and the set grew in
 * #587 and again in #594.
 *
 * #584 retired `transaction_commitment` from the committed source leaves without
 * regenerating `plutus.json`, so four rows compare a two- or three-field SDK
 * schema against a stale three- or four-field blueprint definition and fail on
 * the field count: `ForcedInclusionTxV1Schema`, `L2TransactionSourceSchema`,
 * `ValidationSourceMembershipSchema` and `ValidationClaimWitnessSchema`.
 *
 * #587 then retired the counted publication receipt chain, which took
 * `terminal_receipt_reference` out of `TxOrderPayloadV1`. That is the same kind of
 * staleness and it moves three more rows red — `TxOrderPayloadSchema` on its own
 * field count, and `TxOrderEventSchema` and `TxOrderDatumSchema` because both
 * embed the payload — for a total of **seven** rows against this blueprint. The
 * two mappings for the retired receipt datums, and the one for the retired receipt
 * mint redeemer, were removed rather than left to fail: a red row measures a stale
 * blueprint, while a mapping to a type that no longer exists on the SDK side would
 * assert a retired surface.
 *
 * #594 then re-expressed `verify_order_material` on the §8.8 field-access door and
 * gave the tx-order minting policy its own redeemer — `user_events.MintRedeemer`
 * wrapped beside the §8 carriage vector. That adds an **eighth** red row,
 * `TxOrderMintRedeemerSchema`, and it is red for a different reason from the
 * other seven: they compare a moved SDK schema against a stale blueprint
 * definition and fail on field count, while this one has **no blueprint definition
 * at all** (`missing Aiken blueprint definition
 * midgard/user_events/tx_order_v1/MintRedeemer`) because the type is new in this
 * round. The mapping is kept rather than deferred: the SDK type exists, the Aiken
 * type exists, and the only thing missing is the regeneration that publishes it —
 * which is exactly what a red row here is for.
 *
 * #596 then added the §12.7 canonical-decodability fault family, whose claim
 * redeemer (`CommittedFieldClaimV1`) and step-02 thread state are likewise new
 * types the frozen blueprint never declared. That adds a **ninth** and **tenth**
 * red row, both of the missing-definition kind rather than the field-count kind.
 *
 * #597 moved the TypeScript twins of #592's machine wire change — four
 * `ValidationAuxiliaryWitnessV1` constructors and `ValidationProofItemDatumV1` —
 * and **adds no row and moves no figure**. That is a measured conclusion, not an
 * oversight: both mappings were added, run, and removed again, because neither
 * type can be compared here even against a regenerated blueprint. The reasoning
 * is at the foot of `ABI_MAPPINGS`. The consequence worth carrying into #579 is
 * that regenerating the blueprint will **not** surface that wire change here, so
 * this file's clean bill after regeneration says nothing about it; the
 * cross-language producer vector
 * `typescript_generated_field_chunk_auxiliary_is_exact` in
 * `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak` is what
 * covers it.
 *
 * #601 then added the §12.8 committed-field-shape sibling fault family, whose
 * step-02 thread state is likewise a new type the frozen blueprint never
 * declared. That adds an **eleventh** red row, again of the missing-definition
 * kind. It adds only one: the family's claim redeemer is §12.7's
 * `CommittedFieldClaimV1` reused unchanged, so row 15 already covers it.
 *
 * Measured on 2026-08-13 after #601: `11 failed | 22 passed (33)`; the same
 * command measured `10 failed | 22 passed (32)` on 2026-08-12 after #596 and
 * unchanged after #597, `8 failed | 22 passed (30)` after #594 and
 * `7 failed | 22 passed (29)` on 2026-08-10 after #587. Regenerating the
 * blueprint is [#579](https://github.com/Anastasia-Labs/midgard/issues/579)'s; all
 * eleven are rows 7-17 of the seventeen-test handoff set enumerated in
 * `demo/midgard-fault-proofs/tests/support/submit-init-emulator-shared.ts` (#584's
 * four as rows 7-10, #587's three as 11-13, #594's one as 14, #596's two as
 * 15-16, #601's one as 17), which also owns the six emulator scenarios red for
 * the same cause.
 * Point `MIDGARD_REAL_BLUEPRINT_PATH` at a regenerated blueprint to check the fix
 * — #596 did, against a scratch stock build of its own working tree, and #601 did
 * the same; this file gives `33 passed (33)` there.
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
  ["HeaderSchema", "midgard/ledger_state/HeaderV1"],
  ["TransitionStepSchema", "midgard/ledger_state/TransitionStepV1"],
  ["EventKeySchema", "midgard/ledger_state/EventKey"],
  ["EventToStepValueSchema", "midgard/ledger_state/EventToStepValue"],
  ["ForcedInclusionTxV1Schema", "midgard/ledger_state/ForcedInclusionTxV1"],
  ["L2TransactionSourceSchema", "midgard/ledger_state/L2TransactionSourceV1"],
  ["NativeTxProofSourceSchema", "midgard/ledger_state/NativeTxProofSourceV1"],
  ["TxOrderPayloadSchema", "midgard/ledger_state/TxOrderPayloadV1"],
  ["TxOrderEventSchema", "midgard/ledger_state/TxOrderEventV1"],
  // `TxFieldPreimageV1Schema` and `TxFieldReceiptV1Schema` were mapped here until
  // #587 retired both twins with the counted publication receipt chain. Their
  // Aiken definitions are still in the frozen blueprint and go with it when #579
  // regenerates; a mapping kept for them would assert a retired surface rather
  // than measure a stale one.
  [
    "CekProgramMaterialDatumSchema",
    "midgard/ledger_state/CekProgramMaterialDatumV1",
  ],
  ["TxOrderDatumSchema", "midgard/user_events/tx_order_v1/Datum"],
  [
    "TxOrderSpendRedeemerSchema",
    "midgard/user_events/tx_order_v1/SpendRedeemer",
  ],
  ["TxOrderMintRedeemerSchema", "midgard/user_events/tx_order_v1/MintRedeemer"],
  [
    "ValidationMachineStateSchema",
    "midgard/validation_trace_v1/ValidationMachineStateV1",
  ],
  [
    "ValidationTraceDescriptorSchema",
    "midgard/validation_trace_v1/ValidationTraceDescriptorV1",
  ],
  [
    "ValidationTraceProofSchema",
    "midgard/validation_trace_v1/ValidationTraceProof",
  ],
  [
    "ValidationDisputeSchema",
    "midgard/validation_dispute_v1/ValidationDisputeV1",
  ],
  [
    "ValidationSourceMembershipSchema",
    "midgard/validation_claim_v1/ValidationSourceMembershipV1",
  ],
  [
    "ValidationClaimWitnessSchema",
    "midgard/validation_claim_v1/ValidationClaimWitnessV1",
  ],
  ["OperatorVerdictSchema", "midgard/rejection_reason_v1/OperatorVerdictV1"],
  ["RejectionReasonSchema", "midgard/rejection_reason_v1/RejectionReasonV1"],
  // `MidgardTxValiditySchema` has no row: after the #640 format wave no
  // validator ABI mentions `midgard/ledger_state/MidgardTxValidity` — the
  // forced leaf carries `OperatorVerdictV1` and the compact wire carries the
  // validity scalar as a plain Int — so the blueprint publishes no definition
  // to compare against. What pins that schema instead is `VALIDITY_VECTORS`
  // below, which binds each arm to its raw code and its Plutus constructor.
  // #596's §12.7 canonical-decodability family. Both types are **new**, so both
  // rows are red for #594's reason rather than #584/#587's: the SDK type exists,
  // the Aiken type exists, and only the regeneration that publishes them is
  // missing. Verified green against a scratch build of the working tree
  // (`MIDGARD_REAL_BLUEPRINT_PATH`), so what these rows measure at #579 is the
  // publication and not a shape disagreement.
  [
    "CommittedFieldClaimSchema",
    "midgard/fraud_proofs/canonical_decodability/rule/CommittedFieldClaimV1",
  ],
  [
    "CanonicalDecodabilityStep02StateSchema",
    "midgard/fraud_proofs/canonical_decodability/step_02/State",
  ],
  // #601's §12.8 committed-field-shape family adds **one** row, not two. Its
  // claim redeemer is §12.7's `CommittedFieldClaimV1` reused unchanged — one
  // accusation, one wire spelling (§6.1) — so the row above already covers it,
  // and only the step-02 thread state is a new type. That state is structurally
  // identical to §12.7's and is deliberately a separate Aiken type: the two
  // verdict code spaces differ (0..10 there, 0..3 here), so one type would let a
  // §12.7 code satisfy this family's bounds check. Red for #594's reason —
  // missing definition, not field count — and verified green against a scratch
  // build of the working tree.
  [
    "CommittedFieldShapeStep02StateSchema",
    "midgard/fraud_proofs/committed_field_shape/step_02/State",
  ],
  // **#597 adds no row, and that is a measured conclusion rather than an
  // omission.** #592 moved two wire surfaces this file would naturally cover —
  // four `ValidationAuxiliaryWitnessV1` constructors onto §8's `FieldCarriageV1`
  // (1 `TransactionFieldChunkWitness`, 2 `RequiredSignerItemWitness`, 29
  // `TransactionRedeemerItemBeginWitness`, 30 `TransactionFieldItemWitness`) and
  // the whole of `ValidationProofItemDatumV1` — and #597 moved both TypeScript
  // twins to match. Neither can be measured here, for two different reasons, and
  // both were checked by adding the mapping and running it rather than reasoned
  // about:
  //
  //   * `midgard/validation_machine_v1/ValidationAuxiliaryWitnessV1` **is** in
  //     the blueprint, but `normalizeSchema` fully inlines `$ref`s and refuses a
  //     definition that reappears while resolving. This sum reaches
  //     `midgard/cek_builtin_v1/BlsExpressionWitnessV1` — genuinely recursive in
  //     Aiken — through its `CekCoreStepWitness` arm, so the row fails on
  //     `recursive ABI schema …` before it compares a single field, and would go
  //     on failing after #579 regenerates.
  //   * `midgard/validation_machine_v1/ValidationProofItemDatumV1` is in **no**
  //     blueprint, frozen or regenerated. Measured against a scratch stock build
  //     of this working tree (`MIDGARD_REAL_BLUEPRINT_PATH`), the definition is
  //     still absent: the datum is read as `Data` off an `InlineDatum` inside
  //     `canonical_decode_item_semantic_v1.proof_item_from_reference`, so it
  //     never reaches a validator's declared ABI surface and Aiken emits no
  //     definition for it. That row also cannot pass after regeneration.
  //
  // Both would therefore be gates that cannot pass, which is the mirror of the
  // gate that cannot fail and no more use as a handoff signal. **So #592's wire
  // change is invisible to this file by construction, not by staleness** — which
  // is worth knowing for #579, because regenerating the blueprint will not
  // surface it either. What pins those two surfaces instead is the cross-language
  // producer vector `typescript_generated_field_chunk_auxiliary_is_exact` in
  // `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak`,
  // emitted by `demo/midgard-validation/scripts/generate-validation-one-step-aiken-fixture.mjs`:
  // it checks the bytes this TypeScript half emits against the Aiken decoder and
  // the Aiken constructor's own field names, which is agreement between the two
  // languages rather than agreement of each with a blueprint.
] as const;

// The #640 format wave retired the five coarse rejection arms: the compact
// wire scalar is two-valued (0/1) and the rejection reason moved to the forced
// leaf's `OperatorVerdictV1`. Codes 2..5 are no longer decodable.
const VALIDITY_VECTORS = [
  ["TxIsValid", 0n, "d87980"],
  ["TxIsInvalid", 1n, "d87a80"],
] as const satisfies readonly [NativeMidgardTxValidity, bigint, string][];

describe("SDK/Aiken canonical V1 schema parity", () => {
  it("has no retired transaction-order validator, export, or parser", () => {
    const retiredName =
      /(?:TxOrder(?:Datum|Event|Payload)|ForcedInclusionTxV1)V(?:2|3)/u;
    expect(Object.keys(SDK).filter((name) => retiredName.test(name))).toEqual(
      [],
    );

    const sources = [
      "demo/midgard-sdk/src/user-events/tx-order.ts",
      "demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.ts",
      "onchain/aiken/lib/midgard/user-events/tx-order-v1.ak",
      "onchain/aiken/validators/user-events/tx-order-v1.ak",
    ];
    for (const relativePath of sources) {
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
    expect(() => decodeValidityCode(2n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() => decodeValidityCode(6n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() => Data.from("d87b80", SDK.MidgardTxValiditySchema)).toThrow();
    expect(() => Data.from("d87f80", SDK.MidgardTxValiditySchema)).toThrow();
  });
});
