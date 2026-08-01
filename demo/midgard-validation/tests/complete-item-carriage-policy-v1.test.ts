import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

/**
 * C21 production searches: no production code path may force a prover into
 * chunked or incremental input when the complete canonical item fits, and
 * the complete-item direct and inline-datum reference routes must stay
 * present in the deployed surface. Each scan is a pure predicate over the
 * checked-in source so a hostile negative control can prove it fires.
 */

const read = (path: string): string =>
  readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");

const machineSource = read("../src/validation-machine.ts");
const encoderSource = read("../src/validation-machine-data.ts");
const sdkProofItemSource = read(
  "../../midgard-sdk/src/fraud-proof/validation-proof-item-v1.ts",
);
const sdkWitnessSource = read(
  "../../midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
);
const submitSource = read(
  "../../midgard-fault-proofs/src/validation-dispute/submit.ts",
);
const itemSemanticAiken = read(
  "../../../onchain/aiken/validators/fraud-proofs/validation-trace/canonical-decode-item-semantic-v1.ak",
);
const proofItemAiken = read(
  "../../../onchain/aiken/validators/fraud-proofs/validation-trace/proof-item-v1.ak",
);
const blueprint = JSON.parse(read("../../../onchain/aiken/plutus.json")) as {
  readonly definitions: Record<
    string,
    {
      readonly anyOf?: readonly {
        readonly index?: number;
        readonly title?: string;
        readonly fields?: readonly { readonly title?: string }[];
      }[];
    }
  >;
};

const completeItemGuardPrecedesFieldItemWitness = (source: string): boolean => {
  const guardIndex = source.indexOf("maxSinglePublicationCompleteItemBytes");
  if (guardIndex < 0) {
    return false;
  }
  const window = source.slice(guardIndex, guardIndex + 700);
  return window.includes('kind: "transactionFieldItem"');
};

const generalFieldChunkFallbackIsGuarded = (source: string): boolean => {
  const guardIndex = source.indexOf("maxSinglePublicationCompleteItemBytes");
  if (guardIndex < 0) {
    return false;
  }
  const window = source.slice(guardIndex, guardIndex + 2_600);
  return (
    window.includes("midgardBoundedItemChunkCountV1") &&
    window.includes('kind: "transactionFieldChunk"')
  );
};

const countOccurrences = (source: string, needle: string): number =>
  source.split(needle).length - 1;

describe("C21 complete-item carriage production searches", () => {
  it("keeps the complete-item witness first and guards the only general-field chunk fallback by the measured threshold", () => {
    expect(completeItemGuardPrecedesFieldItemWitness(machineSource)).toBe(true);
    expect(generalFieldChunkFallbackIsGuarded(machineSource)).toBe(true);
    // Exactly one size threshold decides complete-versus-chunk carriage.
    expect(
      countOccurrences(machineSource, "maxSinglePublicationCompleteItemBytes"),
    ).toBe(1);
    // The scriptSources output fold carries only the authenticated
    // collection-proof tuple (C21-STAGE4 Option A): stage-4 evidence is
    // O(1) in output size, and the byte reveal lives solely in
    // canonicalDecode plus the stage-5 output traversal.
    expect(machineSource).toContain('kind: "transactionRedeemerItemBegin"');
    expect(machineSource).not.toContain("itemCbor: Buffer.from(outputCbor)");
  });

  it("pins the exact bounded-witness emitter inventory so new chunked producers reopen this row", () => {
    // 1 type declaration + 9 production emitters. Every emitter belongs to a
    // family listed in docs/exec-plans/evidence/necessity/; adding an emitter
    // without a §3.2 necessity artifact must fail this pin.
    expect(
      countOccurrences(machineSource, 'kind: "transactionFieldChunk"'),
    ).toBe(10);
    // 1 type declaration + canonicalDecode threshold route. The scriptSources
    // output fold emits the proof-only transactionRedeemerItemBegin witness
    // since C21-STAGE4 Option A dropped its byte reveal.
    expect(
      countOccurrences(machineSource, 'kind: "transactionFieldItem"'),
    ).toBe(2);
    expect(encoderSource).toContain('case "transactionFieldItem":');
    expect(encoderSource).toContain('case "transactionFieldChunk":');
  });

  it("keeps the public complete-item publication API free of prover-managed chunking", () => {
    expect(sdkProofItemSource).toContain(
      "deriveValidationProofItemPublicationV1",
    );
    expect(sdkProofItemSource).toContain("itemCbor: string");
    // The publication surface accepts the complete canonical item only; no
    // chunk proofs, offsets, folds, or cursors appear in its API.
    for (const forbidden of ["chunk", "Chunk", "offset", "cursor", "fold"]) {
      expect(sdkProofItemSource).not.toContain(forbidden);
    }
    // The wire ABI keeps the complete-item witness and datum shapes.
    expect(sdkWitnessSource).toContain("TransactionFieldItemWitness");
    const datumBlock = sdkWitnessSource.slice(
      sdkWitnessSource.indexOf(
        "ValidationProofItemDatumV1Schema = Data.Object",
      ),
    );
    for (const field of [
      "version: Data.Integer()",
      "transaction_id: Data.Bytes({ minLength: 32, maxLength: 32 })",
      "transaction_commitment: Data.Bytes({ minLength: 32, maxLength: 32 })",
      "collection_proof: BoundedCollectionItemProofV1Schema",
      "item_cbor: Data.Bytes()",
    ]) {
      expect(datumBlock).toContain(field);
    }
  });

  it("keeps §3.2 ordering in the submission selector: direct first, then automatic reference publication", () => {
    expect(submitSource).toMatch(
      /maxReliableDirectCompleteItemBytes\s*\n?\s*\?\s*"direct"\s*\n?\s*:\s*"reference"/u,
    );
    // Ingress accepts either representation for canonical decode; a prover is
    // never forced to hand-chunk a fitting item.
    expect(submitSource).toContain(
      "must carry an authenticated chunk or complete item",
    );
  });

  it("keeps both deployed complete-item routes and the append-only publication lock", () => {
    expect(itemSemanticAiken).toContain("Verify {");
    expect(itemSemanticAiken).toContain("item_cbor: ByteArray");
    expect(itemSemanticAiken).toContain("VerifyReference {");
    expect(itemSemanticAiken).toContain("reference_input_index: Int");
    expect(itemSemanticAiken).toContain("proof_item_from_reference");
    // The proof-item lock is a bare fail: published complete items are
    // append-only L1 evidence resolved through reference inputs.
    expect(proofItemAiken).toMatch(/else\(_\)\s*\{[\s\S]*fail[\s\S]*\}/u);
    expect(proofItemAiken).not.toContain("spend");

    const action =
      blueprint.definitions[
        "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/ActionV1"
      ];
    expect(action?.anyOf?.map((ctor) => ctor.title)).toEqual([
      "Verify",
      "VerifyReference",
    ]);
    expect(action?.anyOf?.[0]?.fields?.map((field) => field.title)).toEqual([
      "input_index",
      "output_index",
      "transition",
      "collection_proof",
      "item_cbor",
    ]);
    expect(action?.anyOf?.[1]?.fields?.map((field) => field.title)).toEqual([
      "input_index",
      "output_index",
      "transition",
      "reference_input_index",
    ]);
  });

  it("proves the scans fire on hostile sources", () => {
    // A producer that drops the threshold guard entirely.
    expect(
      completeItemGuardPrecedesFieldItemWitness(
        'pushWitness("canonicalDecode", cbor, { kind: "transactionFieldChunk", collectionProof, chunkProof });',
      ),
    ).toBe(false);
    // A producer that forces chunk carriage before the complete-item route.
    expect(
      completeItemGuardPrecedesFieldItemWitness(
        "const limit = MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;\n" +
          `${"// padding\n".repeat(90)}kind: "transactionFieldItem"`,
      ),
    ).toBe(false);
    expect(
      generalFieldChunkFallbackIsGuarded(
        "maxSinglePublicationCompleteItemBytes; unrelated();",
      ),
    ).toBe(false);
  });
});
