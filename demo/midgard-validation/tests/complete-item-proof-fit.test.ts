import "node:fs";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-carriage";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "./validation-fixtures.js";
import "./complete-item-proof-fit.resolve-carriage-for-plan.js";

import {
  buildMidgardBoundedItem,
  commitMidgardBoundedItem,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
import {
  encodeValidationSemanticResolutionRedeemer,
  parseExactAikenDataCbor,
  selectValidationCompleteItemCarriage,
  validationOneStepEvidenceHash,
} from "@al-ft/midgard-fault-proofs";
import { deriveValidationProofItemPublication } from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildValidationOneStepArgument,
  ValidationMachineCarriageResolutionRequiredError,
} from "../src/index.js";
import {
  buildTraceWithOutputs,
  carriageResolverForTrace,
  encodeInlineCompleteItemObserveRedeemer,
  findFieldItemStep,
  makeExactSizeOutputItem,
  validationDisputeBlueprint,
} from "./complete-item-proof-fit.resolve-carriage-for-plan.js";

describe("complete-item proof fit V1", () => {
  it("keeps the complete-item witness across the tier-1 carriage domain and past it, and reaches the chunked fallback", async () => {
    // The producer's complete-versus-chunk threshold is
    // `maxSinglePublicationCompleteItemBytes` (14,396), and the arithmetic
    // around it is worth stating because it is not obvious: a single-output
    // field-2 preimage is `81 ‖ 59 <len:2> ‖ item`, four bytes wider than its
    // item, so the largest item §8.3's tier-1 cap admits is 14,332 — 64 bytes
    // below the threshold.
    //
    // **#600 restores what #597's narrowing removed.** The trace producer no
    // longer names a tier at all: it records the field and its §5.1 preimage and
    // the tier is resolved at evidence commitment, so the producer builds across
    // the whole admissible range and the threshold above the tier-1 cap is
    // reachable again.
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - 4;
    const atCap = makeExactSizeOutputItem(largestTier1Item);
    expect(atCap.length).toBe(largestTier1Item);

    const fittingTrace = await buildTraceWithOutputs([atCap]);
    const fitting = findFieldItemStep(fittingTrace, largestTier1Item);
    expect(fitting.witness.auxiliary?.kind).toBe("transactionFieldItem");
    // The complete item is carried whole below the threshold, so canonicalDecode
    // emits no chunked step for field 2 here.
    expect(
      fittingTrace.witnesses.some(
        (witness) =>
          witness.phase === "canonicalDecode" &&
          witness.auxiliary?.kind === "transactionFieldChunk" &&
          witness.auxiliary.fieldIndex === 2,
      ),
    ).toBe(false);

    // One byte past the tier-1 cap the producer keeps going — §8.4 selects tier
    // 2 for those bytes and that is a fact about the carriage, not about whether
    // the step exists. This is the row that would have caught #597's narrowing.
    const aboveCap = makeExactSizeOutputItem(largestTier1Item + 1);
    const aboveCapTrace = await buildTraceWithOutputs([aboveCap]);
    const above = findFieldItemStep(aboveCapTrace, largestTier1Item + 1);
    expect(above.witness.auxiliary?.kind).toBe("transactionFieldItem");
    expect(
      selectMidgardFieldCarriageTier(above.planInput.fieldPreimage.length),
    ).toBe("RawUtxo");

    // The chunked fallback, reachable again (#597's Deviation retired). An item
    // above `maxSinglePublicationCompleteItemBytes` forces canonicalDecode's
    // chunked route, which needs a preimage §8.4 carries above tier 1 — exactly
    // the domain the named refusal used to remove. Exercised, not scanned for.
    const chunkedItem =
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes + 1;
    expect(chunkedItem).toBeGreaterThan(largestTier1Item);
    const chunkedTrace = await buildTraceWithOutputs([
      makeExactSizeOutputItem(chunkedItem),
    ]);
    const chunkedSteps = chunkedTrace.witnesses.filter(
      (witness) =>
        witness.phase === "canonicalDecode" &&
        witness.auxiliary?.kind === "transactionFieldChunk" &&
        witness.auxiliary.fieldIndex === 2,
    );
    expect(chunkedSteps.length).toBeGreaterThan(0);
    // And it is chunked *because* the item crossed the threshold, not because
    // the field is large: field 2 emits no complete-item step for this item.
    expect(
      chunkedTrace.witnesses.some(
        (witness) =>
          witness.phase === "canonicalDecode" &&
          witness.auxiliary?.kind === "transactionFieldItem" &&
          witness.auxiliary.fieldIndex === 2,
      ),
    ).toBe(false);
  }, 240_000);

  it("builds a block-path trace for a field preimage above the tier-1 cap", async () => {
    // **The regression pin for #597's unrecorded consequence (#600).** This
    // producer is not only the dispute path's: the operator's block-build
    // routine runs it once per transaction in a block
    // (`demo/midgard-node/src/mpf/validation-trace.ts`, wired at
    // `:4480-4483`), where a thrown carriage refusal fails the **whole block**.
    // While the producer named a tier, a single legal ~14.3 KB output — far
    // under `maxLedgerOutputPreimageBytes` — was enough to do that.
    //
    // So this row asserts the plain thing the narrowing broke: the trace builds.
    // It deliberately uses no resolver and no L1 context, because the block-build
    // caller has none and never will.
    const itemBytes = MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes;
    const trace = await buildTraceWithOutputs([
      makeExactSizeOutputItem(itemBytes),
    ]);
    expect(trace.verdict).toBe("accepted");

    // Field 2's preimage is the whole point: §8.4 puts these bytes at tier 3,
    // and the producer neither knows nor cares.
    const expectedPreimageBytes = encodeMidgardFieldPreimage([
      Buffer.alloc(itemBytes),
    ]).length;
    expect(selectMidgardFieldCarriageTier(expectedPreimageBytes)).toBe(
      "Certified",
    );
    const fieldTwoSteps = trace.witnesses.filter(
      (witness) =>
        witness.auxiliary !== null &&
        "fieldPreimage" in witness.auxiliary &&
        witness.auxiliary.fieldIndex === 2 &&
        witness.auxiliary.fieldPreimage.length === expectedPreimageBytes,
    );
    expect(fieldTwoSteps.length).toBeGreaterThan(0);

    // …and the other half of the same rule, on the same trace: what the block
    // path may do freely, *evidence commitment* may not. Building the one-step
    // argument for one of these steps without a resolver is the caller that has
    // no reference inputs asking for an `evidence_hash` over indices that would
    // have to point at nothing, and it refuses by name rather than emitting a
    // tier-1 `Inline` §8.4 does not admit at this length (#600).
    const { stateIndex } = findFieldItemStep(trace, itemBytes, "scriptSources");
    let thrown: unknown = null;
    try {
      buildValidationOneStepArgument({ trace, stateIndex });
    } catch (error) {
      thrown = error;
    }
    expect(thrown).toBeInstanceOf(
      ValidationMachineCarriageResolutionRequiredError,
    );
    const refusal = thrown as ValidationMachineCarriageResolutionRequiredError;
    expect(refusal.fieldIndex).toBe(2);
    expect(refusal.preimageLength).toBe(expectedPreimageBytes);
    expect(refusal.selectedTier).toBe("Certified");
    expect(refusal.maxTier1PreimageBytes).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    // The same step with the submitter's resolver commits without complaint.
    expect(() =>
      buildValidationOneStepArgument({
        trace,
        stateIndex,
        resolveFieldCarriage: carriageResolverForTrace(trace),
      }),
    ).not.toThrow();
  }, 240_000);

  it("keeps stage-4 one-step evidence O(1) in output size at every admissible output", async () => {
    // C21-STAGE4-GAP closure, restored to "every admissible output size" (#600).
    // Before the original fix, the stage-4 fold revealed the complete output
    // bytes *in addition to* a per-item opening, its evidence crossed the
    // 16,384-byte L1 envelope at a measured 14,774-byte single-output best case,
    // and the deployed direct-only carriage bounded it near 8,769 bytes — so a
    // dishonest operator could finalize an invalid block by forging exactly that
    // fold step for a legal large output.
    //
    // #597 could only reach the tier-1 domain and substituted a framing bound
    // for the ±8 O(1) assertion, because tier-1 `Inline` carriage *is* the
    // preimage. Both halves of that Deviation retire here:
    //
    // (a) **Above the cap the assertion is the original ±8 O(1) form.** A
    //     tier-2/3 carriage is reference-input indices and carries no preimage
    //     at all, which is what
    //     `onchain/aiken/lib/midgard/validation-machine/` says in
    //     terms. So the auxiliary stops growing with the output entirely, and
    //     the 14,774 B and 16,384 B probes — the two the closure used to cover
    //     and #597 re-pinned to a refusal — assert a built argument again.
    //
    // (b) **Inside tier 1 the framing bound stays**, because there the carriage
    //     genuinely is the preimage and no O(1) claim is true. It is still the
    //     property C21-STAGE4 protects: carriage plus Plutus-data segment
    //     framing and nothing else — no per-item opening, frontier or sibling
    //     path, and no second copy of the item.
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - 4;

    const probe = async (
      itemBytes: number,
    ): Promise<{
      readonly auxiliaryBytes: number;
      readonly evidenceBytes: number;
      readonly preimageBytes: number;
      readonly tier: string;
    }> => {
      const trace = await buildTraceWithOutputs([
        makeExactSizeOutputItem(itemBytes),
      ]);
      const step = findFieldItemStep(trace, itemBytes, "scriptSources");
      expect(step.witness.auxiliary?.kind).toBe("transactionRedeemerItemBegin");
      const auxiliary = step.witness.auxiliary;
      if (auxiliary?.kind !== "transactionRedeemerItemBegin") {
        throw new Error("stage-4 step is not the carriage-only witness");
      }
      const argument = buildValidationOneStepArgument({
        trace,
        stateIndex: step.stateIndex,
        resolveFieldCarriage: carriageResolverForTrace(trace),
      });
      return {
        auxiliaryBytes: argument.auxiliaryCbor.length,
        evidenceBytes: argument.evidenceCbor.length,
        preimageBytes: auxiliary.fieldPreimage.length,
        tier: selectMidgardFieldCarriageTier(auxiliary.fieldPreimage.length),
      };
    };

    const atTier1Cap = await probe(largestTier1Item);
    const small = await probe(256);

    // (b) Inside tier 1: Plutus data splits a long byte string into 64-byte
    // segments with a two-byte header each, so the framing is
    // `ceil(n / 64) * 2` plus a small constant — proportional to the preimage
    // and to **nothing else**.
    const framingBound = (preimageBytes: number): number =>
      preimageBytes + Math.ceil(preimageBytes / 64) * 2 + 64;
    expect(atTier1Cap.tier).toBe("Inline");
    expect(small.tier).toBe("Inline");
    expect(atTier1Cap.auxiliaryBytes).toBeLessThanOrEqual(
      framingBound(atTier1Cap.preimageBytes),
    );
    expect(small.auxiliaryBytes).toBeLessThanOrEqual(
      framingBound(small.preimageBytes),
    );
    const overheadAtCap = atTier1Cap.auxiliaryBytes - atTier1Cap.preimageBytes;
    const overheadSmall = small.auxiliaryBytes - small.preimageBytes;
    expect(atTier1Cap.evidenceBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
    );

    // (a) Above the cap: the two probes the C21 closure used to cover, built
    // rather than refused, with the ±8 O(1) assertion the closure originally
    // made. The auxiliary is reference-input indices, so it does not move with
    // the output at all — 14,774 B and 16,384 B outputs, 1,610 bytes apart,
    // produce auxiliaries within 8 bytes of each other.
    const aboveCap = [];
    for (const { itemBytes, tier } of [
      // The old C21 frontier: §8.4 carries its field preimage as tier 2.
      { itemBytes: 14_774, tier: "RawUtxo" as const },
      // The exact maximum admissible output: tier 3.
      {
        itemBytes: MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
        tier: "Certified" as const,
      },
    ]) {
      const measured = await probe(itemBytes);
      expect(measured.tier).toBe(tier);
      expect(measured.preimageBytes).toBeGreaterThan(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      );
      // O(1): the auxiliary carries indices, never the preimage, so it is a
      // small constant rather than a function of the output.
      expect(measured.auxiliaryBytes).toBeLessThan(128);
      expect(measured.evidenceBytes).toBeLessThan(
        MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
      );
      aboveCap.push(measured);
    }
    // The ±8 O(1) assertion itself, across the whole above-cap range.
    const [tier2, tier3] = aboveCap;
    expect(
      Math.abs(tier3!.auxiliaryBytes - tier2!.auxiliaryBytes),
    ).toBeLessThanOrEqual(8);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          scriptSourcesStageFourCarriageEvidenceV1: {
            largestTier1Item,
            atTier1Cap,
            small,
            overheadAtCap,
            overheadSmall,
            aboveCap,
            evidenceEnvelopeBytes: 16 * 1024 - 1,
          },
        }),
      );
    }
  }, 240_000);

  it("encodes ABI-exact transition-only and observe complete-item redeemers for the maximum shapes", async () => {
    const directMax =
      MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const item = makeExactSizeOutputItem(directMax);
    const trace = await buildTraceWithOutputs([item]);
    const { stateIndex } = findFieldItemStep(trace, directMax);
    const oneStepArgument = buildValidationOneStepArgument({
      trace,
      stateIndex,
    });
    expect(oneStepArgument.resolverIndex).toBe(0);
    expect(oneStepArgument.semanticResolverIndex).toBe(1);
    expect(selectValidationCompleteItemCarriage(directMax)).toBe("direct");

    const observeRedeemer = encodeInlineCompleteItemObserveRedeemer({
      auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      inputIndex: 1n,
      outputIndex: 0n,
    });
    const semanticRedeemer = encodeValidationSemanticResolutionRedeemer({
      oneStepArgument,
      inputIndex: 1n,
      outputIndex: 0n,
    });
    // The observe `Observe` arm is unmoved by #620 — the subtraction deleted a
    // body conjunct, not a redeemer field — so the preimage-bearing redeemer
    // still parses against the committed blueprint exactly, before and after
    // the wave's regeneration.
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "fraud_proofs/validation_trace/canonical_decode_item_observe_v1/SpendRedeemer",
      cbor: observeRedeemer.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    // The item-semantic `Verify` now parses against the blueprint. #620
    // reshaped it from `(input_index, output_index, transition, carriage)` to
    // the transition-only `(input_index, output_index, transition)` and
    // retired the `VerifyReference` arm; the #617 wave's single regeneration
    // has carried that reshape into the committed `plutus.json`, so the row
    // no longer pins a frozen four-field list and instead checks the emitted
    // redeemer against the regenerated definition — the stronger check the
    // deferral was standing in for.
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
      cbor: semanticRedeemer.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    const verifyFields = (
      validationDisputeBlueprint as {
        readonly definitions: Record<
          string,
          {
            readonly anyOf?: readonly {
              readonly title?: string;
              readonly fields?: readonly { readonly title?: string }[];
            }[];
          }
        >;
      }
    ).definitions[
      "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/ActionV1"
    ]?.anyOf;
    expect(verifyFields?.map((ctor) => ctor.title)).toEqual(["Verify"]);
    expect(verifyFields?.[0]?.fields?.map((field) => field.title)).toEqual([
      "input_index",
      "output_index",
      "transition",
    ]);
    // Raw wire pin for the transition-only `Verify`: three fields under
    // constructor 0, no carriage.
    const semanticAction = Data.from(semanticRedeemer.toString("hex"));
    expect(semanticAction).toBeInstanceOf(Constr);
    const semanticContinue = (semanticAction as Constr<unknown>).fields[0];
    expect(semanticContinue).toBeInstanceOf(Constr);
    expect((semanticContinue as Constr<unknown>).index).toBe(0);
    expect((semanticContinue as Constr<unknown>).fields).toHaveLength(3);

    expect(observeRedeemer.length).toBeGreaterThan(directMax);
    expect(observeRedeemer.length).toBeLessThan(16 * 1024);
    expect(semanticRedeemer.length).toBeLessThan(2_048);
    // Option B: the staged commitment is transition-only —
    // `hash_one_step_evidence(transition, NoAuxiliaryWitness)`.
    expect(
      validationOneStepEvidenceHash({
        transitionCbor: oneStepArgument.transitionCbor,
        auxiliaryCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
      }),
    ).toMatch(/^[0-9a-f]{64}$/u);

    const bounded = buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: item,
    });
    expect(
      commitMidgardBoundedItem({
        fieldIndex: 2,
        itemIndex: 0,
        totalLength: item.length,
        frontier: bounded.frontier,
      }).toString("hex"),
    ).toBe(bounded.commitment.toString("hex"));
  }, 120_000);

  it("measures that oversized items overflow every single-publication transaction, not just the item bound", () => {
    // §3.2: proof-fit decisions measure the actual publication transaction.
    // Construct the complete signed Conway publication for oversized shapes
    // and record the exact overshoot that necessitates bounded fallbacks.
    // #597: what a publication holds is the field's whole §5.1 preimage, so the
    // measured shape is the single-item envelope of the oversized item — which
    // is the smallest genuine field that could carry it, making the overshoot a
    // lower bound rather than an inflated one.
    const measurePublicationTransaction = (itemBytes: number): number => {
      const publication = deriveValidationProofItemPublication({
        transactionId: "44".repeat(32),
        transactionCommitment: "55".repeat(32),
        fieldPreimage: encodeMidgardFieldPreimage([
          Buffer.alloc(itemBytes, 0xa5),
        ]).toString("hex"),
      });
      const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 4));
      const paymentKeyHash = signingKey.to_public().hash();
      const address = CML.Address.from_raw_bytes(
        Buffer.concat([
          Buffer.from([0x60]),
          Buffer.from(paymentKeyHash.to_raw_bytes()),
        ]),
      );
      const scriptAddress = CML.Address.from_raw_bytes(
        Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x66)]),
      );
      const inputs = CML.TransactionInputList.new();
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
          0n,
        ),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          scriptAddress,
          CML.Value.from_coin(70_000_000n),
          CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(publication.datumCbor),
          ),
          undefined,
        ),
      );
      outputs.add(
        CML.TransactionOutput.new(
          address,
          CML.Value.from_coin(1_000_000_000n),
          undefined,
          undefined,
        ),
      );
      const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
      const witnessSet = CML.TransactionWitnessSet.new();
      const vkeys = CML.VkeywitnessList.new();
      vkeys.add(
        CML.Vkeywitness.new(
          signingKey.to_public(),
          signingKey.sign(Buffer.alloc(32, 5)),
        ),
      );
      witnessSet.set_vkeywitnesses(vkeys);
      return CML.Transaction.new(
        body,
        witnessSet,
        true,
        undefined,
      ).to_cbor_bytes().length;
    };

    const maxOutputItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
    );
    expect(maxOutputItem).toBeGreaterThan(16 * 1024);
    const maxAggregateItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes,
    );
    expect(maxAggregateItem).toBeGreaterThan(16 * 1024);
    // The exact publication-threshold shape still fits the same framing.
    const thresholdItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
    );
    expect(thresholdItem).toBeLessThanOrEqual(16 * 1024);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          oversizedCompletePublicationOvershootV1: {
            maxLedgerOutputPreimageBytes:
              MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
            maxLedgerOutputPublicationTransactionBytes: maxOutputItem,
            maxAggregateFieldItemBytes:
              MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes,
            maxAggregatePublicationTransactionBytes: maxAggregateItem,
            thresholdItemBytes:
              MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
            thresholdPublicationTransactionBytes: thresholdItem,
            maxTxSizeBytes: 16 * 1024,
          },
        }),
      );
    }
  });
});
