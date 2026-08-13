import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardBoundedItemV1,
  commitMidgardBoundedItemV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { deriveValidationProofItemPublicationV1 } from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../../midgard-fault-proofs/src/aiken-blueprint-data.js";
import {
  encodeValidationSemanticResolutionRedeemerV1,
  selectValidationCompleteItemCarriageV1,
  validationOneStepEvidenceHashV1,
} from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  ValidationMachineCarriageTierUnsupportedErrorV1,
  type ValidationMachineWorkWitness,
} from "../src/index.js";
import {
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

const validationBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const validationDisputeBlueprint = JSON.parse(
  readFileSync(validationBlueprintPath, "utf8"),
) as unknown;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const encodeBoundedBytesItem = (payload: Buffer): Buffer => {
  if (payload.length < 24) {
    return Buffer.concat([Buffer.from([0x40 + payload.length]), payload]);
  }
  if (payload.length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, payload.length]), payload]);
  }
  throw new Error("bounded datum chunk must stay below 256 bytes");
};

/**
 * Canonical Plutus-data filler: a single definite byte string when it fits 64
 * bytes, otherwise an indefinite list of 64-byte-bounded byte strings, which
 * matches Aiken's `cbor.serialise` list convention.
 */
const canonicalDatumFiller = (payloadBytes: number): Buffer => {
  if (payloadBytes <= 64) {
    return encodeBoundedBytesItem(Buffer.alloc(payloadBytes, 0xa5));
  }
  const items: Buffer[] = [];
  let remaining = payloadBytes;
  while (remaining > 0) {
    const take = Math.min(remaining, 64);
    items.push(encodeBoundedBytesItem(Buffer.alloc(take, 0xa5)));
    remaining -= take;
  }
  return Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
};

/**
 * Builds a canonical output item whose exact encoded length equals
 * `targetItemBytes` by sizing an inline-datum payload to close the gap.
 */
const makeExactSizeOutputItem = (targetItemBytes: number): Buffer => {
  const probe = (payloadBytes: number): Buffer =>
    encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 10n, assets: new Map() },
      datum: { kind: "inline", cbor: canonicalDatumFiller(payloadBytes) },
    });
  let payload = Math.max(0, targetItemBytes - probe(0).length);
  for (let attempt = 0; attempt < 12; attempt += 1) {
    const candidate = probe(payload);
    const delta = targetItemBytes - candidate.length;
    if (delta === 0) {
      return candidate;
    }
    payload += delta;
    if (payload < 0) {
      throw new Error(
        `target item of ${targetItemBytes.toString()} bytes is below the minimum output framing`,
      );
    }
  }
  throw new Error(
    `could not converge on an exact ${targetItemBytes.toString()}-byte output item`,
  );
};

const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  const spentOutput = makeOutput(10n);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

const findFieldItemStep = (
  trace: DeterministicValidationMachineTrace,
  itemBytes: number,
  phase: "canonicalDecode" | "scriptSources" = "canonicalDecode",
): {
  readonly stateIndex: number;
  readonly witness: ValidationMachineWorkWitness;
} => {
  const expectedPreimageBytes = encodeMidgardFieldPreimageV1([
    Buffer.alloc(itemBytes),
  ]).length;
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    // #597: both constructors carry a §8 `FieldCarriageV1` now and neither
    // carries an item length or a field index, so a step is located by phase,
    // kind, and the *bytes the carriage delivers* — which for these traces is
    // field 2's single-item §5.1 envelope. Matching on content rather than on a
    // claimed number is the same discipline the door itself keeps, and it is
    // what keeps this from selecting field 0's step, whose complete-item witness
    // comes first in the canonicalDecode walk.
    if (
      witness.phase !== phase ||
      !(
        witness.auxiliary?.kind === "transactionFieldItem" ||
        (phase === "scriptSources" &&
          witness.auxiliary?.kind === "transactionRedeemerItemBegin")
      )
    ) {
      continue;
    }
    const { carriage } = witness.auxiliary;
    if (
      carriage.carriage === "Inline" &&
      carriage.preimage.length === expectedPreimageBytes
    ) {
      return { stateIndex: index, witness };
    }
  }
  throw new Error(
    `trace has no ${phase} complete-item witness of ${itemBytes.toString()} bytes`,
  );
};

/**
 * The deployed `canonical_decode_item_semantic_v1` spend ABI takes the item's
 * source as one exact `Verify` field — a §8 `FieldCarriageV1` (#597, tracking
 * #592's move off `collection_proof`/`item_cbor`). The production submit path
 * constructs this shape inside `makeSemanticResolutionRedeemer`; this mirror
 * keeps the measured redeemer byte-identical to the deployed ABI without a live
 * transaction context.
 */
const encodeDirectCompleteItemVerifyRedeemer = ({
  transitionCbor,
  auxiliaryCbor,
  inputIndex,
  outputIndex,
}: {
  readonly transitionCbor: Buffer;
  readonly auxiliaryCbor: Buffer;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): Buffer => {
  const transition = Data.from(transitionCbor.toString("hex"));
  const auxiliary = Data.from(auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error(
      "direct complete-item redeemer requires a TransactionFieldItemWitness auxiliary",
    );
  }
  return Buffer.from(
    Data.to(
      new Constr(1, [
        new Constr(0, [
          inputIndex,
          outputIndex,
          transition,
          auxiliary.fields[0]!,
        ]),
      ]),
    ),
    "hex",
  );
};

/**
 * Digs the carriage refusal out of whatever wrapper it arrived in.
 *
 * The trace producer runs inside an Effect, so a thrown
 * {@link ValidationMachineCarriageTierUnsupportedErrorV1} reaches a caller
 * wrapped in a `FiberFailure` whose cause hangs off a symbol key. Asserting
 * `toBeInstanceOf` on the wrapper would pass only by accident; walking to the
 * real error is what lets the rows below read its measured fields.
 */
const carriageRefusalOf = (
  thrown: unknown,
): ValidationMachineCarriageTierUnsupportedErrorV1 => {
  const seen = new Set<unknown>();
  const walk = (
    value: unknown,
  ): ValidationMachineCarriageTierUnsupportedErrorV1 | null => {
    if (typeof value !== "object" || value === null || seen.has(value)) {
      return null;
    }
    seen.add(value);
    if (value instanceof ValidationMachineCarriageTierUnsupportedErrorV1) {
      return value;
    }
    const container = value as Record<string | symbol, unknown>;
    for (const key of [
      ...Object.getOwnPropertyNames(container),
      ...Object.getOwnPropertySymbols(container),
    ]) {
      const found = walk(container[key]);
      if (found !== null) {
        return found;
      }
    }
    return null;
  };
  const found = walk(thrown);
  if (found === null) {
    throw new Error(
      `expected a ValidationMachineCarriageTierUnsupportedErrorV1, got ${String(thrown)}`,
    );
  }
  return found;
};

describe("complete-item proof fit V1", () => {
  it("keeps the complete-item witness across the tier-1 carriage domain, and refuses above it", async () => {
    // The producer's complete-versus-chunk threshold is
    // `maxSinglePublicationCompleteItemBytes` (14,396). **That threshold now
    // sits outside the carriage domain this producer can emit**, and the
    // arithmetic is worth stating because it is not obvious: a single-output
    // field-2 preimage is `81 ‖ 59 <len:2> ‖ item`, four bytes wider than its
    // item, so the largest item tier 1 admits is 14,332 — 64 bytes below the
    // threshold. Both of this row's original probes (14,396 and 14,397) are
    // therefore above the cap.
    //
    // **DEVIATION — #597, confirmed by orchestrator RULING 2 (2026-08-12).**
    // The consequence is that the canonicalDecode *chunked*
    // fallback is unreachable: it needs an item above 14,396, which forces a
    // preimage above 14,400, which §8.4 carries as tier 2 or 3. The fallback is
    // still there and still guarded — `complete-item-carriage-policy-v1.test.ts`
    // pins that by source scan — but no tier-1 trace exercises it, so what this
    // row can assert is the complete-item route across the admitted domain and
    // the named refusal above it. #600 restores the rest.
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 - 4;
    const atCap = makeExactSizeOutputItem(largestTier1Item);
    expect(atCap.length).toBe(largestTier1Item);

    const fittingTrace = await buildTraceWithOutputs([atCap]);
    const fitting = findFieldItemStep(fittingTrace, largestTier1Item);
    expect(fitting.witness.auxiliary?.kind).toBe("transactionFieldItem");
    // The complete item is carried whole, so canonicalDecode emits no chunked
    // step for field 2 anywhere in the admitted domain.
    expect(
      fittingTrace.witnesses.some(
        (witness) =>
          witness.phase === "canonicalDecode" &&
          witness.auxiliary?.kind === "transactionFieldChunk" &&
          witness.auxiliary.fieldIndex === 2,
      ),
    ).toBe(false);

    // One byte more and §8.4 stops admitting tier 1, so the producer refuses by
    // name instead of emitting a step no prover could submit.
    const aboveCap = makeExactSizeOutputItem(largestTier1Item + 1);
    const refusal = await buildTraceWithOutputs([aboveCap]).then(
      () => null,
      (cause: unknown) => cause,
    );
    const typed = carriageRefusalOf(refusal);
    expect(typed.fieldIndex).toBe(2);
    expect(typed.preimageLength).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 + 1,
    );
    expect(typed.selectedTier).toBe("RawUtxo");
    expect(typed.followUpIssue).toBe(600);
    // The threshold the producer's complete-versus-chunk guard uses is above the
    // cap, which is exactly why the chunked branch is unreachable today.
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
    ).toBeGreaterThan(largestTier1Item);
  }, 120_000);

  it("keeps stage-4 one-step evidence free of per-item proof material across the tier-1 carriage domain", async () => {
    // C21-STAGE4-GAP closure (Option A). Before the fix, the stage-4 fold
    // revealed the complete output bytes *in addition to* a per-item opening,
    // its evidence crossed the 16,384-byte L1 envelope at a measured
    // 14,774-byte single-output best case, and the deployed direct-only carriage
    // bounded it near 8,769 bytes — so a dishonest operator could finalize an
    // invalid block by forging exactly that fold step for a legal large output.
    //
    // **DEVIATION — #597, confirmed by orchestrator RULING 2 (2026-08-12).**
    // This row used to assert stage-4 evidence was O(1) in output size at every
    // admissible size, and that assertion is **not recoverable under tier 1**.
    //
    // (a) Why the old ±8 bound cannot hold. Under §8 the fold's evidence is a
    //     `FieldCarriageV1`, which is O(1) **only under tiers 2–3**, where the
    //     preimage rides reference inputs —
    //     `onchain/aiken/lib/midgard/validation-machine-v1.ak:9190` says exactly
    //     that. Tier-1 `Inline` carriage *is* the preimage, so under the only
    //     tier this producer can emit (see `machineFieldCarriageV1`) the
    //     auxiliary is proportional to the field by construction. Measured here:
    //     a 260-byte preimage gives a 280-byte auxiliary (overhead 20) and a
    //     14,336-byte preimage gives 14,795 (overhead 459). The overhead is the
    //     CBOR segment framing below, not a constant, so no ±8 bound exists to
    //     assert.
    //
    //     The substitute pin is
    //         auxiliaryBytes <= preimageBytes + ceil(preimageBytes / 64) * 2 + 64
    //     checked at both probes. That is the property C21-STAGE4 actually
    //     protects: the auxiliary is the carriage plus Plutus-data segment
    //     framing and **nothing else** — no per-item opening, frontier or
    //     sibling path, and no second copy of the item beside the carriage.
    //
    // (b) #600 restores the O(1) form. The bound above collapses to a constant
    //     the moment tiers 2–3 are emittable, because a tier-2/3 carriage is
    //     reference-input indices and carries no preimage at all; the same
    //     constructor and the same pin then express the original assertion.
    //
    // (c) Recorded as a deviation from RULING 2's item 3, which had assumed the
    //     ±8 bound survived inside tier 1. The two probes above the cap are
    //     re-pinned to the named refusal, as that ruling directs.
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 - 4;

    const probe = async (
      itemBytes: number,
    ): Promise<{
      readonly auxiliaryBytes: number;
      readonly evidenceBytes: number;
      readonly preimageBytes: number;
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
      const { carriage } = auxiliary;
      if (carriage.carriage !== "Inline") {
        throw new Error("stage-4 carriage is not tier-1 Inline");
      }
      const argument = buildValidationOneStepArgumentV1({
        trace,
        stateIndex: step.stateIndex,
      });
      return {
        auxiliaryBytes: argument.auxiliaryCbor.length,
        evidenceBytes: argument.evidenceCbor.length,
        preimageBytes: carriage.preimage.length,
      };
    };

    const atTier1Cap = await probe(largestTier1Item);
    const small = await probe(256);

    // Plutus data splits a long byte string into 64-byte segments with a
    // two-byte header each, so the framing is `ceil(n / 64) * 2` plus a small
    // constant — proportional to the preimage and to **nothing else**. Anything
    // per-item (an opening, a frontier, a sibling path) would exceed this bound,
    // and a second copy of the item would roughly double the whole figure. This
    // is the substitute for the retired ±8 O(1) assertion; see (a) above.
    const framingBound = (preimageBytes: number): number =>
      preimageBytes + Math.ceil(preimageBytes / 64) * 2 + 64;
    expect(atTier1Cap.auxiliaryBytes).toBeLessThanOrEqual(
      framingBound(atTier1Cap.preimageBytes),
    );
    expect(small.auxiliaryBytes).toBeLessThanOrEqual(
      framingBound(small.preimageBytes),
    );
    const overheadAtCap = atTier1Cap.auxiliaryBytes - atTier1Cap.preimageBytes;
    const overheadSmall = small.auxiliaryBytes - small.preimageBytes;
    // Everything in the admitted domain still builds inside the L1 envelope.
    expect(atTier1Cap.evidenceBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );

    // Above the cap the producer refuses, by name, with the numbers a reader
    // needs to see that the refusal is §8.4's partition and not an accident.
    // These are the two probes the closure used to cover.
    for (const { itemBytes, tier } of [
      // The old C21 frontier: §8.4 carries its field preimage as tier 2.
      { itemBytes: 14_774, tier: "RawUtxo" as const },
      // The exact maximum admissible output: tier 3.
      {
        itemBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
        tier: "Certified" as const,
      },
    ]) {
      const refusal = await buildTraceWithOutputs([
        makeExactSizeOutputItem(itemBytes),
      ]).then(
        () => null,
        (cause: unknown) => cause,
      );
      const typed = carriageRefusalOf(refusal);
      expect(typed.fieldIndex).toBe(2);
      expect(typed.selectedTier).toBe(tier);
      expect(typed.maxTier1PreimageBytes).toBe(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      );
      expect(typed.followUpIssue).toBe(600);
      expect(typed.preimageLength).toBeGreaterThan(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      );
      // The refusal names §8.4's own selection, so it cannot be satisfied by
      // quietly widening tier 1.
      expect(selectMidgardFieldCarriageTierV1(typed.preimageLength)).toBe(tier);
    }

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          scriptSourcesStageFourCarriageEvidenceV1: {
            largestTier1Item,
            atTier1Cap,
            small,
            overheadAtCap,
            overheadSmall,
            evidenceEnvelopeBytes: 16 * 1024 - 1,
          },
        }),
      );
    }
  }, 240_000);

  it("encodes ABI-exact direct and reference complete-item proof redeemers for the maximum shapes", async () => {
    const directMax =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const item = makeExactSizeOutputItem(directMax);
    const trace = await buildTraceWithOutputs([item]);
    const { stateIndex } = findFieldItemStep(trace, directMax);
    const oneStepArgument = buildValidationOneStepArgumentV1({
      trace,
      stateIndex,
    });
    expect(oneStepArgument.resolverIndex).toBe(0);
    expect(oneStepArgument.semanticResolverIndex).toBe(1);
    expect(selectValidationCompleteItemCarriageV1(directMax)).toBe("direct");

    const directRedeemer = encodeDirectCompleteItemVerifyRedeemer({
      transitionCbor: oneStepArgument.transitionCbor,
      auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      inputIndex: 1n,
      outputIndex: 0n,
    });
    const referenceRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument,
      inputIndex: 1n,
      outputIndex: 0n,
      proofItemReferenceInputIndex: 0n,
    });
    // The `VerifyReference` route is unmoved by #597 — it names a reference
    // input, not a carriage — so it still parses against the committed
    // blueprint exactly.
    parseExactAikenDataCbor({
      blueprint: validationDisputeBlueprint,
      definitionName:
        "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
      cbor: referenceRedeemer.toString("hex"),
      maxBytes: 16 * 1024 - 1,
    });
    // The `Verify` route is not parsed against the blueprint, and the reason is
    // recorded rather than worked around: #592 reshaped it from
    // `(input_index, output_index, transition, collection_proof, item_cbor)` to
    // `(input_index, output_index, transition, carriage)` in the Aiken source,
    // and left `plutus.json` byte-identical because blueprints move once, in
    // #579's single regeneration (#587's precedent). So the committed blueprint
    // still declares five fields while the deployed source declares four, and
    // this row measures that recorded state instead of asserting agreement that
    // does not exist yet. `complete-item-carriage-policy-v1.test.ts` pins the
    // same divergence from the Aiken side.
    const frozenVerify = (
      validationDisputeBlueprint as {
        readonly definitions: Record<
          string,
          {
            readonly anyOf?: readonly {
              readonly fields?: readonly { readonly title?: string }[];
            }[];
          }
        >;
      }
    ).definitions[
      "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/ActionV1"
    ]?.anyOf?.[0]?.fields?.map((field) => field.title);
    expect(frozenVerify).toEqual([
      "input_index",
      "output_index",
      "transition",
      "collection_proof",
      "item_cbor",
    ]);
    // When #579 regenerates, that list becomes the four-field form and this
    // redeemer parses against it; until then its shape is asserted directly.
    const directAction = Data.from(directRedeemer.toString("hex"));
    expect(directAction).toBeInstanceOf(Constr);
    const directContinue = (directAction as Constr<unknown>).fields[0];
    expect(directContinue).toBeInstanceOf(Constr);
    expect((directContinue as Constr<unknown>).index).toBe(0);
    expect((directContinue as Constr<unknown>).fields).toHaveLength(4);

    expect(directRedeemer.length).toBeGreaterThan(directMax);
    expect(directRedeemer.length).toBeLessThan(16 * 1024);
    expect(referenceRedeemer.length).toBeLessThan(2_048);
    expect(
      validationOneStepEvidenceHashV1({
        transitionCbor: oneStepArgument.transitionCbor,
        auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      }),
    ).toMatch(/^[0-9a-f]{64}$/u);

    const bounded = buildMidgardBoundedItemV1({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: item,
    });
    expect(
      commitMidgardBoundedItemV1({
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
      const publication = deriveValidationProofItemPublicationV1({
        transactionId: "44".repeat(32),
        transactionCommitment: "55".repeat(32),
        fieldPreimage: encodeMidgardFieldPreimageV1([
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
      MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
    );
    expect(maxOutputItem).toBeGreaterThan(16 * 1024);
    const maxAggregateItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionAggregateFieldBytes,
    );
    expect(maxAggregateItem).toBeGreaterThan(16 * 1024);
    // The exact publication-threshold shape still fits the same framing.
    const thresholdItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
    );
    expect(thresholdItem).toBeLessThanOrEqual(16 * 1024);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          oversizedCompletePublicationOvershootV1: {
            maxLedgerOutputPreimageBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
            maxLedgerOutputPublicationTransactionBytes: maxOutputItem,
            maxAggregateFieldItemBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionAggregateFieldBytes,
            maxAggregatePublicationTransactionBytes: maxAggregateItem,
            thresholdItemBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
            thresholdPublicationTransactionBytes: thresholdItem,
            maxTxSizeBytes: 16 * 1024,
          },
        }),
      );
    }
  });
});
