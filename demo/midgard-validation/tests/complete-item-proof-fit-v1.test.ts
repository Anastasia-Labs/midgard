import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardBoundedItemV1,
  commitMidgardBoundedItemV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  type MidgardFieldCarriagePlanV1,
  planMidgardFieldCarriageV1,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveFieldPreimageCertificationV1,
  deriveValidationProofItemPublicationV1,
  fieldPreimagePublicationDatumCborV1,
  resolveMidgardFieldCarriageAgainstReferenceInputsV1,
} from "@al-ft/midgard-sdk";
import { CML, Constr, Data, type UTxO } from "@lucid-evolution/lucid";
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
  ValidationMachineCarriageResolutionRequiredErrorV1,
  type ValidationMachineFieldCarriagePlanInputV1,
  type ValidationMachineFieldCarriageResolverV1,
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
  readonly planInput: ValidationMachineFieldCarriagePlanInputV1;
} => {
  const expectedPreimageBytes = encodeMidgardFieldPreimageV1([
    Buffer.alloc(itemBytes),
  ]).length;
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    // #600: both constructors carry the carriage *plan input* now — which field
    // and its §5.1 preimage — so a step is located by phase, kind, and the bytes
    // it read, which for these traces is field 2's single-item envelope.
    // Matching on content rather than on a claimed number is the same discipline
    // the door itself keeps, and it is what keeps this from selecting field 0's
    // step, whose complete-item witness comes first in the canonicalDecode walk.
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
    const planInput = witness.auxiliary;
    if (planInput.fieldPreimage.length === expectedPreimageBytes) {
      return { stateIndex: index, witness, planInput };
    }
  }
  throw new Error(
    `trace has no ${phase} complete-item witness of ${itemBytes.toString()} bytes`,
  );
};

/**
 * Option B (#620): the item-semantic `Verify` is transition-only, so the
 * redeemer that embeds the item's §8 `FieldCarriageV1` — and therefore the one
 * whose envelope fit the direct frontier measures — is now the observe stage's
 * inline `Observe` arm, `(input_index, output_index, carriage)`. The
 * production submit path constructs this shape inside `submitStage`'s observe
 * encode; this mirror keeps the measured redeemer byte-identical to the
 * deployed ABI without a live transaction context.
 */
const encodeInlineCompleteItemObserveRedeemer = ({
  auxiliaryCbor,
  inputIndex,
  outputIndex,
}: {
  readonly auxiliaryCbor: Buffer;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): Buffer => {
  const auxiliary = Data.from(auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error(
      "inline complete-item observation requires a TransactionFieldItemWitness auxiliary",
    );
  }
  return Buffer.from(
    Data.to(
      new Constr(1, [
        new Constr(0, [inputIndex, outputIndex, auxiliary.fields[0]!]),
      ]),
    ),
    "hex",
  );
};

/** A prover key hash — §8.6's `owner`, the min-Ada reclaim authority. */
const CARRIAGE_OWNER = Buffer.alloc(28, 0x7c);
/** The §8.6 certificate minting policy, a validator parameter (#579 rider 2). */
const CERTIFICATE_POLICY_ID = "ab".repeat(28);
const CERTIFICATE_ADDRESS = "addr_test1_field_preimage_certificate";
const PROVER_KEY_ADDRESS = "addr_test1_prover_key_address";

const carriageUtxo = ({
  txHash,
  outputIndex,
  address,
  datum,
  assets,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly datum: string;
  readonly assets?: Record<string, bigint>;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n, ...(assets ?? {}) },
  datum,
});

/**
 * The §8 carriage a dispute submitter would resolve for one field, together with
 * the reference-input set it resolved against (#600).
 *
 * Everything here comes from producers: `planMidgardFieldCarriageV1` decides the
 * tier by §8.4's partition over the preimage's length, the chunk datums are
 * `fieldPreimagePublicationDatumCborV1`'s bytes, the manifest is
 * `deriveFieldPreimageCertificationV1`'s, and the indices come back from
 * `resolveMidgardFieldCarriageAgainstReferenceInputsV1`, which locates each one
 * **by content** against the canonically-sorted list (§8.7). No index is written
 * down anywhere in this file.
 *
 * The reference-input set deliberately contains a **decoy** — the published
 * spending validator a real step reads through `readFrom`, which sorts into the
 * same list and shifts every carriage index that follows it. A resolver that
 * counted only carriage UTxOs would produce indices that are right here and
 * wrong on L1; including it is what makes these vectors measure the real thing
 * (ruling D3-A).
 */
const resolveCarriageForPlan = (
  plan: MidgardFieldCarriagePlanV1,
): {
  readonly carriage: ReturnType<
    typeof resolveMidgardFieldCarriageAgainstReferenceInputsV1
  >;
  readonly referenceInputs: readonly UTxO[];
} => {
  const scriptReference = carriageUtxo({
    txHash: "00".repeat(32),
    outputIndex: 0,
    address: "addr_test1_published_validator",
    datum: "d87980",
  });
  const publications = plan.publications.map((publication, offset) =>
    carriageUtxo({
      txHash: `${(offset + 3).toString(16).padStart(2, "0")}`.repeat(32),
      outputIndex: offset,
      address: PROVER_KEY_ADDRESS,
      datum: fieldPreimagePublicationDatumCborV1(publication.bytes),
    }),
  );
  const certificate =
    plan.tier === "Certified"
      ? [
          ((): UTxO => {
            const certification = deriveFieldPreimageCertificationV1(plan);
            return carriageUtxo({
              txHash: "f1".repeat(32),
              outputIndex: 0,
              address: CERTIFICATE_ADDRESS,
              datum: certification.datumCbor,
              assets: {
                [`${CERTIFICATE_POLICY_ID}${certification.assetNameHex}`]: 1n,
              },
            });
          })(),
        ]
      : [];
  const referenceInputs = [scriptReference, ...publications, ...certificate];
  return {
    carriage: resolveMidgardFieldCarriageAgainstReferenceInputsV1({
      plan,
      referenceInputs,
      certificatePolicyId: CERTIFICATE_POLICY_ID,
    }),
    referenceInputs,
  };
};

/**
 * The resolver a submitter hands `buildValidationOneStepArgumentV1` — #600's
 * seam, as the dispute path uses it.
 */
const carriageResolverForTrace = (
  trace: DeterministicValidationMachineTrace,
): ValidationMachineFieldCarriageResolverV1 => {
  const txId = Buffer.from(trace.states[0]!.transactionId);
  return ({ fieldIndex, fieldPreimage }) =>
    resolveCarriageForPlan(
      planMidgardFieldCarriageV1({
        owner: CARRIAGE_OWNER,
        txId,
        fieldIndex,
        preimage: fieldPreimage,
      }),
    ).carriage;
};

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
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 - 4;
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
      selectMidgardFieldCarriageTierV1(above.planInput.fieldPreimage.length),
    ).toBe("RawUtxo");

    // The chunked fallback, reachable again (#597's Deviation retired). An item
    // above `maxSinglePublicationCompleteItemBytes` forces canonicalDecode's
    // chunked route, which needs a preimage §8.4 carries above tier 1 — exactly
    // the domain the named refusal used to remove. Exercised, not scanned for.
    const chunkedItem =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes + 1;
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
    // (`demo/midgard-node/src/workers/utils/mpf.ts:1194-1234`, wired at
    // `:4480-4483`), where a thrown carriage refusal fails the **whole block**.
    // While the producer named a tier, a single legal ~14.3 KB output — far
    // under `maxLedgerOutputPreimageBytes` — was enough to do that.
    //
    // So this row asserts the plain thing the narrowing broke: the trace builds.
    // It deliberately uses no resolver and no L1 context, because the block-build
    // caller has none and never will.
    const itemBytes = MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes;
    const trace = await buildTraceWithOutputs([
      makeExactSizeOutputItem(itemBytes),
    ]);
    expect(trace.verdict).toBe("accepted");

    // Field 2's preimage is the whole point: §8.4 puts these bytes at tier 3,
    // and the producer neither knows nor cares.
    const expectedPreimageBytes = encodeMidgardFieldPreimageV1([
      Buffer.alloc(itemBytes),
    ]).length;
    expect(selectMidgardFieldCarriageTierV1(expectedPreimageBytes)).toBe(
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
      buildValidationOneStepArgumentV1({ trace, stateIndex });
    } catch (error) {
      thrown = error;
    }
    expect(thrown).toBeInstanceOf(
      ValidationMachineCarriageResolutionRequiredErrorV1,
    );
    const refusal =
      thrown as ValidationMachineCarriageResolutionRequiredErrorV1;
    expect(refusal.fieldIndex).toBe(2);
    expect(refusal.preimageLength).toBe(expectedPreimageBytes);
    expect(refusal.selectedTier).toBe("Certified");
    expect(refusal.maxTier1PreimageBytes).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    // The same step with the submitter's resolver commits without complaint.
    expect(() =>
      buildValidationOneStepArgumentV1({
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
    //     `onchain/aiken/lib/midgard/validation-machine-v1.ak:9189-9192` says in
    //     terms. So the auxiliary stops growing with the output entirely, and
    //     the 14,774 B and 16,384 B probes — the two the closure used to cover
    //     and #597 re-pinned to a refusal — assert a built argument again.
    //
    // (b) **Inside tier 1 the framing bound stays**, because there the carriage
    //     genuinely is the preimage and no O(1) claim is true. It is still the
    //     property C21-STAGE4 protects: carriage plus Plutus-data segment
    //     framing and nothing else — no per-item opening, frontier or sibling
    //     path, and no second copy of the item.
    const largestTier1Item = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 - 4;

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
      const argument = buildValidationOneStepArgumentV1({
        trace,
        stateIndex: step.stateIndex,
        resolveFieldCarriage: carriageResolverForTrace(trace),
      });
      return {
        auxiliaryBytes: argument.auxiliaryCbor.length,
        evidenceBytes: argument.evidenceCbor.length,
        preimageBytes: auxiliary.fieldPreimage.length,
        tier: selectMidgardFieldCarriageTierV1(auxiliary.fieldPreimage.length),
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
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
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
        itemBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
        tier: "Certified" as const,
      },
    ]) {
      const measured = await probe(itemBytes);
      expect(measured.tier).toBe(tier);
      expect(measured.preimageBytes).toBeGreaterThan(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      );
      // O(1): the auxiliary carries indices, never the preimage, so it is a
      // small constant rather than a function of the output.
      expect(measured.auxiliaryBytes).toBeLessThan(128);
      expect(measured.evidenceBytes).toBeLessThan(
        MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
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

    const observeRedeemer = encodeInlineCompleteItemObserveRedeemer({
      auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      inputIndex: 1n,
      outputIndex: 0n,
    });
    const semanticRedeemer = encodeValidationSemanticResolutionRedeemerV1({
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
    // The item-semantic `Verify` is not parsed against the blueprint, and the
    // reason is recorded rather than worked around: #620 reshaped it from
    // `(input_index, output_index, transition, carriage)` to the
    // transition-only `(input_index, output_index, transition)` in the Aiken
    // source and retired the `VerifyReference` arm, leaving `plutus.json`
    // byte-identical because blueprints move once, in the wave's single
    // regeneration (#587's precedent, as at #592/#579). Until that
    // regeneration runs, this row pins the committed blueprint's frozen
    // four-field list; the regeneration flips it to the one-arm
    // ["input_index", "output_index", "transition"].
    // `complete-item-carriage-policy-v1.test.ts` pins the new shape from the
    // Aiken side, and the raw wire pin below holds the emitted bytes
    // meanwhile.
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
      "carriage",
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
      validationOneStepEvidenceHashV1({
        transitionCbor: oneStepArgument.transitionCbor,
        auxiliaryCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
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
