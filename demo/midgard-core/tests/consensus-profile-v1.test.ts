import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertMidgardConsensusV1ReleaseReady,
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
} from "../src/index.js";

describe("canonical V1 consensus profile", () => {
  it("pins the indivisible V1 version tuple", () => {
    expect(MIDGARD_CONSENSUS_PROFILE_V1).toMatchObject({
      profileId: "midgard-consensus-v1",
      protocolVersion: 1,
      nativeTransactionVersion: 1,
      nativeTransactionProofSourceVersion: 1,
      transitionStepSchemaVersion: 1,
      headerSchemaVersion: 1,
      stateQueueSchemaVersion: 1,
      transactionOrderSchemaVersion: 1,
      transactionFieldPublicationSchemaVersion: 1,
      forcedTransactionJournalVersion: 1,
      daPayloadVersion: 1,
      daEnvelopeVersion: 1,
      daTransportProtocolVersion: 1,
      daRuntimeManifestSchemaVersion: "midgard-da-libp2p-runtime-manifest-v1",
      validationMachineVersion: 1,
      validationTraceDescriptorVersion: 1,
      validationDisputeVersion: 1,
      cekProgramEnvelopeVersion: 1,
      cekValueSchemaVersion: 1,
      cekProgramMaterialVersion: 1,
      cekProgramMaterialSidecarVersion: 1,
      proofSubmissionEnvelopeVersion: 1,
      scriptProofSchemaVersion: 1,
      ledgerOutputSchemaVersion: 1,
      mpfProofSchemaVersion: 1,
      deploymentManifestSchemaVersion: "midgard-deployment-manifest-v1",
      protocolInfoApiVersion: 1,
    });
    expect(MIDGARD_CONSENSUS_PROFILE_V1_DIGEST).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxCekBlobChunkBytes +
        MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionFieldProofOverheadBytes,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxCekBlobChunkBytes +
        MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerMembershipProofOverheadBytes,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionAggregateFieldBytes,
    ).toBeGreaterThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes).toBeGreaterThan(
      51_110,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxOutputValueCborBytes).toBe(5_000);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramNodeCount).toBe(1_597_819);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramEnvelopeBytes).toBe(50);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxScriptEnvelopeResolverArgumentsBytes,
    ).toBe(7_546);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramMaterialBytes).toBe(
      67_108_418,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekBlobChunkBytes).toBe(4_095);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekBuiltinTag).toBe(86);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekDirectBlsMillerLoopLeaves).toBe(
      10,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxBlsFinalBuiltinTransitionCpuUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxBlsFinalBuiltinTransitionMemoryUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits * 0.8,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.secpNonResidueFailureTransitionCpuUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.secpNonResidueFailureTransitionMemoryUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits * 0.8,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.firstRejectedBlsFinalVerificationCpuUnits,
    ).toBeGreaterThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8,
    );
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedTransactionExecutionMemoryUnits,
    ).toBe(16_500_000);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedTransactionExecutionCpuUnits,
    ).toBe(10_000_000_000);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes).toBeGreaterThan(
      8 * 1024,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldPublicationUnsignedTransactionBytes,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldChunkReceiptPublicationMemoryUnits,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldChunkReceiptPublicationCpuUnits,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.canonicalReceiptOrderVerificationMemoryUnits,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxValidationBisectionRounds).toBe(32);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount).toBe(
      0xffff_ffff,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.validationDisputeResponseWindowMs).toBe(
      300_000,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.minValidationDisputeMaturityMs).toBe(
      39_600_000,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs).toBe(604_800_000);
  });

  // Regression pin for C21-CORE-ENVELOPE.
  //
  // `maxReliableDirectCompleteItemBytes` is the single value that decides
  // complete-item carriage: `selectValidationCompleteItemCarriageV1` carries an
  // item at or below it DIRECTLY inside the proof redeemer, and anything above
  // it by reference to a published proof-item UTxO. Two other numbers in this
  // repository measure a DIFFERENT transaction shape and must never be bound
  // here:
  //
  //   14,676 - `scripts/measure-validation-proof-item-envelope.mjs`, whose
  //            "direct" route sources the validator from a reference input and
  //            embeds no script witness at all (13,998 on the retired
  //            counted-shape basis the script modelled before #597).
  //   13,282 - the single-transaction semantic-proof frontier recorded in
  //            `docs/exec-plans/evidence/necessity/`, the same by-reference
  //            basis measured on a complete signed transaction.
  //
  // Since the #617 reference-script wiring (#597 ruling a; commits dce643b0 +
  // 0a074421, landing on the branch as cherry-picks) the deployed direct
  // route also sources every validator by reference, and since Option B
  // (#620) the item preimage rides the OBSERVE door rather than the
  // authenticate redeemer. The frontier is still measured end-to-end through
  // the production submitter's five-stage lifecycle, whose dispute-thread
  // continuation, prepare-selected step, and protocol framing the two
  // single-transaction models above omit; the re-pinned reliable frontier
  // (13,522) now sits between them. Binding either number here would replace
  // an end-to-end measurement with a single-transaction model.
  it("pins the direct complete-item carriage frontier in both directions", () => {
    const reserve =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes;
    const budget =
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes - reserve;
    const frontier =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;

    // Anchored to the deployed five-stage measurement, OBSERVE limiting.
    // Lane-level re-pin 2026-08-23 at the #617 wave sign-off (owner ruling
    // (b) on the #622 sign-off table, 2026-08-22): the reserve cost steer
    // remains 13,522. Removing the claim-registry witness added 56 bytes of
    // transaction headroom and moved the measured exact frontier from 14,004
    // to 14,058 after the larger item crossed a CBOR framing width.
    expect(reserve).toBe(512);
    expect(budget).toBe(15_872);
    expect(frontier).toBe(13_522);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxExactDirectCompleteItemBytes,
    ).toBe(14_058);

    // Mirrors `selectValidationCompleteItemCarriageV1`. The production selector
    // lives in `@al-ft/midgard-fault-proofs` (importing it here would invert the
    // package dependency), and the validation carriage-policy suite pins its
    // source to exactly this constant with these `<=` semantics. Evaluating the
    // rule at literal byte counts therefore pins the deployed boundary: any
    // rebind of the constant flips one of these four answers.
    const carriage = (itemBytes: number): "direct" | "reference" =>
      itemBytes <= frontier ? "direct" : "reference";

    // Direction 1: the frontier item is carried directly.
    expect(carriage(13_522)).toBe("direct");
    // Direction 2: one byte over is NOT. Since Option B (#620) this boundary
    // steers cost rather than soundness — #622 measured item 13,523
    // completing to award past it, "a cost line, not a cliff" — but the
    // builder still refuses to widen it on its own: the pre-sign projection
    // and the envelope gate are the operative guards, proven live at 14,059.
    expect(carriage(13_523)).toBe("reference");
    // The retired single-transaction by-reference frontiers (13,282 from the
    // necessity evidence, 13,998 from the measurement script's counted-shape
    // model) no longer straddle the boundary the same way: the re-pinned
    // five-stage frontier now sits above 13,282 — exactly the band #622
    // measured as paying an unnecessary ~15K-byte publication before the
    // rebind — and still below 13,998.
    expect(carriage(13_282)).toBe("direct");
    expect(carriage(13_998)).toBe("reference");
    expect(frontier).toBeGreaterThan(13_282);
    expect(frontier).toBeLessThan(13_998);

    // The owner-signed cost steer intentionally retained the 56 bytes freed by
    // claim-registry removal instead of widening the direct route.
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemProofTransactionBytes,
    ).toBe(budget - 56);
    expect(
      Math.max(
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemAuthenticationTransactionBytes,
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemObservationTransactionBytes,
      ),
    ).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemProofTransactionBytes,
    );
    // Observation, not authentication, is the stage that governs the bound
    // since Option B (#620) made the canonical-decode item semantic
    // transition-only: the item preimage left the authenticate redeemer, so
    // authenticate became item-size-independent (2,600 bytes at every
    // measured item) and the observe door became the stage that grows with
    // the item. Re-pinned at the #617 wave sign-off from the #622 measured
    // table, under the owner's 2026-08-22 ruling (b).
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemObservationTransactionBytes,
    ).toBeGreaterThan(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemAuthenticationTransactionBytes,
    );

    // The zero-reserve frontier may exceed the reliable one by the reserve
    // plus the deliberately retained 56-byte claim-registry headroom.
    const slack =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxExactDirectCompleteItemBytes -
      frontier;
    expect(slack).toBeGreaterThan(0);
    expect(slack).toBeLessThanOrEqual(reserve + 56);

    // Carrying the whole item preimage inline in the redeemer is what makes
    // direct carriage expensive: the identical proof resolved by reference to
    // a published proof-item UTxO costs a fraction of the transaction, which
    // is why the reference route exists at all.
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.referenceCompleteItemProofTransactionBytes,
    ).toBeLessThan(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemProofTransactionBytes,
    );

    // Reference carriage must stay reachable: a non-empty band of items sits
    // above the direct frontier and still publishes in one transaction, and
    // the selector's hard cap stays at or below every publication measurement.
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
    ).toBeGreaterThan(frontier);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
    ).toBeLessThanOrEqual(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationBytes,
    );
  });

  it("fits a worst-case one-step redeemer inside a concrete Conway proof transaction", () => {
    const hash = (fill: number): Buffer => Buffer.alloc(32, fill);
    const input = (fill: number): CML.TransactionInput =>
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(hash(fill)),
        0n,
      );
    const inputs = CML.TransactionInputList.new();
    inputs.add(input(0));
    const outputs = CML.TransactionOutputList.new();
    const signingKey = CML.PrivateKey.from_normal_bytes(hash(4));
    const paymentKeyHash = signingKey.to_public().hash();
    const address = CML.Address.from_raw_bytes(
      Buffer.concat([
        Buffer.from([0x60]),
        Buffer.from(paymentKeyHash.to_raw_bytes()),
      ]),
    );
    outputs.add(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(2_000_000n),
        CML.DatumOption.new_datum(
          CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
        ),
        undefined,
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    const collateral = CML.TransactionInputList.new();
    collateral.add(input(1));
    body.set_collateral_inputs(collateral);
    const references = CML.TransactionInputList.new();
    references.add(input(2));
    body.set_reference_inputs(references);
    body.set_total_collateral(200_000n);
    body.set_script_data_hash(CML.ScriptDataHash.from_raw_bytes(hash(3)));
    body.set_ttl(0x7fff_ffff_ffff_ffffn);
    const requiredSigners = CML.Ed25519KeyHashList.new();
    requiredSigners.add(paymentKeyHash);
    body.set_required_signers(requiredSigners);

    // This byte-string datum serializes larger than the largest measured
    // resolver argument (14,082 bytes), so it conservatively exercises the
    // same outer transaction framing without duplicating Aiken's Data codec.
    const oversizedArgument = CML.PlutusData.new_bytes(Buffer.alloc(14_103));
    const redeemerMap = CML.MapRedeemerKeyToRedeemerVal.new();
    redeemerMap.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Spend, 0n),
      CML.RedeemerVal.new(
        oversizedArgument,
        CML.ExUnits.new(14_000_000n, 10_000_000_000n),
      ),
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    witnessSet.set_redeemers(
      CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemerMap),
    );
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(
      CML.Vkeywitness.new(signingKey.to_public(), signingKey.sign(hash(5))),
    );
    witnessSet.set_vkeywitnesses(vkeys);
    const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
    const argumentBytes = oversizedArgument.to_cbor_bytes().length;
    const transactionBytes = transaction.to_cbor_bytes().length;
    expect(argumentBytes).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.concreteConwayProofArgumentBytes,
    );
    expect(transactionBytes - argumentBytes).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.concreteConwayProofTransactionFramingBytes,
    );
    expect(transactionBytes).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.concreteConwayProofTransactionBytes,
    );
    expect(transactionBytes).toBeLessThan(16 * 1024);
  });

  it("retains the requested mint, script, observer, protected-output, and forced surfaces", () => {
    expect(MIDGARD_CONSENSUS_PROFILE_V1.features).toEqual(
      expect.arrayContaining([
        "mint_burn",
        "reference_inputs",
        "plutus_v3_scripts",
        "midgard_v1_scripts",
        "redeemers",
        "reference_scripts",
        "script_payment_credentials",
        "protected_outputs",
        "required_observers",
        "valid_forced_transactions",
        "invalid_forced_transactions",
      ]),
    );
    expect(MIDGARD_CONSENSUS_PROFILE_V1.requiredProofFamilies).toEqual(
      expect.arrayContaining([
        "validation-machine-one-step",
        "validation-dispute-timeout",
        "forced-transaction-verdict-mismatch",
      ]),
    );
  });

  it("accepts only the exact compiled V1 profile", () => {
    const roundTrip = JSON.parse(JSON.stringify(MIDGARD_CONSENSUS_PROFILE_V1));
    expect(isMidgardConsensusProfileV1(roundTrip)).toBe(true);
    expect(
      isMidgardConsensusProfileV1({
        ...roundTrip,
        nativeTransactionVersion: 2,
      }),
    ).toBe(false);
    expect(
      isMidgardConsensusProfileV1({
        ...roundTrip,
        ignoredByJson: undefined,
      }),
    ).toBe(false);

    expect(
      isMidgardConsensusProfileV1({
        ...MIDGARD_CONSENSUS_PROFILE_V1,
        validationMachineVersion: 9,
      }),
    ).toBe(false);
  });

  it("deep-freezes consensus arrays and bounds", () => {
    expect(Object.isFrozen(MIDGARD_CONSENSUS_PROFILE_V1)).toBe(true);
    expect(Object.isFrozen(MIDGARD_CONSENSUS_PROFILE_V1.features)).toBe(true);
    expect(
      Object.isFrozen(MIDGARD_CONSENSUS_PROFILE_V1.requiredProofFamilies),
    ).toBe(true);
    expect(Object.isFrozen(MIDGARD_CONSENSUS_PROFILE_V1.limits)).toBe(true);
  });

  it("fails closed until validator-hash-bound L1 release evidence is compiled in", () => {
    expect(MIDGARD_V1_RELEASE_EVIDENCE_DIGEST).toBeNull();
    expect(assertMidgardConsensusV1ReleaseReady).toThrow(/not activated/u);
  });
});
