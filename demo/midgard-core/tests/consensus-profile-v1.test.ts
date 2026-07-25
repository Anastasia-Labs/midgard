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
      daRuntimeManifestSchemaVersion:
        "midgard-da-libp2p-runtime-manifest-v1",
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
    ).toBeGreaterThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
    ).toBeGreaterThan(51_110);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxOutputValueCborBytes).toBe(5_000);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramNodeCount).toBe(
      1_597_819,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramEnvelopeBytes).toBe(50);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxScriptEnvelopeResolverArgumentsBytes,
    ).toBe(7_546);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekProgramMaterialBytes).toBe(
      67_108_422,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekBlobChunkBytes).toBe(4_095);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxCekBuiltinTag).toBe(86);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxCekDirectBlsMillerLoopLeaves,
    ).toBe(10);
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
      MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
    ).toBeGreaterThan(8 * 1024);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldPublicationUnsignedTransactionBytes,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS
        .maxFieldChunkReceiptPublicationMemoryUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits,
    );
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS
        .maxFieldChunkReceiptPublicationCpuUnits,
    ).toBeLessThan(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits);
    expect(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS
        .canonicalReceiptOrderVerificationMemoryUnits,
    ).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxValidationBisectionRounds).toBe(
      32,
    );
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxValidationMachineStepCount).toBe(
      0xffff_ffff,
    );
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.validationDisputeResponseWindowMs,
    ).toBe(300_000);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.minValidationDisputeMaturityMs,
    ).toBe(39_600_000);
    expect(MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs).toBe(
      604_800_000,
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
    const roundTrip = JSON.parse(
      JSON.stringify(MIDGARD_CONSENSUS_PROFILE_V1),
    );
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
    expect(Object.isFrozen(MIDGARD_CONSENSUS_PROFILE_V1.features)).toBe(
      true,
    );
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
