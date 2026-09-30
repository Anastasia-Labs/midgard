import {
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import { computeHash32 } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  LedgerColumns,
  phaseAAddressCacheStats,
  phaseAPublicKeyCacheStats,
  RejectCodes,
  resetPhaseAAddressCache,
  resetPhaseAPublicKeyCache,
  runPhaseAValidation,
  validatePhaseASingle,
} from "../src/index.js";
import {
  addressAtRawNetworkNibble,
  expectSinglePhaseAAcceptance,
  expectSinglePhaseARejection,
  phaseAConfig,
  runPhaseA,
} from "./phase-a.outref-projection.js";
import {
  EMPTY_CBOR_NULL,
  encodeByteList,
  encodeRecomputedNativeTx,
  makeNativeTx,
  makeOutput,
  makeQueued,
  nativeScriptWitness,
  outRefFromByte,
} from "./validation-fixtures.js";

describe("phase A validation", () => {
  it("reuses address projections and evicts them at the configured bound", async () => {
    const firstKey = CML.PrivateKey.generate_ed25519();
    const secondKey = CML.PrivateKey.generate_ed25519();
    const firstAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(firstKey.to_public().hash()),
    ).to_address();
    const secondAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(secondKey.to_public().hash()),
    ).to_address();
    try {
      resetPhaseAAddressCache(1);
      const first = makeNativeTx({
        outputs: [makeOutput(10n, Buffer.from(firstAddress.to_raw_bytes()))],
      });
      const second = makeNativeTx({
        outputs: [makeOutput(10n, Buffer.from(secondAddress.to_raw_bytes()))],
      });
      await expectSinglePhaseAAcceptance(first);
      await expectSinglePhaseAAcceptance(first);
      expect(phaseAAddressCacheStats()).toMatchObject({
        size: 1,
        hits: 1,
        misses: 1,
        evictions: 0,
      });
      await expectSinglePhaseAAcceptance(second);
      expect(phaseAAddressCacheStats()).toMatchObject({
        size: 1,
        misses: 2,
        evictions: 1,
      });
    } finally {
      resetPhaseAAddressCache();
      firstAddress.free();
      secondAddress.free();
      firstKey.free();
      secondKey.free();
    }
  });

  it("reuses public keys and frees the least-recently-used key on eviction", async () => {
    const firstKey = CML.PrivateKey.generate_ed25519();
    const secondKey = CML.PrivateKey.generate_ed25519();
    try {
      resetPhaseAPublicKeyCache(1);
      const first = makeNativeTx({ privateKey: firstKey });
      const second = makeNativeTx({ privateKey: secondKey });
      await expectSinglePhaseAAcceptance(first);
      await expectSinglePhaseAAcceptance(first);
      expect(phaseAPublicKeyCacheStats()).toMatchObject({
        size: 1,
        hits: 1,
        misses: 1,
        evictions: 0,
      });

      await expectSinglePhaseAAcceptance(second);
      expect(phaseAPublicKeyCacheStats()).toMatchObject({
        size: 1,
        hits: 1,
        misses: 2,
        evictions: 1,
      });
      await expectSinglePhaseAAcceptance(first);
      expect(phaseAPublicKeyCacheStats()).toMatchObject({
        size: 1,
        misses: 3,
        evictions: 2,
      });
    } finally {
      resetPhaseAPublicKeyCache();
      firstKey.free();
      secondKey.free();
    }
  });

  it("keeps the exported single-tx validator identical to the batch reference", async () => {
    const fixture = makeNativeTx();
    const queued = makeQueued(fixture.txId, fixture.txCbor);
    const config = {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    };
    const single = validatePhaseASingle(queued, config);
    const batch = await Effect.runPromise(
      runPhaseAValidation([queued], config),
    );
    expect(
      "ledgerTx" in single ? batch.accepted[0] : batch.rejected[0],
    ).toStrictEqual(single);
  });

  it("keeps the process-local signature verifier outside serializable config", () => {
    const fixture = makeNativeTx({ invalidVkeyWitness: true });
    const queued = makeQueued(fixture.txId, fixture.txCbor);
    const reference = validatePhaseASingle(queued, phaseAConfig);
    expect("ledgerTx" in reference).toBe(false);
    if (!("ledgerTx" in reference)) {
      expect(reference.code).toBe(RejectCodes.InvalidSignature);
    }

    let calls = 0;
    const injected = validatePhaseASingle(queued, phaseAConfig, {
      verifyVKeyWitnessSignature: (txBodyHash, value) => {
        calls += 1;
        expect(txBodyHash).toStrictEqual(fixture.txId);
        expect(value.vkey).toHaveLength(32);
        expect(value.signature).toHaveLength(64);
        return true;
      },
    });
    expect(calls).toBe(1);
    expect("ledgerTx" in injected).toBe(true);
  });
  it("accepts canonical native transactions", async () => {
    const fixture = makeNativeTx();

    const accepted = await expectSinglePhaseAAcceptance(fixture);

    expect(accepted.ledgerTx.txId.equals(fixture.txId)).toBe(true);
    expect(accepted.submission.txCbor.equals(fixture.txCbor)).toBe(true);
  });

  it("materializes Phase B candidate metadata from accepted native transaction CBOR", async () => {
    const spent = outRefFromByte(0x01);
    const output = makeOutput(8n);
    const fixture = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      fee: 2n,
      validityIntervalStart: 10n,
      validityIntervalEnd: 20n,
      networkId: 0n,
    });

    const accepted = await expectSinglePhaseAAcceptance(fixture);

    expect(accepted.graph.spentOutRefHexes).toEqual([spent.toString("hex")]);
    expect(accepted.graph.referenceOutRefHexes).toEqual([]);
    expect(accepted.ledgerTx.fee).toBe(2n);
    expect(accepted.derived.outputSum.lovelace).toBe(8n);
    expect(
      accepted.graph.produced.map((entry) =>
        entry[LedgerColumns.OUTPUT].toString("hex"),
      ),
    ).toEqual([output.toString("hex")]);
  });

  it("rejects malformed canonical CBOR before admission", async () => {
    const result = await runPhaseA([
      makeQueued(Buffer.alloc(32, 0xaa), Buffer.from("80", "hex")),
    ]);

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.CborDeserialization);
  });

  it("rejects queue/native transaction id mismatches", async () => {
    const fixture = makeNativeTx();
    const result = await runPhaseA([
      makeQueued(Buffer.alloc(32, 0xbb), fixture.txCbor),
    ]);

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.TxHashMismatch);
  });

  it("rejects native validity values other than TxIsValid", async () => {
    const fixture = makeNativeTx({ validity: "TxIsInvalid" });
    await expectSinglePhaseARejection(
      fixture,
      RejectCodes.IsValidFalseForbidden,
    );
  });

  it("rejects non-empty auxiliary data hashes", async () => {
    const fixture = makeNativeTx({
      auxiliaryDataHash: computeHash32(Buffer.from("auxiliary-data")),
    });
    await expectSinglePhaseARejection(fixture, RejectCodes.AuxDataForbidden);
  });

  it.each([
    [1, 1],
    [2, 1],
  ])("rejects non-increasing required observer hashes %j", async (...bytes) => {
    const fixture = makeNativeTx({
      requiredObserverItems: bytes.map((byte) => Buffer.alloc(28, byte)),
    });
    await expectSinglePhaseARejection(fixture, RejectCodes.InvalidFieldType);
  });

  it("accepts strictly increasing required observer hashes", async () => {
    await expectSinglePhaseAAcceptance(
      makeNativeTx({
        requiredObserverItems: [Buffer.alloc(28, 1), Buffer.alloc(28, 2)],
      }),
    );
  });

  it("rejects explicit Cardano network ids that do not match configuration", async () => {
    const fixture = makeNativeTx({ networkId: 1n });
    await expectSinglePhaseARejection(fixture, RejectCodes.NetworkIdMismatch);
  });

  it.each([
    { label: "testnet output on mainnet", expected: 1n, rawNibble: 0 },
    { label: "mainnet output on testnet", expected: 0n, rawNibble: 1 },
    {
      label: "foreign unprotected network nibble 2",
      expected: 0n,
      rawNibble: 2,
    },
    {
      label: "foreign protected network nibble 15",
      expected: 0n,
      rawNibble: 15,
    },
  ])("defers $label to the staged output scan", ({ expected, rawNibble }) => {
    const fixture = makeNativeTx({
      outputs: [makeOutput(10n, addressAtRawNetworkNibble(rawNibble))],
    });
    const admitted = validatePhaseASingle(
      makeQueued(fixture.txId, fixture.txCbor),
      {
        ...phaseAConfig,
        expectedNetworkId: expected,
      },
    );
    expect(admitted).toHaveProperty("ledgerTx");
    if ("ledgerTx" in admitted) {
      expect(admitted.derived.expectedNetworkId).toBe(expected);
    }
  });

  it("accepts matching protected output networks", () => {
    const fixture = makeNativeTx({
      outputs: [makeOutput(10n, addressAtRawNetworkNibble(8))],
    });
    expect(
      validatePhaseASingle(
        makeQueued(fixture.txId, fixture.txCbor),
        phaseAConfig,
      ),
    ).toHaveProperty("ledgerTx");
  });

  it("preserves validity rejection priority over a wrong-network output", () => {
    const fixture = makeNativeTx({
      validity: "TxIsInvalid",
      outputs: [makeOutput(10n, addressAtRawNetworkNibble(15))],
    });
    expect(
      validatePhaseASingle(
        makeQueued(fixture.txId, fixture.txCbor),
        phaseAConfig,
      ),
    ).toMatchObject({ code: RejectCodes.IsValidFalseForbidden });
  });

  it("rejects transactions below the configured minimum fee", async () => {
    const fixture = makeNativeTx({ fee: 0n });
    await expectSinglePhaseARejection(fixture, RejectCodes.MinFee, {
      ...phaseAConfig,
      minFeeB: 1n,
    });
  });

  it("rejects empty native spend inputs", async () => {
    const fixture = makeNativeTx({ spendInputs: [] });
    await expectSinglePhaseARejection(fixture, RejectCodes.EmptyInputs);
  });

  it("rejects duplicate spend inputs and spend/reference overlap", async () => {
    const duplicated = outRefFromByte(0x03);
    const duplicateSpend = makeNativeTx({
      spendInputs: [duplicated, duplicated],
    });
    const overlappingReference = makeNativeTx({
      spendInputs: [duplicated],
      referenceInputs: [duplicated],
    });

    const result = await runPhaseA([
      makeQueued(duplicateSpend.txId, duplicateSpend.txCbor),
      makeQueued(overlappingReference.txId, overlappingReference.txCbor),
    ]);

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected.map((rejection) => rejection.code)).toEqual([
      RejectCodes.DuplicateInputInTx,
      RejectCodes.DuplicateInputInTx,
    ]);
  });

  it("rejects malformed native output bytes", async () => {
    const fixture = makeNativeTx({ outputs: [Buffer.from("ff", "hex")] });
    await expectSinglePhaseARejection(fixture, RejectCodes.InvalidOutput);
  });

  it("rejects invalid validity interval bounds", async () => {
    const fixture = makeNativeTx({
      validityIntervalStart: 20n,
      validityIntervalEnd: 10n,
    });
    await expectSinglePhaseARejection(
      fixture,
      RejectCodes.InvalidValidityIntervalFormat,
    );
  });

  it("rejects malformed required signer preimages", async () => {
    const fixture = makeNativeTx({
      requiredSignerItems: [Buffer.alloc(27, 0x04)],
    });
    await expectSinglePhaseARejection(fixture, RejectCodes.InvalidFieldType);
  });

  it("rejects missing required native key witnesses", async () => {
    const fixture = makeNativeTx({
      requiredSignerItems: [Buffer.alloc(28, 0x05)],
    });
    await expectSinglePhaseARejection(
      fixture,
      RejectCodes.MissingRequiredWitness,
    );
  });

  it("rejects invalid native key witness signatures", async () => {
    const fixture = makeNativeTx({ invalidVkeyWitness: true });
    await expectSinglePhaseARejection(fixture, RejectCodes.InvalidSignature);
  });

  it("rejects an unsatisfied native script before Phase B", async () => {
    const fixture = makeNativeTx({
      scriptWitnesses: [
        nativeScriptWitness({
          type: "sig",
          keyHash: Buffer.alloc(28, 0x06),
        }),
      ],
    });
    await expectSinglePhaseARejection(fixture, RejectCodes.NativeScriptInvalid);
  });

  it("rejects malformed redeemer witness preimages as invalid field data", async () => {
    const base = makeNativeTx();
    const fixture = encodeRecomputedNativeTx({
      ...base.tx,
      witnessSet: {
        ...base.tx.witnessSet,
        redeemerTxWitsPreimageCbor: encodeByteList([
          Buffer.from(EMPTY_CBOR_NULL),
        ]),
      },
    });
    await expectSinglePhaseARejection(fixture, RejectCodes.InvalidFieldType);
  });

  it("requires V1 material and rejects a non-canonical profile tuple", async () => {
    const fixture = makeNativeTx();
    const queued = makeQueued(fixture.txId, fixture.txCbor);
    const v1Config = {
      ...phaseAConfig,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    };
    const missing = validatePhaseASingle(
      { ...queued, programMaterialSidecarCbor: undefined },
      v1Config,
    );
    expect(missing).toMatchObject({
      code: RejectCodes.CekProgramMaterial,
    });

    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    const accepted = validatePhaseASingle(
      { ...queued, programMaterialSidecarCbor: sidecar },
      v1Config,
    );
    expect("ledgerTx" in accepted).toBe(true);
    if ("ledgerTx" in accepted) {
      expect(accepted.submission.programMaterialSidecarCbor).toEqual(sidecar);
    }

    const unsupportedProfile = validatePhaseASingle(queued, {
      ...phaseAConfig,
      consensusProfile: {
        ...MIDGARD_CONSENSUS_PROFILE,
        protocolVersion: 2,
      } as unknown as typeof MIDGARD_CONSENSUS_PROFILE,
    });
    expect(unsupportedProfile).toMatchObject({
      code: RejectCodes.TxVersion,
    });
  });

  it("rejects unclaimed material unless reference programs remain unresolved", () => {
    const node = { kind: "error" as const };
    const materialSidecar = encodeMidgardCekProgramMaterialSidecar([
      {
        kind: "term",
        root: hashMidgardCekTermNode(node),
        preimage: encodeMidgardCekTermNode(node),
      },
    ]);
    const withoutReferences = makeNativeTx();
    const rejected = validatePhaseASingle(
      {
        ...makeQueued(withoutReferences.txId, withoutReferences.txCbor),
        programMaterialSidecarCbor: materialSidecar,
      },
      phaseAConfig,
    );
    expect(rejected).toMatchObject({
      code: RejectCodes.CekProgramMaterial,
    });

    const unresolvedReference = makeNativeTx({
      referenceInputs: [outRefFromByte(0x72)],
    });
    const deferred = validatePhaseASingle(
      {
        ...makeQueued(unresolvedReference.txId, unresolvedReference.txCbor),
        programMaterialSidecarCbor: materialSidecar,
      },
      phaseAConfig,
    );
    expect("ledgerTx" in deferred).toBe(true);
  });
});
