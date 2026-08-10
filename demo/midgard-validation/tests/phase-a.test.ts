import {
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  hashMidgardCekTermNodeV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeHash32,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  LedgerColumns,
  midgardOutRefToCbor,
  phaseAAddressCacheStats,
  phaseAPublicKeyCacheStats,
  RejectCodes,
  resetPhaseAAddressCache,
  resetPhaseAPublicKeyCache,
  runPhaseAValidation,
  validatePhaseASingle,
} from "../src/index.js";
import type {
  PhaseAValidatedTx,
  RejectCode,
  RejectedTx,
} from "../src/types.js";
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

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

/**
 * This suite used to assert that `midgardOutRefToCbor` reproduced CML's
 * canonical `TransactionInput` CBOR. That is no longer the contract, and the
 * change is deliberate (#586's owner ruling): the ledger out-ref key is spec
 * §5.3's field-0/1 item, `82 ‖ 58 20 tx_id ‖ 19 index_be16`, a fixed 38 bytes
 * with a non-minimal 3-byte index — the same bytes on-chain `ledger_outref_key`
 * derives. CML minimises the index, so it disagrees for every index below 256,
 * and it is the *disagreement* that has to be pinned now.
 */
describe("outref projection", () => {
  it.each([0n, 23n, 24n, 255n, 256n, 65_535n])(
    "encodes the §5.3 fixed-index ledger key for index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const encoded = midgardOutRefToCbor({ txId, index });

      expect(encoded).toStrictEqual(
        encodeMidgardSpendInputItemV1({ txId, outputIndex: Number(index) }),
      );
      // The fixed width is the point of the encoding: it is what makes the
      // stride-40 arithmetic access in the field-access door sound.
      expect(encoded.length).toBe(38);
      expect(encoded.subarray(0, 3)).toStrictEqual(
        Buffer.from([0x82, 0x58, 0x20]),
      );
      expect(encoded.subarray(3, 35)).toStrictEqual(txId);
      expect(encoded.subarray(35)).toStrictEqual(
        Buffer.from([0x19, Number(index >> 8n), Number(index & 0xffn)]),
      );
    },
  );

  it.each([0n, 23n, 24n, 255n])(
    "deliberately differs from CML's minimal-index CBOR for index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const hash = CML.TransactionHash.from_raw_bytes(txId);
      const input = CML.TransactionInput.new(hash, index);
      try {
        // Below 256 CML spells the index in one or two bytes, so its encoding is
        // 36 or 37 bytes. Accepting it as a ledger key would mean two byte forms
        // for one out-ref — exactly what on-chain `decode_midgard_tx_input_cbor`
        // rejects when it asserts the `0x19` head.
        expect(Buffer.from(input.to_cbor_bytes())).not.toStrictEqual(
          midgardOutRefToCbor({ txId, index }),
        );
      } finally {
        input.free();
        hash.free();
      }
    },
  );

  it.each([256n, 65_535n])(
    "coincides with CML's minimal-index CBOR only where minimal is already uint16, at index %s",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      const hash = CML.TransactionHash.from_raw_bytes(txId);
      const input = CML.TransactionInput.new(hash, index);
      try {
        expect(midgardOutRefToCbor({ txId, index })).toStrictEqual(
          Buffer.from(input.to_cbor_bytes()),
        );
      } finally {
        input.free();
        hash.free();
      }
    },
  );

  it.each([65_536n, 0xffff_ffff_ffff_ffffn])(
    "refuses index %s, which is outside §5.3's uint16 output-index domain",
    (index) => {
      const txId = Buffer.alloc(32, Number(index & 0xffn));
      expect(() => midgardOutRefToCbor({ txId, index })).toThrow();
    },
  );
});

const runPhaseA = (
  queued: Parameters<typeof runPhaseAValidation>[0],
  config = phaseAConfig,
) => Effect.runPromise(runPhaseAValidation(queued, config));

const queuedNativeTx = (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
) => makeQueued(fixture.txId, fixture.txCbor);

const expectSinglePhaseAAcceptance = async (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
): Promise<PhaseAValidatedTx> => {
  const result = await runPhaseA([queuedNativeTx(fixture)]);
  expect(result.rejected).toHaveLength(0);
  expect(result.accepted).toHaveLength(1);
  return result.accepted[0];
};

const expectSinglePhaseARejection = async (
  fixture: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">,
  expectedCode: RejectCode,
  config = phaseAConfig,
): Promise<RejectedTx> => {
  const result = await runPhaseA([queuedNativeTx(fixture)], config);
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0].code).toBe(expectedCode);
  return result.rejected[0];
};

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
    const fixture = makeNativeTx({ validity: "InvalidSignature" });
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

  it("rejects explicit Cardano network ids that do not match configuration", async () => {
    const fixture = makeNativeTx({ networkId: 1n });
    await expectSinglePhaseARejection(fixture, RejectCodes.NetworkIdMismatch);
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
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    };
    const missing = validatePhaseASingle(
      { ...queued, programMaterialSidecarCbor: undefined },
      v1Config,
    );
    expect(missing).toMatchObject({
      code: RejectCodes.CekProgramMaterial,
    });

    const sidecar = encodeMidgardCekProgramMaterialSidecarV1([]);
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
        ...MIDGARD_CONSENSUS_PROFILE_V1,
        protocolVersion: 2,
      } as unknown as typeof MIDGARD_CONSENSUS_PROFILE_V1,
    });
    expect(unsupportedProfile).toMatchObject({
      code: RejectCodes.TxVersion,
    });
  });

  it("rejects unclaimed material unless reference programs remain unresolved", () => {
    const node = { kind: "error" as const };
    const materialSidecar = encodeMidgardCekProgramMaterialSidecarV1([
      {
        kind: "term",
        root: hashMidgardCekTermNodeV1(node),
        preimage: encodeMidgardCekTermNodeV1(node),
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
