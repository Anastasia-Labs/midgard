import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { buildMidgardCanonicalCekProgramV1 } from "@al-ft/midgard-validation/cek-program";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  BuilderInvariantError,
  CompleteTx,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "../src/index.js";

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";

const fakeProvider: MidgardProvider = {
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 44n, minFeeB: 155381n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 44n,
    minFeeB: 155381n,
    networkId: 255n,
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://test",
    protocolInfoSource: "node",
  }),
};

const zeroFeeProvider: MidgardProvider = {
  ...fakeProvider,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
};

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const enterpriseAddressFor = (privateKey: CML.PrivateKey): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();

const makeUtxo = (
  ref: OutRef,
  assets: Readonly<Record<string, bigint>>,
  outputAddress = address,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(outputAddress, assets),
  });

const makeReferenceUtxo = (ref: OutRef, script: Uint8Array): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(
      address,
      { lovelace: 3_000_000n },
      {
        scriptRef: {
          type: "MidgardV1",
          script: Buffer.from(script).toString("hex"),
        },
      },
    ),
  });

describe("TxBuilder finalization", () => {
  it("completes a simple balanced unsigned native transaction", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([
        makeUtxo(makeOutRef(0x22, 1), { lovelace: 2_000_000n }),
        makeUtxo(makeOutRef(0x11, 0), { lovelace: 1_000_000n }),
      ])
      .addSigner("bb".repeat(28))
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete();

    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      completed.txCbor,
    );
    expect(completed.txId).toEqual(computeMidgardNativeTxIdV1(decoded));
    expect(completed.toHash()).toBe(completed.txIdHex);
    expect(completed.toCBOR()).toBe(completed.txHex);
    expect(completed.toJSON()).toMatchObject({
      txId: completed.txIdHex,
      txCbor: completed.txHex,
    });
    expect(completed.metadata).toMatchObject({
      fee: 0n,
      inputCount: 2,
      outputCount: 2,
      requiredSignerCount: 1,
    });
  });

  it("applies setMinFee as an auditable fee floor", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .setMinFee(10n)
      .collectFrom([makeUtxo(makeOutRef(0x10), { lovelace: 1_000_010n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    expect(completed.metadata.fee).toBe(10n);
  });

  it("requires exact material for historical reference-script envelopes", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const canonical = buildMidgardCanonicalCekProgramV1(
      Buffer.from("010100200101", "hex"),
    );
    const spend = makeUtxo(makeOutRef(0x31), { lovelace: 2_000_000n });
    const reference = makeReferenceUtxo(
      makeOutRef(0x32),
      canonical.envelopeCbor,
    );
    const builder = midgard
      .newTx()
      .collectFrom([spend])
      .readFrom([reference])
      .pay.ToAddress(address, { lovelace: 2_000_000n });

    await expect(builder.complete({ fee: 0n })).rejects.toThrow(
      /Incomplete or mismatched CEK program material/u,
    );

    const material = [...canonical.material.values()];
    const completed = await builder.complete({
      fee: 0n,
      programMaterial: material,
    });
    expect(completed.programMaterial).toEqual(
      [...material].sort((left, right) =>
        Buffer.compare(Buffer.from(left.root), Buffer.from(right.root)),
      ),
    );

    const corrupted = material.map((entry, index) =>
      index === 0
        ? { ...entry, preimage: Buffer.concat([entry.preimage, Buffer.of(0)]) }
        : entry,
    );
    await expect(
      builder.complete({ fee: 0n, programMaterial: corrupted }),
    ).rejects.toThrow(/Invalid canonical CEK program material/u);

    const rawReference = makeReferenceUtxo(
      makeOutRef(0x33),
      Buffer.from("010100200101", "hex"),
    );
    await expect(
      midgard
        .newTx()
        .collectFrom([spend])
        .readFrom([rawReference])
        .pay.ToAddress(address, { lovelace: 2_000_000n })
        .complete({ fee: 0n, programMaterial: material }),
    ).rejects.toThrow(
      /V1 reference script must contain a canonical CEK program envelope/u,
    );
    const importedRaw = midgard.fromTx(completed.txHex, {
      resolvedSpendInputs: [spend],
      resolvedReferenceInputs: [reference],
      programMaterial: material,
    });
    expect(importedRaw.txHex).toBe(completed.txHex);
    const detached = new CompleteTx(completed.tx, completed.metadata, {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      networkId: 0n,
      programMaterial: material,
      resolvedReferenceOutputsByOutRef: new Map([
        [
          Buffer.from(outRefToCbor(reference)).toString("hex"),
          Buffer.from(reference.cbor!.output!),
        ],
      ]),
    });
    expect(() => midgard.fromTx(detached)).toThrow(
      /Missing resolved reference input/u,
    );
    expect(
      midgard.fromTx(detached, {
        resolvedSpendInputs: [spend],
        resolvedReferenceInputs: [reference],
      }).txHex,
    ).toBe(completed.txHex);
    expect(midgard.fromTx(completed).txHex).toBe(completed.txHex);
    const conflictingReference = makeUtxo(makeOutRef(0x32), {
      lovelace: 4_000_000n,
    });
    expect(() =>
      midgard.fromTx(completed, {
        resolvedSpendInputs: [spend],
        resolvedReferenceInputs: [conflictingReference],
      }),
    ).toThrow(/Conflicting resolved reference inputs/u);

    expect(() =>
      midgard.fromTx(completed.txHex, {
        resolvedSpendInputs: [spend],
        programMaterial: material,
      }),
    ).toThrow(/Missing resolved reference input/u);

    const wrongReference = makeReferenceUtxo(
      makeOutRef(0x34),
      canonical.envelopeCbor,
    );
    expect(() =>
      midgard.fromTx(completed.txHex, {
        resolvedSpendInputs: [spend],
        resolvedReferenceInputs: [wrongReference],
        programMaterial: material,
      }),
    ).toThrow(/Unexpected resolved reference input/u);

    const corruptedRawMaterial = material.map((entry, index) =>
      index === 0
        ? { ...entry, preimage: Buffer.concat([entry.preimage, Buffer.of(0)]) }
        : entry,
    );
    expect(() =>
      midgard.fromTx(completed.txHex, {
        resolvedSpendInputs: [spend],
        resolvedReferenceInputs: [reference],
        programMaterial: corruptedRawMaterial,
      }),
    ).toThrow(/Invalid canonical CEK program material/u);

    const signer = CML.PrivateKey.generate_ed25519();
    const signerAddress = enterpriseAddressFor(signer);
    const signerSpend = makeUtxo(
      makeOutRef(0x37),
      { lovelace: 2_000_000n },
      signerAddress,
    );
    const signable = await midgard
      .newTx()
      .collectFrom([signerSpend])
      .readFrom([reference])
      .addSigner(signer.to_public().hash().to_hex())
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete({ fee: 0n, programMaterial: material });
    const assembled = signable.assemble(
      await signable.sign.withPrivateKey(signer).partial(),
    );
    expect(assembled).toBeInstanceOf(CompleteTx);
    if (assembled instanceof CompleteTx) {
      expect(
        midgard.fromTx(assembled, { resolvedSpendInputs: [signerSpend] }).txHex,
      ).toBe(assembled.txHex);
    }
  });

  it("requires exact resolution for native reference inputs", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x35), { lovelace: 2_000_000n });
    const reference = makeUtxo(makeOutRef(0x36), { lovelace: 3_000_000n });
    const completed = await midgard
      .newTx()
      .collectFrom([spend])
      .readFrom([reference])
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete({ fee: 0n });

    expect(completed.programMaterial).toEqual([]);
    expect(() =>
      midgard.fromTx(completed.txHex, { resolvedSpendInputs: [spend] }),
    ).toThrow(/Missing resolved reference input/u);
    expect(() =>
      midgard.fromTx(completed.txHex, {
        partial: true,
        resolvedSpendInputs: [spend],
      }),
    ).toThrow(/Missing resolved reference input/u);

    const imported = midgard.fromTx(completed.txHex, {
      resolvedSpendInputs: [spend],
      resolvedReferenceInputs: [reference],
    });
    expect(imported.programMaterial).toEqual([]);
    const partial = midgard.fromTx(completed.txHex, {
      partial: true,
      resolvedSpendInputs: [spend],
      resolvedReferenceInputs: [reference],
    });
    expect("submit" in partial).toBe(false);
  });

  it("rejects metadata-only non-native references from both metadata ingress APIs", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x34), { lovelace: 2_000_000n });
    const reference = makeUtxo(makeOutRef(0x35), { lovelace: 3_000_000n });
    const metadata = {
      ...makeOutRef(0x35),
      language: "MidgardV1" as const,
      scriptHash: "44".repeat(28),
    };
    const makeBuilder = () =>
      midgard
        .newTx()
        .collectFrom([spend])
        .readFrom([reference])
        .observe(metadata.scriptHash, {
          data: CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
        })
        .pay.ToAddress(address, { lovelace: 2_000_000n });

    const builders = [
      midgard
        .newTx()
        .collectFrom([spend])
        .readFrom([reference], { trustedReferenceScripts: [metadata] })
        .observe(metadata.scriptHash, {
          data: CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
        })
        .pay.ToAddress(address, { lovelace: 2_000_000n }),
      makeBuilder().attach.ReferenceScriptMetadata(metadata),
    ];

    for (const builder of builders) {
      await expect(builder.complete({ fee: 0n })).rejects.toMatchObject({
        name: "BuilderInvariantError",
        message: expect.stringContaining(
          "Metadata-only non-native reference scripts require",
        ),
        detail: expect.stringContaining(
          `reference:${metadata.txHash}#${metadata.outputIndex}`,
        ),
      });
    }
  });

  it("preserves metadata-only NativeCardano reference inputs", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const spend = makeUtxo(makeOutRef(0x36), { lovelace: 2_000_000n });
    const reference = makeUtxo(makeOutRef(0x37), { lovelace: 3_000_000n });
    const metadata = {
      ...makeOutRef(0x37),
      language: "NativeCardano" as const,
      scriptHash: "55".repeat(28),
    };

    const completed = await midgard
      .newTx()
      .collectFrom([spend])
      .readFrom([reference], { trustedReferenceScripts: [metadata] })
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete({ fee: 0n });

    expect(completed.metadata.referenceInputCount).toBe(1);
    expect(completed.programMaterial).toEqual([]);
  });

  it("materializes canonical hashes, empty buckets, and default sentinels", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(completed.txCbor);
    const witnessCompact = deriveMidgardNativeTxWitnessSetCompactV1(
      tx.witnessSet,
    );
    // §4: one plain `blake2b_256` per field over its §5.1 preimage bytes. The
    // field index is not an argument because it is not in the hash input — field
    // identity is positional, carried by the compact slot being compared.
    const fieldCommitment = (_fieldIndex: number, preimageCbor: Uint8Array) =>
      midgardFieldCommitmentV1(preimageCbor);

    expect(tx.compact.transactionBody.spendInputsHash).toEqual(
      fieldCommitment(0, tx.body.spendInputsPreimageCbor),
    );
    expect(tx.compact.transactionBody.referenceInputsHash).toEqual(
      fieldCommitment(1, EMPTY_CBOR_LIST),
    );
    expect(tx.compact.transactionBody.requiredObserversHash).toEqual(
      fieldCommitment(3, EMPTY_CBOR_LIST),
    );
    expect(tx.compact.transactionBody.requiredSignersHash).toEqual(
      fieldCommitment(4, EMPTY_CBOR_LIST),
    );
    expect(tx.compact.transactionBody.mintHash).toEqual(
      fieldCommitment(5, EMPTY_CBOR_LIST),
    );
    expect(tx.body.scriptIntegrityHash).toEqual(EMPTY_NULL_ROOT);
    expect(tx.body.auxiliaryDataHash).toEqual(EMPTY_NULL_ROOT);
    expect(witnessCompact.addrTxWitsHash).toEqual(
      fieldCommitment(7, EMPTY_CBOR_LIST),
    );
    expect(witnessCompact.scriptTxWitsHash).toEqual(
      fieldCommitment(6, EMPTY_CBOR_LIST),
    );
    expect(witnessCompact.redeemerTxWitsHash).toEqual(
      fieldCommitment(8, EMPTY_CBOR_LIST),
    );
    // §4 is plain hashing over the preimage bytes, so the committed field hash
    // *is* `blake2b_256` of the preimage. Under the retired counted scheme this
    // was deliberately unequal — a domain-tagged Merkle root over decomposed
    // items — and the inequality was what the assertion pinned.
    expect(tx.compact.transactionBody.spendInputsHash).toEqual(
      computeHash32(tx.body.spendInputsPreimageCbor),
    );
    expect(tx.body.validityIntervalStart).toBe(MIDGARD_POSIX_TIME_NONE);
    expect(tx.body.validityIntervalEnd).toBe(MIDGARD_POSIX_TIME_NONE);
  });

  it("sorts spend inputs while preserving authored output order", async () => {
    const midgard = await LucidMidgard.new(fakeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([
        makeUtxo(makeOutRef(0x22, 1), { lovelace: 2_000_000n }),
        makeUtxo(makeOutRef(0x11, 0), { lovelace: 1_000_000n }),
      ])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete();
    const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(completed.txCbor);

    const spendInputs = decodeMidgardNativeByteListPreimage(
      tx.body.spendInputsPreimageCbor,
    ).map((bytes) => CML.TransactionInput.from_cbor_bytes(bytes));
    expect(
      spendInputs.map(
        (input) =>
          `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      ),
    ).toEqual([`${"11".repeat(32)}#0`, `${"22".repeat(32)}#1`]);

    const outputs = decodeMidgardNativeByteListPreimage(
      tx.body.outputsPreimageCbor,
    );
    expect(outputs).toHaveLength(2);
    expect(
      outputs[0]?.equals(
        encodeMidgardTxOutput(address, { lovelace: 1_000_000n }),
      ),
    ).toBe(true);
    expect(
      outputs[1]?.equals(
        encodeMidgardTxOutput(address, { lovelace: 2_000_000n }),
      ),
    ).toBe(true);
  });

  it("rejects unbalanced explicit completion before producing bytes", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);

    await expect(
      midgard
        .newTx()
        .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 2_000_000n })])
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete(),
    ).rejects.toThrow(BuilderInvariantError);
  });

  it("runs shared Phase A local validation during completion", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const balanced = midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n });

    const completed = await balanced.complete({ localValidation: "phase-a" });

    expect(completed.metadata.localValidation).toMatchObject({
      phase: "phase-a",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
  });

  it("runs explicit local preflight from completed transactions without marking final acceptance", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    const report = await completed.validate("phase-a");
    const programReport = await Effect.runPromise(
      completed.validateProgram("phase-a"),
    );
    const safeReport = await completed.validateSafe("phase-a");

    expect(report).toMatchObject({
      phase: "phase-a",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
    expect(programReport.acceptedTxIds).toEqual([completed.txIdHex]);
    expect(safeReport).toMatchObject({
      ok: true,
      value: { phase: "phase-a", acceptedTxIds: [completed.txIdHex] },
    });
    expect(completed.metadata.localValidation).toBeUndefined();
  });

  it("requires explicit pre-state for Phase B local validation", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });

    await expect(
      midgard
        .newTx()
        .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .complete({ localValidation: "phase-b" }),
    ).rejects.toThrow(
      'complete({ localValidation: "phase-b" }) requires localPreState',
    );
  });

  it("requires explicit pre-state for Phase B local preflight", async () => {
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    await expect(completed.validate("phase-b")).rejects.toThrow(
      'validate("phase-b") requires localPreState',
    );
    await expect(completed.validateSafe("phase-b")).resolves.toMatchObject({
      ok: false,
      error: { name: "BuilderInvariantError" },
    });
  });

  it("runs shared Phase B local validation against explicit pre-state", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const inputAddress = enterpriseAddressFor(privateKey);
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = decodeMidgardUtxo({
      outRef: makeOutRef(0x11),
      outRefCbor: outRefToCbor(makeOutRef(0x11)),
      outputCbor: encodeMidgardTxOutput(inputAddress, {
        lovelace: 1_000_000n,
      }),
    });
    const inputOutRefCbor = Buffer.from(input.cbor!.outRef!);
    const inputOutputCbor = Buffer.from(input.cbor!.output!);
    const unsigned = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();
    const completed = await unsigned.sign.withPrivateKey(privateKey).complete();
    const report = await completed.validate("phase-b", {
      localPreState: new Map([
        [inputOutRefCbor.toString("hex"), inputOutputCbor],
      ]),
    });

    expect(report).toMatchObject({
      phase: "phase-b",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
      statePatch: {
        deletedOutRefs: [inputOutRefCbor.toString("hex")],
      },
    });
  });

  it("runs explicit Phase B local preflight against shared validator pre-state", async () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const inputAddress = enterpriseAddressFor(privateKey);
    const midgard = await LucidMidgard.new(zeroFeeProvider, {
      network: "Preview",
      networkId: 0,
    });
    const input = decodeMidgardUtxo({
      outRef: makeOutRef(0x12),
      outRefCbor: outRefToCbor(makeOutRef(0x12)),
      outputCbor: encodeMidgardTxOutput(inputAddress, {
        lovelace: 1_000_000n,
      }),
    });
    const inputOutRefCbor = Buffer.from(input.cbor!.outRef!);
    const inputOutputCbor = Buffer.from(input.cbor!.output!);
    const unsigned = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();
    const completed = await unsigned.sign.withPrivateKey(privateKey).complete();

    const report = await completed.validate("phase-b", {
      localPreState: new Map([
        [inputOutRefCbor.toString("hex"), inputOutputCbor],
      ]),
    });

    expect(report).toMatchObject({
      phase: "phase-b",
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
      preStateSource: "explicit",
      preStateAuthoritative: false,
      statePatch: {
        deletedOutRefs: [inputOutRefCbor.toString("hex")],
      },
    });
  });

  it("exposes completed transaction buffers as immutable snapshots", async () => {
    const midgard = await LucidMidgard.new(fakeProvider);
    const completed = await midgard
      .newTx()
      .collectFrom([makeUtxo(makeOutRef(0x11), { lovelace: 1_000_000n })])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .complete();

    const txId = completed.txId;
    txId[0] ^= 0xff;
    expect(completed.txId.toString("hex")).toBe(completed.txIdHex);

    const txCbor = completed.txCbor;
    txCbor[0] ^= 0xff;
    expect(completed.txCbor.toString("hex")).toBe(completed.txHex);

    const tx = completed.tx;
    tx.body.outputsPreimageCbor[0] ^= 0xff;
    expect(completed.tx.body.outputsPreimageCbor.toString("hex")).toBe(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.from(completed.txHex, "hex"),
      ).body.outputsPreimageCbor.toString("hex"),
    );
  });
});
