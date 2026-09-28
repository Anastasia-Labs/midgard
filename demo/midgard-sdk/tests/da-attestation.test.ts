import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  Constr,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyDaAttestationSignatureWitnesses,
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  castStateQueueNodeToData,
  type DaAttestationBuildError,
  type DaAttestationBuildFailureReason,
  DaAttestationDatum,
  DaAttestationMintRedeemer,
  type DaAttestationReferenceScripts,
  DaAttestationSpendRedeemer,
  type DaAttestationStateQueueTarget,
  daAttestationUnit,
  type DaAttestationUtxo,
  daAvailabilityCommitmentHash,
  type DaAvailabilityParameters,
  type DaBondPoolDatum,
  daBondPoolUnit,
  type DaParamsDatum,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  encodeDaAttestationSignatureWitnesses,
  encodeDaBondPoolDatum,
  encodeLinkedListNodeView,
  incompleteAddDaAttestationSignaturesTxProgram,
  incompleteApplyDaAttestationToStateQueueTxProgram,
  incompleteInitDaAttestationTxProgram,
  LinkedListDatum,
  type LinkedListNodeView,
  type MidgardValidators,
  NO_DA_ATTESTATION,
  signerIndexIsDaAttested,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  StateQueueNode,
  type StateQueueUTxO,
} from "../src/index.js";

const signature = (byte: string): string => byte.repeat(64);
const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitment({
    deploymentIdentity: h28(0x71),
    headerHash,
    payload: Uint8Array.of(1),
    responseGeometry: availabilityResponseGeometry({
      chunkByteLength: 4096,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

type RecordedPayment = {
  readonly address: string;
  /** Absent for `pay.ToAddress`. */
  readonly datum?: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

type Recording = {
  readonly withdrawals: {
    readonly address: string;
    readonly amount: bigint;
    readonly redeemer: unknown;
  }[];
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer: unknown }[];
  readonly mints: { readonly assets: Assets; readonly redeemer: unknown }[];
  readonly payments: RecordedPayment[];
  readonly signerKeys: string[];
  readonly validityRanges: {
    readonly validFrom: number;
    readonly validTo: number;
  }[];
  /** Every `utxosAtWithUnit` query, in order: the apply's pool fetches. */
  readonly unitQueries: { readonly address: string; readonly unit: string }[];
};

/**
 * `chain` is what `utxosAtWithUnit` answers from, and a test may swap its
 * contents between builds: the apply builder must query it on every build.
 */
const makeRecordingLucid = (
  chain: { utxos: readonly UTxO[] } = { utxos: [] },
): {
  readonly lucid: LucidEvolution;
  readonly record: Recording;
} => {
  const record: Recording = {
    withdrawals: [],
    reads: [],
    collects: [],
    mints: [],
    payments: [],
    signerKeys: [],
    validityRanges: [],
    unitQueries: [],
  };
  const lucid = {
    config: () => ({ network: "Custom" }),
    utxosAtWithUnit: async (address: string, unit: string) => {
      record.unitQueries.push({ address, unit });
      return chain.utxos.filter(
        (utxo) => utxo.address === address && (utxo.assets[unit] ?? 0n) > 0n,
      );
    },
    newTx: () => {
      const tx = {
        validFrom: (validFrom: number) => {
          record.validityRanges.push({ validFrom, validTo: Number.NaN });
          return tx;
        },
        validTo: (validTo: number) => {
          const latest = record.validityRanges.at(-1);
          if (latest !== undefined) {
            record.validityRanges[record.validityRanges.length - 1] = {
              validFrom: latest.validFrom,
              validTo,
            };
          }
          return tx;
        },
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer: unknown) => {
          record.collects.push({ inputs, redeemer });
          return tx;
        },
        withdraw: (address: string, amount: bigint, redeemer: unknown) => {
          record.withdrawals.push({ address, amount, redeemer });
          return tx;
        },
        mintAssets: (assets: Assets, redeemer: unknown) => {
          record.mints.push({ assets, redeemer });
          return tx;
        },
        pay: {
          ToContract: (
            address: string,
            datum: RecordedPayment["datum"],
            assets: Assets,
          ) => {
            record.payments.push({ address, datum, assets });
            return tx;
          },
          ToAddress: (address: string, assets: Assets) => {
            record.payments.push({ address, assets });
            return tx;
          },
        },
        addSignerKey: (keyHash: string) => {
          record.signerKeys.push(keyHash);
          return tx;
        },
      };
      return tx;
    },
  } as unknown as LucidEvolution;
  return { lucid, record };
};

const makeUtxo = (
  outputIndex: number,
  assets: Assets = { lovelace: 1n },
  datum: string | null = null,
  address = `addr_test_${outputIndex.toString()}`,
): UTxO =>
  ({
    txHash: outputIndex.toString(16).padStart(64, "0"),
    outputIndex,
    address,
    assets,
    datum,
  }) as UTxO;

const validator = (policyByte: number, address: string) =>
  ({
    policyId: h28(policyByte),
    spendingScriptAddress: address,
    spendingScriptHash: h28(policyByte),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as unknown as MidgardValidators["daAttestation"];

const makeFixture = () => {
  const contracts = {
    daAttestation: validator(0xaa, "addr_da_attestation"),
    stateQueue: validator(0xbb, "addr_state_queue"),
    daBondPool: validator(0xdd, "addr_da_bond_pool"),
  } as Pick<MidgardValidators, "daAttestation" | "daBondPool" | "stateQueue">;
  const headerHash = h28(0x10);
  const stateQueueNode: StateQueueNode = {
    proven_fraud: null,
    header: {
      prevUtxosRoot: h32(0x01),
      utxosRoot: h32(0x02),
      withdrawalsRoot: h32(0x05),
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
      transactionsRoot: h32(0x03),
      depositsRoot: h32(0x04),
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28(0x06),
      operatorVkey: h28(0x07),
      protocolVersion: 0n,
    },
    da_attestation: NO_DA_ATTESTATION,
  };
  const linkedListNode: LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: castStateQueueNodeToData(
      stateQueueNode,
    ) as LinkedListNodeView["data"],
  };
  const stateQueueUnit =
    contracts.stateQueue.policyId +
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    headerHash;
  const stateQueueUtxo: StateQueueUTxO = {
    utxo: makeUtxo(
      1,
      { lovelace: 3_000_000n, [stateQueueUnit]: 1n },
      encodeLinkedListNodeView(linkedListNode),
      contracts.stateQueue.spendingScriptAddress,
    ),
    datum: linkedListNode,
    assetName: STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  };
  const target: DaAttestationStateQueueTarget = {
    stateQueueUtxo,
    stateQueueNode,
    headerHash,
  };
  // Q63 (F04 §4) floors both governed thresholds at two, so the fixture is a
  // 2-of-2 committee over a 2-of-2 owner set. Both sets are sorted-unique.
  const daParamsDatum: DaParamsDatum = {
    committee: h32(0x11) + h32(0x22),
    committee_signers_hash: h32(0x33),
    da_threshold: 2n,
    owners: [h28(0x44), h28(0x55)],
    update_threshold: 2n,
  };
  const daParamsUtxo = makeUtxo(2, { lovelace: 2_000_000n });
  const attestationUnit = daAttestationUnit(
    contracts.daAttestation,
    headerHash,
  );
  const attestationDatum: DaAttestationDatum = {
    header_hash: headerHash,
    availability_commitment: availabilityCommitment(headerHash),
    da_threshold: 2n,
    committee_signers_hash: daParamsDatum.committee_signers_hash,
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: [h28(0x66)] },
      stakeCredential: null,
    },
    attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
    attestation_count: 0n,
  };
  const attestation: DaAttestationUtxo = {
    utxo: makeUtxo(
      3,
      { lovelace: 5_000_000n, [attestationUnit]: 1n },
      Data.to(attestationDatum, DaAttestationDatum),
      contracts.daAttestation.spendingScriptAddress,
    ),
    datum: attestationDatum,
  };
  const referenceScripts: DaAttestationReferenceScripts = {
    daAttestationMinting: makeUtxo(4),
    daAttestationSpending: makeUtxo(5),
    stateQueueMinting: makeUtxo(6),
    stateQueueSpending: makeUtxo(7),
  };
  return {
    contracts,
    headerHash,
    daParamsDatum,
    daParamsUtxo,
    target,
    attestation,
    attestationUnit,
    referenceScripts,
    availabilityParameters: AVAILABILITY_PARAMETERS,
    applyValidityRange: { validFrom: 1_000n, validTo: 2_000n },
    // Output index 0 of the all-zero tx hash: it sorts before every other
    // reference input although the builder reads it second, so a pool index
    // taken from the `readFrom` order instead of the ledger's sorted order is
    // off by one.
    pool: (datum: DaBondPoolDatum, lovelace: bigint): UTxO =>
      makeUtxo(
        0,
        {
          lovelace,
          [daBondPoolUnit(contracts.daBondPool.policyId)]: 1n,
        },
        encodeDaBondPoolDatum(datum),
        contracts.daBondPool.spendingScriptAddress,
      ),
  };
};

/**
 * Only `da_bond_lovelace` and `da_bond_pool_floor_lovelace` matter to the
 * apply pre-check; the rest is a well-formed filler.
 */
const AVAILABILITY_PARAMETERS: DaAvailabilityParameters = {
  response_geometry: availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  }),
  da_bond_lovelace: 100_000_000n,
  challenger_bond_lovelace: 50_000_000n,
  max_open_fee_lovelace: 1_000_000n,
  max_publication_fee_lovelace: 1_000_000n,
  max_settlement_fee_lovelace: 1_000_000n,
  max_close_fee_lovelace: 1_000_000n,
  max_timeout_fee_lovelace: 1_000_000n,
  da_slash_penalty_lovelace: 10_000_000n,
  da_bond_min_top_up_lovelace: 10_000_000n,
  da_bond_pool_floor_lovelace: 5_000_000n,
  challenge_record_lovelace: 27_000_000n,
};

/** A pool backing exactly one DA bond above its floor: the apply boundary. */
const EXACTLY_BONDED_POOL_LOVELACE =
  AVAILABILITY_PARAMETERS.da_bond_pool_floor_lovelace +
  AVAILABILITY_PARAMETERS.da_bond_lovelace;

const outRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The contract is *which* UTxOs the builder puts in the reference-input set,
 * not the order of the `readFrom` calls that got them there: the ledger sorts
 * reference inputs canonically before any validator sees them.
 */
const referenceSet = (record: Recording): readonly string[] =>
  [...new Set(record.reads.flat().map(outRefKey))].sort();

const collectedSet = (record: Recording): readonly string[] =>
  [
    ...new Set(record.collects.flatMap(({ inputs }) => inputs).map(outRefKey)),
  ].sort();

const expectedSet = (utxos: readonly UTxO[]): readonly string[] =>
  [...new Set(utxos.map(outRefKey))].sort();

const run = <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<A> => Effect.runPromise(program);

/**
 * A refusal is only evidence when it is *this* refusal: every negative below
 * names the precondition it means to trip, so a fixture that happens to be
 * malformed for some unrelated reason no longer satisfies the test.
 */
const expectBuildRefusal = async <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
  reason: DaAttestationBuildFailureReason,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left._tag).toBe("DaAttestationBuildError");
    expect(result.left.reason, result.left.message).toBe(reason);
  }
};

describe("DA attestation witness helpers", () => {
  it("sorts and packs indexed witnesses deterministically", async () => {
    await expect(
      run(
        encodeDaAttestationSignatureWitnesses([
          { signerIndex: 2, signatureHex: signature("bb") },
          { signerIndex: 0, signatureHex: signature("aa") },
        ]),
      ),
    ).resolves.toBe(`00${signature("aa")}02${signature("bb")}`);
  });

  it("applies witnesses to the MSB-first bitmap", async () => {
    const result = await run(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: EMPTY_ATTESTED_SIGNER_BITMAP,
        witnesses: [
          { signerIndex: 1, signatureHex: signature("bb") },
          { signerIndex: 0, signatureHex: signature("aa") },
        ],
        committeeSize: 2,
      }),
    );

    expect(result.attestedSigners).toBe(`c0${"00".repeat(31)}`);
    expect(result.attestationCount).toBe(2n);
    expect(result.packedWitnesses).toBe(
      `00${signature("aa")}01${signature("bb")}`,
    );
    expect(signerIndexIsDaAttested(result.attestedSigners, 0)).toBe(true);
    expect(signerIndexIsDaAttested(result.attestedSigners, 1)).toBe(true);
    expect(signerIndexIsDaAttested(result.attestedSigners, 2)).toBe(false);
  });

  it("rejects malformed, duplicate, already-attested, and out-of-committee witnesses", async () => {
    await expectBuildRefusal(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: "aa" },
      ]),
      "invalid_signature_hex",
    );
    await expectBuildRefusal(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: signature("aa") },
        { signerIndex: 0, signatureHex: signature("bb") },
      ]),
      "duplicate_signature_witness",
    );
    await expectBuildRefusal(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: `80${"00".repeat(31)}`,
        witnesses: [{ signerIndex: 0, signatureHex: signature("aa") }],
      }),
      "signer_already_attested",
    );
    await expectBuildRefusal(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: EMPTY_ATTESTED_SIGNER_BITMAP,
        witnesses: [{ signerIndex: 2, signatureHex: signature("aa") }],
        committeeSize: 2,
      }),
      "signer_outside_committee",
    );
  });
});

describe("DA attestation SDK builders", () => {
  it("assembles the init transaction shape from explicit inputs", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid();

    await run(
      incompleteInitDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        target: fixture.target,
        referenceScripts: fixture.referenceScripts,
        attestationOutputLovelace: 5_000_000n,
        rescueBeneficiary: fixture.attestation.datum.rescue_beneficiary,
        availabilityCommitment:
          fixture.attestation.datum.availability_commitment,
      }),
    );

    expect(referenceSet(record)).toEqual(
      expectedSet([
        fixture.daParamsUtxo,
        fixture.target.stateQueueUtxo.utxo,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.stateQueueMinting,
      ]),
    );
    // Exactness in both directions: the reference set must not silently pick up
    // the spending-script references this transaction has no spend for.
    expect(referenceSet(record)).not.toContain(
      outRefKey(fixture.referenceScripts.daAttestationSpending),
    );
    expect(record.mints[0]?.assets).toEqual({
      [fixture.attestationUnit]: 1n,
    });
    expect(record.payments[0]?.address).toBe(
      fixture.contracts.daAttestation.spendingScriptAddress,
    );
    expect(record.payments[0]?.assets).toEqual({
      lovelace: 5_000_000n,
      [fixture.attestationUnit]: 1n,
    });
    const datum = Data.from(
      record.payments[0]!.datum!.value,
      DaAttestationDatum,
    );
    expect(datum).toMatchObject({
      header_hash: fixture.headerHash,
      attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: 0n,
    });
  });

  it("assembles add-signatures with updated datum and preserved assets", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid();

    await run(
      incompleteAddDaAttestationSignaturesTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        // Both committee members sign, reaching the floor-compliant 2-of-2.
        witnesses: [
          { signerIndex: 0, signatureHex: signature("aa") },
          { signerIndex: 1, signatureHex: signature("bb") },
        ],
        referenceScripts: fixture.referenceScripts,
      }),
    );

    expect(referenceSet(record)).toEqual(
      expectedSet([
        fixture.daParamsUtxo,
        fixture.referenceScripts.daAttestationSpending,
      ]),
    );
    expect(collectedSet(record)).toEqual(
      expectedSet([fixture.attestation.utxo]),
    );
    expect(record.payments[0]?.assets).toEqual(fixture.attestation.utxo.assets);
    const datum = Data.from(
      record.payments[0]!.datum!.value,
      DaAttestationDatum,
    );
    expect(datum.attested_signers).toBe(`c0${"00".repeat(31)}`);
    expect(datum.attestation_count).toBe(2n);
    const redeemer = Data.from(
      (record.collects[0]!.redeemer as (ctx: unknown) => string)({
        outputs: record.payments.map((payment) => ({
          address: payment.address,
          assets: payment.assets,
          datum: payment.datum?.value,
        })),
        referenceInputs: record.reads[0],
      }),
      DaAttestationSpendRedeemer,
    );
    expect(redeemer).toMatchObject({
      AddSignatures: {
        signatures: `00${signature("aa")}01${signature("bb")}`,
      },
    });
    expect(record.signerKeys).toHaveLength(0);
  });

  it("preflights add-signatures committee compatibility", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    await expectBuildRefusal(
      incompleteAddDaAttestationSignaturesTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: {
          ...fixture.daParamsDatum,
          committee_signers_hash: h32(0x44),
        },
        attestation: fixture.attestation,
        witnesses: [{ signerIndex: 0, signatureHex: signature("aa") }],
        referenceScripts: fixture.referenceScripts,
      }),
      "params_committee_hash_mismatch",
    );
  });

  it("assembles apply with DA burn, beneficiary refund, pool reference and state-queue datum update", async () => {
    const fixture = makeFixture();
    const pool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const { lucid, record } = makeRecordingLucid({ utxos: [pool] });

    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
    );

    // The pool is fetched by the builder itself, at the pool script address
    // under its NFT unit.
    expect(record.unitQueries).toEqual([
      {
        address: fixture.contracts.daBondPool.spendingScriptAddress,
        unit: daBondPoolUnit(fixture.contracts.daBondPool.policyId),
      },
    ]);
    // Exactly the governed params, the pool and the four scripts apply runs:
    // no hub oracle and no availability-challenge script any more.
    expect(referenceSet(record)).toEqual(
      expectedSet([
        fixture.daParamsUtxo,
        pool,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.daAttestationSpending,
        fixture.referenceScripts.stateQueueMinting,
        fixture.referenceScripts.stateQueueSpending,
      ]),
    );
    // The apply spends the attestation and the state-queue node and nothing
    // else — the reference-only UTxOs (the pool included) stay out of the
    // input set.
    expect(collectedSet(record)).toEqual(
      expectedSet([
        thresholdAttestation(fixture).utxo,
        fixture.target.stateQueueUtxo.utxo,
      ]),
    );
    // Only the DAAT burn: no bond mint and no availability-policy mint.
    expect(record.mints.map(({ assets }) => assets)).toEqual([
      { [fixture.attestationUnit]: -1n },
    ]);
    expect(record.withdrawals).toEqual([]);
    expect(record.validityRanges).toEqual([
      { validFrom: 1_000, validTo: 2_000 },
    ]);
    expect(record.payments).toHaveLength(2);
    expect(record.payments[0]?.address).toBe(
      fixture.contracts.stateQueue.spendingScriptAddress,
    );
    expect(record.payments[0]?.assets).toEqual(
      fixture.target.stateQueueUtxo.utxo.assets,
    );
    // The refund: the attestation's whole value less the burned DAAT, to the
    // frozen beneficiary.
    expect(record.payments[1]).toEqual({
      address: beneficiaryAddress,
      assets: { lovelace: 5_000_000n },
    });
    const linkedListDatum = Data.from(
      record.payments[0]!.datum!.value,
      LinkedListDatum,
    );
    expect("Node" in linkedListDatum.data).toBe(true);
    if ("Node" in linkedListDatum.data) {
      const stateQueueNode = Data.castFrom(
        linkedListDatum.data.Node.data,
        StateQueueNode,
      );
      expect(stateQueueNode.header).toEqual(
        fixture.target.stateQueueNode.header,
      );
      expect(stateQueueNode.da_attestation).toEqual({
        Attested: {
          commitment_hash: daAvailabilityCommitmentHash(
            fixture.attestation.datum.availability_commitment,
          ),
        },
      });
    }
  });

  it("encodes the apply redeemer with the pool index over the sorted reference inputs and positional outputs", async () => {
    const fixture = makeFixture();
    const pool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const { lucid, record } = makeRecordingLucid({ utxos: [pool] });
    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
    );

    // A change output identical to the refund (same address, same lovelace)
    // follows the explicit outputs: a value lookup could not tell the two
    // apart, the positional index can.
    const outputs: TxOutput[] = [
      ...recordedOutputs(record),
      { address: beneficiaryAddress, assets: { lovelace: 5_000_000n } },
    ];
    const redeemerCbor = applyMintRedeemer(record)(
      applyMintContext(fixture, record, outputs),
    );
    // Sorted inputs: the node (tx 01) then the attestation (tx 03). Sorted
    // reference inputs: pool (tx 00), params (02), DAAT mint (04), DAAT spend
    // (05), queue mint (06), queue spend (07). The pool is read second but
    // sits first on the ledger.
    expect(Data.from(redeemerCbor, DaAttestationMintRedeemer)).toEqual({
      ApplyToStateQueue: {
        da_attestation_input_index: 1n,
        da_params_ref_input_index: 1n,
        state_queue_input_index: 0n,
        state_queue_output_index: 0n,
        state_queue_mint_ref_script_input_index: 4n,
        pool_ref_input_index: 0n,
        refund_output_index: 1n,
      },
    });
    // Byte-level: constructor 1, fields in the Aiken declaration order.
    expect(redeemerCbor).toBe(
      Data.to(new Constr(1, [1n, 1n, 0n, 0n, 4n, 0n, 1n])),
    );
  });

  it("refuses to write a refund index that does not point at the refund", async () => {
    const fixture = makeFixture();
    const pool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const { lucid, record } = makeRecordingLucid({ utxos: [pool] });
    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
    );
    const [node, refund] = recordedOutputs(record);
    const change: TxOutput = {
      address: beneficiaryAddress,
      assets: { lovelace: 1_234_567n },
    };

    expect(() =>
      applyMintRedeemer(record)(
        applyMintContext(fixture, record, [node!, change, refund!]),
      ),
    ).toThrow(/beneficiary refund is not at position 1/u);
  });

  it("refuses to apply against a withdrawing pool", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid({
      utxos: [
        fixture.pool(
          { Withdrawing: { unlock_at: 9_999n } },
          EXACTLY_BONDED_POOL_LOVELACE,
        ),
      ],
    });

    await expectBuildRefusal(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
      "pool-withdrawing",
    );
  });

  it("refuses to apply against a pool backing one lovelace less than a DA bond", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid({
      utxos: [fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE - 1n)],
    });

    // The pool's total lovelace still exceeds `da_bond_lovelace`; only the
    // floor-excluded backing is short.
    await expectBuildRefusal(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
      "pool-under-backed",
    );
  });

  it("refuses to apply when no authentic pool can be fetched", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid({ utxos: [] });

    await expectBuildRefusal(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
      "pool-unavailable",
    );
  });

  it("skipPoolPrecheck (test-only) builds against a withdrawing or short pool and still references it", async () => {
    const fixture = makeFixture();
    for (const pool of [
      fixture.pool(
        { Withdrawing: { unlock_at: 9_999n } },
        EXACTLY_BONDED_POOL_LOVELACE,
      ),
      fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE - 1n),
    ]) {
      const { lucid, record } = makeRecordingLucid({ utxos: [pool] });
      await run(
        incompleteApplyDaAttestationToStateQueueTxProgram(
          lucid,
          fixture.contracts,
          { ...applyConfig(fixture), skipPoolPrecheck: true },
        ),
      );
      expect(referenceSet(record)).toContain(outRefKey(pool));
    }
  });

  it("re-fetches the pool on every build instead of reusing an earlier outref", async () => {
    const fixture = makeFixture();
    const firstPool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const chain = { utxos: [firstPool] as readonly UTxO[] };
    const { lucid, record } = makeRecordingLucid(chain);

    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
    );
    // A top-up spends the pool and recreates it at a new outref.
    const toppedUpPool: UTxO = {
      ...firstPool,
      txHash: "ee".repeat(32),
      assets: {
        ...firstPool.assets,
        lovelace: EXACTLY_BONDED_POOL_LOVELACE + 10_000_000n,
      },
    };
    chain.utxos = [toppedUpPool];
    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        applyConfig(fixture),
      ),
    );

    expect(record.unitQueries).toHaveLength(2);
    expect(record.reads[1]?.map(outRefKey)).toContain(outRefKey(toppedUpPool));
    expect(record.reads[1]?.map(outRefKey)).not.toContain(outRefKey(firstPool));
  });

  it("preflights apply header and threshold requirements before touching the pool", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid({
      utxos: [fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE)],
    });

    await expectBuildRefusal(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        { ...applyConfig(fixture), attestation: fixture.attestation },
      ),
      "threshold_not_reached",
    );
    await expectBuildRefusal(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        {
          ...applyConfig(fixture),
          attestation: {
            ...fixture.attestation,
            // Threshold is satisfied, so the header-hash mismatch is the only
            // reason this must be refused.
            datum: {
              ...thresholdAttestation(fixture).datum,
              header_hash: h28(0x99),
            },
          },
        },
      ),
      "attestation_header_mismatch",
    );
    expect(record.unitQueries).toEqual([]);
  });
});

const beneficiaryAddress = credentialToAddress("Custom", {
  type: "Key",
  hash: h28(0x66),
});

const thresholdAttestation = (
  fixture: ReturnType<typeof makeFixture>,
): DaAttestationUtxo => ({
  ...fixture.attestation,
  datum: {
    ...fixture.attestation.datum,
    attested_signers: `c0${"00".repeat(31)}`,
    attestation_count: 2n,
  },
});

const applyConfig = (fixture: ReturnType<typeof makeFixture>) => ({
  daParamsUtxo: fixture.daParamsUtxo,
  daParamsDatum: fixture.daParamsDatum,
  target: fixture.target,
  attestation: thresholdAttestation(fixture),
  referenceScripts: fixture.referenceScripts,
  validityRange: fixture.applyValidityRange,
  availabilityParameters: fixture.availabilityParameters,
});

const recordedOutputs = (record: Recording): TxOutput[] =>
  record.payments.map((payment) => ({
    address: payment.address,
    assets: payment.assets,
    datum: payment.datum?.value ?? null,
  }));

const applyMintRedeemer = (
  record: Recording,
): ((ctx: RedeemerContext) => string) => {
  const redeemer = record.mints[0]?.redeemer;
  if (typeof redeemer !== "function") {
    throw new Error("apply mint redeemer is not a context builder");
  }
  return redeemer as (ctx: RedeemerContext) => string;
};

/**
 * The script-context projection the ledger would hand the apply mint: spent
 * inputs in canonical (tx hash, index) order, reference inputs exactly as the
 * builder read them (the helper under test must sort them itself), and the
 * given final outputs.
 */
const applyMintContext = (
  fixture: ReturnType<typeof makeFixture>,
  record: Recording,
  outputs: readonly TxOutput[],
): RedeemerContext => {
  const inputs = record.collects
    .flatMap(({ inputs: collected }) => collected)
    .sort((left, right) => {
      const l = outRefKey(left),
        r = outRefKey(right);
      return l < r ? -1 : l > r ? 1 : 0;
    });
  const ownPurpose = {
    tag: "mint",
    index: 0n,
    policyId: fixture.contracts.daAttestation.policyId,
    redeemerListIndex: 0n,
  } as const;
  return {
    inputs,
    referenceInputs: record.reads.flat(),
    outputs,
    redeemers: [ownPurpose],
    ownPurpose,
    inputIndex: (input: Pick<UTxO, "txHash" | "outputIndex">) => {
      const index = inputs.findIndex(
        (candidate) =>
          candidate.txHash === input.txHash &&
          candidate.outputIndex === input.outputIndex,
      );
      return index < 0 ? undefined : BigInt(index);
    },
  } as unknown as RedeemerContext;
};
