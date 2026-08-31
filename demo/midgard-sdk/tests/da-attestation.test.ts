import {
  type Assets,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyDaAttestationSignatureWitnesses,
  availabilityResponseGeometryV1,
  buildDaAvailabilityCommitmentV1,
  castStateQueueNodeV1ToData,
  type DaAttestationBuildError,
  DaAttestationDatum,
  type DaAttestationReferenceScripts,
  DaAttestationSpendRedeemer,
  type DaAttestationStateQueueTarget,
  daAttestationUnit,
  type DaAttestationUtxo,
  type DaParamsDatum,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  encodeDaAttestationSignatureWitnesses,
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
  StateQueueNodeV1,
  type StateQueueUTxO,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const signature = (byte: string): string => byte.repeat(64);
const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitmentV1({
    deploymentIdentity: h28("71"),
    headerHash,
    payload: Uint8Array.of(1),
    bondOwner: h28("72"),
    responseGeometry: availabilityResponseGeometryV1({
      chunkByteLength: 4096,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

type RecordedPayment = {
  readonly address: string;
  readonly datum: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

type Recording = {
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer: unknown }[];
  readonly mints: { readonly assets: Assets; readonly redeemer: unknown }[];
  readonly payments: RecordedPayment[];
  readonly signerKeys: string[];
};

const makeRecordingLucid = (): {
  readonly lucid: LucidEvolution;
  readonly record: Recording;
} => {
  const record: Recording = {
    reads: [],
    collects: [],
    mints: [],
    payments: [],
    signerKeys: [],
  };
  const lucid = {
    newTx: () => {
      const tx = {
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer: unknown) => {
          record.collects.push({ inputs, redeemer });
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

const validator = (policyByte: string, address: string) =>
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
    daAttestation: validator("aa", "addr_da_attestation"),
    stateQueue: validator("bb", "addr_state_queue"),
    availabilityChallenge: validator("cc", "addr_availability_challenge"),
  } as Pick<
    MidgardValidators,
    "availabilityChallenge" | "daAttestation" | "stateQueue"
  >;
  const headerHash = h28("10");
  const stateQueueNode: StateQueueNodeV1 = {
    header: {
      prevUtxosRoot: h32("01"),
      utxosRoot: h32("02"),
      withdrawalsRoot: h32("05"),
      ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
      transactionsRoot: h32("03"),
      depositsRoot: h32("04"),
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28("06"),
      operatorVkey: h28("07"),
      protocolVersion: 0n,
    },
    da_attestation: NO_DA_ATTESTATION,
  };
  const linkedListNode: LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: castStateQueueNodeV1ToData(
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
    committee: h32("11") + h32("22"),
    committee_signers_hash: h32("33"),
    da_threshold: 2n,
    owners: [h28("44"), h28("55")],
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
      paymentCredential: { PublicKeyCredential: [h28("66")] },
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
    availabilityChallengeMinting: makeUtxo(8),
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
    hubOracleRefInput: makeUtxo(9),
  };
};

const run = <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<A> => Effect.runPromise(program);

const expectBuildFailure = async <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left._tag).toBe("DaAttestationBuildError");
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
    await expectBuildFailure(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: "aa" },
      ]),
    );
    await expectBuildFailure(
      encodeDaAttestationSignatureWitnesses([
        { signerIndex: 0, signatureHex: signature("aa") },
        { signerIndex: 0, signatureHex: signature("bb") },
      ]),
    );
    await expectBuildFailure(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: `80${"00".repeat(31)}`,
        witnesses: [{ signerIndex: 0, signatureHex: signature("aa") }],
      }),
    );
    await expectBuildFailure(
      applyDaAttestationSignatureWitnesses({
        attestedSignersHex: EMPTY_ATTESTED_SIGNER_BITMAP,
        witnesses: [{ signerIndex: 2, signatureHex: signature("aa") }],
        committeeSize: 2,
      }),
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

    expect(record.reads).toEqual([
      [
        fixture.daParamsUtxo,
        fixture.target.stateQueueUtxo.utxo,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.stateQueueMinting,
      ],
    ]);
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
      record.payments[0]!.datum.value,
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

    expect(record.reads).toEqual([
      [fixture.daParamsUtxo, fixture.referenceScripts.daAttestationSpending],
    ]);
    expect(record.collects[0]?.inputs).toEqual([fixture.attestation.utxo]);
    expect(record.payments[0]?.assets).toEqual(fixture.attestation.utxo.assets);
    const datum = Data.from(
      record.payments[0]!.datum.value,
      DaAttestationDatum,
    );
    expect(datum.attested_signers).toBe(`c0${"00".repeat(31)}`);
    expect(datum.attestation_count).toBe(2n);
    const redeemer = Data.from(
      (record.collects[0]!.redeemer as (ctx: unknown) => string)({
        outputs: record.payments.map((payment) => ({
          address: payment.address,
          assets: payment.assets,
          datum: payment.datum.value,
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

    await expectBuildFailure(
      incompleteAddDaAttestationSignaturesTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: {
          ...fixture.daParamsDatum,
          committee_signers_hash: h32("44"),
        },
        attestation: fixture.attestation,
        witnesses: [{ signerIndex: 0, signatureHex: signature("aa") }],
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("assembles apply with DA burn and state-queue datum update", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid();
    const thresholdAttestation: DaAttestationUtxo = {
      ...fixture.attestation,
      datum: {
        ...fixture.attestation.datum,
        attested_signers: `c0${"00".repeat(31)}`,
        attestation_count: 2n,
      },
    };

    await run(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        {
          hubOracleRefInput: fixture.hubOracleRefInput,
          daParamsUtxo: fixture.daParamsUtxo,
          daParamsDatum: fixture.daParamsDatum,
          target: fixture.target,
          attestation: thresholdAttestation,
          referenceScripts: fixture.referenceScripts,
        },
      ),
    );

    expect(record.reads).toEqual([
      [
        fixture.hubOracleRefInput,
        fixture.daParamsUtxo,
        fixture.referenceScripts.availabilityChallengeMinting,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.daAttestationSpending,
        fixture.referenceScripts.stateQueueMinting,
        fixture.referenceScripts.stateQueueSpending,
      ],
    ]);
    expect(record.collects.map((entry) => entry.inputs)).toEqual([
      [thresholdAttestation.utxo],
      [fixture.target.stateQueueUtxo.utxo],
    ]);
    expect(record.mints[0]?.assets).toEqual({
      [fixture.attestationUnit]: -1n,
    });
    expect(record.payments[0]?.address).toBe(
      fixture.contracts.stateQueue.spendingScriptAddress,
    );
    expect(record.payments[0]?.assets).toEqual(
      fixture.target.stateQueueUtxo.utxo.assets,
    );
    const linkedListDatum = Data.from(
      record.payments[0]!.datum.value,
      LinkedListDatum,
    );
    expect("Node" in linkedListDatum.data).toBe(true);
    if ("Node" in linkedListDatum.data) {
      const stateQueueNode = Data.castFrom(
        linkedListDatum.data.Node.data,
        StateQueueNodeV1,
      );
      expect(stateQueueNode.header).toEqual(
        fixture.target.stateQueueNode.header,
      );
      expect(stateQueueNode.da_attestation).toMatchObject({
        Attested: {
          da_bond_asset_name: expect.stringMatching(/^[0-9a-f]{64}$/u),
        },
      });
    }
  });

  it("preflights apply header and threshold requirements", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    await expectBuildFailure(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        {
          hubOracleRefInput: fixture.hubOracleRefInput,
          daParamsUtxo: fixture.daParamsUtxo,
          daParamsDatum: fixture.daParamsDatum,
          target: fixture.target,
          attestation: fixture.attestation,
          referenceScripts: fixture.referenceScripts,
        },
      ),
    );
    await expectBuildFailure(
      incompleteApplyDaAttestationToStateQueueTxProgram(
        lucid,
        fixture.contracts,
        {
          hubOracleRefInput: fixture.hubOracleRefInput,
          daParamsUtxo: fixture.daParamsUtxo,
          daParamsDatum: fixture.daParamsDatum,
          target: fixture.target,
          attestation: {
            ...fixture.attestation,
            // Threshold is satisfied, so the header-hash mismatch is the only
            // reason this must be refused.
            datum: {
              ...fixture.attestation.datum,
              header_hash: h28("99"),
              attested_signers: `c0${"00".repeat(31)}`,
              attestation_count: 2n,
            },
          },
          referenceScripts: fixture.referenceScripts,
        },
      ),
    );
  });
});
