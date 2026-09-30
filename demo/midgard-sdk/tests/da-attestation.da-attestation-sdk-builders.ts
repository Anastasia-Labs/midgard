import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  Constr,
  credentialToAddress,
  Data,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DaAttestationDatum,
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  daAvailabilityCommitmentHash,
  daBondPoolUnit,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  incompleteAddDaAttestationSignaturesTxProgram,
  incompleteApplyDaAttestationToStateQueueTxProgram,
  incompleteInitDaAttestationTxProgram,
  LinkedListDatum,
  StateQueueNode,
} from "../src/index.js";
import {
  applyConfig,
  applyMintContext,
  applyMintRedeemer,
  collectedSet,
  expectBuildRefusal,
  expectedSet,
  outRefKey,
  recordedOutputs,
  referenceSet,
  run,
  thresholdAttestation,
} from "./da-attestation.da-attestation-witness-helpers.js";
import {
  EXACTLY_BONDED_POOL_LOVELACE,
  makeFixture,
  makeRecordingLucid,
  signature,
} from "./da-attestation.make-fixture.js";

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
