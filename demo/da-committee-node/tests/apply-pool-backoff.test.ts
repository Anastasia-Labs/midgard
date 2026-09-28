import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  type Assets,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isRecoverableL1Race } from "../src/coordinator/on-chain.js";
import {
  DA_BOND_POOL_APPLY_BACKOFF_REASONS,
  DaBondPoolApplyBackoffError,
} from "../src/coordinator/pool-backoff.js";
import { buildApplyAttestationTx } from "../src/coordinator/tx-builders.js";

/**
 * The coordinator's race patterns (`on-chain.ts`). A backoff message matching
 * any of them would be retried as a race by anything classifying on text.
 */
const RACE_PATTERNS = [
  /selected DA attestation candidate disappeared/i,
  /expected exactly one DA attestation UTxO .* found 0/i,
  /state queue header .* was not found/i,
  /input.*not.*found/i,
  /utxo.*not.*found/i,
  /\bspent\b/i,
];

const AVAILABILITY_PARAMETERS: SDK.DaAvailabilityParameters = {
  response_geometry: SDK.availabilityResponseGeometry({
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

const makeUtxo = (
  outputIndex: number,
  assets: Assets = { lovelace: 1n },
  datum: string | null = null,
  address = `addr_test_${outputIndex.toString()}`,
  txHash = outputIndex.toString(16).padStart(64, "0"),
): UTxO => ({ txHash, outputIndex, address, assets, datum }) as UTxO;

const validator = (policyByte: number, address: string) =>
  ({
    policyId: h28(policyByte),
    spendingScriptAddress: address,
    spendingScriptHash: h28(policyByte),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as unknown as SDK.MidgardValidators["daAttestation"];

/**
 * A lucid stand-in that answers the pool fetch from `chain` (or throws
 * `chain.failure`), records every pool query and every reference-input set,
 * and completes to a marker so a successful build is observable.
 */
const makeLucid = (chain: { utxos: readonly UTxO[]; failure?: Error }) => {
  const unitQueries: string[] = [];
  const reads: UTxO[][] = [];
  const lucid = {
    config: () => ({ network: "Custom" }),
    utxosAtWithUnit: async (address: string, unit: string) => {
      unitQueries.push(unit);
      if (chain.failure !== undefined) {
        throw chain.failure;
      }
      return chain.utxos.filter(
        (utxo) => utxo.address === address && (utxo.assets[unit] ?? 0n) > 0n,
      );
    },
    newTx: () => {
      const tx: Record<string, unknown> = {};
      const chainable = () => tx;
      Object.assign(tx, {
        validFrom: chainable,
        validTo: chainable,
        readFrom: (inputs: UTxO[]) => {
          reads.push(inputs);
          return tx;
        },
        collectFrom: chainable,
        mintAssets: chainable,
        withdraw: chainable,
        addSignerKey: chainable,
        pay: { ToContract: chainable, ToAddress: chainable },
        complete: async () => ({ built: true }),
      });
      return tx;
    },
  } as unknown as LucidEvolution;
  return { lucid, unitQueries, reads };
};

const makeFixture = () => {
  const contracts = {
    daAttestation: validator(0xaa, "addr_da_attestation"),
    stateQueue: validator(0xbb, "addr_state_queue"),
    daBondPool: validator(0xdd, "addr_da_bond_pool"),
  };
  const headerHash = h28(0x10);
  const stateQueueNode: SDK.StateQueueNode = {
    proven_fraud: null,
    header: {
      prevUtxosRoot: h32(0x01),
      utxosRoot: h32(0x02),
      withdrawalsRoot: h32(0x05),
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
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
    da_attestation: SDK.NO_DA_ATTESTATION,
  };
  const linkedListNode: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData(
      stateQueueNode,
    ) as SDK.LinkedListNodeView["data"],
  };
  const stateQueueUnit =
    contracts.stateQueue.policyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    headerHash;
  const stateQueueUtxo: SDK.StateQueueUTxO = {
    utxo: makeUtxo(
      1,
      { lovelace: 3_000_000n, [stateQueueUnit]: 1n },
      SDK.encodeLinkedListNodeView(linkedListNode),
      contracts.stateQueue.spendingScriptAddress,
    ),
    datum: linkedListNode,
    assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  };
  const daParamsDatum: SDK.DaParamsDatum = {
    committee: h32(0x11) + h32(0x22),
    committee_signers_hash: h32(0x33),
    da_threshold: 2n,
    owners: [h28(0x44), h28(0x55)],
    update_threshold: 2n,
  };
  const attestationUnit = SDK.daAttestationUnit(
    contracts.daAttestation,
    headerHash,
  );
  // A threshold-reached attestation, so the pool is the only thing that can
  // stop the build.
  const attestationDatum: SDK.DaAttestationDatum = {
    header_hash: headerHash,
    availability_commitment: SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: h28(0x71),
      headerHash,
      payload: Uint8Array.of(1),
      responseGeometry: AVAILABILITY_PARAMETERS.response_geometry,
    }),
    da_threshold: 2n,
    committee_signers_hash: daParamsDatum.committee_signers_hash,
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: [h28(0x66)] },
      stakeCredential: null,
    },
    attested_signers: `c0${"00".repeat(31)}`,
    attestation_count: 2n,
  };
  const pool = (
    datum: SDK.DaBondPoolDatum,
    lovelace: bigint,
    txHash = "00".repeat(32),
  ): UTxO =>
    makeUtxo(
      0,
      {
        lovelace,
        [SDK.daBondPoolUnit(contracts.daBondPool.policyId)]: 1n,
      },
      SDK.encodeDaBondPoolDatum(datum),
      contracts.daBondPool.spendingScriptAddress,
      txHash,
    );
  const buildArgs = (lucid: LucidEvolution) =>
    ({
      lucid,
      contracts,
      target: { stateQueueUtxo, stateQueueNode, headerHash },
      attestationUtxo: makeUtxo(
        3,
        { lovelace: 5_000_000n, [attestationUnit]: 1n },
        Data.to(attestationDatum as never, SDK.DaAttestationDatum as never),
        contracts.daAttestation.spendingScriptAddress,
      ),
      attestationDatum,
      daParamsUtxo: makeUtxo(2, { lovelace: 2_000_000n }),
      daParamsDatum,
      referenceScripts: {
        daAttestationMinting: makeUtxo(4),
        daAttestationSpending: makeUtxo(5),
        stateQueueMinting: makeUtxo(6),
        stateQueueSpending: makeUtxo(7),
      },
      validityRange: { validFrom: 1_000n, validTo: 2_000n },
      availabilityParameters: AVAILABILITY_PARAMETERS,
    }) as unknown as Parameters<typeof buildApplyAttestationTx>[0];
  return { pool, buildArgs };
};

const outRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const expectBackoff = async (
  build: Promise<unknown>,
  reason: (typeof DA_BOND_POOL_APPLY_BACKOFF_REASONS)[number],
): Promise<DaBondPoolApplyBackoffError> => {
  const error = await build.then(
    () => {
      throw new Error("expected the apply build to back off");
    },
    (rejection: unknown) => rejection,
  );
  expect(error).toBeInstanceOf(DaBondPoolApplyBackoffError);
  const backoff = error as DaBondPoolApplyBackoffError;
  expect(backoff.reason).toBe(reason);
  for (const pattern of RACE_PATTERNS) {
    expect(backoff.message).not.toMatch(pattern);
  }
  expect(isRecoverableL1Race(backoff)).toBe(false);
  return backoff;
};

describe("apply against the pooled DA bond", () => {
  it("builds against a Bonded pool backing exactly one DA bond, reading the pool as a reference input", async () => {
    const fixture = makeFixture();
    const pool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const { lucid, unitQueries, reads } = makeLucid({ utxos: [pool] });

    await expect(
      buildApplyAttestationTx(fixture.buildArgs(lucid)),
    ).resolves.toEqual({ built: true });
    expect(unitQueries).toHaveLength(1);
    expect(reads.flat().map(outRefKey)).toContain(outRefKey(pool));
  });

  it("re-fetches the pool on every build, so a retry after churn reads the new outref", async () => {
    const fixture = makeFixture();
    const firstPool = fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE);
    const chain = { utxos: [firstPool] as readonly UTxO[] };
    const { lucid, unitQueries, reads } = makeLucid(chain);

    await buildApplyAttestationTx(fixture.buildArgs(lucid));
    // A top-up spends the pool and recreates it at a new outref.
    const toppedUp = fixture.pool(
      "Bonded",
      EXACTLY_BONDED_POOL_LOVELACE + 10_000_000n,
      "ee".repeat(32),
    );
    chain.utxos = [toppedUp];
    const readsBefore = reads.length;
    await buildApplyAttestationTx(fixture.buildArgs(lucid));

    expect(unitQueries).toHaveLength(2);
    const secondBuildReads = reads.slice(readsBefore).flat().map(outRefKey);
    expect(secondBuildReads).toContain(outRefKey(toppedUp));
    expect(secondBuildReads).not.toContain(outRefKey(firstPool));
  });

  it("backs off typed (pool-under-backed) when the pool backs one lovelace less than a DA bond", async () => {
    const fixture = makeFixture();
    const { lucid } = makeLucid({
      utxos: [fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE - 1n)],
    });

    const backoff = await expectBackoff(
      buildApplyAttestationTx(fixture.buildArgs(lucid)),
      "pool-under-backed",
    );
    expect(backoff.message).toMatch(/top it up/u);
  });

  it("backs off typed (pool-withdrawing) when the pool is Withdrawing, however well backed", async () => {
    const fixture = makeFixture();
    const { lucid } = makeLucid({
      utxos: [
        fixture.pool(
          { Withdrawing: { unlock_at: 9_999n } },
          EXACTLY_BONDED_POOL_LOVELACE * 10n,
        ),
      ],
    });

    const backoff = await expectBackoff(
      buildApplyAttestationTx(fixture.buildArgs(lucid)),
      "pool-withdrawing",
    );
    expect(backoff.detail).toMatch(/unlock_at=9999/u);
  });

  it("backs off typed (pool-unavailable) when no authentic pool is at the address", async () => {
    const fixture = makeFixture();
    const { lucid } = makeLucid({ utxos: [] });

    const backoff = await expectBackoff(
      buildApplyAttestationTx(fixture.buildArgs(lucid)),
      "pool-unavailable",
    );
    expect(backoff.detail).toMatch(/found 0/u);
  });

  it("backs off typed (pool-unavailable) when the provider fails with race-like text, keeping that text out of the message", async () => {
    const fixture = makeFixture();
    const { lucid } = makeLucid({
      utxos: [],
      failure: new Error("utxo not found: pool input was spent"),
    });

    const backoff = await expectBackoff(
      buildApplyAttestationTx(fixture.buildArgs(lucid)),
      "pool-unavailable",
    );
    expect(backoff.detail).toMatch(/utxo not found: pool input was spent/u);
  });

  it("passes a non-pool build refusal through unchanged", async () => {
    const fixture = makeFixture();
    const { lucid, unitQueries } = makeLucid({
      utxos: [fixture.pool("Bonded", EXACTLY_BONDED_POOL_LOVELACE)],
    });
    const args = fixture.buildArgs(lucid);

    const error = await buildApplyAttestationTx({
      ...args,
      attestationDatum: {
        ...args.attestationDatum,
        attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
        attestation_count: 0n,
      },
    }).then(
      () => undefined,
      (rejection: unknown) => rejection,
    );

    expect(error).not.toBeInstanceOf(DaBondPoolApplyBackoffError);
    expect(error).toMatchObject({
      _tag: "DaAttestationBuildError",
      reason: "threshold_not_reached",
    });
    expect(unitQueries).toEqual([]);
  });
});
