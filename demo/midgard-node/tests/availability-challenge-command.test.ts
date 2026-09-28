import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  assertAvailabilityCommandRemovalCapital,
  assertAvailabilityTimeoutCollateral,
  availabilityTimeoutCollateralLovelace,
  availabilityTimeoutRentRefundAddress,
  buildAvailabilityCommandTransaction,
  parseAvailabilityOutRef,
  planAvailabilityCommandAction,
  recoverAvailabilityOpenCommitment,
  runAvailabilityChallengeCommand,
} from "../src/commands/availability-challenge.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  attestAvailability,
  availabilityDeployment,
  createAvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

const hash = "11".repeat(28);
const challenge = SDK.buildDaAvailabilityChallengeDatumPlan({
  commitment: SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "22".repeat(28),
    headerHash: hash,
    payload: Uint8Array.of(1),
    responseGeometry: parameters.response_geometry,
  }),
  challengerFundingOutRef: {
    transactionId: "66".repeat(32),
    outputIndex: 0n,
  },
  challenger: "77".repeat(28),
  openedAt: 1_000n,
  parameters,
});
const lock = (datum: SDK.CorrectionLockDatum): UTxO => ({
  txHash: "88".repeat(32),
  outputIndex: 0,
  address: "correction-lock",
  assets: { lovelace: 3_000_000n },
  datum: Data.to(datum, SDK.CorrectionLockDatum),
});
const snapshot = {
  headerHash: hash,
  recordDatum: challenge.record,
  terminalDatum: challenge.terminalAccumulator,
  correctionLock: lock("Idle"),
};

describe("operational availability commands", () => {
  it("requires the challenger bond, the challenge record and the full current descendant removal reserve", () => {
    const collateral = {
      ...lock("Idle"),
      address: "actor",
      assets: { lovelace: 100_000_000n },
    };
    // Open spends one challenger coin holding exactly the challenger bond, the
    // challenge record lovelace and the fee.
    const opening =
      parameters.challenger_bond_lovelace +
      parameters.challenge_record_lovelace +
      parameters.max_open_fee_lovelace;
    const available = {
      ...collateral,
      txHash: "99".repeat(32),
      assets: {
        lovelace:
          opening + 2n * parameters.max_timeout_fee_lovelace + 1_000_000n,
      },
    };
    const funding = {
      action: "open" as const,
      parameters,
      remainingRemovalSteps: 2,
      minimumChangeLovelace: 1_000_000n,
      walletAddress: "actor",
      walletUtxos: [{ ...available, datum: undefined }, collateral],
      collateral,
      reservedOutRefs: new Set<string>(),
    };
    expect(() =>
      assertAvailabilityCommandRemovalCapital(funding),
    ).not.toThrow();
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        remainingRemovalSteps: 3,
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          {
            ...available,
            datum: undefined,
            assets: { lovelace: available.assets.lovelace - 1n },
          },
          collateral,
        ],
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        reservedOutRefs: new Set([`${available.txHash}#0`]),
      }),
    ).toThrow(/reserved inputs/);
  });

  it("does not count collateral, foreign outputs or native assets as timeout working capital", () => {
    const collateral: UTxO = {
      txHash: "88".repeat(32),
      outputIndex: 0,
      address: "actor",
      assets: { lovelace: 100_000_000n },
    };
    const reserve = 2n * parameters.max_timeout_fee_lovelace + 1_000_000n;
    const funding = {
      action: "timeout" as const,
      parameters,
      remainingRemovalSteps: 2,
      minimumChangeLovelace: 1_000_000n,
      walletAddress: "actor",
      collateral,
      reservedOutRefs: new Set<string>(),
    };
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          collateral,
          { ...collateral, txHash: "99".repeat(32), address: "foreign" },
          {
            ...collateral,
            txHash: "aa".repeat(32),
            assets: { lovelace: reserve, token: 1n },
          },
        ],
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          collateral,
          {
            ...collateral,
            txHash: "bb".repeat(32),
            assets: { lovelace: reserve },
          },
        ],
      }),
    ).not.toThrow();
  });

  it("parses exact output references and refuses ambiguous indexes", () => {
    expect(parseAvailabilityOutRef(`${"aa".repeat(32)}#3`)).toEqual({
      txHash: "aa".repeat(32),
      outputIndex: 3,
    });
    for (const value of [
      `${"aa".repeat(32)}#03`,
      `${"aa".repeat(32)}#65536`,
      "unknown#0",
    ])
      expect(() => parseAvailabilityOutRef(value)).toThrow(/canonical/);
  });

  it("refuses invalid journal and missing actor credentials before reading manifests or calling providers", async () => {
    const options = {
      headerHash: hash,
      manifest: "/unread-manifest.json",
      journal: "/tmp/availability.sqlite",
      walletSeedEnv: "AVAILABILITY_ACTOR_SEED",
    };
    await expect(
      runAvailabilityChallengeCommand(
        "status",
        { ...options, journal: "relative.sqlite" },
        {},
      ),
    ).rejects.toThrow(/absolute durable path/);
    await expect(
      runAvailabilityChallengeCommand("open", options, {}),
    ).rejects.toThrow(/actor seed is missing/);
  });

  it("refuses premature timeout and settles expired tranches before the head timeout", () => {
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        snapshot,
        Number(challenge.responseDeadline),
      ),
    ).toThrow(/deadline/);
    expect(
      planAvailabilityCommandAction(
        "timeout",
        snapshot,
        Number(challenge.responseDeadline) + 1,
      ),
    ).toBe("settle");
    const terminal = {
      ...snapshot,
      terminalDatum: {
        ...challenge.terminalAccumulator,
        next_tranche_index: 1n,
        has_timed_out_tranche: true,
      },
    };
    expect(
      planAvailabilityCommandAction(
        "timeout",
        terminal,
        Number(challenge.responseDeadline) + 1,
      ),
    ).toBe("timeout");
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        {
          ...terminal,
          terminalDatum: {
            ...terminal.terminalDatum,
            has_timed_out_tranche: false,
          },
        },
        Number(challenge.responseDeadline) + 1,
      ),
    ).toThrow(/Fully answered/);
  });

  it("resumes descendant pruning and head removal only under the matching availability lock", () => {
    const locked = {
      headerHash: hash,
      correctionLock: lock({
        Locked: {
          target_header_hash: hash,
          correction_identity: {
            AvailabilityChallenge: {
              challenge_asset_name: challenge.challengeAssetName,
            },
          },
        },
      }),
    };
    const descendant: SDK.StateQueueUTxO = {
      utxo: lock("Idle"),
      assetName: "00",
      datum: { key: "Empty", next: "Empty", data: new Constr(0, []) },
    };
    expect(
      planAvailabilityCommandAction(
        "timeout",
        { ...locked, descendant },
        10_000,
      ),
    ).toBe("prune");
    expect(planAvailabilityCommandAction("timeout", locked, 10_000)).toBe(
      "remove",
    );
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        { ...locked, headerHash: "99".repeat(28) },
        10_000,
      ),
    ).toThrow(/matching active removal lock/);
  });

  it("recovers the Open commitment from the indexed DA attestation datum whose hash the queue node holds", async () => {
    const commitment = challenge.record.commitment;
    const commitmentHash = SDK.daAvailabilityCommitmentHash(commitment);
    const attestation = (
      availability_commitment: SDK.DaAvailabilityCommitment,
      header_hash = hash,
    ) =>
      Data.to(
        {
          header_hash,
          availability_commitment,
          da_threshold: 1n,
          committee_signers_hash: "88".repeat(32),
          rescue_beneficiary: {
            paymentCredential: { PublicKeyCredential: ["77".repeat(28)] },
            stakeCredential: null,
          },
          attested_signers: "00".repeat(32),
          attestation_count: 1n,
        },
        SDK.DaAttestationDatum,
      );
    const other = SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: "22".repeat(28),
      headerHash: hash,
      payload: Uint8Array.of(2),
      responseGeometry: parameters.response_geometry,
    });
    const policy = "ab".repeat(28);
    const requested: string[] = [];
    const kupo =
      (matches: unknown, ok = true) =>
      async (url: string) => {
        requested.push(url);
        return { ok, status: ok ? 200 : 503, json: async () => matches };
      };
    const input = {
      kupoUrl: "http://kupo.test/",
      daAttestationPolicyId: policy,
      headerHash: hash,
      commitmentHash,
    };
    // Unrelated, undecodable and differently-committed outputs are skipped;
    // only the datum hashing to the node's commitment_hash is returned.
    await expect(
      recoverAvailabilityOpenCommitment({
        ...input,
        fetch: kupo([
          { datum: null },
          { datum: "d87980" },
          { datum: attestation(other) },
          { datum: attestation(commitment, "99".repeat(28)) },
          { datum: attestation(commitment) },
        ]),
      }),
    ).resolves.toEqual(commitment);
    expect(requested).toEqual([
      `http://kupo.test/matches/${policy}.${SDK.daAttestationAssetName(hash)}?resolve_hashes`,
    ]);
    await expect(
      recoverAvailabilityOpenCommitment({
        ...input,
        fetch: kupo([{ datum: attestation(other) }]),
      }),
    ).rejects.toThrow(/holds a commitment hashing to/);
    await expect(
      recoverAvailabilityOpenCommitment({
        ...input,
        fetch: kupo([{ datum: attestation(commitment, "99".repeat(28)) }]),
      }),
    ).rejects.toThrow(/holds a commitment hashing to/);
    await expect(
      recoverAvailabilityOpenCommitment({ ...input, fetch: kupo([]) }),
    ).rejects.toThrow(/holds a commitment hashing to/);
    await expect(
      recoverAvailabilityOpenCommitment({
        ...input,
        fetch: kupo([{ datum_hash: null }, { datum: attestation(commitment) }]),
      }),
    ).rejects.toThrow(/did not resolve datums/);
    await expect(
      recoverAvailabilityOpenCommitment({ ...input, fetch: kupo([], false) }),
    ).rejects.toThrow(/HTTP 503/);
  });

  it("sizes timeout collateral to the ledger percentage of penalty plus the timeout fee cap", () => {
    const requiredLovelace =
      ((parameters.da_slash_penalty_lovelace +
        parameters.max_timeout_fee_lovelace) *
        3n +
        1n) /
      2n;
    expect(
      availabilityTimeoutCollateralLovelace({
        parameters,
        collateralPercentage: 150,
      }),
    ).toBe(requiredLovelace);
    const coin = (lovelace: bigint): UTxO => ({
      txHash: "aa".repeat(32),
      outputIndex: 0,
      address: "actor",
      assets: { lovelace },
    });
    expect(() =>
      assertAvailabilityTimeoutCollateral({
        parameters,
        collateralPercentage: 150,
        collateral: coin(requiredLovelace),
      }),
    ).not.toThrow();
    expect(() =>
      assertAvailabilityTimeoutCollateral({
        parameters,
        collateralPercentage: 150,
        collateral: coin(requiredLovelace - 1n),
      }),
    ).toThrow(/timeout collateral holds/);
  });

  it("returns timeout queue rent to an actor address distinct from the protected challenger output", () => {
    const actor = "77".repeat(28);
    const rent = availabilityTimeoutRentRefundAddress("Preprod", actor);
    expect(rent).not.toBe(
      credentialToAddress("Preprod", { type: "Key", hash: actor }),
    );
    expect(paymentCredentialOf(rent)).toEqual({ type: "Key", hash: actor });
  });

  it("builds Open from the indexed commitment, then settles and times out from the challenge record and the DA bond pool", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const header = f.target.headerHash;
    const attested = await attestAvailability(f);
    f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
    // Apply spent the attestation output; the Kupo index still holds its
    // datum, and the command takes the commitment that hashes to the node's.
    const attestationDatum = Data.to(
      {
        header_hash: header,
        availability_commitment: attested.commitment,
        da_threshold: 1n,
        committee_signers_hash: "88".repeat(32),
        rescue_beneficiary: {
          paymentCredential: { PublicKeyCredential: [f.responderKey] },
          stakeCredential: null,
        },
        attested_signers: "00".repeat(32),
        attestation_count: 1n,
      },
      SDK.DaAttestationDatum,
    );
    const kupoRequests: string[] = [];
    vi.stubGlobal("fetch", async (url: string) => {
      kupoRequests.push(url);
      return {
        ok: true,
        status: 200,
        json: async () => [{ datum: attestationDatum }],
      };
    });
    // The command reads wall time; the emulator keeps its own clock.
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => f.emulator.now());
    try {
      const context = {
        daChallengeWindowMs: f.timing.daChallengeWindowMs,
        daAttestationPolicyId: f.contracts.daAttestation.policyId,
        kupoUrl: "http://kupo.test/",
      };
      const openingLovelace =
        parameters.challenger_bond_lovelace +
        parameters.challenge_record_lovelace +
        parameters.max_open_fee_lovelace;
      const funding = (
        await f.submit(
          "prepare command challenger funding",
          f.lucid.newTx().pay.ToAddress(f.challenger.address, {
            lovelace: openingLovelace,
          }),
          true,
        )
      ).find((utxo) => utxo.assets.lovelace === openingLovelace)!;
      const [collateral] = await f.collateralInputs([funding]);
      const label = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
      const options = {
        headerHash: header,
        collateralOutRef: label(collateral!),
        fundingOutRef: label(funding),
      };
      const snapshotNow = () =>
        SDK.fetchDaAvailabilityChallengeSnapshot(f.lucid, d, header);
      const build = (
        snapshot: SDK.DaAvailabilityChallengeSnapshot,
        action: SDK.DaAvailabilityTransactionAction,
      ) =>
        buildAvailabilityCommandTransaction(
          f.lucid,
          d,
          context,
          snapshot,
          action,
          options,
          f.challengerKey,
          new Set(),
        );
      const submitBuilt = async (built: SDK.BuiltDaAvailabilityTransaction) => {
        const signed = await built.tx.sign.withWallet().complete();
        expect(await signed.submit()).toBe(built.txId);
        f.emulator.awaitBlock(1);
        return f.lucid.utxosByOutRef(
          built.expectedOutputs.map((_, outputIndex) => ({
            txHash: built.txId,
            outputIndex,
          })),
        );
      };

      let snapshot = await snapshotNow();
      expect(snapshot.recordDatum).toBeUndefined();
      await submitBuilt(await build(snapshot, "open"));
      expect(kupoRequests).toEqual([
        `http://kupo.test/matches/${f.contracts.daAttestation.policyId}.${SDK.daAttestationAssetName(header)}?resolve_hashes`,
      ]);
      snapshot = await snapshotNow();
      const record = snapshot.recordDatum!;
      expect(record.commitment).toEqual(attested.commitment);
      expect(record.challenger).toBe(f.challengerKey);
      expect(snapshot.record!.assets.lovelace).toBe(
        parameters.challenge_record_lovelace,
      );

      f.advanceToMs(record.response_deadline + 1_000n);
      expect(
        planAvailabilityCommandAction("timeout", snapshot, Date.now()),
      ).toBe("settle");
      await submitBuilt(await build(snapshot, "settle"));
      snapshot = await snapshotNow();
      expect(
        planAvailabilityCommandAction("timeout", snapshot, Date.now()),
      ).toBe("timeout");

      const pool = snapshot.pool!;
      expect(snapshot.poolDatum).toBe("Bonded");
      const timeout = await build(snapshot, "timeout");
      // A fully backed pool's penalty pays the whole exact fee.
      expect(timeout.feeLovelace).toBe(parameters.da_slash_penalty_lovelace);
      const outputs = await submitBuilt(timeout);
      const rentAddress = availabilityTimeoutRentRefundAddress(
        f.lucid.config().network!,
        f.challengerKey,
      );
      expect(outputs.some((utxo) => utxo.address === rentAddress)).toBe(true);
      const challengerPayouts = outputs.filter(
        (utxo) => utxo.address === f.challenger.address,
      );
      expect(challengerPayouts.map((utxo) => utxo.assets)).toEqual([
        {
          lovelace:
            parameters.challenger_bond_lovelace -
            parameters.max_settlement_fee_lovelace +
            parameters.challenge_record_lovelace +
            parameters.da_bond_lovelace -
            parameters.da_slash_penalty_lovelace,
        },
      ]);
      const slashedPool = outputs.find(
        (utxo) => utxo.assets[f.poolUnit] === 1n,
      )!;
      expect(slashedPool.assets).toEqual({
        ...pool.assets,
        lovelace: pool.assets.lovelace - parameters.da_bond_lovelace,
      });
      expect(slashedPool.datum).toBe(pool.datum);
      expect((await snapshotNow()).queue).toBeUndefined();
    } finally {
      clock.mockRestore();
      vi.unstubAllGlobals();
    }
  }, 300_000);
});
