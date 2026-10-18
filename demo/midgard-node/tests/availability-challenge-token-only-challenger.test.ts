import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  availabilityPreparationAmounts,
  buildAvailabilityCommandTransaction,
  prepareAvailabilityOpeningInput,
} from "../src/commands/availability-challenge.js";
import {
  attestationHistory,
  expectPreparation,
  flipped,
  GOLD,
  label,
  makeTokenOnly,
  OPENING_LOVELACE,
  outRef,
  smallestTokenChange,
  workflowCollateral,
} from "./availability-challenge-token-only-challenger.fixture.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  assertAvailabilityRefusal,
  attestAvailability,
  AVAILABILITY_EMULATOR_PARAMETERS,
  availabilityDeployment,
  createAvailabilityEmulatorLucid,
  createAvailabilityFixture,
  openAvailability,
} from "./helpers/availability-challenge-emulator.js";

describe("token-only DA challenger", () => {
  it("prepares the opening input, collateral and working capital from token coins, then opens, settles and times out", async () => {
    const directory = await mkdtemp(join(tmpdir(), "availability-token-only-"));
    const journal = openAvailabilityOperationJournal(
      join(directory, "operations.sqlite"),
    );
    const clock = vi.spyOn(Date, "now");
    try {
      const f = await createAvailabilityFixture(1);
      clock.mockImplementation(() => f.emulator.now());
      const d = availabilityDeployment(f);
      const header = f.target.headerHash;
      const attested = await attestAvailability(f);
      const unit = await makeTokenOnly(f, [30_000_000_000n, 5_000_000n]);
      const before = await f.lucid.wallet().getUtxos();
      const context: SDK.DaAvailabilityOperationContext = {
        deploymentIdentity: "ab".repeat(32),
        actor: f.challengerKey,
        journal,
        stateQueuePolicyId: d.contracts.stateQueue.policyId,
        minimumConfirmationDepth: 10,
        transactionLimits: SDK.daAvailabilityOperationLimits(
          f.lucid,
          d.parameters,
        ),
        nowMs: () => f.emulator.now(),
        assertActuationCurrent: () => undefined,
        observe: async () => ({ status: "unspent", currentSlot: 0 }),
        submit: (cbor) => f.emulator.submitTx(cbor),
      };
      const prepared = await prepareAvailabilityOpeningInput({
        lucid: f.lucid,
        deployment: d,
        snapshot: await SDK.fetchDaAvailabilityChallengeSnapshot(
          f.lucid,
          d,
          header,
        ),
        context,
        headerHash: header,
        reservedOutRefs: new Set(journal.reservedOutRefs(f.challengerKey)),
      });
      expect(prepared.status).toBe("submitted");
      f.emulator.awaitBlock(1);
      const protocol = f.lucid.config().protocolParameters!;
      const amounts = availabilityPreparationAmounts({
        parameters,
        collateralPercentage: protocol.collateralPercentage,
        coinsPerUtxoByte: protocol.coinsPerUtxoByte,
        walletAddress: f.challenger.address,
        // The target is the only block after the root.
        remainingRemovalSteps: 1,
      });
      expect(amounts.openingLovelace).toBe(OPENING_LOVELACE);
      expect(amounts.collateralLovelace).toBe(workflowCollateral(f));
      expect(prepared).toMatchObject({
        openingLovelace: OPENING_LOVELACE.toString(),
        collateralLovelace: amounts.collateralLovelace.toString(),
        workingCapitalLovelace: amounts.workingCapitalLovelace.toString(),
        feeLovelace: parameters.max_open_fee_lovelace.toString(),
        openingOutRef: `${prepared.txHash}#0`,
        collateralOutRef: `${prepared.txHash}#1`,
        workingCapitalOutRef: `${prepared.txHash}#2`,
      });
      expectPreparation(
        journal.findTransaction(prepared.txHash)!.intent.signedCbor,
        {
          address: f.challenger.address,
          spendable: before,
          reserved: [
            OPENING_LOVELACE,
            amounts.collateralLovelace,
            amounts.workingCapitalLovelace,
          ],
          unit,
        },
      );

      const options = {
        headerHash: header,
        fundingOutRef: prepared.openingOutRef!,
        collateralOutRef: prepared.collateralOutRef!,
      };
      const snapshotNow = () =>
        SDK.fetchDaAvailabilityChallengeSnapshot(f.lucid, d, header);
      const step = async (
        snapshot: SDK.DaAvailabilityChallengeSnapshot,
        action: SDK.DaAvailabilityTransactionAction,
      ) => {
        const built = await buildAvailabilityCommandTransaction(
          f.lucid,
          d,
          {
            daChallengeWindowMs: f.timing.daChallengeWindowMs,
            daAttestationPolicyId: f.contracts.daAttestation.policyId,
            unitHistory: attestationHistory(f, attested.commitment),
          },
          snapshot,
          action,
          options,
          f.challengerKey,
          new Set(journal.reservedOutRefs(f.challengerKey)),
        );
        // Every step posts the one prepared collateral coin.
        expect(built.collateralOutRefs.map(label)).toEqual([
          prepared.collateralOutRef,
        ]);
        const signed = await built.tx.sign.withWallet().complete();
        expect(await signed.submit()).toBe(built.txId);
        f.emulator.awaitBlock(1);
        return built;
      };

      const open = await step(await snapshotNow(), "open");
      expect(open.spentOutRefs.map(label)).toContain(prepared.openingOutRef);
      let snapshot = await snapshotNow();
      expect(snapshot.recordDatum?.challenger).toBe(f.challengerKey);
      expect(snapshot.record?.assets.lovelace).toBe(
        parameters.challenge_record_lovelace,
      );
      f.advanceToMs(snapshot.recordDatum!.response_deadline + 1_000n);
      await step(snapshot, "settle");
      snapshot = await snapshotNow();
      await step(snapshot, "timeout");
      expect((await snapshotNow()).queue).toBeUndefined();
      // Collateral is never spent by a valid step; the working capital stays
      // for a longer removal path.
      for (const [ref, lovelace] of [
        [prepared.collateralOutRef!, amounts.collateralLovelace],
        [prepared.workingCapitalOutRef!, amounts.workingCapitalLovelace],
      ] as const)
        expect(
          (await f.lucid.utxosByOutRef([outRef(ref)])).map((u) => u.assets),
        ).toEqual([{ lovelace }]);
    } finally {
      clock.mockRestore();
      journal.close();
      await rm(directory, { recursive: true, force: true });
    }
  }, 300_000);

  it("refuses a preparation a token-only wallet cannot afford with the exact lovelace, and prepares at exactly that amount", async () => {
    const unit = "ab".repeat(28) + GOLD;
    const wallet = async (lovelace: bigint) => {
      const account = generateEmulatorAccountFromPrivateKey({
        lovelace,
        [unit]: 7n,
      });
      const emulator = new Emulator(
        [account],
        AVAILABILITY_EMULATOR_PARAMETERS,
      );
      const lucid = await createAvailabilityEmulatorLucid(emulator);
      lucid.selectWallet.fromPrivateKey(account.privateKey);
      const protocol = lucid.config().protocolParameters!;
      const amounts = availabilityPreparationAmounts({
        parameters,
        collateralPercentage: protocol.collateralPercentage,
        coinsPerUtxoByte: protocol.coinsPerUtxoByte,
        walletAddress: account.address,
        remainingRemovalSteps: 1,
      });
      const reserved = [
        amounts.openingLovelace,
        amounts.collateralLovelace,
        amounts.workingCapitalLovelace,
      ];
      const prepare = () =>
        SDK.buildDaAvailabilityFundingPreparationTx(lucid, {
          outputLovelace: amounts.openingLovelace,
          feeLovelace: amounts.feeLovelace,
          collateralLovelace: amounts.collateralLovelace,
          workingCapitalLovelace: amounts.workingCapitalLovelace,
          validFrom: BigInt(emulator.now()),
          validTo: BigInt(emulator.now() + 60_000),
        });
      return { account, emulator, lucid, protocol, reserved, prepare };
    };
    const refusal = async (w: Awaited<ReturnType<typeof wallet>>) => {
      const error = await w.prepare().then(
        () => undefined,
        (cause: unknown) => cause,
      );
      expect(error).toBeInstanceOf(SDK.DaAvailabilityTransactionError);
      return error as SDK.DaAvailabilityTransactionError;
    };

    const poor = await wallet(10_000_000n);
    const first = await refusal(poor);
    expect(first.reason).toBe("preparation-insufficient-lovelace");
    const required = first.detail!.requiredLovelace!;
    expect(first.detail).toEqual({
      requiredLovelace: required,
      availableLovelace: 10_000_000n,
    });
    // The requirement is the prepared coins, the fee and the smallest change
    // coin that carries the tokens and meets its own minimum ADA.
    const changeLovelace =
      required -
      poor.reserved.reduce((n, l) => n + l, 0n) -
      parameters.max_open_fee_lovelace;
    expect(changeLovelace).toBe(
      smallestTokenChange(
        poor.protocol.coinsPerUtxoByte,
        poor.account.address,
        { [unit]: 7n },
      ),
    );
    expect(first.message).toContain(required.toString());
    expect(first.message).toContain("10000000");

    const short = await refusal(await wallet(required - 1n));
    expect(short.reason).toBe("preparation-insufficient-lovelace");
    expect(short.detail).toEqual({
      requiredLovelace: required,
      availableLovelace: required - 1n,
    });

    const exact = await wallet(required);
    const before = await exact.lucid.wallet().getUtxos();
    const signed = await (await exact.prepare()).sign.withWallet().complete();
    const layout = expectPreparation(signed.toCBOR(), {
      address: exact.account.address,
      spendable: before,
      reserved: exact.reserved,
      unit,
    });
    expect(layout.outputs.slice(3)).toEqual([
      {
        address: exact.account.address,
        assets: { lovelace: changeLovelace, [unit]: 7n },
      },
    ]);
    await signed.submit();
    exact.emulator.awaitBlock(1);
  }, 120_000);

  it("refuses an Open without a prepared input, naming the input and the preparation", async () => {
    const f = await createAvailabilityFixture(1);
    const d = availabilityDeployment(f);
    const attested = await attestAvailability(f);
    const unit = await makeTokenOnly(f, [30_000_000_000n]);
    const [tokenCoin] = await f.lucid.wallet().getUtxos();
    expect(tokenCoin!.assets[unit]).toBe(10n);
    const protocol = f.lucid.config().protocolParameters!;
    const collateralLovelace =
      (parameters.max_open_fee_lovelace *
        BigInt(protocol.collateralPercentage) +
        99n) /
      100n;
    const params = (challengerFunding: UTxO) => ({
      collateralInputs: [tokenCoin!],
      feeLovelace: parameters.max_open_fee_lovelace,
      validFrom: BigInt(f.emulator.now()),
      validTo: BigInt(f.emulator.now() + 60_000),
      commitment: attested.commitment,
      queue: attested.queue,
      challengerFunding,
      challenger: f.challengerKey,
      daChallengeWindowMs: f.timing.daChallengeWindowMs,
    });
    const expected = {
      address: f.challenger.address,
      lovelace: OPENING_LOVELACE,
      bondLovelace: parameters.challenger_bond_lovelace,
      recordLovelace: parameters.challenge_record_lovelace,
      feeLovelace: parameters.max_open_fee_lovelace,
      collateralLovelace,
      collateralReturnLovelace: SDK.daAvailabilityPlainCoinMinAda(
        protocol.coinsPerUtxoByte,
        f.challenger.address,
        collateralLovelace,
      ),
    };
    for (const funding of [
      // The wallet's own token coin.
      tokenCoin!,
      // The exact amount, but carrying a token.
      { ...tokenCoin!, assets: { lovelace: OPENING_LOVELACE, [unit]: 1n } },
      // The exact amount with a datum.
      {
        ...tokenCoin!,
        assets: { lovelace: OPENING_LOVELACE },
        datum: Data.void(),
      },
      // ADA-only, one lovelace short.
      { ...tokenCoin!, assets: { lovelace: OPENING_LOVELACE - 1n } },
    ]) {
      const error = await flipped(
        SDK.buildOpenDaAvailabilityChallengeTxProgram(
          f.lucid,
          d,
          params(funding),
        ),
      );
      expect(error.reason).toBe("challenger-funding-not-exact");
      expect(error.detail).toEqual({ opening: expected });
      for (const part of [
        "exact isolated",
        "ADA-only",
        "no datum",
        `exactly ${OPENING_LOVELACE.toString()} lovelace`,
        `collateral of exactly ${collateralLovelace.toString()}`,
        "buildDaAvailabilityFundingPreparationTx",
        "availability-challenge prepare",
      ])
        expect(error.message).toContain(part);
    }
  }, 300_000);

  it("refuses at the Open withdrawal a prepared Open that spends an extra wallet coin, and lands without it", async () => {
    const f = await createAvailabilityFixture(1);
    const attested = await attestAvailability(f);
    const unit = await makeTokenOnly(f, [30_000_000_000n]);
    const before = await f.lucid.wallet().getUtxos();
    const collateral = workflowCollateral(f);
    const signed = await (
      await SDK.buildDaAvailabilityFundingPreparationTx(f.lucid, {
        outputLovelace: OPENING_LOVELACE,
        feeLovelace: parameters.max_open_fee_lovelace,
        collateralLovelace: collateral,
        validFrom: BigInt(f.emulator.now()),
        validTo: BigInt(f.emulator.now() + 60_000),
      })
    ).sign
      .withWallet()
      .complete();
    expectPreparation(signed.toCBOR(), {
      address: f.challenger.address,
      spendable: before,
      reserved: [OPENING_LOVELACE, collateral],
      unit,
    });
    const txHash = await signed.submit();
    f.emulator.awaitBlock(1);
    const [funding, , change] = await f.lucid.utxosByOutRef(
      [0, 1, 2].map((outputIndex) => ({ txHash, outputIndex })),
    );
    expect(funding!.assets).toEqual({ lovelace: OPENING_LOVELACE });
    expect(change!.assets[unit]).toBe(10n);
    const open = await openAvailability(f, attested, {}, { funding });
    // OpenChallenge admits exactly two inputs, the opening input and the
    // queue node; that is its first conjunct.
    await assertAvailabilityRefusal(
      open.build({ extraInput: change }),
      {
        purpose: "withdraw",
        script: "availability-challenge open withdrawal",
      },
      f.scriptNames,
    );
    const landed = await open.submit();
    expect(landed.record.assets.lovelace).toBe(
      parameters.challenge_record_lovelace,
    );
    expect(await f.lucid.utxosByOutRef([funding!])).toEqual([]);
  }, 300_000);
});
