import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";
import {
  attestAvailability,
  type AvailabilityFixture,
  createAvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

const deployment = (f: AvailabilityFixture): SDK.DaAvailabilityDeployment => {
  const names = [
    "availability-challenge spending",
    "availability-challenge minting",
    ...(["open", "settle", "close", "timeout"] as const).map(
      (arm) => `availability-challenge ${arm} withdrawal`,
    ),
    "state-queue spending",
    "state-queue minting",
    "state-queue unavailable-timeout withdrawal",
    "correction-lock spending",
  ];
  const referenceScripts = Object.fromEntries(
    names.map((name) => [name, f.reference(name)]),
  );
  const unit = Object.keys(referenceScripts[names[0]!]!.assets).find(
    (unit) => unit !== "lovelace",
  )!;
  return {
    contracts: f.contracts,
    hubOraclePolicyId: f.contracts.hubOracle.policyId,
    referenceScriptAuthPolicyId: unit.slice(0, 56),
    parameters,
    referenceScripts,
    hubOracleRefInput: f.hubOracleRefInput,
  };
};
const submit = async (
  f: AvailabilityFixture,
  built: SDK.BuiltDaAvailabilityTransaction,
) => {
  const signed = await built.tx.sign.withWallet().complete();
  expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(15872);
  expect(signed.toHash()).toBe(built.txId);
  const redeemers = CML.Transaction.from_cbor_hex(signed.toCBOR())
    .witness_set()
    .redeemers()
    ?.to_flat_format();
  let memory = 0n,
    steps = 0n;
  for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
    memory += redeemers!.get(i).ex_units().mem();
    steps += redeemers!.get(i).ex_units().steps();
  }
  expect(memory).toBeGreaterThan(0n);
  expect(memory).toBeLessThanOrEqual(13_200_000n);
  expect(steps).toBeLessThanOrEqual(8_000_000_000n);
  expect(built.collateralOutRefs).toHaveLength(1);

  const id = await signed.submit();
  f.emulator.awaitBlock(1);
  return f.lucid.utxosByOutRef(
    built.expectedOutputs.map((_, outputIndex) => ({
      txHash: id,
      outputIndex,
    })),
  );
};
const resources = async (
  f: AvailabilityFixture,
  feeLovelace: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  const collateralInputs = (await f.lucid.wallet().getUtxos())
    .filter((u) => u.assets.lovelace === 10_000_000n && !u.datum)
    .slice(0, 1);
  return {
    collateralInputs,
    feeLovelace,
    validFrom: BigInt(f.emulator.now()),
    validTo: BigInt(f.emulator.now() + 60_000),
  };
};
const open = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => {
  const bonded = await attestAvailability(f);
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const funding = await f.submit(
    "prepare challenger resources",
    f.lucid
      .newTx()
      .pay.ToAddress(f.challenger.address, {
        lovelace:
          parameters.challenger_bond_lovelace +
          parameters.max_open_fee_lovelace,
      })
      .pay.ToAddress(f.challenger.address, { lovelace: 10_000_000n }),
    true,
  );
  const p = {
    ...(await resources(f, parameters.max_open_fee_lovelace)),
    bond: {
      ...bonded.bond,
      datum: SDK.encodeDaAvailabilityBondDatum(
        Data.from(bonded.bond.datum!, SDK.DaAvailabilityBondDatum),
      ),
    },
    queue: bonded.queue,
    challengerFunding: funding.find(
      (u) =>
        u.assets.lovelace ===
        parameters.challenger_bond_lovelace + parameters.max_open_fee_lovelace,
    )!,
    challenger: f.challengerKey,
  };
  await expect(
    Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
        ...p,
        challengerFunding: {
          ...p.challengerFunding,
          assets: { lovelace: p.challengerFunding.assets.lovelace + 1n },
        },
      }),
    ),
  ).rejects.toThrow(/exact isolated/);
  await expect(
    Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(
        f.lucid,
        {
          ...d,
          referenceScripts: {
            ...d.referenceScripts,
            "availability-challenge open withdrawal":
              d.referenceScripts["availability-challenge close withdrawal"]!,
          },
        },
        p,
      ),
    ),
  ).rejects.toThrow(/Unauthentic reference script/);
  await submit(
    f,
    await Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, p),
    ),
  );
  return SDK.fetchDaAvailabilityChallengeSnapshot(
    f.lucid,
    d,
    f.target.headerHash,
  );
};
describe("production SDK availability builders", () => {
  it("executes maximum-commitment opening and resumes a signed maximum-proof publication within the ledger size limit", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "availability-sdk-maximum-"),
    );
    const journalPath = join(directory, "operations.sqlite");
    let journal = openAvailabilityOperationJournal(journalPath);
    try {
      const f = await createAvailabilityFixture(64 * 1024 * 1024);
      const d = deployment(f);
      const bonded = await attestAvailability(f);
      f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
      const funding = await f.submit(
        "prepare maximum challenger resources",
        f.lucid
          .newTx()
          .pay.ToAddress(f.challenger.address, {
            lovelace:
              parameters.challenger_bond_lovelace +
              parameters.max_open_fee_lovelace,
          })
          .pay.ToAddress(f.challenger.address, { lovelace: 10_000_000n }),
        true,
      );
      const submissions: { action: string; txHash: string; cbor: string }[] =
        [];
      let action = "open";
      let ambiguousPublication: { txHash: string; cbor: string } | undefined;
      const context = (): SDK.DaAvailabilityOperationContext => ({
        deploymentIdentity: "ab".repeat(32),
        actor: f.challengerKey,
        stateQueuePolicyId: d.contracts.stateQueue.policyId,
        journal,
        minimumConfirmationDepth: 100,
        transactionLimits: SDK.daAvailabilityOperationLimits(
          f.lucid,
          d.parameters,
        ),
        nowMs: () => f.emulator.now(),
        assertActuationCurrent: () => undefined,
        observe: SDK.createDaAvailabilityOperationObserver({
          lucid: {
            ...f.lucid,
            transactionStatus: async (txHash) => {
              const status = await f.lucid.transactionStatus(txHash);
              return status.status === "confirmed"
                ? {
                    ...status,
                    confirmation: {
                      ...status.confirmation,
                      blockHash: createHash("sha256")
                        .update(`emulator:${status.confirmation.blockHeight}`)
                        .digest("hex"),
                    },
                  }
                : status;
            },
          },
          readBoundary: async () => ({
            pointId: `emulator:${f.emulator.slot}`,
            slot: f.emulator.slot,
          }),
        }),
        submit: async (cbor) => {
          const transaction = CML.Transaction.from_cbor_hex(cbor);
          const redeemers = transaction
            .witness_set()
            .redeemers()
            ?.to_flat_format();
          let memory = 0n,
            steps = 0n;
          for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
            memory += redeemers!.get(i).ex_units().mem();
            steps += redeemers!.get(i).ex_units().steps();
          }
          expect(cbor.length / 2).toBeLessThanOrEqual(16_384);
          expect(memory).toBeGreaterThan(0n);
          expect(memory).toBeLessThanOrEqual(13_200_000n);
          expect(steps).toBeLessThanOrEqual(8_000_000_000n);
          const txHash = await f.emulator.submitTx(cbor);
          submissions.push({ action, txHash, cbor });
          f.emulator.awaitBlock(1);
          if (
            action === "publish" &&
            cbor.length / 2 >= 15_900 &&
            ambiguousPublication === undefined
          ) {
            ambiguousPublication = { txHash, cbor };
            throw new Error("maximum publication accepted; response lost");
          }
          return txHash;
        },
      });
      const opened = await SDK.runDaAvailabilityOperation(context(), {
        action: "open",
        headerHash: f.target.headerHash,
        build: async () =>
          (
            await Effect.runPromise(
              SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, {
                ...(await resources(f, parameters.max_open_fee_lovelace)),
                bond: bonded.bond,
                queue: bonded.queue,
                challenger: f.challengerKey,
                challengerFunding: funding.find(
                  (input) =>
                    input.assets.lovelace ===
                    parameters.challenger_bond_lovelace +
                      parameters.max_open_fee_lovelace,
                )!,
              }),
            )
          ).tx,
      });
      expect(opened.status).toBe("submitted");
      expect(
        CML.Transaction.from_cbor_hex(submissions[0]!.cbor)
          .body()
          .outputs()
          .len(),
      ).toBe(19);
      await SDK.reconcileDaAvailabilityOperations(context());
      let snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        d,
        f.target.headerHash,
      );
      expect(snapshot.tranches).toHaveLength(16);
      const bond = snapshot.bondDatum!;
      if (!("ChallengedBond" in bond))
        throw new Error("Expected maximum challenged bond");
      const [tranche] = SDK.planDaAvailabilityPublications({
        commitment: bond.ChallengedBond.commitment,
        payload: f.payload,
        challengeAssetName: bond.ChallengedBond.challenge_asset_name,
      });
      expect(tranche!.publications[0]!.chunk_byte_length).toBe(14_020n);
      expect(
        tranche!.publications[0]!.chunk_siblings.length,
      ).toBeGreaterThanOrEqual(8);
      action = "publish";
      let publishedCount = 0;
      for (const publication of tranche!.publications.slice(0, 25)) {
        const current = snapshot.tranches[0]!;
        try {
          const result = await SDK.runDaAvailabilityOperation(context(), {
            action: "publish",
            headerHash: f.target.headerHash,
            build: async () =>
              (
                await Effect.runPromise(
                  SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
                    ...(await resources(
                      f,
                      parameters.max_publication_fee_lovelace,
                    )),
                    thread: current.utxo,
                    previousCarrier: current.carrier,
                    publication,
                  }),
                )
              ).tx,
          });
          expect(result.status).toBe("submitted");
          publishedCount += 1;
        } catch (cause) {
          expect(cause).toBeInstanceOf(Error);
          expect((cause as Error).message).toContain(
            "maximum publication accepted; response lost",
          );
          publishedCount += 1;
          break;
        }
        await SDK.reconcileDaAvailabilityOperations(context());
        snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
          f.lucid,
          d,
          f.target.headerHash,
        );
      }
      expect(ambiguousPublication).toBeDefined();
      expect(ambiguousPublication!.cbor.length / 2).toBeGreaterThan(15_872);
      const submittedBeforeRestart = submissions.length;
      journal.close();
      journal = openAvailabilityOperationJournal(journalPath);
      const replacement = vi.fn(async () => {
        throw new Error(
          "must recover persisted maximum transaction without replacement",
        );
      });
      const recovered = await SDK.runDaAvailabilityOperation(context(), {
        action: "publish",
        headerHash: f.target.headerHash,
        build: replacement,
      });
      expect(recovered.status).toBe("included");
      expect(recovered.txHash).toBe(ambiguousPublication!.txHash);
      expect(submissions).toHaveLength(submittedBeforeRestart);
      expect(replacement).not.toHaveBeenCalled();
      const persisted = journal
        .unfinalized(context().deploymentIdentity, context().actor)
        .find((record) => record.intent.txHash === recovered.txHash);
      expect(persisted?.intent.signedCbor).toBe(ambiguousPublication!.cbor);
      const resumed = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        d,
        f.target.headerHash,
      );
      expect(resumed.tranches).toHaveLength(16);
      expect(resumed.tranches[0]!.carrier?.txHash).toBe(recovered.txHash);
      const continued = Data.from(
        resumed.tranches[0]!.utxo.datum!,
        SDK.DaAvailabilityTrancheDatum,
      );
      if (!("Active" in continued))
        throw new Error("Expected resumable active tranche");
      expect(continued.Active.next_offset).toBe(
        BigInt(publishedCount) * 14_020n,
      );
      process.stdout.write(
        `${JSON.stringify({ scenario: "sdk-executor-maximum", openingSignedBytes: submissions[0]!.cbor.length / 2, maximumPublicationSignedBytes: ambiguousPublication!.cbor.length / 2, publishedChunks: publishedCount, resumedOffset: continued.Active.next_offset.toString() })}\n`,
      );
    } finally {
      journal.close();
      await rm(directory, { recursive: true, force: true });
    }
  }, 180_000);

  it("builds authenticated opening, exact carriers, ordered settlement and close with live local evaluation", async () => {
    const f = await createAvailabilityFixture();
    const d = deployment(f);
    let snapshot = await open(f, d);
    const b = snapshot.bondDatum!;
    if (!("ChallengedBond" in b)) throw new Error("Expected challenged bond");
    const publications = SDK.planDaAvailabilityPublications({
      commitment: b.ChallengedBond.commitment,
      payload: f.payload,
      challengeAssetName: b.ChallengedBond.challenge_asset_name,
    })[0]!.publications;
    let thread = snapshot.tranches[0]!.utxo;
    let carrier: UTxO | undefined;
    for (const publication of publications) {
      const p = {
        ...(await resources(f, parameters.max_publication_fee_lovelace)),
        thread,
        previousCarrier: carrier,
        publication,
      };
      if (!carrier)
        await expect(
          Effect.runPromise(
            SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
              ...p,
              feeLovelace: 1n,
            }),
          ),
        ).rejects.toThrow(/Balance Insufficient|fee/i);
      if (carrier)
        await expect(
          Effect.runPromise(
            SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
              ...p,
              previousCarrier: {
                ...carrier,
                outputIndex: carrier.outputIndex + 1,
              },
            }),
          ),
        ).rejects.toThrow(/exact latest carrier/);
      const outputs = await submit(
        f,
        await Effect.runPromise(
          SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, p),
        ),
      );
      thread = outputs[0]!;
      carrier = outputs[1]!;
    }
    snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(snapshot.tranches[0]!.carrier?.txHash).toBe(carrier!.txHash);
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          bond: snapshot.bond!,
          terminal: snapshot.terminal!,
          thread,
          carrier,
        }),
      ),
    );
    const outputs = await submit(
      f,
      await Effect.runPromise(
        SDK.buildCloseDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_close_fee_lovelace)),
          bond: snapshot.bond!,
          terminal: terminal!,
          queue: snapshot.queue!.utxo,
        }),
      ),
    );
    expect(outputs[1]!.assets.lovelace).toBe(parameters.da_bond_lovelace);
    expect(outputs[2]!.assets.lovelace).toBe(
      parameters.challenger_bond_lovelace -
        2n * parameters.max_publication_fee_lovelace -
        parameters.max_settlement_fee_lovelace -
        parameters.max_close_fee_lovelace,
    );
    const closed = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(closed.bond).toBeUndefined();
    expect(closed.tranches).toHaveLength(0);
  }, 180_000);
  it("settles expired state and atomically removes the unavailable head", async () => {
    const f = await createAvailabilityFixture(1),
      d = deployment(f);
    const s = await open(f, d);
    const t = s.tranches[0]!;
    await expect(
      Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          bond: s.bond!,
          terminal: s.terminal!,
          thread: t.utxo,
        }),
      ),
    ).rejects.toThrow(/deadline/);
    const b = s.bondDatum!;
    if (!("ChallengedBond" in b)) throw new Error("Expected challenged bond");
    f.emulator.awaitSlot(
      Math.ceil(
        (Number(b.ChallengedBond.response_deadline) - f.emulator.now()) / 1000,
      ) + 1,
    );
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          bond: s.bond!,
          terminal: s.terminal!,
          thread: t.utxo,
        }),
      ),
    );
    const outputs = await submit(
      f,
      await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_timeout_fee_lovelace)),
          bond: s.bond!,
          terminal: terminal!,
          queue: s.queue!.utxo,
          confirmedState: s.confirmedState.utxo,
          correctionLock: s.correctionLock,
          headerHash: f.target.headerHash,
          challengeAssetName: b.ChallengedBond.challenge_asset_name,
          rentRefundAddress: f.responder.address,
        }),
      ),
    );
    expect(outputs[2]!.assets.lovelace).toBe(parameters.da_bond_lovelace);
    const removed = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(removed.queue).toBeUndefined();
  }, 180_000);
  it("resumes a locked nonresponding head through descendant pruning and final removal", async () => {
    const f = await createAvailabilityFixture(1, 2),
      d = deployment(f);
    let s = await open(f, d);
    const b = s.bondDatum!;
    if (!("ChallengedBond" in b)) throw new Error("Expected challenge");
    f.emulator.awaitSlot(
      Math.ceil(
        (Number(b.ChallengedBond.response_deadline) - f.emulator.now()) / 1000,
      ) + 1,
    );
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          bond: s.bond!,
          terminal: s.terminal!,
          thread: s.tranches[0]!.utxo,
        }),
      ),
    );
    await submit(
      f,
      await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_timeout_fee_lovelace)),
          bond: s.bond!,
          terminal: terminal!,
          queue: s.queue!.utxo,
          confirmedState: s.confirmedState.utxo,
          descendant: s.descendant!.utxo,
          correctionLock: s.correctionLock,
          headerHash: f.target.headerHash,
          challengeAssetName: b.ChallengedBond.challenge_asset_name,
          rentRefundAddress: f.responder.address,
        }),
      ),
    );
    s = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(s.bond).toBeUndefined();
    expect(s.descendant).toBeDefined();
    const step = async (prune: boolean) => {
      const r = await resources(f, parameters.max_timeout_fee_lovelace);
      const funding = (await f.lucid.wallet().getUtxos()).find(
        (u) =>
          u.assets.lovelace >
            parameters.max_timeout_fee_lovelace + 2_000_000n &&
          !r.collateralInputs.some(
            (c) => c.txHash === u.txHash && c.outputIndex === u.outputIndex,
          ),
      )!;
      const p = {
        ...r,
        queue: s.queue!.utxo,
        confirmedState: s.confirmedState.utxo,
        descendant: s.descendant?.utxo,
        correctionLock: s.correctionLock,
        headerHash: f.target.headerHash,
        challengeAssetName: b.ChallengedBond.challenge_asset_name,
        rentRefundAddress: f.responder.address,
        feeFunding: funding,
      };
      await submit(
        f,
        await Effect.runPromise(
          (prune
            ? SDK.buildPruneDaUnavailableBlockDescendantTxProgram
            : SDK.buildRemoveDaUnavailableHeadTxProgram)(f.lucid, d, p),
        ),
      );
      s = await SDK.fetchDaAvailabilityChallengeSnapshot(
        f.lucid,
        d,
        f.target.headerHash,
      );
    };
    await step(true);
    expect(s.descendant).toBeUndefined();
    expect(s.queue).toBeDefined();
    await step(false);
    expect(s.queue).toBeUndefined();
    expect(s.confirmedState.datum.next).toBe("Empty");
  }, 180_000);
  it("settles a partially answered tranche with its exact latest carrier before timeout", async () => {
    const f = await createAvailabilityFixture(),
      d = deployment(f);
    const s = await open(f, d);
    const b = s.bondDatum!;
    if (!("ChallengedBond" in b)) throw new Error("Expected challenge");
    const publication = SDK.planDaAvailabilityPublications({
      commitment: b.ChallengedBond.commitment,
      payload: f.payload,
      challengeAssetName: b.ChallengedBond.challenge_asset_name,
    })[0]!.publications[0]!;
    const [thread, carrier] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_publication_fee_lovelace)),
          thread: s.tranches[0]!.utxo,
          publication,
        }),
      ),
    );
    f.emulator.awaitSlot(
      Math.ceil(
        (Number(b.ChallengedBond.response_deadline) - f.emulator.now()) / 1000,
      ) + 1,
    );
    const settlement = {
      ...(await resources(f, parameters.max_settlement_fee_lovelace)),
      bond: s.bond!,
      terminal: s.terminal!,
      thread: thread!,
      carrier: carrier!,
    };
    await expect(
      Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...settlement,
          carrier: undefined,
        }),
      ),
    ).rejects.toThrow(/exact latest carrier/);
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, settlement),
      ),
    );
    await submit(
      f,
      await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_timeout_fee_lovelace)),
          bond: s.bond!,
          terminal: terminal!,
          queue: s.queue!.utxo,
          confirmedState: s.confirmedState.utxo,
          correctionLock: s.correctionLock,
          headerHash: f.target.headerHash,
          challengeAssetName: b.ChallengedBond.challenge_asset_name,
          rentRefundAddress: f.responder.address,
        }),
      ),
    );
    expect(
      await f.lucid.utxosAt(
        d.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
  }, 180_000);
});
