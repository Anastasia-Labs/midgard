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
  availabilityDeployment,
  type AvailabilityFixture,
  createAvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";

const deployment = availabilityDeployment;
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
  // G9/H1: plain-ADA collateral in at most three inputs, covering the ledger
  // collateral percentage of the exact fee.
  expect(built.collateralOutRefs.length).toBeGreaterThanOrEqual(1);
  expect(built.collateralOutRefs.length).toBeLessThanOrEqual(3);
  expect(
    built.collateralOutRefs.reduce((t, u) => t + u.assets.lovelace, 0n),
  ).toBeGreaterThanOrEqual((built.feeLovelace * 150n + 99n) / 100n);
  for (const c of built.collateralOutRefs)
    expect(Object.keys(c.assets)).toEqual(["lovelace"]);

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
  /** A publication's range must close by the challenge's response deadline. */
  responseDeadline?: bigint,
): Promise<SDK.DaAvailabilityTransactionResources> => {
  const collateralInputs = await f.collateralInputs();
  const validFrom = BigInt(f.emulator.now());
  const validTo =
    responseDeadline !== undefined &&
    responseDeadline + 1n < validFrom + 60_000n
      ? responseDeadline + 1n
      : validFrom + 60_000n;
  return { collateralInputs, feeLovelace, validFrom, validTo };
};
/** The timeout derives its own exact fee: resources without one. */
const timeoutResources = async (f: AvailabilityFixture) => {
  const { feeLovelace: _fee, ...rest } = await resources(f, 1n);
  return rest;
};
const OPEN_FUNDING_LOVELACE =
  parameters.challenger_bond_lovelace +
  parameters.challenge_record_lovelace +
  parameters.max_open_fee_lovelace;
const fundChallenger = async (f: AvailabilityFixture, name: string) => {
  f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
  const funding = await f.submit(
    name,
    f.lucid.newTx().pay.ToAddress(f.challenger.address, {
      lovelace: OPEN_FUNDING_LOVELACE,
    }),
    true,
  );
  return funding.find((u) => u.assets.lovelace === OPEN_FUNDING_LOVELACE)!;
};
const recordOf = (s: SDK.DaAvailabilityChallengeSnapshot) => {
  if (!s.recordDatum) throw new Error("Expected a challenge record");
  return s.recordDatum;
};
const open = async (
  f: AvailabilityFixture,
  d: SDK.DaAvailabilityDeployment,
) => {
  const attested = await attestAvailability(f);
  const challengerFunding = await fundChallenger(
    f,
    "prepare challenger resources",
  );
  const p = {
    ...(await resources(f, parameters.max_open_fee_lovelace)),
    commitment: attested.commitment,
    queue: attested.queue,
    challengerFunding,
    challenger: f.challengerKey,
    daChallengeWindowMs: f.timing.daChallengeWindowMs,
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
  const outputs = await submit(
    f,
    await Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(f.lucid, d, p),
    ),
  );
  expect(outputs[0]!.assets.lovelace).toBe(
    parameters.challenge_record_lovelace,
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
      const attested = await attestAvailability(f);
      const challengerFunding = await fundChallenger(
        f,
        "prepare maximum challenger resources",
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
                commitment: attested.commitment,
                queue: attested.queue,
                challenger: f.challengerKey,
                challengerFunding,
                daChallengeWindowMs: f.timing.daChallengeWindowMs,
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
      const record = recordOf(snapshot);
      const [tranche] = SDK.planDaAvailabilityPublications({
        commitment: record.commitment,
        payload: f.payload,
        challengeAssetName: record.challenge_asset_name,
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
                      record.response_deadline,
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
    const poolBefore = await f.getPool();
    let snapshot = await open(f, d);
    const b = recordOf(snapshot);
    const publications = SDK.planDaAvailabilityPublications({
      commitment: b.commitment,
      payload: f.payload,
      challengeAssetName: b.challenge_asset_name,
    })[0]!.publications;
    let thread = snapshot.tranches[0]!.utxo;
    let carrier: UTxO | undefined;
    for (const publication of publications) {
      const p = {
        ...(await resources(
          f,
          parameters.max_publication_fee_lovelace,
          b.response_deadline,
        )),
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
          record: snapshot.record!,
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
          record: snapshot.record!,
          terminal: terminal!,
          queue: snapshot.queue!.utxo,
        }),
      ),
    );
    expect(outputs).toHaveLength(2);
    expect(outputs[1]!.address).toBe(f.challenger.address);
    expect(outputs[1]!.assets.lovelace).toBe(
      parameters.challenger_bond_lovelace -
        2n * parameters.max_publication_fee_lovelace -
        parameters.max_settlement_fee_lovelace -
        parameters.max_close_fee_lovelace +
        parameters.challenge_record_lovelace,
    );
    expect((await f.getPool()).assets).toEqual(poolBefore.assets);
    const closed = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(closed.record).toBeUndefined();
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
          record: s.record!,
          terminal: s.terminal!,
          thread: t.utxo,
        }),
      ),
    ).rejects.toThrow(/deadline/);
    const b = recordOf(s);
    f.advanceToMs(b.response_deadline + 1_000n);
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          record: s.record!,
          terminal: s.terminal!,
          thread: t.utxo,
        }),
      ),
    );
    const pool = await f.getPool();
    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
        ...(await timeoutResources(f)),
        record: s.record!,
        terminal: terminal!,
        pool,
        queue: s.queue!.utxo,
        confirmedState: s.confirmedState.utxo,
        correctionLock: s.correctionLock,
        headerHash: f.target.headerHash,
        challengeAssetName: b.challenge_asset_name,
        rentRefundAddress: f.responder.address,
      }),
    );
    // A fully backed pool's penalty pays the whole fee: c = 0.
    expect(built.feeLovelace).toBe(parameters.da_slash_penalty_lovelace);
    expect(built.timeoutFeePartLovelace).toBe(
      parameters.da_slash_penalty_lovelace,
    );
    const outputs = await submit(f, built);
    expect(outputs[2]!.address).toBe(f.challenger.address);
    expect(outputs[2]!.assets.lovelace).toBe(
      parameters.challenger_bond_lovelace -
        parameters.max_settlement_fee_lovelace +
        parameters.challenge_record_lovelace +
        parameters.da_bond_lovelace -
        parameters.da_slash_penalty_lovelace,
    );
    expect(outputs[3]!.assets).toEqual({
      ...pool.assets,
      lovelace: pool.assets.lovelace - parameters.da_bond_lovelace,
    });
    expect(outputs[3]!.datum).toBe(pool.datum);
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
    const b = recordOf(s);
    f.advanceToMs(b.response_deadline + 1_000n);
    const [terminal] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(f.lucid, d, {
          ...(await resources(f, parameters.max_settlement_fee_lovelace)),
          record: s.record!,
          terminal: s.terminal!,
          thread: s.tranches[0]!.utxo,
        }),
      ),
    );
    await submit(
      f,
      await Effect.runPromise(
        SDK.buildTimeoutDaAvailabilityChallengeTxProgram(f.lucid, d, {
          ...(await timeoutResources(f)),
          record: s.record!,
          terminal: terminal!,
          pool: await f.getPool(),
          queue: s.queue!.utxo,
          confirmedState: s.confirmedState.utxo,
          descendant: s.descendant!.utxo,
          correctionLock: s.correctionLock,
          headerHash: f.target.headerHash,
          challengeAssetName: b.challenge_asset_name,
          rentRefundAddress: f.responder.address,
        }),
      ),
    );
    s = await SDK.fetchDaAvailabilityChallengeSnapshot(
      f.lucid,
      d,
      f.target.headerHash,
    );
    expect(s.record).toBeUndefined();
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
        challengeAssetName: b.challenge_asset_name,
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
    const b = recordOf(s);
    const publication = SDK.planDaAvailabilityPublications({
      commitment: b.commitment,
      payload: f.payload,
      challengeAssetName: b.challenge_asset_name,
    })[0]!.publications[0]!;
    const [thread, carrier] = await submit(
      f,
      await Effect.runPromise(
        SDK.buildPublishDaAvailabilityChunkTxProgram(f.lucid, d, {
          ...(await resources(
            f,
            parameters.max_publication_fee_lovelace,
            b.response_deadline,
          )),
          thread: s.tranches[0]!.utxo,
          publication,
        }),
      ),
    );
    f.advanceToMs(b.response_deadline + 1_000n);
    const settlement = {
      ...(await resources(f, parameters.max_settlement_fee_lovelace)),
      record: s.record!,
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
          ...(await timeoutResources(f)),
          record: s.record!,
          terminal: terminal!,
          pool: await f.getPool(),
          queue: s.queue!.utxo,
          confirmedState: s.confirmedState.utxo,
          correctionLock: s.correctionLock,
          headerHash: f.target.headerHash,
          challengeAssetName: b.challenge_asset_name,
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
