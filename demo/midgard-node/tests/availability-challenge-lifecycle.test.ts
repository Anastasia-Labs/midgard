import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  TEST_AVAILABILITY_CHALLENGE,
  TEST_AVAILABILITY_PARAMETERS,
} from "./helpers/availability-challenge.js";
import {
  advanceAvailabilityDeadline,
  assertAvailabilityRefusal,
  attestAvailability,
  type AvailabilityFixture,
  buildAvailabilityClose,
  buildAvailabilityPublication,
  buildAvailabilitySettlement,
  buildAvailabilityTimeout,
  createAvailabilityFixture,
  openAvailability,
  reportAvailabilityScenario,
} from "./helpers/availability-challenge-emulator.js";

// One emulator block: the fixture's submit advances the ledger by
// `emulator.awaitBlock(1)`, twenty one-second slots.
const EMULATOR_BLOCK_MS = 20_000;
const SMALL_RESPONSE_WINDOW_MS = BigInt(
  TEST_AVAILABILITY_CHALLENGE.responseClasses.smallResponseWindowMs,
);
const FULL_RESPONSE_WINDOW_MS =
  TEST_AVAILABILITY_CHALLENGE.responseClasses.fullResponseWindowMs;
const MAX_VALIDITY_RANGE_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.max_validity_range_ms,
);
const P = TEST_AVAILABILITY_PARAMETERS;
/** Timeout slash of a pool holding at least `da_bond` of backing (c = 0). */
const FULL_SLASH_PAYOUT = P.da_bond_lovelace - P.da_slash_penalty_lovelace;

const txFee = (f: AvailabilityFixture, name: string) =>
  f.measurements.find((m) => m.name === name)?.fee;

// The full-response scenario lands 301 publications and one settlement between
// the two tranches, one block each, after the open; every one must land before
// the shared response deadline.
const FULL_RESPONSE_BLOCKS = 301 + 1;
const FULL_RESPONSE_FITS_WINDOW =
  FULL_RESPONSE_BLOCKS * EMULATOR_BLOCK_MS < FULL_RESPONSE_WINDOW_MS;

/**
 * The widest backdated range a challenger can give an open: the lower bound
 * the maximum validity range before the upper one. The upper bound sits two
 * blocks ahead, because `openAvailability` first lands a funding transaction
 * and the ledger admits the open only strictly before its upper bound. Waits
 * first if the lower bound would precede the Lucid instance's zero time.
 */
const OPEN_UPPER_LEAD_MS = 2n * BigInt(EMULATOR_BLOCK_MS);
const widestBackdatedOpenValidity = (f: AvailabilityFixture) => {
  const earliest = BigInt(f.lucid.slotToUnixTime(0));
  const deficit =
    earliest -
    (BigInt(f.emulator.now()) + OPEN_UPPER_LEAD_MS - MAX_VALIDITY_RANGE_MS);
  if (deficit > 0n) f.emulator.awaitSlot(Number((deficit + 999n) / 1_000n));
  const validTo = BigInt(f.emulator.now()) + OPEN_UPPER_LEAD_MS;
  return { validFrom: validTo - MAX_VALIDITY_RANGE_MS, validTo };
};

describe("availability challenge real ledger lifecycle under Van Rossem limits", () => {
  it("attests against the pooled bond, opens, publishes ordered carriers, settles and closes with exact refunds", async () => {
    const f = await createAvailabilityFixture();
    const poolBefore = await f.getPool();
    const attested = await attestAvailability(f, {
      refuseCommitmentPreimageMismatch: true,
    });
    const open = await openAvailability(f, attested);
    await assertAvailabilityRefusal(open.build({ omitSigner: true }), {
      purpose: "withdraw",
      script: "availability-challenge open withdrawal",
    });
    await assertAvailabilityRefusal(open.build({ omitYield: true }), {
      purpose: "mint",
      script: "availability-challenge minting",
    });
    await assertAvailabilityRefusal(open.build({ wrongYield: true }), {
      purpose: "mint",
      script: "availability-challenge minting",
    });
    // A commitment other than the preimage of the node's commitment_hash.
    const [first, ...rest] = f.commitment.tranche_descriptors;
    await assertAvailabilityRefusal(
      open.build({
        commitment: {
          ...f.commitment,
          tranche_descriptors: [
            { ...first!, chunk_commitment: "00".repeat(32) },
            ...rest,
          ],
        },
      }),
      { purpose: "withdraw", script: "availability-challenge open withdrawal" },
    );
    const state = await open.submit();
    expect(state.record.assets.lovelace).toBe(P.challenge_record_lovelace);
    f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
    const [tranche] = SDK.planDaAvailabilityPublications({
      commitment: f.commitment,
      payload: f.payload,
      challengeAssetName: open.plan.challengeAssetName,
    });
    expect(tranche?.publications).toHaveLength(2);
    let thread = state.threads[0]!;
    let carrier: UTxO | undefined;
    await assertAvailabilityRefusal(
      buildAvailabilityPublication(
        f,
        thread,
        tranche!.publications[0]!,
        undefined,
        { badChunk: true },
      ),
      { purpose: "spend", script: "availability-challenge spending" },
    );
    for (const publication of tranche!.publications) {
      if (carrier)
        await assertAvailabilityRefusal(
          buildAvailabilityPublication(f, thread, publication),
          { purpose: "spend", script: "availability-challenge spending" },
        );
      const outputs = await f.submit(
        `publish chunk ${publication.chunk_index}`,
        buildAvailabilityPublication(f, thread, publication, carrier),
      );
      thread = outputs[0]!;
      carrier = outputs[1]!;
    }
    expect(
      Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum),
    ).toHaveProperty("Receipt");
    f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
    const [terminal] = await f.submit(
      "settle published tranche",
      buildAvailabilitySettlement(
        f,
        open,
        state.record,
        state.terminal,
        thread,
        carrier,
      ),
    );
    await assertAvailabilityRefusal(
      buildAvailabilityClose(f, open, state.record, state.queue, terminal!, {
        redirectRefund: true,
      }),
      {
        purpose: "withdraw",
        script: "availability-challenge close withdrawal",
      },
    );
    const outputs = await f.submit(
      "close published challenge",
      buildAvailabilityClose(f, open, state.record, state.queue, terminal!),
    );
    expect(outputs).toHaveLength(2);
    // The one challenger refund returns the reserve less every fee, and the
    // burned record's lovelace.
    expect(outputs[1]!.address).toBe(f.challenger.address);
    expect(outputs[1]!.assets.lovelace).toBe(
      P.challenger_bond_lovelace -
        2n * P.max_publication_fee_lovelace -
        P.max_settlement_fee_lovelace -
        P.max_close_fee_lovelace +
        P.challenge_record_lovelace,
    );
    // A close never touches the pooled bond.
    const poolAfter = await f.getPool();
    expect(poolAfter.assets).toEqual(poolBefore.assets);
    expect(poolAfter.datum).toBe(poolBefore.datum);
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    const published = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(outputs[0]!),
    );
    expect(
      Data.castFrom(published.data, SDK.StateQueueNode).da_attestation,
    ).toEqual({
      Published: {
        terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
          f.commitment,
        ),
      },
    });
    expect(
      f.measurements.find(({ name }) => name === "open 1 tranches")?.outputs,
    ).toBe(4);
    reportAvailabilityScenario("happy", f);
  }, 180_000);

  it("refuses settlement one slot before a backdated open's upper-anchored deadline, then times out a nonresponding attestation with queue removal", async () => {
    const f = await createAvailabilityFixture(1);
    const attested = await attestAvailability(f);
    const { validFrom, validTo } = widestBackdatedOpenValidity(f);
    const open = await openAvailability(f, attested, { validFrom, validTo });
    const state = await open.submit();
    expect(open.plan.responseDeadline).toBe(
      validTo - 1n + SMALL_RESPONSE_WINDOW_MS,
    );
    // The last whole slot before the deadline, long past where a lower-bound
    // anchor would have put it.
    const lastSlotBeforeDeadline = validTo + SMALL_RESPONSE_WINDOW_MS - 1_000n;
    expect(lastSlotBeforeDeadline).toBeGreaterThan(
      validFrom + SMALL_RESPONSE_WINDOW_MS,
    );
    f.emulator.awaitSlot(
      Number((lastSlotBeforeDeadline - BigInt(f.emulator.now())) / 1_000n),
    );
    expect(BigInt(f.emulator.now())).toBe(lastSlotBeforeDeadline);
    await assertAvailabilityRefusal(
      buildAvailabilitySettlement(
        f,
        open,
        state.record,
        state.terminal,
        state.threads[0]!,
        undefined,
        { bypassDeadlinePlanner: true },
      ),
      {
        purpose: "withdraw",
        script: "availability-challenge settle withdrawal",
      },
    );
    advanceAvailabilityDeadline(f, open);
    const [terminal] = await f.submit(
      "settle unresponded tranche",
      buildAvailabilitySettlement(
        f,
        open,
        state.record,
        state.terminal,
        state.threads[0]!,
      ),
    );
    const poolBefore = await f.getPool();
    await assertAvailabilityRefusal(
      (
        await buildAvailabilityTimeout(
          f,
          open,
          state.record,
          state.queue,
          terminal!,
          { redirectPayout: true },
        )
      ).tx,
      {
        purpose: "withdraw",
        script: "availability-challenge timeout withdrawal",
      },
    );
    const timeout = await buildAvailabilityTimeout(
      f,
      open,
      state.record,
      state.queue,
      terminal!,
    );
    const outputs = await f.submit(
      "timeout and unavailable head removal",
      timeout.tx,
    );
    expect(
      await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      ),
    ).toHaveLength(0);
    expect(Data.from(outputs[1]!.datum!, SDK.CorrectionLockDatum)).toBe("Idle");
    // Full slash: the pool gives up da_bond; the penalty share is the whole
    // fee (c = 0), the rest merges into the one challenger output.
    expect(txFee(f, "timeout and unavailable head removal")).toBe(
      P.da_slash_penalty_lovelace,
    );
    expect(outputs[2]!.address).toBe(f.challenger.address);
    expect(outputs[2]!.assets.lovelace).toBe(
      P.challenger_bond_lovelace -
        P.max_settlement_fee_lovelace +
        P.challenge_record_lovelace +
        FULL_SLASH_PAYOUT,
    );
    expect(outputs[3]!.address).toBe(poolBefore.address);
    expect(outputs[3]!.assets).toEqual({
      ...poolBefore.assets,
      lovelace: poolBefore.assets.lovelace - P.da_bond_lovelace,
    });
    expect(outputs[3]!.datum).toBe(poolBefore.datum);
    expect(outputs[4]!.address).toBe(f.responder.address);
    expect(outputs[4]!.assets.lovelace).toBe(state.queue.assets.lovelace);
    expect(timeout.plan).toMatchObject({
      taken: P.da_bond_lovelace,
      feePart: P.da_slash_penalty_lovelace,
      feeLovelace: P.da_slash_penalty_lovelace,
    });
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    reportAvailabilityScenario("no-response", f);
  }, 180_000);

  it("anchors a backdated open at its upper bound, so the honest response lands after the lower-anchored deadline and early settlement is refused", async () => {
    const f = await createAvailabilityFixture();
    const attested = await attestAvailability(f);
    const { validFrom, validTo } = widestBackdatedOpenValidity(f);
    const open = await openAvailability(f, attested, { validFrom, validTo });
    // Differential pair: the same open with its window anchored at the
    // backdated lower bound is refused by the validator.
    await assertAvailabilityRefusal(open.build({ anchorAt: validFrom }), {
      purpose: "withdraw",
      script: "availability-challenge open withdrawal",
    });
    // The open lands in the current slot, strictly before its upper bound.
    const landedAt = BigInt(f.emulator.now());
    expect(landedAt).toBeLessThan(validTo);
    const state = await open.submit();
    const deadline = validTo - 1n + SMALL_RESPONSE_WINDOW_MS;
    const record = SDK.parseDaAvailabilityChallengeRecordCbor(
      state.record.datum!,
    );
    expect(record.opened_at).toBe(validTo - 1n);
    expect(record.response_deadline).toBe(deadline);
    expect(record.commitment).toEqual(f.commitment);
    // The committee keeps at least the whole window after the open lands.
    expect(deadline - landedAt).toBeGreaterThanOrEqual(
      SMALL_RESPONSE_WINDOW_MS,
    );
    // Pass the deadline a lower-bound anchor would have set.
    const lowerAnchoredDeadline = validFrom + SMALL_RESPONSE_WINDOW_MS;
    const now = BigInt(f.emulator.now());
    if (now <= lowerAnchoredDeadline)
      f.emulator.awaitSlot(Number((lowerAnchoredDeadline - now) / 1_000n) + 1);
    expect(BigInt(f.emulator.now())).toBeGreaterThan(lowerAnchoredDeadline);
    await assertAvailabilityRefusal(
      buildAvailabilitySettlement(
        f,
        open,
        state.record,
        state.terminal,
        state.threads[0]!,
        undefined,
        { bypassDeadlinePlanner: true },
      ),
      {
        purpose: "withdraw",
        script: "availability-challenge settle withdrawal",
      },
    );
    f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
    const [tranche] = SDK.planDaAvailabilityPublications({
      commitment: f.commitment,
      payload: f.payload,
      challengeAssetName: open.plan.challengeAssetName,
    });
    let thread = state.threads[0]!;
    let carrier: UTxO | undefined;
    for (const publication of tranche!.publications) {
      const outputs = await f.submit(
        `backdated open chunk ${publication.chunk_index}`,
        buildAvailabilityPublication(f, thread, publication, carrier),
      );
      thread = outputs[0]!;
      carrier = outputs[1]!;
    }
    expect(
      Data.from(thread.datum!, SDK.DaAvailabilityTrancheDatum),
    ).toHaveProperty("Receipt");
    f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
    const [terminal] = await f.submit(
      "settle backdated published tranche",
      buildAvailabilitySettlement(
        f,
        open,
        state.record,
        state.terminal,
        thread,
        carrier,
      ),
    );
    const outputs = await f.submit(
      "close backdated published challenge",
      buildAvailabilityClose(f, open, state.record, state.queue, terminal!),
    );
    expect(outputs[1]!.address).toBe(f.challenger.address);
    expect(outputs[1]!.assets.lovelace).toBe(
      terminal!.assets.lovelace -
        P.max_close_fee_lovelace +
        P.challenge_record_lovelace,
    );
  }, 180_000);

  it("opens all 16 tranches, publishes a maximum chunk with the full-tranche proof, and settles partial timeout", async () => {
    const f = await createAvailabilityFixture(64 * 1024 * 1024);
    const open = await openAvailability(f, await attestAvailability(f));
    const state = await open.submit();
    expect(state.threads).toHaveLength(16);
    expect(
      f.measurements.find(({ name }) => name === "open 16 tranches")?.outputs,
    ).toBe(19);
    const [tranche, secondTranche] = SDK.planDaAvailabilityPublications({
      commitment: f.commitment,
      payload: f.payload,
      challengeAssetName: open.plan.challengeAssetName,
    });
    const publication = tranche!.publications[0]!;
    expect(publication.chunk_byte_length).toBe(14_020n);
    expect(tranche!.descriptor.chunk_count).toBe(300n);
    // Both publications, one block each, land inside the selected full window.
    expect(
      BigInt(f.emulator.now()) + 2n * BigInt(EMULATOR_BLOCK_MS),
    ).toBeLessThan(open.plan.responseDeadline);
    f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
    const [firstThread, carrier] = await f.submit(
      "maximum full tranche chunk 0",
      buildAvailabilityPublication(f, state.threads[0]!, publication),
    );
    const [partialThread, partialCarrier] = await f.submit(
      "maximum second tranche partial response",
      buildAvailabilityPublication(
        f,
        state.threads[1]!,
        secondTranche!.publications[0]!,
      ),
    );
    f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
    advanceAvailabilityDeadline(f, open);
    let terminal = state.terminal;
    for (let i = 0; i < state.threads.length; i += 1) {
      [terminal] = await f.submit(
        `settle maximum commitment tranche ${i}`,
        buildAvailabilitySettlement(
          f,
          open,
          state.record,
          terminal,
          i === 0 ? firstThread! : i === 1 ? partialThread! : state.threads[i]!,
          i === 0 ? carrier : i === 1 ? partialCarrier : undefined,
        ),
      );
    }
    const terminalDatum = Data.from(
      terminal!.datum!,
      SDK.DaAvailabilityTerminalAccumulatorDatum,
    );
    expect(terminalDatum.next_tranche_index).toBe(16n);
    expect(terminalDatum.has_timed_out_tranche).toBe(true);
    await f.submit(
      "maximum commitment timeout and queue correction",
      (
        await buildAvailabilityTimeout(
          f,
          open,
          state.record,
          state.queue,
          terminal!,
        )
      ).tx,
    );
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    reportAvailabilityScenario("maximum-first-chunk", f);
  }, 300_000);

  // Runs only where the selected full response window fits every publication
  // block; on the short testing windows the deadline passes mid-tranche.
  it.skipIf(!FULL_RESPONSE_FITS_WINDOW)(
    `publishes all 301 chunks of a full 4 MiB tranche and a second tranche, then closes (needs ${FULL_RESPONSE_BLOCKS} blocks of ${EMULATOR_BLOCK_MS} ms inside the ${FULL_RESPONSE_WINDOW_MS} ms full window)`,
    async () => {
      const f = await createAvailabilityFixture(4 * 1024 * 1024 + 1);
      const open = await openAvailability(f, await attestAvailability(f));
      const state = await open.submit();
      const tranches = SDK.planDaAvailabilityPublications({
        commitment: f.commitment,
        payload: f.payload,
        challengeAssetName: open.plan.challengeAssetName,
      });
      expect(tranches.map(({ publications }) => publications.length)).toEqual([
        300, 1,
      ]);
      let terminal = state.terminal;
      const recovered: SDK.DaAvailabilityPublicationDatum[] = [];
      for (
        let trancheIndex = 0;
        trancheIndex < tranches.length;
        trancheIndex += 1
      ) {
        f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
        let thread = state.threads[trancheIndex]!;
        let carrier: UTxO | undefined;
        for (const publication of tranches[trancheIndex]!.publications) {
          const outputs = await f.submit(
            `full response tranche ${trancheIndex} chunk ${publication.chunk_index}`,
            buildAvailabilityPublication(f, thread, publication, carrier),
          );
          thread = outputs[0]!;
          carrier = outputs[1]!;
          recovered.push(
            Data.from(carrier.datum!, SDK.DaAvailabilityPublicationDatum),
          );
        }
        f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
        [terminal] = await f.submit(
          `full response settlement ${trancheIndex}`,
          buildAvailabilitySettlement(
            f,
            open,
            state.record,
            terminal!,
            thread,
            carrier,
          ),
        );
      }
      expect(recovered).toHaveLength(301);
      expect(
        Buffer.concat(recovered.map(({ chunk }) => Buffer.from(chunk, "hex"))),
      ).toEqual(Buffer.from(f.payload));
      const outputs = await f.submit(
        "two tranche complete close",
        buildAvailabilityClose(f, open, state.record, state.queue, terminal!),
      );
      expect(outputs[1]!.assets.lovelace).toBe(
        P.challenger_bond_lovelace -
          301n * P.max_publication_fee_lovelace -
          2n * P.max_settlement_fee_lovelace -
          P.max_close_fee_lovelace +
          P.challenge_record_lovelace,
      );
      expect(
        await f.lucid.utxosAt(
          f.contracts.availabilityChallenge.spendingScriptAddress,
        ),
      ).toHaveLength(0);
      reportAvailabilityScenario("full-response", f);
    },
    300_000,
  );
});
