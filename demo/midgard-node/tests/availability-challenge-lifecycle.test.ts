import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS } from "./helpers/availability-challenge.js";
import {
  advanceAvailabilityDeadline,
  assertAvailabilityRefusal,
  attestAvailability,
  buildAvailabilityClose,
  buildAvailabilityPublication,
  buildAvailabilitySettlement,
  buildAvailabilityTimeout,
  createAvailabilityFixture,
  openAvailability,
  reportAvailabilityScenario,
} from "./helpers/availability-challenge-emulator.js";

describe("availability challenge real ledger lifecycle under Van Rossem limits", () => {
  it("attests, bonds, opens, publishes ordered carriers, settles and closes with exact refunds", async () => {
    const f = await createAvailabilityFixture();
    const bonded = await attestAvailability(f, {
      refuseBondSubstitution: true,
    });
    const open = await openAvailability(f, bonded);
    await assertAvailabilityRefusal(open.build({ omitSigner: true }));
    await assertAvailabilityRefusal(open.build({ omitYield: true }));
    await assertAvailabilityRefusal(open.build({ wrongYield: true }));
    const state = await open.submit();
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
    );
    for (const publication of tranche!.publications) {
      if (carrier)
        await assertAvailabilityRefusal(
          buildAvailabilityPublication(f, thread, publication),
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
        state.bond,
        state.terminal,
        thread,
        carrier,
      ),
    );
    await assertAvailabilityRefusal(
      buildAvailabilityClose(f, open, state.bond, state.queue, terminal!, {
        redirectRefund: true,
      }),
    );
    const outputs = await f.submit(
      "close published challenge",
      buildAvailabilityClose(f, open, state.bond, state.queue, terminal!),
    );
    expect(outputs[1]!.address).toBe(f.responder.address);
    expect(outputs[1]!.assets.lovelace).toBe(
      TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace,
    );
    expect(outputs[2]!.address).toBe(f.challenger.address);
    expect(outputs[2]!.assets.lovelace).toBe(
      TEST_AVAILABILITY_PARAMETERS.challenger_bond_lovelace -
        2n * TEST_AVAILABILITY_PARAMETERS.max_publication_fee_lovelace -
        TEST_AVAILABILITY_PARAMETERS.max_settlement_fee_lovelace -
        TEST_AVAILABILITY_PARAMETERS.max_close_fee_lovelace,
    );
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

  it("refuses premature settlement and then times out a nonresponding attestation with queue removal", async () => {
    const f = await createAvailabilityFixture(1);
    const open = await openAvailability(f, await attestAvailability(f));
    const state = await open.submit();
    await assertAvailabilityRefusal(
      buildAvailabilitySettlement(
        f,
        open,
        state.bond,
        state.terminal,
        state.threads[0]!,
        undefined,
        { bypassDeadlinePlanner: true },
      ),
    );
    advanceAvailabilityDeadline(f, open);
    const [terminal] = await f.submit(
      "settle unresponded tranche",
      buildAvailabilitySettlement(
        f,
        open,
        state.bond,
        state.terminal,
        state.threads[0]!,
      ),
    );
    await assertAvailabilityRefusal(
      buildAvailabilityTimeout(f, open, state.bond, state.queue, terminal!, {
        redirectSlash: true,
      }),
    );
    const outputs = await f.submit(
      "timeout and unavailable head removal",
      buildAvailabilityTimeout(f, open, state.bond, state.queue, terminal!),
    );
    expect(
      await f.lucid.utxosAtWithUnit(
        f.contracts.stateQueue.spendingScriptAddress,
        f.queueUnit,
      ),
    ).toHaveLength(0);
    expect(Data.from(outputs[1]!.datum!, SDK.CorrectionLockDatum)).toBe("Idle");
    expect(outputs[2]!.address).toBe(f.challenger.address);
    expect(outputs[2]!.assets.lovelace).toBe(
      TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace,
    );
    expect(outputs[3]!.address).toBe(f.challenger.address);
    expect(outputs[3]!.assets.lovelace).toBe(
      TEST_AVAILABILITY_PARAMETERS.challenger_bond_lovelace -
        TEST_AVAILABILITY_PARAMETERS.max_settlement_fee_lovelace -
        TEST_AVAILABILITY_PARAMETERS.max_timeout_fee_lovelace,
    );
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    reportAvailabilityScenario("no-response", f);
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
    f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
    let firstThread = state.threads[0]!;
    let carrier: UTxO | undefined;
    for (const publication of tranche!.publications) {
      const outputs = await f.submit(
        `maximum full tranche chunk ${publication.chunk_index}`,
        buildAvailabilityPublication(f, firstThread, publication, carrier),
      );
      firstThread = outputs[0]!;
      carrier = outputs[1]!;
    }
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
          state.bond,
          terminal,
          i === 0 ? firstThread : i === 1 ? partialThread! : state.threads[i]!,
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
      buildAvailabilityTimeout(f, open, state.bond, state.queue, terminal!),
    );
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    reportAvailabilityScenario("maximum", f);
  }, 300_000);

  it("publishes all 301 chunks of a full 4 MiB tranche and a second tranche, then closes", async () => {
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
          state.bond,
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
      buildAvailabilityClose(f, open, state.bond, state.queue, terminal!),
    );
    expect(outputs[2]!.assets.lovelace).toBe(
      TEST_AVAILABILITY_PARAMETERS.challenger_bond_lovelace -
        301n * TEST_AVAILABILITY_PARAMETERS.max_publication_fee_lovelace -
        2n * TEST_AVAILABILITY_PARAMETERS.max_settlement_fee_lovelace -
        TEST_AVAILABILITY_PARAMETERS.max_close_fee_lovelace,
    );
    expect(
      await f.lucid.utxosAt(
        f.contracts.availabilityChallenge.spendingScriptAddress,
      ),
    ).toHaveLength(0);
    reportAvailabilityScenario("full-response", f);
  }, 300_000);
});
