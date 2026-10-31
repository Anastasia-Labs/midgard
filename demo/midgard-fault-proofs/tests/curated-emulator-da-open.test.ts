import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import {
  attestAvailability,
  buildAvailabilityClose,
  buildAvailabilityPublication,
  buildAvailabilitySettlement,
  createAvailabilityFixture,
  openAvailability,
} from "midgard-node/tests/helpers/availability-challenge-emulator";
import { describe, expect, it } from "vitest";

type Fixture = Awaited<ReturnType<typeof createAvailabilityFixture>>;

const walletLovelace = async (f: Fixture) =>
  (await f.lucid.wallet().getUtxos()).reduce(
    (total, utxo) => total + utxo.assets.lovelace,
    0n,
  );

describe("preprod-emulator-curated DA availability challenge", () => {
  it("opens a challenge with a key no profile names, and the bond it posts funds the committee's publication", async () => {
    // The fixture's challenger is a fresh key; the curated profile lists only
    // its operator.
    const f = await createAvailabilityFixture();
    expect(Object.keys(SELECTED_DEPLOYMENT_PROFILE.allow_lists)).toEqual([
      "operators",
    ]);
    expect(SELECTED_DEPLOYMENT_PROFILE.allow_lists.operators).not.toContain(
      f.challengerKey,
    );

    const open = await openAvailability(f, await attestAvailability(f));
    const state = await open.submit();
    expect(
      state.record.assets[open.policy + open.plan.challengeAssetName],
    ).toBe(1n);

    // The committee member publishes every chunk; each publication's fee and
    // carrier come out of the challenge thread, not the member's wallet.
    f.lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
    const responderBefore = await walletLovelace(f);
    const [tranche] = SDK.planDaAvailabilityPublications({
      commitment: f.commitment,
      payload: f.payload,
      challengeAssetName: open.plan.challengeAssetName,
    });
    const publications = tranche!.publications;
    expect(publications.length).toBeGreaterThan(0);
    let thread = state.threads[0]!;
    let carrier: UTxO | undefined;
    for (const publication of publications) {
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
    expect(await walletLovelace(f)).toBe(responderBefore);

    // Settlement and close return the bond less every fee it paid.
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
    const outputs = await f.submit(
      "close published challenge",
      buildAvailabilityClose(f, open, state.record, state.queue, terminal!),
    );
    const P = f.parameters;
    expect(outputs[1]!.address).toBe(f.challenger.address);
    expect(outputs[1]!.assets.lovelace).toBe(
      P.challenger_bond_lovelace -
        BigInt(publications.length) * P.max_publication_fee_lovelace -
        P.max_settlement_fee_lovelace -
        P.max_close_fee_lovelace +
        P.challenge_record_lovelace,
    );
  }, 600_000);
});
