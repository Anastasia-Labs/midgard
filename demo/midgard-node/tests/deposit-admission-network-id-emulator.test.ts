import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { makeUserEventBuilderFixture } from "./reserve-payout-builders.make-user-event-builder-fixture.js";
import {
  findUtxoWithUnit,
  submitWithWallet,
} from "./reserve-payout-builders.submit-with-wallet.js";

type Fixture = Awaited<ReturnType<typeof makeUserEventBuilderFixture>>;

const depositConfig = (fixture: Fixture) => ({
  additionalAssets: {},
  l2Address: fixture.beneficiary.address,
  l2Datum: null,
  lovelace: 5_000_000n,
  referenceScripts: { depositMinting: fixture.depositMintingReference },
  // The emulator's clock starts at its genesis and does not follow wall time.
  validity: {
    validFrom: fixture.lucid.slotToUnixTime(fixture.lucid.currentSlot()),
    validTo: SDK.resolveUserEventValidTo(fixture.lucid),
  },
});

/** The SDK's prepared deposit admission with only the payload's L2 network id
 * replaced. The SDK only ever writes 0 or 1, so a foreign id is substituted
 * after preparation; the replacement has the same encoded size, so the
 * prepared funding still covers it. */
const buildAdmissionWithL2NetworkId = async (
  fixture: Fixture,
  l2NetworkId: bigint,
) => {
  const config = depositConfig(fixture);
  const prepared = await Effect.runPromise(
    SDK.prepareDepositSubmissionProgram(
      fixture.lucid,
      fixture.contracts,
      config,
    ),
  );
  const payload = Data.from(
    prepared.request.payloadCbor,
    SDK.EventHistoryPayload,
  );
  if (!("DepositPayload" in payload))
    throw new Error("Prepared deposit payload must be a DepositPayload");
  expect(payload.DepositPayload.event.info.l2_network_id).toBe(0n);
  payload.DepositPayload.event.info.l2_network_id = l2NetworkId;
  const payloadCbor = Data.to(payload, SDK.EventHistoryPayload);
  expect(payloadCbor.length).toBe(prepared.request.payloadCbor.length);
  return SDK.buildEventHistoryAdmission(prepared.context, {
    ...prepared.request,
    payloadCbor,
    ...config.validity,
  });
};

describe("deposit admission L2 network id (emulator)", () => {
  it("admits an honest deposit, whose SDK-built L2 network id is 0", async () => {
    const fixture = await makeUserEventBuilderFixture();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(
        fixture.lucid,
        fixture.contracts,
        depositConfig(fixture),
      ),
    );
    const txHash = await submitWithWallet(built.tx);
    await fixture.lucid.awaitTx(txHash);
    const order = findUtxoWithUnit(
      await fixture.lucid.utxosAt(built.metadata.depositAddress),
      built.metadata.depositAuthUnit,
    );
    const node = Data.from(order.datum ?? "", SDK.EventHistoryNode);
    if (node.payload === "RootContent" || !("Order" in node.payload))
      throw new Error("The admitted deposit must be a history Order");
    const location = node.payload.Order.facts.location;
    if (!("Inline" in location))
      throw new Error("The admitted deposit payload must be inline");
    const payload = location.Inline.payload;
    if (!("DepositPayload" in payload))
      throw new Error("The admitted payload must be a DepositPayload");
    expect(payload.DepositPayload.event.info.l2_network_id).toBe(0n);
  });

  it("refuses an L2 network id of 2 at admission, while 1 builds", async () => {
    const fixture = await makeUserEventBuilderFixture();
    // Control: the same substitution path with a supported id evaluates.
    const mainnet = await buildAdmissionWithL2NetworkId(fixture, 1n);
    expect(mainnet.tx.toCBOR()).toMatch(/^[0-9a-f]+$/);
    // Only the network id differs from the control, so the refusal is the
    // admission bound in the history observer's Deposit arm, which runs as the
    // transaction's only withdrawal.
    await expect(buildAdmissionWithL2NetworkId(fixture, 2n)).rejects.toThrow(
      /failed script execution Withdraw\[0\]/u,
    );
  });
});
