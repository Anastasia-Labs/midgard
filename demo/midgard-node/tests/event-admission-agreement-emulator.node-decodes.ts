/** The node's decode of an admitted event-history order. */
import {
  type EventListConfig,
  openOrder,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { addressDataToBech32 } from "../src/commands/withdrawal-utils.js";
import { userEventEntry } from "../src/l1-events/entries.js";
import type { makeUserEventBuilderFixture } from "./reserve-payout-builders.make-user-event-builder-fixture.js";

export type Fixture = Awaited<ReturnType<typeof makeUserEventBuilderFixture>>;
export type Group = "deposit" | "withdrawal" | "external deposit";

/** Whether the node decodes the admitted order: the follower's `openOrder`,
 * the node's user-event entry, and for a withdrawal its L1 address mapping. */
export const nodeDecodes = (
  fixture: Fixture,
  group: Group,
  order: UTxO,
  retained: readonly UTxO[],
): boolean => {
  const kind = group === "withdrawal" ? "withdrawal" : "deposit";
  const contract =
    kind === "deposit"
      ? fixture.contracts.deposit
      : fixture.contracts.withdrawal;
  const list = { kind, policyId: contract.policyId } as EventListConfig;
  const network = fixture.lucid.config().network!;
  try {
    const event = openOrder(order, list, retained);
    if (event === "not_an_order") return false;
    const at = { txHash: Buffer.alloc(32), index: 0 };
    const projected: ProjectedEvent = {
      ...event,
      kind,
      admission: {
        blockHash: "00".repeat(32),
        slot: 0,
        height: 0,
        txHash: "00".repeat(32),
        txIndex: 0,
        outRef: at,
      },
      retirement: null,
      location: at,
    };
    const entry = userEventEntry(projected, network);
    if (entry.kind === "withdrawal") {
      addressDataToBech32(
        network,
        Data.from(entry.entry.l1Address, SDK.AddressData),
      );
      addressDataToBech32(
        network,
        Data.from(entry.entry.refundAddress, SDK.AddressData),
      );
    }
    return true;
  } catch {
    return false;
  }
};
