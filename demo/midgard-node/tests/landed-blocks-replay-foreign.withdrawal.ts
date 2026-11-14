/**
 * The foreign replayer bench's honest withdrawal block (N3): the block on
 * an empty parent that names the view's one withdrawal, for the production
 * replayer.
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import { eventsAt } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { WithdrawalsDB } from "../src/database/index.js";
import { eventKeyCbor } from "../src/mpf/trace-events.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { countsFromLengths } from "./da-payload.record.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { listOf } from "./helpers/l1-events-chain.js";
import { emptyBlock } from "./landed-blocks-replay-foreign.fixture.js";

/**
 * The honest block on an empty parent naming the view's one withdrawal:
 * with no ledger output it settles as `NonExistentWithdrawalUtxo` and keeps
 * the ledger empty.
 */
export const withdrawalBlock = async (store: FactStore, nonce: OutRef) => {
  const view = (await store.currentView())!;
  const read = await eventsAt(store, listOf("withdrawal"), view.point);
  if (read.kind !== "ok") throw new Error(`eventsAt: ${read.kind}`);
  const [event] = read.value;
  if (event === undefined) throw new Error("the view holds no withdrawal");
  const info = aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
  );
  // The event's l2_outref is its id.
  const ledgerOutRef = await Effect.runPromise(
    WithdrawalsDB.toLedgerOutRef({
      [WithdrawalsDB.Columns.L2_OUTREF]: Buffer.from(event.idCbor, "hex"),
    }),
  );
  const { settlementEventInfo } = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger({
      // Body field 1, a 28-byte string: its bytes follow the `581c` head.
      l2Owner: plutusConstrFieldCbor(info, [0, 1]).slice(4),
      l2ValueCbor: plutusConstrFieldCbor(info, [0, 2]),
      eventInfoCbor: info,
      ledgerOutRef,
      ledgerOutput: null,
    }),
  );
  const settlement = settlementEventInfo.toString("hex");
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: {
      withdrawal_id: {
        transactionId: nonce.txHash.toString("hex"),
        outputIndex: BigInt(nonce.index),
      },
    },
  };
  const base = await emptyBlock();
  const counts = countsFromLengths({ withdrawals: 1 });
  const block = await rebind({
    ...base,
    block_body: {
      ...base.block_body,
      header: { ...base.block_body.header, ...counts },
      counts,
      withdrawals: [[event.idCbor, settlement]],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          (await Effect.runPromise(eventKeyCbor(eventKey))).toString("hex"),
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "Withdrawal",
          }).toString("hex"),
        ],
      ],
    },
  });
  return { block, idCbor: event.idCbor, settlement };
};
