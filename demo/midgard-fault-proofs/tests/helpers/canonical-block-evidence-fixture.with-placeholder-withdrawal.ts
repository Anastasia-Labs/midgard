import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";

const PLACEHOLDER_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: h32(0xd5),
  outputIndex: 0n,
};

/** Committed with an invalid-signature verdict, so it leaves the ledger as is. */
const PLACEHOLDER_WITHDRAWAL_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: PLACEHOLDER_WITHDRAWAL_ID,
    l2_owner: h28(0xd6),
    l2_value: new Map([["", new Map([["", 2_000_000n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [h28(0xd7)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: [h32(0xd8), h32(0xd9) + h32(0xd9)],
  validity: "IncorrectWithdrawalSignature",
};

const countedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
) =>
  (
    await buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    )
  ).root;

/**
 * `fixture` (a block with no events) carrying one placeholder withdrawal, so a
 * retained block that has no events of its own can be committed (a committed
 * block carries at least one event). Its single trace step spans the block's
 * ledger roots. Refuses a block that already carries events or steps.
 */
export const withPlaceholderWithdrawalBlock = async <
  Fixture extends {
    readonly header: SDK.Header;
    readonly headerHash: string;
    readonly payload: SDK.DaPayload;
    readonly payloadEnvelopeCbor: Buffer;
  },
>(
  fixture: Fixture,
): Promise<Fixture> => {
  const body = fixture.payload.block_body;
  if (
    fixture.header.totalEventCount !== 0n ||
    body.transition_trace.length !== 0
  )
    throw new Error("a placeholder withdrawal goes on a block with no events");
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: PLACEHOLDER_WITHDRAWAL_ID },
  };
  const withdrawals: SDK.DaPayloadEntry[] = [
    [
      Data.to(PLACEHOLDER_WITHDRAWAL_ID, SDK.OutputReference),
      SDK.committedWithdrawalValueBytes(PLACEHOLDER_WITHDRAWAL_INFO),
    ],
  ];
  const eventToStep: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase: "Withdrawal" }, SDK.EventToStepValue),
    ],
  ];
  const transitionTrace: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Withdrawal",
          pre_utxos_root: fixture.header.prevUtxosRoot,
          post_utxos_root: fixture.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const counts = {
    ...body.counts,
    withdrawalCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const header: SDK.Header = {
    ...fixture.header,
    ...counts,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawals,
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStep,
    ),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTrace,
    ),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...fixture.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      withdrawals,
      event_to_step: eventToStep,
      transition_trace: transitionTrace,
      counts,
    },
  };
  return {
    ...fixture,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};
