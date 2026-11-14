import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildAuthenticatedRootFromEncodedEntries } from "../../src/workers/commit-block-header/transition-roots.js";

const CHILD_WITHDRAWAL_ID: SDK.OutputReference = {
  transactionId: "d5".repeat(32),
  outputIndex: 0n,
};

/** Committed with an invalid-signature verdict, so it leaves the ledger as is. */
const CHILD_WITHDRAWAL_INFO: SDK.WithdrawalInfo = {
  body: {
    l2_outref: CHILD_WITHDRAWAL_ID,
    l2_owner: "d6".repeat(28),
    l2_value: new Map([["", new Map([["", 2_000_000n]])]]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["d7".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["d8".repeat(32), "d9".repeat(64)],
  validity: "IncorrectWithdrawalSignature",
};

/**
 * The child's one event, a placeholder withdrawal (a committed block carries
 * at least one event), whose single trace step keeps the ledger at
 * `utxosRoot`.
 */
export const signedIntentContinuationChildEvents = (utxosRoot: string) => {
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: CHILD_WITHDRAWAL_ID },
  };
  return {
    withdrawals: [
      [
        Data.to(CHILD_WITHDRAWAL_ID, SDK.OutputReference),
        SDK.committedWithdrawalValueBytes(CHILD_WITHDRAWAL_INFO),
      ],
    ] as SDK.DaPayloadEntry[],
    event_to_step: [
      [
        Data.to(eventKey, SDK.EventKey),
        Data.to({ step_index: 0n, phase: "Withdrawal" }, SDK.EventToStepValue),
      ],
    ] as SDK.DaPayloadEntry[],
    transition_trace: [
      [
        Data.to(0n),
        Data.to(
          {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: utxosRoot,
            post_utxos_root: utxosRoot,
          },
          SDK.TransitionStep,
        ),
      ],
    ] as SDK.DaPayloadEntry[],
  };
};

const entriesRoot = (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
) =>
  buildAuthenticatedRootFromEncodedEntries(
    domain,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  ).pipe(Effect.map(({ root }) => root));

/** The header roots of `signedIntentContinuationChildEvents`. */
export const signedIntentContinuationChildRoots = (
  events: ReturnType<typeof signedIntentContinuationChildEvents>,
) =>
  Effect.all({
    withdrawalsRoot: entriesRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      events.withdrawals,
    ),
    eventToStepRoot: entriesRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      events.event_to_step,
    ),
    transitionTraceRoot: entriesRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      events.transition_trace,
    ),
  });
