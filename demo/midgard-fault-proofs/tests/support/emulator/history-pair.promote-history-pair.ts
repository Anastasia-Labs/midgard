import {
  addressDataFromBech32,
  EVENT_WAIT_DURATION_MS,
  EventHistoryNode,
  EventHistoryObserve,
  type EventHistoryPayload,
  type EventHistoryWitness,
  prepareEventHistoryPayload,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { index, same } from "./history-pair.index.js";
import { setupHistoryPair } from "./history-pair.setup-history-pair.js";

export const promoteHistoryPair = async (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
  sharedRefund = false,
  payloads = historyPairPayloads(h),
) => {
  const fillers = await Promise.all(
    h.applied.map(
      async (a, i) =>
        (await h.lucid.utxosAt(a.address)).find(
          (u) => u.assets[a.policyId + h.keys[i]!] === 1n,
        )!,
    ),
  );
  const plans = payloads.map((payload) =>
    prepareEventHistoryPayload(
      payload,
      { PublicKeyCredential: [h.owner] },
      { inlineLimitBytes: 512n, maxPayloadBytes: 5000n, maxPayloadNodes: 512n },
    ),
  );
  const retained: (UTxO | undefined)[] = [];
  for (let i = 0; i < plans.length; i++) {
    const plan = plans[i]!;
    if (plan.kind === "Inline") {
      retained.push(undefined);
      continue;
    }
    const hash = await h.submit(
      `prepublish-${i}-history-content`,
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .pay.ToContract(
          h.applied[i]!.retention.address,
          { kind: "inline", value: plan.datumCbor },
          { lovelace: 15_000_000n },
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    retained.push(
      (await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }]))[0]!,
    );
  }
  const funding = await h.funding();
  const inputs = [...funding, ...fillers, ...h.eventNonces];
  const refs = [
    h.hub,
    ...h.scripts,
    ...retained.filter((u): u is UTxO => u !== undefined),
  ];
  const b = h.bounds();
  let tx = h.lucid
    .newTx()
    .collectFrom([...funding, ...h.eventNonces])
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo);
  for (let i = 0; i < 2; i++) {
    const a = h.applied[i]!;
    const filler = fillers[i]!;
    const plan = plans[i]!;
    const id = i === 0 ? h.eventNonces[0]! : h.eventNonces[1]!;
    const node: EventHistoryNode = {
      position: { Key: [h.keys[i]!] },
      next: null,
      protected_until: b.protectedUntil,
      payload: {
        Order: {
          facts: {
            event_id: {
              transactionId: id.txHash,
              outputIndex: BigInt(id.outputIndex),
            },
            inclusion_time:
              BigInt(b.validTo - 1) + BigInt(EVENT_WAIT_DURATION_MS),
            location: plan.location,
            structural_lovelace: i === 0 ? 3_000_000n : 0n,
            structural_refund_key: h.owner,
          },
        },
      },
    };
    tx = tx
      .collectFrom([filler], Data.to(index(inputs, filler)))
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          {
            Apply: {
              hub_reference_index: index(refs, h.hub),
              operation: {
                PromoteFiller: {
                  filler_input_index: index(inputs, filler),
                  order_output_index: BigInt(i),
                  refund_output_index: sharedRefund ? 2n : BigInt(2 + i),
                  nonce_input_index: index(inputs, h.eventNonces[i]!),
                  external_reference_index: retained[i]
                    ? index(refs, retained[i]!)
                    : null,
                },
              },
            },
          },
          EventHistoryObserve,
        ),
      )
      .pay.ToContract(
        a.address,
        { kind: "inline", value: Data.to(node, EventHistoryNode) },
        {
          lovelace: i === 0 ? 23_000_000n : 20_000_000n,
          [a.policyId + h.keys[i]!]: 1n,
        },
      );
  }
  tx = tx.pay.ToAddress(
    credentialToAddress("Custom", { type: "Key", hash: h.owner }),
    { lovelace: 3_000_000n },
  );
  if (!sharedRefund)
    tx = tx.pay.ToAddress(
      credentialToAddress("Custom", { type: "Key", hash: h.owner }),
      { lovelace: 3_000_000n },
    );
  const unsigned = await tx.complete({
    coinSelection: false,
    localUPLCEval: true,
  });
  // Wallet payment witnesses fund admission; no filler-owner required signer is added.
  expect(
    CML.Transaction.from_cbor_hex(unsigned.toCBOR())
      .body()
      .required_signers()
      ?.len() ?? 0,
  ).toBe(0);
  await h.submit("promote-both-without-filler-owner-approval", unsigned);
  h.emulator.awaitSlot(
    Math.max(40, Number((h.protectionDurationMs + 10_999n) / 1000n)),
  );
  return payloads;
};

export const insertHistoryFillerAfter = async (
  h: Pick<
    Awaited<ReturnType<typeof setupHistoryPair>>,
    "applied" | "lucid" | "funding" | "hub" | "scripts" | "bounds" | "owner"
  >,
  kind: "Deposit" | "Withdrawal",
  witness: EventHistoryWitness,
  key: string,
  excluded: UTxO[],
) => {
  const a = h.applied[kind === "Deposit" ? 0 : 1]!;
  const pred = witness.anchor.utxo;
  const node = witness.anchor.node;
  const funding = (await h.funding()).filter(
    (u) => !excluded.some((x) => same(x, u)),
  );
  const inputs = [...funding, pred];
  const refs = [h.hub, h.scripts[kind === "Deposit" ? 0 : 1]!];
  const b = h.bounds();
  return h.lucid
    .newTx()
    .collectFrom(funding)
    .collectFrom([pred], Data.to(index(inputs, pred)))
    .readFrom(refs)
    .validFrom(b.lower)
    .validTo(b.validTo)
    .withdraw(
      a.rewardAddress,
      0n,
      Data.to(
        {
          Apply: {
            hub_reference_index: index(refs, h.hub),
            operation: {
              InsertFiller: {
                predecessor_input_index: index(inputs, pred),
                predecessor_output_index: 0n,
                filler_output_index: 1n,
              },
            },
          },
        },
        EventHistoryObserve,
      ),
    )
    .mintAssets({ [a.policyId + key]: 1n }, Data.void())
    .pay.ToContract(
      a.address,
      {
        kind: "inline",
        value: Data.to(
          { ...node, next: key, protected_until: b.protectedUntil },
          EventHistoryNode,
        ),
      },
      pred.assets,
    )
    .pay.ToContract(
      a.address,
      {
        kind: "inline",
        value: Data.to(
          {
            position: { Key: [key] },
            next: node.next,
            protected_until: b.protectedUntil,
            payload: { Filler: { refund_key: h.owner } },
          },
          EventHistoryNode,
        ),
      },
      { lovelace: 3_000_000n, [a.policyId + key]: 1n },
    )
    .complete({ coinSelection: false, localUPLCEval: true });
};

export const historyPairPayloads = (
  h: Awaited<ReturnType<typeof setupHistoryPair>>,
) => {
  const destination = Effect.runSync(addressDataFromBech32(h.hubAddress));
  const payloads: EventHistoryPayload[] = h.eventNonces.map((n, i) => {
    const id = { transactionId: n.txHash, outputIndex: BigInt(n.outputIndex) };
    return i === 0
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: destination,
                l2_network_id: 0n,
                l2_datum: null,
              },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: h.owner,
                  l2_value: new Map([["", new Map([["", 100_000_000n]])]]),
                  l1_address: destination,
                  l1_datum: "NoDatum",
                },
                signature: ["55".repeat(32), "66".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: destination,
            refund_datum: "NoDatum",
          },
        };
  });
  return payloads;
};
