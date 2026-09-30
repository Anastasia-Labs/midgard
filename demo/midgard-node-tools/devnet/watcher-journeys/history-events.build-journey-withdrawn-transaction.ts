import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type HistoryPredecessor,
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
  signHistoryTransaction,
} from "./history-cases.js";
import {
  authenticateEvent,
  type HistoryEventBlockInput,
  requireEventWindow,
  retainJourneyWithdrawals,
  type StagedHistoryEvent,
} from "./history-events.retain-journey-withdrawals.js";

export const buildJourneyWithdrawalEvent = async (
  input: HistoryEventBlockInput & {
    category: "fabricatedWithdrawal" | "withdrawalMistag" | "doubleWithdraw";
    withdrawals: readonly StagedHistoryEvent[];
    honest?: boolean;
  },
) => {
  const events = input.withdrawals.map((withdrawal) => {
    const datum = authenticateEvent(withdrawal);
    if (datum.kind !== "Withdrawal")
      throw new Error(
        "Withdrawal journey requires an authenticated withdrawal Order",
      );
    requireEventWindow(datum.facts.inclusion_time, input);
    return { ...datum.event, infoCbor: datum.infoCbor.toString("hex") };
  });
  const first = events[0];
  if (first === undefined)
    throw new Error("Withdrawal journey requires a real staged event");
  if (input.category === "doubleWithdraw") {
    const second = events[1];
    if (
      events.length !== 2 ||
      second === undefined ||
      SDK.committedWithdrawalKeyBytes(first.id) ===
        SDK.committedWithdrawalKeyBytes(second.id)
    )
      throw new Error(
        "Double-withdraw journey needs two distinct real event identities",
      );
    if (
      Data.to(first.info.body.l2_outref, SDK.OutputReference) !==
      Data.to(second.info.body.l2_outref, SDK.OutputReference)
    )
      throw new Error(
        "Double-withdraw staged events must name the same L2 output",
      );
    const ordered = [...events].sort((left, right) =>
      SDK.committedWithdrawalKeyBytes(left.id).localeCompare(
        SDK.committedWithdrawalKeyBytes(right.id),
      ),
    );
    const claims = ordered.map((event, index) => ({
      id: event.id,
      infoCbor: replacePlutusConstrFieldCbor(
        event.infoCbor,
        [2],
        Data.to(
          input.honest && index === 1
            ? "NonExistentWithdrawalUtxo"
            : "WithdrawalIsValid",
          SDK.WithdrawalValidity,
        ),
      ),
    }));
    return retainJourneyWithdrawals({ ...input, claims });
  }
  if (events.length !== 1)
    throw new Error("Withdrawal journey needs exactly one staged event");
  let infoCbor = first.infoCbor;
  if (!input.honest && input.category === "withdrawalMistag")
    infoCbor = replacePlutusConstrFieldCbor(
      infoCbor,
      [2],
      Data.to("NonExistentWithdrawalUtxo", SDK.WithdrawalValidity),
    );
  if (!input.honest && input.category === "fabricatedWithdrawal") {
    const key = first.info.body.l2_owner;
    const diverted = (key.startsWith("00") ? "01" : "00") + key.slice(2);
    infoCbor = replacePlutusConstrFieldCbor(
      infoCbor,
      [0, 3],
      Data.to(
        {
          paymentCredential: { PublicKeyCredential: [diverted] },
          stakeCredential: null,
        },
        SDK.AddressData,
      ),
    );
  }
  return retainJourneyWithdrawals({
    ...input,
    claims: [{ id: first.id, infoCbor }],
  });
};

/** Body for the common L1 staging owner to sign and submit through the SDK. */
export const journeyWithdrawalBody = (input: {
  predecessor: HistoryPredecessor;
  owner: string;
}): SDK.WithdrawalBody => {
  const selected = ownedHistoryLedgerEntries(input.predecessor, input.owner)[0];
  const entry =
    selected === undefined
      ? undefined
      : [selected.outRef.toString("hex"), selected.output.toString("hex")];
  if (entry === undefined)
    throw new Error("Withdrawal journey has no deposited ledger output");
  const reference = decodeMidgardSpendInputItem(Buffer.from(entry[0], "hex"));
  const output = decodeMidgardTxOutput(Buffer.from(entry[1], "hex"));
  return {
    l2_outref: {
      transactionId: Buffer.from(reference.txId).toString("hex"),
      outputIndex: BigInt(reference.outputIndex),
    },
    l2_owner: input.owner,
    l2_value: new Map([
      ["", new Map([["", output.value.lovelace]])],
      ...Array.from(
        output.value.assets,
        ([policy, assets]) => [policy, new Map(assets)] as const,
      ),
    ]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [input.owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
};

/** A withdrawal followed by an accepted claim that still spends/reads its output. */
export const buildJourneyWithdrawnTransaction = async (
  input: HistoryEventBlockInput & {
    category: "withdrawnInput" | "withdrawnReferenceInput";
    withdrawal: StagedHistoryEvent;
    ledgerOwnerSeedPhrase: string;
    honest?: boolean;
  },
) => {
  const datum = authenticateEvent(input.withdrawal);
  if (datum.kind !== "Withdrawal")
    throw new Error(
      "Withdrawal journey requires an authenticated withdrawal Order",
    );
  requireEventWindow(datum.facts.inclusion_time, input);
  const withdrawn = encodeMidgardSpendInputItem({
    txId: Buffer.from(datum.event.info.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(datum.event.info.body.l2_outref.outputIndex),
  });
  const withdrawnOutput = input.predecessor.payload.block_body.utxos.find(
    ([key]) => key === withdrawn.toString("hex"),
  );
  const owner = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
  )
    .to_public()
    .hash()
    .to_hex();
  const unspent = ownedHistoryLedgerEntries(input.predecessor, owner).find(
    ({ outRef }) => !outRef.equals(withdrawn),
  );
  const remaining =
    unspent === undefined
      ? undefined
      : [unspent.outRef.toString("hex"), unspent.output.toString("hex")];
  if (withdrawnOutput === undefined || remaining === undefined)
    throw new Error(
      "Withdrawn-input journeys need two actual predecessor outputs, one withdrawn and one retained for the valid control",
    );
  const withdrawalBlock = await retainJourneyWithdrawals({
    ...input,
    claims: [{ id: datum.event.id, infoCbor: datum.infoCbor.toString("hex") }],
  });
  if (withdrawalBlock.classifications[0]?.validity !== "WithdrawalIsValid")
    throw new Error(
      "Withdrawn-input journey requires an honestly valid staged withdrawal",
    );
  const key = CML.PrivateKey.from_bech32(
    walletFromSeed(input.ledgerOwnerSeedPhrase, { network: "Custom" })
      .paymentKey,
  );
  const selected =
    !input.honest && input.category === "withdrawnInput"
      ? withdrawnOutput
      : remaining;
  const transaction = signHistoryTransaction(
    {
      spendInputs: [Buffer.from(selected[0], "hex")],
      referenceInputs:
        !input.honest && input.category === "withdrawnReferenceInput"
          ? [withdrawn]
          : [],
      outputs: [Buffer.from(selected[1], "hex")],
      fee: 0n,
      networkId: input.predecessor.header.expectedNetworkId,
    },
    key,
  );
  const transactions = await retainHistoryTransactions({
    ...input,
    predecessor: {
      ...withdrawalBlock,
      header: {
        ...withdrawalBlock.header,
        endTime: input.predecessor.header.endTime,
      },
    },
    transactions: [transaction],
  });
  const mappings: SDK.DaPayloadEntry[] = [
    ...withdrawalBlock.payload.block_body.event_to_step,
    ...transactions.payload.block_body.event_to_step.map(
      ([key, value]): SDK.DaPayloadEntry => [
        key,
        Data.to(
          { ...Data.from(value, SDK.EventToStepValue), step_index: 1n },
          SDK.EventToStepValue,
        ),
      ],
    ),
  ];
  const transitions: SDK.DaPayloadEntry[] = [
    ...withdrawalBlock.payload.block_body.transition_trace,
    ...transactions.payload.block_body.transition_trace.map(
      ([, value]): SDK.DaPayloadEntry => [
        Data.to(1n),
        Data.to(
          { ...Data.from(value, SDK.TransitionStep), step_index: 1n },
          SDK.TransitionStep,
        ),
      ],
    ),
  ];
  const root = (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [eventRoot, traceRoot] = await Promise.all([
    root(SDK.ROOT_DOMAINS.eventToStep, mappings),
    root(SDK.ROOT_DOMAINS.transitionTrace, transitions),
  ]);
  const counts = {
    ...transactions.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
  };
  const header: SDK.Header = {
    ...transactions.header,
    ...counts,
    prevHeaderHash: input.predecessor.headerHash,
    prevUtxosRoot: input.predecessor.header.utxosRoot,
    withdrawalsRoot: withdrawalBlock.header.withdrawalsRoot,
    eventToStepRoot: eventRoot.root,
    transitionTraceRoot: traceRoot.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const sort = (entries: SDK.DaPayloadEntry[]) =>
    entries.sort(([left], [right]) => left.localeCompare(right));
  const payload: SDK.DaPayload = {
    ...transactions.payload,
    block_body: {
      ...transactions.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      withdrawals: withdrawalBlock.payload.block_body.withdrawals,
      event_to_step: sort(mappings),
      transition_trace: sort(transitions),
    },
  };
  return {
    ...transactions,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    actualWithdrawal: input.withdrawal,
  };
};
