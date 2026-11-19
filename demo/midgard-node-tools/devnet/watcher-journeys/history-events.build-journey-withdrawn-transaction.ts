import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs/transition-trace/phas";
import * as SDK from "@al-ft/midgard-sdk";
import { type CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { journeyOwnerKey } from "./forced-transaction.js";
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
import { buildAtJourneyMinFee } from "./journey-fee.js";
import {
  checkedJourneyConstruction,
  journeyLedgerPairs,
} from "./journey-ledger.js";
import {
  type OwnedOutput,
  type OwnerSpend,
  selectOwnerOutput,
} from "./owner-output-selection.js";

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
  /** The owned output to withdraw; the owner's first output when absent. */
  outRef?: Buffer;
}): SDK.WithdrawalBody => {
  const owned = ownedHistoryLedgerEntries(input.predecessor, input.owner);
  const { outRef } = input;
  const selected =
    outRef === undefined
      ? owned[0]
      : owned.find((entry) => entry.outRef.equals(outRef));
  const entry =
    selected === undefined
      ? undefined
      : [selected.outRef.toString("hex"), selected.output.toString("hex")];
  if (entry === undefined)
    throw new Error("Withdrawal journey has no deposited ledger output");
  return ownedOutputWithdrawalBody({
    owner: input.owner,
    outRef: Buffer.from(entry[0], "hex"),
    output: decodeMidgardTxOutput(Buffer.from(entry[1], "hex")),
  });
};

/** The withdrawal of one owned output, paid to the owner's own key. */
export const ownedOutputWithdrawalBody = (input: {
  owner: string;
  outRef: Uint8Array;
  output: MidgardTxOutput;
}): SDK.WithdrawalBody => {
  const reference = decodeMidgardSpendInputItem(input.outRef);
  const { output } = input;
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

/** The header fields a pass-through prices and signs against. */
type PassThroughHeader = Pick<
  SDK.Header,
  "minFeeA" | "minFeeB" | "expectedNetworkId"
>;

/**
 * The owner's signed transaction spending `entry` into one output at its
 * address carrying its value less the fee, reading `referenceInputs`, at the
 * minimum fee `header` prices it at: a withdrawn-input family's valid
 * control, and its faults. Its `outputs` are the outputs it pays.
 */
export const signOwnerPassThrough = (
  input: Readonly<{
    header: PassThroughHeader;
    key: CML.PrivateKey;
    entry: OwnedOutput;
    referenceInputs: readonly Buffer[];
  }>,
) => {
  const output = decodeMidgardTxOutput(input.entry.output);
  return buildAtJourneyMinFee(
    input.header,
    (fee) => {
      const outputs = [
        encodeMidgardTxOutput({
          ...output,
          value: { ...output.value, lovelace: output.value.lovelace - fee },
        }),
      ];
      return {
        ...signHistoryTransaction(
          {
            spendInputs: [input.entry.outRef],
            referenceInputs: [...input.referenceInputs],
            outputs,
            fee,
            networkId: input.header.expectedNetworkId,
          },
          input.key,
        ),
        outputs,
      };
    },
    output.value.lovelace,
  );
};

/**
 * What a withdrawn-input family pays from the retained output it spends
 * besides the withdrawn one: its valid control's pass-through and, when it
 * `readsWithdrawn`, its reference-input fault's, which reads `withdrawn`.
 */
export const withdrawnRetainedSpend =
  (
    input: Readonly<{
      header: PassThroughHeader;
      key: CML.PrivateKey;
      withdrawn: Buffer;
      readsWithdrawn: boolean;
    }>,
  ): OwnerSpend =>
  (entry) => [
    ...signOwnerPassThrough({ ...input, entry, referenceInputs: [] }).outputs,
    ...(input.readsWithdrawn
      ? signOwnerPassThrough({
          ...input,
          entry,
          referenceInputs: [input.withdrawn],
        }).outputs
      : []),
  ];

/**
 * A withdrawal followed by an accepted claim that still spends/reads its
 * output. The honest control spends a retained output instead, the owner's
 * smallest other output that funds it (`selectOwnerOutput`), as the
 * reference-input fault does; with `controlReferencesRetained` the
 * reference-input control also reads a second retained output, any one other
 * than the one it spends, since a read output pays nothing, so the control
 * carries a reference input that exists.
 */
export const buildJourneyWithdrawnTransaction = async (
  input: HistoryEventBlockInput & {
    category: "withdrawnInput" | "withdrawnReferenceInput";
    withdrawal: StagedHistoryEvent;
    ledgerOwnerSeedPhrase: string;
    honest?: boolean;
    controlReferencesRetained?: boolean;
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
  const withdrawnOutput = journeyLedgerPairs(input.predecessor.ledger).find(
    ([key]) => key === withdrawn.toString("hex"),
  );
  const key = journeyOwnerKey(input.ledgerOwnerSeedPhrase);
  const owner = key.to_public().hash().to_hex();
  const retained = ownedHistoryLedgerEntries(input.predecessor, owner).filter(
    ({ outRef }) => !outRef.equals(withdrawn),
  );
  if (withdrawnOutput === undefined)
    throw new Error(
      "Withdrawn-input journeys need the withdrawn output on their predecessor",
    );
  const unspent = selectOwnerOutput(
    retained,
    withdrawnRetainedSpend({
      header: input.predecessor.header,
      key,
      withdrawn,
      readsWithdrawn: input.category === "withdrawnReferenceInput",
    }),
    `the ${input.category} family's spend of a retained output`,
  );
  const referencesRetained =
    input.honest === true &&
    input.category === "withdrawnReferenceInput" &&
    input.controlReferencesRetained === true;
  const controlReference = referencesRetained
    ? retained.find(({ outRef }) => !outRef.equals(unspent.outRef))
    : undefined;
  if (referencesRetained && controlReference === undefined)
    throw new Error(
      "A reference-input control needs a second retained predecessor output",
    );
  const withdrawalBlock = await retainJourneyWithdrawals({
    ...input,
    claims: [{ id: datum.event.id, infoCbor: datum.infoCbor.toString("hex") }],
  });
  if (withdrawalBlock.classifications[0]?.validity !== "WithdrawalIsValid")
    throw new Error(
      "Withdrawn-input journey requires an honestly valid staged withdrawal",
    );
  const transaction = signOwnerPassThrough({
    header: input.predecessor.header,
    key,
    entry:
      !input.honest && input.category === "withdrawnInput"
        ? {
            outRef: withdrawn,
            output: Buffer.from(withdrawnOutput[1], "hex"),
          }
        : unspent,
    referenceInputs:
      !input.honest && input.category === "withdrawnReferenceInput"
        ? [withdrawn]
        : controlReference === undefined
          ? []
          : [controlReference.outRef],
  });
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
  const eventCounts = {
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
  };
  const counts = { ...transactions.payload.block_body.counts, ...eventCounts };
  // The header keeps the base header's pinned validation-trace pair; only the
  // body declares its validation-trace members.
  const header: SDK.Header = {
    ...transactions.header,
    ...eventCounts,
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
  return checkedJourneyConstruction(
    {
      ...transactions,
      header,
      headerHash,
      payload,
      payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
        mode: "identity",
      }),
      actualWithdrawal: input.withdrawal,
      targetEventKey: {
        L2TransactionEventKey: { tx_id: transaction.txId },
      } satisfies SDK.EventKey,
    },
    transactions.ledger.entries,
  );
};
