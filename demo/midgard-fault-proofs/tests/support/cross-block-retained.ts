import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import { buildInvalidForcedTransitionTraceFixture } from "./submit-init-emulator-fixtures.js";

export const crossBlockRetainedFixture = async ({
  operatorVkey,
  now,
  kind = "forced-transaction",
  prevHeaderHash = SDK.GENESIS_HEADER_HASH,
  prevUtxosRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
}: {
  operatorVkey: string;
  now: number;
  kind?: "deposit" | "withdrawal" | "forced-transaction";
  prevHeaderHash?: string;
  prevUtxosRoot?: string;
}) => {
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey,
    now,
  });
  const body = base.reconstruction.payload.block_body;
  const field =
    kind === "deposit"
      ? "deposits"
      : kind === "withdrawal"
        ? "withdrawals"
        : "forced_transactions";
  const phase =
    kind === "deposit"
      ? "Deposit"
      : kind === "withdrawal"
        ? "Withdrawal"
        : "ForcedTransaction";
  const key = base.eventKey.ForcedTransactionEventKey.tx_order_id;
  const eventKey: SDK.EventKey =
    kind === "deposit"
      ? { DepositEventKey: { deposit_id: key } }
      : kind === "withdrawal"
        ? { WithdrawalEventKey: { withdrawal_id: key } }
        : base.eventKey;
  const entries: SDK.DaPayloadEntry[] =
    kind === "forced-transaction"
      ? [...body.forced_transactions]
      : [
          [
            Data.to(key, SDK.OutputReference),
            kind === "deposit"
              ? Data.to(
                  Data.from(DEPOSIT_VALUE_CBOR, SDK.DepositInfo),
                  SDK.DepositInfo,
                )
              : SDK.committedWithdrawalValueBytes(
                  Data.from(WITHDRAWAL_VALUE_CBOR, SDK.WithdrawalInfo),
                ),
          ],
        ];
  const priorStep = Data.from(
    body.transition_trace[0]![1],
    SDK.TransitionStepSchema,
  );
  const traces: SDK.DaPayloadEntry[] = [
    [
      body.transition_trace[0]![0],
      Data.to(
        {
          ...priorStep,
          event_key: eventKey,
          phase,
          pre_utxos_root: prevUtxosRoot,
        },
        SDK.TransitionStepSchema,
      ),
    ],
  ];
  const events: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase }, SDK.EventToStepValue),
    ],
  ];
  const count = (
    domain: Parameters<typeof buildCountedRoot>[0],
    rows: readonly SDK.DaPayloadEntry[],
  ) =>
    buildCountedRoot(
      domain,
      rows.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const source = await count(
    kind === "deposit"
      ? SDK.ROOT_DOMAINS.deposits
      : kind === "withdrawal"
        ? SDK.ROOT_DOMAINS.withdrawals
        : SDK.ROOT_DOMAINS.forcedTransactionsV1,
    entries,
  );
  const trace = await count(SDK.ROOT_DOMAINS.transitionTrace, traces);
  const event = await count(SDK.ROOT_DOMAINS.eventToStep, events);
  const validations =
    kind === "forced-transaction" ? [...body.validation_traces] : [];
  const validation = await count(
    SDK.ROOT_DOMAINS.validationTraces,
    validations,
  );
  const counts = {
    ...body.counts,
    depositCount: kind === "deposit" ? 1n : 0n,
    withdrawalCount: kind === "withdrawal" ? 1n : 0n,
    forcedTransactionCount: kind === "forced-transaction" ? 1n : 0n,
    validationTraceCount: BigInt(validations.length),
  };
  const header: SDK.Header = {
    ...base.header,
    endTime: BigInt(now) + 61_000n,
    prevHeaderHash,
    prevUtxosRoot,
    ...counts,
    depositsRoot: kind === "deposit" ? source.root : SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot:
      kind === "withdrawal" ? source.root : SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot:
      kind === "forced-transaction" ? source.root : SDK.EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: trace.root,
    eventToStepRoot: event.root,
    validationTracesRoot: validation.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.reconstruction.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      counts,
      deposits: [],
      withdrawals: [],
      forced_transactions: [],
      [field]: entries,
      forced_transaction_preimages:
        kind === "forced-transaction"
          ? [...body.forced_transaction_preimages]
          : [],
      transition_trace: traces,
      event_to_step: events,
      validation_traces: validations,
    },
  };
  const payloadEnvelopeCbor = await reencodeFixturePayload(payload);
  const baseline = await buildCanonicalBlockFixture({ transactions: [] });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(baseline, {
      header,
      headerHash,
    }),
    payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "cross-block-retained-test",
      grade: "security",
    },
  });
  return { header, headerHash, payload, payloadEnvelopeCbor, evidence };
};
const DEPOSIT_VALUE_CBOR =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";
const WITHDRAWAL_VALUE_CBOR =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";
