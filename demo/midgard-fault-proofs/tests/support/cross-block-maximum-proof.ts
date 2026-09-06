import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";
const DEPOSIT_KEY_CBOR =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";
const DEPOSIT_VALUE_CBOR =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";
const WITHDRAWAL_KEY_CBOR = `d8799f5820${"8b".repeat(32)}02ff`;
const WITHDRAWAL_VALUE_CBOR =
  "d8799fd8799fd8799f58207e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e01ff581c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9ca1581c4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4ba14d6d6964676172642d746f6b656e182ad8799fd8799f581c5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5d5dffd87a80ffd87980ff9f5820adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad5840bebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebebeffd87980ff";

const FORCED_KEY_CBOR = `d8799f5820${"9d".repeat(32)}03ff`;
const FORCED_VALUE_CBOR = Data.to(
  {
    tx_id: "ae".repeat(32),
    source: {
      compact_cbor: "80",
      witness_set_compact_cbor: "80",
      field_preimage_lengths_cbor: "80",
    },
    verdict: "ForcedTxValid",
  },
  SDK.ForcedInclusionTxV1,
);

type Variant = "deposit" | "withdrawal" | "forced-transaction";

export const makeMaximumCrossBlockProof = async (variant: Variant) => {
  const keyCbor =
    variant === "deposit"
      ? DEPOSIT_KEY_CBOR
      : variant === "withdrawal"
        ? WITHDRAWAL_KEY_CBOR
        : FORCED_KEY_CBOR;
  let valueCbor =
    variant === "deposit"
      ? DEPOSIT_VALUE_CBOR
      : variant === "withdrawal"
        ? WITHDRAWAL_VALUE_CBOR
        : FORCED_VALUE_CBOR;
  const largeDatum = Array.from({ length: 256 }, () => "ab".repeat(64));
  if (variant === "deposit") {
    const value = Data.from(valueCbor, SDK.DepositInfo);
    value.l2_datum = largeDatum;
    valueCbor = Data.to(value, SDK.DepositInfo);
  } else if (variant === "withdrawal") {
    const value = Data.from(valueCbor, SDK.WithdrawalInfo);
    value.body.l1_datum = { InlineDatum: { data: largeDatum } };
    valueCbor = SDK.committedWithdrawalValueBytes(value);
  } else {
    const value = Data.from(valueCbor, SDK.ForcedInclusionTxV1);
    value.source.compact_cbor = "ab".repeat(16_384);
    valueCbor = Data.to(value, SDK.ForcedInclusionTxV1);
  }
  const domain =
    variant === "deposit"
      ? SDK.ROOT_DOMAINS.deposits
      : variant === "withdrawal"
        ? SDK.ROOT_DOMAINS.withdrawals
        : SDK.ROOT_DOMAINS.forcedTransactionsV1;
  let counted = await buildCountedRoot(domain, [
    {
      key: Buffer.from(keyCbor, "hex"),
      value: Buffer.from(valueCbor, "hex"),
    },
  ]);
  const deep = syntheticDeepMembershipProof({
    key: Buffer.from(keyCbor, "hex"),
    value: Buffer.from(valueCbor, "hex"),
    branchLevels: 64,
  });
  counted = {
    ...counted,
    phasRoot: deep.transactionsPhasRoot,
    root: await Effect.runPromise(
      SDK.commitCountedRootProgram({
        domain,
        phasRoot: deep.transactionsPhasRoot,
        count: 1n,
      }),
    ),
  };
  const proof = Data.from(deep.proofCbor, SDK.Proof);

  const key = Data.from(keyCbor, SDK.OutputReference);
  const committedEvent: SDK.CommittedDuplicateEventProof =
    variant === "deposit"
      ? {
          CommittedDuplicateDepositV1: {
            membership: {
              domain,
              root: counted.root,
              phas_root: counted.phasRoot,
              count: counted.count,
              key,
              value: Data.from(valueCbor, SDK.DepositInfo),
              proof,
            },
          },
        }
      : variant === "withdrawal"
        ? {
            CommittedDuplicateWithdrawalV1: {
              membership: {
                domain,
                root: counted.root,
                phas_root: counted.phasRoot,
                count: counted.count,
                key,
                value: Data.from(valueCbor, SDK.WithdrawalInfo),
                proof,
              },
            },
          }
        : {
            CommittedDuplicateForcedTransactionV1: {
              membership: {
                domain,
                root: counted.root,
                phas_root: counted.phasRoot,
                count: counted.count,
                key,
                value: Data.from(valueCbor, SDK.ForcedInclusionTxV1),
                proof,
              },
            },
          };
  return {
    counted,
    committedEvent: SDK.compactDuplicateEventProof(committedEvent),
    fullValueBytes: valueCbor.length / 2,
  };
};
