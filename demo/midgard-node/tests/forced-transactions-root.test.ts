import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { ForcedTransactionsDB } from "@/database/index.js";
import type { DatabaseError } from "@/database/utils/common.js";
import { resolveForcedTransactionsRoot } from "@/workers/commit-block-header/event-roots.js";
import {
  buildAuthenticatedRootFromEncodedEntries,
  buildRootMembershipProof,
  verifyRootMembershipProof,
} from "@/workers/commit-block-header/transition-roots.js";
import { keyValuePhasProof } from "@/workers/utils/mpf.js";

import { deterministicFixtureTxHash } from "./utils.js";

const h32 = (label: string): string =>
  deterministicFixtureTxHash(label).toString("hex");

const outputReference = (
  label: string,
  outputIndex: bigint,
): SDK.OutputReference => ({
  transactionId: h32(label),
  outputIndex,
});

const outputReferenceCbor = (value: SDK.OutputReference): Buffer =>
  Buffer.from(Data.to(value, SDK.OutputReference), "hex");

const compactTx = (validity: SDK.MidgardTxValidity): SDK.MidgardTxCompact => ({
  body: {
    spend_inputs: h32("spend-inputs"),
    reference_inputs: h32("reference-inputs"),
    outputs: h32("outputs"),
    fee: 0n,
    validity_interval: {
      lower_bound: {
        bound_type: "NegativeInfinity",
        is_inclusive: true,
      },
      upper_bound: {
        bound_type: "PositiveInfinity",
        is_inclusive: false,
      },
    },
    required_observers: h32("required-observers"),
    required_signer_hashes: h32("required-signers"),
    mint: h32("mint"),
    script_integrity_hash: h32("script-integrity"),
    auxiliary_data_hash: h32("auxiliary-data"),
    network_id: "Testnet",
  },
  wits: h32("wits"),
  validity,
});

const forcedEntry = ({
  label,
  txOrderId,
  txCompact,
  inclusionTime,
}: {
  readonly label: string;
  readonly txOrderId: SDK.OutputReference;
  readonly txCompact: SDK.MidgardTxCompact;
  readonly inclusionTime: Date;
}): Effect.Effect<ForcedTransactionsDB.Entry, DatabaseError> =>
  Effect.gen(function* () {
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValue({
      txCompact,
      operatorValidity: txCompact.validity,
    });
    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]:
        outputReferenceCbor(txOrderId),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]:
        deterministicFixtureTxHash(`l1-${label}`),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: Number(
        txOrderId.outputIndex,
      ),
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from(
        `aa${label}`,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from(
        `d879${label}`,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: Buffer.from(
        Data.to(txCompact, SDK.MidgardTxCompact),
        "hex",
      ),
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: txCompact.validity,
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

describe("forced transaction source roots", () => {
  it.effect(
    "keys forced source events by L1 tx-order output reference, not L2 tx id",
    () =>
      Effect.gen(function* () {
        const txCompact = compactTx("TxIsValid");
        const first = yield* forcedEntry({
          label: "01",
          txOrderId: outputReference("same-l2-first-order", 0n),
          txCompact,
          inclusionTime: new Date("2026-06-12T00:00:01.000Z"),
        });
        const second = yield* forcedEntry({
          label: "02",
          txOrderId: outputReference("same-l2-second-order", 1n),
          txCompact,
          inclusionTime: new Date("2026-06-12T00:00:02.000Z"),
        });

        expect(first[ForcedTransactionsDB.Columns.TX_ID]).toEqual(
          second[ForcedTransactionsDB.Columns.TX_ID],
        );
        expect(first[ForcedTransactionsDB.Columns.TX_ORDER_ID]).not.toEqual(
          second[ForcedTransactionsDB.Columns.TX_ORDER_ID],
        );

        const root = yield* resolveForcedTransactionsRoot([first, second]);
        expect(root._tag).toBe("Some");
        if (root._tag === "Some") {
          expect(root.value).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
          const emptyCommit = yield* SDK.commitCountedRootProgram({
            domain: SDK.ROOT_DOMAINS.forcedTransactions,
            phasRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            count: 0n,
          });
          expect(root.value).not.toBe(emptyCommit);
        }
      }),
  );

  it.effect("includes invalid forced transactions as source events", () =>
    Effect.gen(function* () {
      const txOrderId = outputReference("invalid-forced-order", 0n);
      const invalidTxCompact = compactTx("NonExistentInputUtxo");
      const invalid = yield* forcedEntry({
        label: "03",
        txOrderId,
        txCompact: invalidTxCompact,
        inclusionTime: new Date("2026-06-12T00:00:03.000Z"),
      });
      const built = yield* buildAuthenticatedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.forcedTransactions,
        [ForcedTransactionsDB.toRootKeyValue(invalid)],
      );
      const decodedValue = Data.from(
        invalid[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE].toString(
          "hex",
        ),
        SDK.ForcedInclusionTx,
      ) as SDK.ForcedInclusionTx;
      const membership = yield* buildRootMembershipProof({
        root: {
          ...built,
          typedEntries: [
            {
              key: txOrderId,
              value: decodedValue,
            },
          ],
        },
        key: txOrderId,
        value: decodedValue,
        keySchema: SDK.OutputReferenceSchema,
        valueSchema: SDK.ForcedInclusionTxSchema,
      });

      expect(decodedValue.operator_validity).toBe("NonExistentInputUtxo");
      expect(decodedValue.tx_compact).toEqual({
        body: invalidTxCompact.body,
        wits: invalidTxCompact.wits,
      });
      expect(Object.keys(decodedValue)).toEqual([
        "tx_compact",
        "operator_validity",
      ]);
      yield* verifyRootMembershipProof({
        witness: membership,
        keySchema: SDK.OutputReferenceSchema,
        valueSchema: SDK.ForcedInclusionTxSchema,
        options: {
          expectedDomain: SDK.ROOT_DOMAINS.forcedTransactions,
          expectedRoot: built.root,
          expectedCount: 1n,
        },
      });
    }),
  );

  it.effect("rejects duplicate tx-order source keys", () =>
    Effect.gen(function* () {
      const txOrderId = outputReference("duplicate-forced-order", 0n);
      const first = yield* forcedEntry({
        label: "04",
        txOrderId,
        txCompact: compactTx("TxIsValid"),
        inclusionTime: new Date("2026-06-12T00:00:04.000Z"),
      });
      const second = {
        ...first,
        [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: Buffer.from(
          first[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
        ),
      };
      second[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE][0] ^= 1;

      const result = yield* resolveForcedTransactionsRoot([first, second]).pipe(
        Effect.either,
      );

      expect(result._tag).toBe("Left");
    }),
  );

  it.effect(
    "domain-separates forced tx-order settlement proofs from transactions_root",
    () =>
      Effect.gen(function* () {
        const txOrderId = outputReference("settlement-forced-order", 0n);
        const forced = yield* forcedEntry({
          label: "05",
          txOrderId,
          txCompact: compactTx("TxIsValid"),
          inclusionTime: new Date("2026-06-12T00:00:05.000Z"),
        });
        const entry = ForcedTransactionsDB.toRootKeyValue(forced);
        const forcedRoot = yield* buildAuthenticatedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.forcedTransactions,
          [entry],
        );
        const transactionsRoot =
          yield* buildAuthenticatedRootFromEncodedEntries(
            SDK.ROOT_DOMAINS.transactions,
            [entry],
          );
        const proof = yield* keyValuePhasProof(
          [entry.key],
          [entry.value],
          entry.key,
        );
        const txOrderSettlementProof: SDK.RawRootMembershipProof = {
          domain: SDK.ROOT_DOMAINS.forcedTransactions,
          root: forcedRoot.root,
          phas_root: forcedRoot.phasRoot,
          count: forcedRoot.count,
          key: entry.key.toString("hex"),
          value: entry.value.toString("hex"),
          proof,
        };

        expect(forcedRoot.phasRoot).toBe(transactionsRoot.phasRoot);
        expect(txOrderSettlementProof.root).toBe(forcedRoot.root);
        expect(txOrderSettlementProof.root).not.toBe(transactionsRoot.root);
        expect(txOrderSettlementProof.domain).toBe(
          SDK.ROOT_DOMAINS.forcedTransactions,
        );
      }),
  );
});
