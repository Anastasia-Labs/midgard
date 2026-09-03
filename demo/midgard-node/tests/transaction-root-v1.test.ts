import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { encodeForcedInclusionValueV1 } from "../src/database/forcedTransactions.js";
import { ForcedTransactionsDB } from "../src/database/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../src/workers/commit-block-header/transition-roots.js";
import { encodeTransactionRootValue as encodeProductionTransactionRootValue } from "../src/workers/utils/mpf.js";

type GoldenEntry = {
  readonly name: string;
  readonly keyHex: string;
  readonly valueCborHex: string;
  readonly canonicalTransactionCborHex: string;
  readonly txIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly compactCborHex: string;
  readonly witnessSetCompactCborHex: string;
  readonly fieldPreimageLengthsCborHex: string;
};

type GoldenForcedEntry = GoldenEntry & {
  /**
   * The `OperatorVerdictV1` the canonical input asked for, named by its
   * constructor arm: `ForcedTxValid`, or the `RejectionReasonV1` arm carried
   * by `ForcedTxInvalid`. The arm names — not a coarse validity bucket — are
   * the fixture's verdict vocabulary since the #640 format wave.
   */
  readonly verdict: string;
  readonly orderId: {
    readonly transactionId: string;
    readonly outputIndex: string;
  };
};

type GoldenRoot = {
  readonly domain: SDK.RootDomain;
  readonly count: number;
  readonly phasRootHex: string;
  readonly rootHex: string;
  readonly orderedEntries: readonly {
    readonly name: string;
    readonly keyHex: string;
    readonly valueCborHex: string;
  }[];
};

type Golden = {
  readonly schema: string;
  readonly version: number;
  readonly transactions: readonly GoldenEntry[];
  readonly forcedOrders: readonly GoldenForcedEntry[];
  readonly roots: {
    readonly transactions: GoldenRoot;
    readonly forcedTransactions: GoldenRoot;
  };
};

const fixture = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL("./fixtures/transaction-root-v1.generated.json", import.meta.url),
    ),
    "utf8",
  ),
) as Golden;

const run = <A, E>(effect: Effect.Effect<A, E>): Promise<A> =>
  Effect.runPromise(effect);

const assertSourceFields = (
  decoded: SDK.L2TransactionSourceV1 | SDK.ForcedInclusionTxV1,
  entry: GoldenEntry,
) => {
  expect(decoded.tx_id).toBe(entry.txIdHex);
  expect(decoded.source).toEqual({
    compact_cbor: entry.compactCborHex,
    witness_set_compact_cbor: entry.witnessSetCompactCborHex,
    field_preimage_lengths_cbor: entry.fieldPreimageLengthsCborHex,
  });
};

describe("RF-031 transaction and forced root V1 golden contract", () => {
  it("exercises the production normal transaction-root encoder", () => {
    expect(fixture.schema).toBe("midgard-transaction-root-v1-golden");
    expect(fixture.version).toBe(1);
    for (const entry of fixture.transactions) {
      const canonical = Buffer.from(entry.canonicalTransactionCborHex, "hex");
      const encoded = encodeProductionTransactionRootValue(
        canonical,
        MIDGARD_CONSENSUS_PROFILE_V1,
      );
      expect(encoded.toString("hex")).toBe(entry.valueCborHex);
      const decoded = Data.from(
        entry.valueCborHex,
        SDK.L2TransactionSourceV1,
      ) as SDK.L2TransactionSourceV1;
      expect(Data.to(decoded, SDK.L2TransactionSourceV1)).toBe(
        entry.valueCborHex,
      );
      assertSourceFields(decoded, entry);
      expect(entry.keyHex).toBe(entry.txIdHex);
    }
  });

  it("exercises the production forced-order encoder and every exact verdict constructor", async () => {
    expect(new Set(fixture.forcedOrders.map((entry) => entry.verdict))).toEqual(
      new Set<string>([
        "ForcedTxValid",
        "InputNotFound",
        "AddressWitnessSignatureInvalid",
        "PlutusExecutionFailed",
        "FeeBelowMinimum",
        "ValueNotPreserved",
      ]),
    );
    for (const entry of fixture.forcedOrders) {
      const decoded = Data.from(
        entry.valueCborHex,
        SDK.ForcedInclusionTxV1,
      ) as SDK.ForcedInclusionTxV1;
      expect(Data.to(decoded, SDK.ForcedInclusionTxV1)).toBe(
        entry.valueCborHex,
      );
      assertSourceFields(decoded, entry);
      // The leaf's own constructor tags must spell the verdict the canonical
      // input named — the fixture's arm name is checked against the decoded
      // value, never against a table restated here.
      expect(
        decoded.verdict === "ForcedTxValid"
          ? "ForcedTxValid"
          : SDK.rejectionReasonArmOf(decoded.verdict.ForcedTxInvalid.reason),
      ).toBe(entry.verdict);
      const encoded = await run(
        encodeForcedInclusionValueV1({
          nativeTxCbor: Buffer.from(entry.canonicalTransactionCborHex, "hex"),
          verdict: decoded.verdict,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
        }),
      );
      expect(encoded.value.toString("hex")).toBe(entry.valueCborHex);
      const orderId = {
        transactionId: entry.orderId.transactionId,
        outputIndex: BigInt(entry.orderId.outputIndex),
      } satisfies SDK.OutputReference;
      expect(
        Buffer.from(Data.to(orderId, SDK.OutputReference), "hex").toString(
          "hex",
        ),
      ).toBe(entry.keyHex);
    }
  });

  it("binds both exact key domains, sorted order, values, counts, and counted roots", async () => {
    const normalRoot = await run(
      buildAuthenticatedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.transactionsV1,
        fixture.transactions.map((entry) => ({
          key: Buffer.from(entry.keyHex, "hex"),
          value: Buffer.from(entry.valueCborHex, "hex"),
        })),
      ),
    );
    expect(normalRoot.root).toBe(fixture.roots.transactions.rootHex);
    expect(normalRoot.phasRoot).toBe(fixture.roots.transactions.phasRootHex);
    expect(normalRoot.count).toBe(BigInt(fixture.roots.transactions.count));
    expect(
      normalRoot.entries.map((entry) => entry.key.toString("hex")),
    ).toEqual(
      fixture.roots.transactions.orderedEntries.map((entry) => entry.keyHex),
    );

    const forcedRoot = await run(
      buildAuthenticatedRootFromEncodedEntries(
        SDK.ROOT_DOMAINS.forcedTransactionsV1,
        fixture.forcedOrders.map((entry) => ({
          key: Buffer.from(entry.keyHex, "hex"),
          value: Buffer.from(entry.valueCborHex, "hex"),
        })),
      ),
    );
    expect(forcedRoot.root).toBe(fixture.roots.forcedTransactions.rootHex);
    expect(forcedRoot.phasRoot).toBe(
      fixture.roots.forcedTransactions.phasRootHex,
    );
    expect(forcedRoot.count).toBe(
      BigInt(fixture.roots.forcedTransactions.count),
    );
    expect(forcedRoot.domain).toBe(SDK.ROOT_DOMAINS.forcedTransactionsV1);
    expect(forcedRoot.root).not.toBe(normalRoot.root);
    expect(
      fixture.roots.forcedTransactions.orderedEntries.map(
        (entry) => entry.keyHex,
      ),
    ).toEqual(
      [...fixture.forcedOrders]
        .sort((left, right) =>
          Buffer.compare(
            Buffer.from(left.keyHex, "hex"),
            Buffer.from(right.keyHex, "hex"),
          ),
        )
        .map((entry) => entry.keyHex),
    );
  });

  it("does not allow a forced source to masquerade as a normal source", () => {
    const forced = fixture.forcedOrders[0]!;
    expect(() =>
      Data.from(forced.valueCborHex, SDK.L2TransactionSourceV1),
    ).toThrow();
    expect(ForcedTransactionsDB.Columns.TX_ORDER_ID).toBe("tx_order_id");
  });
});
