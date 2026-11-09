/**
 * The field grammar route rule, and the certificate's redeemer index, which
 * names its slot in the ledger's canonical reference-input order rather than
 * the order a builder added the inputs in.
 */
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifiedFieldGrammarRoute,
  FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
  FIELD_GRAMMAR_WALKED_ITEM_LIMIT,
  fieldGrammarRoute,
  routeFaultProofFieldGrammar,
  WALKED_FIELD_GRAMMAR_ROUTE,
} from "../src/field-grammar-certificate.js";

const utxo = (txHashByte: string, outputIndex: number): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: "addr_test1",
  assets: { lovelace: 1n },
});

describe("field grammar route", () => {
  it("walks an item read up to the walked item limit and certifies past it", () => {
    for (const tier of ["Inline", "RawUtxo"] as const) {
      expect(
        fieldGrammarRoute({
          read: "item",
          tier,
          itemCount: FIELD_GRAMMAR_WALKED_ITEM_LIMIT,
        }),
      ).toBe("walked");
      expect(
        fieldGrammarRoute({
          read: "item",
          tier,
          itemCount: FIELD_GRAMMAR_WALKED_ITEM_LIMIT + 1,
        }),
      ).toBe("certified");
    }
  });

  it("walks a count up to the walked count limit, except under tier 3", () => {
    expect(
      fieldGrammarRoute({
        read: "count",
        tier: "RawUtxo",
        itemCount: FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
      }),
    ).toBe("walked");
    expect(
      fieldGrammarRoute({
        read: "count",
        tier: "RawUtxo",
        itemCount: FIELD_GRAMMAR_WALKED_COUNT_LIMIT + 1,
      }),
    ).toBe("certified");
    expect(
      fieldGrammarRoute({ read: "count", tier: "Certified", itemCount: 1 }),
    ).toBe("certified");
    expect(
      fieldGrammarRoute({
        read: "itemAndCount",
        tier: "Certified",
        itemCount: 1,
      }),
    ).toBe("certified");
  });

  it("names the certificate by its canonical reference-input slot", () => {
    const certificate = utxo("91", 0);
    const route = certifiedFieldGrammarRoute(certificate, "route test");
    // Added in this order, the certificate is second; the ledger sorts the
    // set by (txHash, outputIndex), which puts it fourth.
    const added = [
      utxo("7e", 0),
      certificate,
      utxo("5c", 0),
      utxo("d1", 0),
      utxo("3d", 0),
    ];
    expect(route.grammarRefInputIndex(added)).toBe(3n);
    expect(route.referenceInputs).toEqual([certificate]);
    expect(() => route.grammarRefInputIndex([utxo("7e", 0)])).toThrow(
      /not a reference input/u,
    );
  });

  it("carries no certificate on the walked route", () => {
    expect(WALKED_FIELD_GRAMMAR_ROUTE.referenceInputs).toEqual([]);
    expect(
      WALKED_FIELD_GRAMMAR_ROUTE.grammarRefInputIndex([utxo("7e", 0)]),
    ).toBe(null);
  });

  it("walks, as before, when the caller deploys no field grammar certificate", async () => {
    // Nothing is read off these on the walked route.
    const unused = undefined as never;
    for (const tier of ["Inline", "RawUtxo", "Certified"] as const) {
      const route = await routeFaultProofFieldGrammar({
        lucid: unused,
        signer: unused,
        plan: { tier } as never,
        read: "item",
        itemCount: 224,
        source: unused,
        carriageUtxos: [],
        fieldPreimageCertificatePolicyId: "",
        label: "route test",
      });
      expect(route).toBe(WALKED_FIELD_GRAMMAR_ROUTE);
    }
  });
});
