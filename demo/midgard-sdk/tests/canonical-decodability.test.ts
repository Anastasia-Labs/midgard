import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  canonicalDecodabilityEvidenceFromCommittedField,
  CanonicalDecodabilityStep02State,
  canonicalDecodabilityStep02StateFromEvidence,
  CanonicalDecodabilityStep02StateSchema,
  CommittedFieldClaim,
  isCanonicalDecodabilityViolation,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_ENVELOPE_VERDICT_CODE_COUNT,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NAMES,
  MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES,
  midgardEnvelopeVerdict,
  miscountedMidgardFieldPreimage,
} from "../src/fraud-proof/canonical-decodability.js";

/**
 * The TypeScript half of the §12.7 golden channel.
 *
 * The generator emits the fixture from `dist/`; this suite recomputes every
 * vector from `src/`, so a twin edit that has not been rebuilt into a fixture
 * fails here rather than shipping. The Aiken half —
 * `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak`
 * — recomputes the same verdicts under the fork runner. Neither half is
 * authoritative on its own: the fixture is what they are both held to.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/canonical-decodability-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly verdictCodeCount: number;
  readonly verdictNames: readonly string[];
  readonly vectors: readonly {
    readonly label: string;
    readonly note: string;
    readonly preimage: string;
    readonly byteCount: number;
    readonly verdict: number;
    readonly verdictName: string;
  }[];
  readonly wireVectors: readonly {
    readonly label: string;
    readonly aikenType: string;
    readonly value: unknown;
    readonly cborHex: string;
  }[];
};

const bytes = (hexValue: string) => Buffer.from(hexValue, "hex");

describe("§12.7 canonical-decodability golden channel", () => {
  it("pins the channel's identity", () => {
    expect(golden.schema).toBe("midgard-canonical-decodability-golden");
    expect(golden.version).toBe(1);
    expect(golden.specDocument).toBe("docs/spec/midgard-tx.md");
    expect(golden.verdictCodeCount).toBe(MIDGARD_ENVELOPE_VERDICT_CODE_COUNT);
    expect(golden.verdictNames).toEqual([...MIDGARD_ENVELOPE_VERDICT_NAMES]);
  });

  it("recomputes every vector's verdict from src", () => {
    expect(golden.vectors.length).toBeGreaterThan(0);
    for (const vector of golden.vectors) {
      const preimage = bytes(vector.preimage);
      expect(preimage.length).toBe(vector.byteCount);
      expect(midgardEnvelopeVerdict(preimage)).toBe(vector.verdict);
      expect(MIDGARD_ENVELOPE_VERDICT_NAMES[vector.verdict]).toBe(
        vector.verdictName,
      );
    }
  });

  it("reaches every verdict code, and no code outside the space", () => {
    const reached = new Set(golden.vectors.map((vector) => vector.verdict));
    expect(reached.size).toBe(MIDGARD_ENVELOPE_VERDICT_CODE_COUNT);
    for (const code of reached) {
      expect(code).toBeGreaterThanOrEqual(0);
      expect(code).toBeLessThan(MIDGARD_ENVELOPE_VERDICT_CODE_COUNT);
    }
  });

  it("re-encodes every wire vector to the same bytes", () => {
    expect(golden.wireVectors.length).toBeGreaterThan(0);
    for (const vector of golden.wireVectors) {
      const schema =
        vector.aikenType === "State"
          ? CanonicalDecodabilityStep02State
          : CommittedFieldClaim;
      const decoded = Data.from(vector.cborHex, schema);
      expect(Data.to(decoded, schema)).toBe(vector.cborHex);
    }
  });

  it("pins CommittedFieldClaimV1's constructor order as wire format", () => {
    // `BodyFieldClaim` is Constr 0 and `WitnessFieldClaim` Constr 1. Reordering
    // the Aiken sum re-tags every claim a builder emits, and the leading byte
    // below is the only place that is visible.
    const body = golden.wireVectors.find(
      (vector) => vector.label === "claim_body_inline",
    );
    const witness = golden.wireVectors.find(
      (vector) => vector.label === "claim_witness_raw_utxo",
    );
    expect(body?.cborHex.startsWith("d8799f")).toBe(true);
    expect(witness?.cborHex.startsWith("d87a9f")).toBe(true);
  });
});

describe("§12.7 envelope verdict", () => {
  it("calls the empty field grammatical and one byte less ungrammatical", () => {
    expect(midgardEnvelopeVerdict(Buffer.from([0x80]))).toBe(
      MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
    );
    expect(midgardEnvelopeVerdict(Buffer.alloc(0))).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER,
    );
  });

  it("agrees with the honest producer at every header width", () => {
    for (const count of [0, 1, 23, 24, 255, 256, 300]) {
      const items = Array.from({ length: count }, () =>
        Buffer.from([0xde, 0xad]),
      );
      expect(midgardEnvelopeVerdict(encodeMidgardFieldPreimage(items))).toBe(
        MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
      );
      // The deliberately-wrong producer is only wrong when it is asked to be.
      expect(miscountedMidgardFieldPreimage(count, items)).toEqual(
        encodeMidgardFieldPreimage(items),
      );
    }
  });

  it("refuses a declared count in either direction", () => {
    const item = Buffer.from([0xde, 0xad]);
    expect(
      midgardEnvelopeVerdict(miscountedMidgardFieldPreimage(2, [item])),
    ).not.toBe(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL);
    expect(
      midgardEnvelopeVerdict(miscountedMidgardFieldPreimage(1, [item, item])),
    ).toBe(MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES);
  });

  it("refuses a count or a length §5.1 cannot spell", () => {
    // §5.1's widest header is the two-byte `99 NNNN` / `59 LLLL`, and the Aiken
    // twin aborts in `from_int_big_endian(_, 2)` above it. The producer must
    // not instead truncate to the low sixteen bits and hand back an envelope
    // declaring a number nobody asked for.
    expect(() => miscountedMidgardFieldPreimage(0x1_0000, [])).toThrow();
    expect(() =>
      miscountedMidgardFieldPreimage(1, [Buffer.alloc(0x1_0000)]),
    ).toThrow();
    // The boundary itself is still writable.
    expect(miscountedMidgardFieldPreimage(0xffff, [])).toEqual(
      Buffer.from([0x99, 0xff, 0xff]),
    );
  });

  it("never throws, on any prefix of any vector", () => {
    // Totality is the property the whole family rests on: a verdict that could
    // throw would be the door again. Every prefix of every vector is a byte
    // string an operator could commit, so every one of them must have an
    // answer — including the ones that truncate a header mid-width.
    for (const vector of golden.vectors) {
      const preimage = bytes(vector.preimage);
      for (let length = 0; length <= preimage.length; length += 1) {
        const verdict = midgardEnvelopeVerdict(preimage.subarray(0, length));
        expect(Number.isInteger(verdict)).toBe(true);
        expect(verdict).toBeGreaterThanOrEqual(0);
        expect(verdict).toBeLessThan(MIDGARD_ENVELOPE_VERDICT_CODE_COUNT);
      }
    }
  });

  it("answers at §5.4's byte bound without recursing", () => {
    // §5.1's minimum-width item is the empty one, `40`, so 32,764 items plus a
    // three-byte header and one trailing byte is exactly §5.4's per-field bound
    // and is the highest item cardinality any committed field can reach. It is
    // why the twin iterates where the Aiken side recurses: a recursive
    // JavaScript walk overflows the stack here, which would be the abort this
    // family exists to remove, reintroduced off chain.
    const items = Array.from({ length: 32_764 }, () => Buffer.alloc(0));
    const preimage = Buffer.concat([
      encodeMidgardFieldPreimage(items),
      Buffer.from([0x41]),
    ]);
    expect(preimage.length).toBe(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
    expect(midgardEnvelopeVerdict(preimage)).toBe(
      MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES,
    );
  });
});

describe("§12.7 adjudication predicate", () => {
  it("convicts every in-range non-zero code and no grammatical one", () => {
    for (
      let fieldIndex = 0;
      fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
      fieldIndex += 1
    ) {
      expect(
        isCanonicalDecodabilityViolation({
          fieldIndex,
          verdict: MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
        }),
      ).toBe(false);
      for (
        let verdict = 1;
        verdict < MIDGARD_ENVELOPE_VERDICT_CODE_COUNT;
        verdict += 1
      ) {
        expect(isCanonicalDecodabilityViolation({ fieldIndex, verdict })).toBe(
          true,
        );
      }
    }
  });

  it("refuses a state no step 01 could have written", () => {
    const verdict = MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES;
    expect(isCanonicalDecodabilityViolation({ fieldIndex: -1, verdict })).toBe(
      false,
    );
    expect(
      isCanonicalDecodabilityViolation({
        fieldIndex: MIDGARD_COMMITTED_FIELD_COUNT,
        verdict,
      }),
    ).toBe(false);
    expect(
      isCanonicalDecodabilityViolation({
        fieldIndex: 0,
        verdict: MIDGARD_ENVELOPE_VERDICT_CODE_COUNT,
      }),
    ).toBe(false);
    expect(
      isCanonicalDecodabilityViolation({ fieldIndex: 0, verdict: -1 }),
    ).toBe(false);
  });
});

describe("§12.7 evidence", () => {
  const badTxId = "22".repeat(32);

  it("builds the step-02 state the on-chain step 01 derives", () => {
    const preimage = miscountedMidgardFieldPreimage(1, [
      Buffer.from([0xde, 0xad]),
      Buffer.from([0xbe, 0xef]),
    ]);
    const evidence = canonicalDecodabilityEvidenceFromCommittedField({
      badTxId,
      fieldIndex: 2,
      committedPreimage: preimage,
    });
    expect(evidence.violationId).toBe(CANONICAL_DECODABILITY_VIOLATION_ID);
    expect(evidence.verdict).toBe(MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES);
    expect(evidence.verdictName).toBe("trailing_bytes");
    expect(evidence.isViolation).toBe(true);
    expect(evidence.committedPreimageByteCount).toBe(preimage.length);

    const state = canonicalDecodabilityStep02StateFromEvidence(evidence);
    expect(state).toEqual({
      bad_tx_id: badTxId,
      field_index: 2n,
      verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES),
    });
    // The state is a datum, so it has to survive the encoder a builder uses.
    expect(
      Data.from(
        Data.to(state, CanonicalDecodabilityStep02State),
        CanonicalDecodabilityStep02State,
      ),
    ).toEqual(state);
    expect(CanonicalDecodabilityStep02StateSchema).toBeDefined();
  });

  it("never convicts an honest field", () => {
    const evidence = canonicalDecodabilityEvidenceFromCommittedField({
      badTxId,
      fieldIndex: 7,
      committedPreimage: encodeMidgardFieldPreimage([
        Buffer.from([0xde, 0xad]),
      ]),
    });
    expect(evidence.verdict).toBe(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL);
    expect(evidence.verdictName).toBe("grammatical");
    expect(evidence.isViolation).toBe(false);
  });
});
