/** The redeemer select our runner plans for a CEK context step of the
 * redeemer select authenticate, from its evidence, the context control and
 * the native facts: the selection's fields, the summary of the purpose a
 * script of the context's language sees (none for a descriptor-only
 * receive), and the item scan's initial control in the mode that purpose
 * asks for; no select for the skip of a native execution; and its refusal
 * of evidence that is neither. */
import {
  hashMidgardRedeemerItemLeaf,
  hashMidgardRedeemerItemProofControl,
  initialMidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofMode,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import { summarizeMidgardCekLucidData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { planValidationTraceContextRedeemerSelect } from "../src/validation-trace-dispute/one-step-context-redeemer-select.js";

const SCRIPT = "ab".repeat(28);
const COMMITMENT = "cd".repeat(32);
const summary = new Constr<Data>(0, ["", 0n, 0n]);
const control = new Constr<Data>(0, [
  0n,
  new Constr(0, ["", 0n, 0n, 0n]),
  "",
  "",
  summary,
  summary,
  3n,
]);

const select = (purposeKind: bigint, languageTag: bigint) =>
  planValidationTraceContextRedeemerSelect({
    auxiliaryCbor: Data.to(
      new Constr(17, [
        control,
        1n,
        40n,
        COMMITMENT,
        purposeKind,
        0n,
        SCRIPT,
        "",
        languageTag,
        "ef".repeat(32),
        [],
      ]),
    ),
    context: new Constr(0, [9n, languageTag]),
    nativeFactsCbor: Data.to(
      new Constr(0, [
        ...Array<Data>(8).fill(0n),
        2n,
        ...Array<Data>(7).fill(0n),
      ]),
    ),
  });

const initialHash = (mode: MidgardRedeemerItemProofMode, tag: number) =>
  Data.to(
    hashMidgardRedeemerItemProofControl(
      initialMidgardRedeemerItemProofControl({
        mode,
        itemIndex: 1,
        itemCount: 2,
        totalLength: 40,
        itemCommitment: Buffer.from(COMMITMENT, "hex"),
        expectedPurposeTag: tag,
        expectedPointerIndex: 0,
      }),
    ).toString("hex"),
  );

const some = (purpose: Data) => {
  const { root, cborLength, memory } = summarizeMidgardCekLucidData(
    purpose as never,
  );
  return Data.to(
    new Constr(0, [
      new Constr(0, [Buffer.from(root).toString("hex"), cborLength, memory]),
    ]),
  );
};

describe("validation-trace one-step CEK context redeemer select plan", () => {
  it("selects the redeemer below the purpose bound, counting the transaction's redeemers", () => {
    expect(select(1n, 3n)!.selectionFieldsCbor).toStrictEqual(
      [
        control,
        1n,
        2n,
        40n,
        COMMITMENT,
        hashMidgardRedeemerItemLeaf({
          redeemerIndex: 1,
          itemLength: 40,
          itemCommitment: Buffer.from(COMMITMENT, "hex"),
        }).toString("hex"),
        2n,
        1n,
        0n,
        SCRIPT,
        "",
      ].map((field) => Data.to(field)),
    );
  });

  it("summarises the purpose a script of the context's language sees, and scans its data", () => {
    const observe = select(2n, 3n)!;
    expect(observe.purposeCbor).toBe(
      some(new Constr(2, [new Constr(1, [SCRIPT])])),
    );
    expect(observe.initialHashCbor).toBe(
      initialHash(MidgardRedeemerItemProofModes.Data, 3),
    );
    expect(select(2n, 128n)!.purposeCbor).toBe(some(new Constr(2, [SCRIPT])));
    expect(select(3n, 128n)!.purposeCbor).toBe(some(new Constr(3, [SCRIPT])));
  });

  it("summarises no purpose for a descriptor-only receive, and scans its descriptor", () => {
    const receive = select(3n, 3n)!;
    expect(receive.purposeCbor).toBe(Data.to(new Constr(1, [])));
    expect(receive.initialHashCbor).toBe(
      initialHash(MidgardRedeemerItemProofModes.Descriptor, 6),
    );
  });

  it("plans no select for the skip of a native execution, and refuses other evidence", () => {
    const plan = (auxiliary: Data) =>
      planValidationTraceContextRedeemerSelect({
        auxiliaryCbor: Data.to(auxiliary),
        context: new Constr(0, [9n, 3n]),
        nativeFactsCbor: Data.to(new Constr(0, [])),
      });
    expect(plan(new Constr(36, [control, "", "", []]))).toBeUndefined();
    for (const evidence of [
      new Constr(18, Array<Data>(11).fill(0n)),
      new Constr(17, [control]),
    ])
      expect(() => plan(evidence)).toThrow(/neither a select nor a skip/u);
  });
});
