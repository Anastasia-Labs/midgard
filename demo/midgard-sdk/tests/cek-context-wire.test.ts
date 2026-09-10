/**
 * `deriveCekContextBinding` decodes the authenticated CEK context wire into the
 * Aiken records the context-step validator binds, and `encodeCekContextRedeemer`
 * pins that validator's redeemer arity.
 *
 * **Oracle, and its known limit.** `tests/fixtures/cek-context-binding.json`
 * carries one context vector together with the `bound` and `staged` records the
 * Aiken binder is claimed to produce for it. That claim is the cross-language
 * half of the oracle and this suite cannot execute the Aiken side, so the
 * fixture comparison alone would rest on provenance the repository does not
 * currently record (there is no generator for this fixture, unlike the
 * package's three `fixtures:*:check` channels). The perturbation cases below
 * therefore carry the discriminating weight: they state the binding rules as
 * observable cause and effect -- change one input, and exactly the slot that
 * input feeds must change -- so an implementation that drops, transposes or
 * ignores an argument fails here regardless of what the fixture's provenance
 * turns out to be.
 */
import { readFileSync } from "node:fs";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveCekContextBinding,
  encodeCekContextRedeemer,
} from "../src/fraud-proof/cek-context.js";
import { hashCekCoreWitness } from "../src/fraud-proof/cek-core.js";

const golden: { vectorCbor: string; blake2b256: string } = JSON.parse(
  readFileSync(
    new URL("./fixtures/cek-context-binding.json", import.meta.url),
    "utf8",
  ),
);

type Vector = {
  readonly prepared: Data;
  readonly workWitnessCbor: string;
  readonly transactionId: string;
  readonly auxiliary: Data;
  readonly bound: Data;
  readonly staged: Data;
};

const readVector = (): Vector => {
  const vector = Data.from(golden.vectorCbor);
  if (
    !(vector instanceof Constr) ||
    vector.index !== 0 ||
    vector.fields.length !== 6
  )
    throw new Error("Invalid context binding golden");
  const [prepared, workWitnessCbor, transactionId, auxiliary, bound, staged] =
    vector.fields;
  if (typeof workWitnessCbor !== "string" || typeof transactionId !== "string")
    throw new Error("Invalid context binding source bytes");
  return {
    prepared: prepared!,
    workWitnessCbor,
    transactionId,
    auxiliary: auxiliary!,
    bound: bound!,
    staged: staged!,
  };
};

/** The 9-field authenticated work witness, decoded so a case can perturb one field. */
const decodeWorkWitness = (workWitnessCbor: string): unknown[] => {
  const fields = decodeSingleCbor(Buffer.from(workWitnessCbor, "hex"));
  if (!Array.isArray(fields) || fields.length !== 9)
    throw new Error("Invalid work witness golden");
  return [...(fields as unknown[])];
};

const encodeWorkWitness = (fields: readonly unknown[]): string =>
  encodeCbor(fields).toString("hex");

const constrFields = (value: Data, label: string): readonly Data[] => {
  if (!(value instanceof Constr) || value.index !== 0)
    throw new Error(`Expected a record for ${label}`);
  return value.fields;
};

describe("CEK context Aiken wire golden", () => {
  it("matches the Aiken binder and staged native/context projection", () => {
    const vector = readVector();
    // The fixture bytes are canonical Plutus data: re-encoding what we decoded
    // reproduces them exactly, so a hand-edit that is not canonical fails here
    // rather than silently changing what the rest of the file compares against.
    expect(Data.to(Data.from(golden.vectorCbor))).toBe(golden.vectorCbor);

    const actual = deriveCekContextBinding(vector);
    expect(Data.to(actual.bound)).toBe(Data.to(vector.bound));
    expect(Data.to(actual.staged)).toBe(Data.to(vector.staged));
  });

  it("binds the auxiliary witness only through its §4 hash slot", () => {
    const vector = readVector();
    const base = deriveCekContextBinding(vector);
    const baseFields = constrFields(base.bound, "bound");
    expect(baseFields).toHaveLength(4);
    expect(baseFields[2]).toBe(hashCekCoreWitness(vector.auxiliary));

    // A different auxiliary witness must move the hash slot -- and nothing
    // else -- so a binder that dropped the auxiliary from the commitment, or
    // committed the auxiliary somewhere it is not authenticated, fails.
    const perturbed = deriveCekContextBinding({
      ...vector,
      auxiliary: new Constr(1, [vector.auxiliary]),
    });
    const perturbedFields = constrFields(perturbed.bound, "bound");
    expect(perturbedFields[2]).toBe(
      hashCekCoreWitness(new Constr(1, [vector.auxiliary])),
    );
    expect(perturbedFields[2]).not.toBe(baseFields[2]);
    expect(Data.to(perturbedFields[0]!)).toBe(Data.to(baseFields[0]!));
    expect(Data.to(perturbedFields[1]!)).toBe(Data.to(baseFields[1]!));
    expect(perturbedFields[3]).toBe(baseFields[3]);
  });

  it("binds the disputed transaction id and re-commits the bound record in the staged projection", () => {
    const vector = readVector();
    const base = deriveCekContextBinding(vector);
    expect(constrFields(base.bound, "bound")[3]).toBe(vector.transactionId);

    const otherId = "5c".repeat(32);
    const perturbed = deriveCekContextBinding({
      ...vector,
      transactionId: otherId,
    });
    expect(constrFields(perturbed.bound, "bound")[3]).toBe(otherId);

    // The staged projection's first field is the bound record itself, so a
    // stage that re-derived the binding instead of carrying it -- or carried a
    // stale one -- shows up as a mismatch here.
    const stagedFields = constrFields(perturbed.staged, "staged");
    expect(stagedFields).toHaveLength(4);
    expect(Data.to(stagedFields[0]!)).toBe(Data.to(perturbed.bound));
    expect(Data.to(stagedFields[0]!)).not.toBe(Data.to(base.bound));
    // The remaining staged fields are projections of the work witness alone and
    // must not move with the transaction id.
    expect(Data.to(stagedFields[1]!)).toBe(
      Data.to(constrFields(base.staged, "staged")[1]!),
    );
    expect(Data.to(stagedFields[2]!)).toBe(
      Data.to(constrFields(base.staged, "staged")[2]!),
    );
    expect(stagedFields[3]).toEqual(constrFields(base.staged, "staged")[3]);
  });

  it("refuses a work witness whose arity or frontier-peak shape is wrong", () => {
    const vector = readVector();
    const fields = decodeWorkWitness(vector.workWitnessCbor);

    // Baseline: the unmodified re-encoding is accepted, so each refusal below
    // is caused by the single change it makes and not by the surgery itself.
    expect(() =>
      deriveCekContextBinding({
        ...vector,
        workWitnessCbor: encodeWorkWitness(fields),
      }),
    ).not.toThrow();

    expect(() =>
      deriveCekContextBinding({
        ...vector,
        workWitnessCbor: encodeWorkWitness(fields.slice(0, 8)),
      }),
    ).toThrow("Expected exact CEK context array of 9 fields");

    // Field 0 carries the 26-field native view as nested CBOR bytes; index 15 of
    // that view is one of the frontier peak lists, whose entries are exactly
    // two fields each. A three-field peak must be refused rather than silently
    // truncated into the record.
    const nativeBytes = fields[0];
    if (!(nativeBytes instanceof Uint8Array))
      throw new Error("Invalid native view golden");
    const native = decodeSingleCbor(nativeBytes);
    if (!Array.isArray(native) || native.length !== 26)
      throw new Error("Invalid native view golden");
    const peaks = native[15];
    if (!Array.isArray(peaks) || peaks.length === 0)
      throw new Error("Invalid frontier peak golden");
    const widenedNative = [...(native as unknown[])];
    widenedNative[15] = [
      [...(peaks[0] as unknown[]), (peaks[0] as unknown[])[0]],
      ...(peaks as unknown[]).slice(1),
    ];
    expect(() =>
      deriveCekContextBinding({
        ...vector,
        workWitnessCbor: encodeWorkWitness([
          encodeCbor(widenedNative),
          ...fields.slice(1),
        ]),
      }),
    ).toThrow("Expected exact CEK context array of 2 fields");
  });

  it("pins original binder wire and the explicit shared-item successor extension", () => {
    const transition = new Constr(0, ["", new Constr(0, [])]);
    const auxiliary = new Constr(0, []);
    expect(
      Data.from(encodeCekContextRedeemer(1n, 2n, transition, auxiliary)),
    ).toEqual(new Constr(1, [new Constr(0, [1n, 2n, transition, auxiliary])]));
    const next = new Constr(0, []);
    expect(
      Data.from(encodeCekContextRedeemer(1n, 2n, transition, auxiliary, next)),
    ).toEqual(
      new Constr(1, [new Constr(0, [1n, 2n, transition, auxiliary, next])]),
    );
  });
});
