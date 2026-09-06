import { readFileSync } from "node:fs";

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

describe("CEK context Aiken wire golden", () => {
  it("matches the Aiken binder and staged native/context projection", () => {
    const vector = Data.from(golden.vectorCbor);
    if (
      !(vector instanceof Constr) ||
      vector.index !== 0 ||
      vector.fields.length !== 6
    )
      throw new Error("Invalid context binding golden");
    const [prepared, workWitnessCbor, transactionId, auxiliary, bound, staged] =
      vector.fields;
    if (
      typeof workWitnessCbor !== "string" ||
      typeof transactionId !== "string"
    )
      throw new Error("Invalid context binding source bytes");
    const actual = deriveCekContextBinding({
      prepared: prepared!,
      workWitnessCbor,
      transactionId,
      auxiliary: auxiliary!,
    });
    expect(hashCekCoreWitness(vector)).toBe(golden.blake2b256);
    expect(Data.to(actual.bound)).toBe(Data.to(bound!));
    expect(Data.to(actual.staged)).toBe(Data.to(staged!));
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
