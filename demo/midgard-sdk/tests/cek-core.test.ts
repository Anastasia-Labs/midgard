import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeCekCoreRedeemer,
  hashCekCoreWitness,
} from "../src/fraud-proof/cek-core.js";

describe("CEK core continuation wire", () => {
  it.each(["binder", "arm", "settle"] as const)(
    "encodes %s with the computation-thread Continue constructor",
    (kind) => {
      const witness = new Constr(40, []);
      const transition = new Constr(0, [1n]);
      const step = new Constr(0, [2n]);
      const evidence =
        kind === "binder"
          ? { kind, transition, step }
          : kind === "settle"
            ? { kind, transition, witness }
            : { kind, witness };
      const wire = Data.from(encodeCekCoreRedeemer(3n, 4n, evidence));
      expect(wire).toEqual(
        new Constr(1, [
          new Constr(0, [
            3n,
            4n,
            ...(kind === "binder"
              ? [transition, step]
              : kind === "settle"
                ? [transition, witness]
                : [witness]),
          ]),
        ]),
      );
    },
  );
  it("binds the exact witness constructor and payload", () => {
    const witness = new Constr(40, ["ab"]);
    expect(hashCekCoreWitness(witness)).toHaveLength(64);
    expect(hashCekCoreWitness(witness)).not.toBe(
      hashCekCoreWitness(new Constr(39, ["ab"])),
    );
    expect(hashCekCoreWitness(witness)).not.toBe(
      hashCekCoreWitness(new Constr(40, ["ac"])),
    );
  });
});
