import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { blueprintAbi } from "./sdk-abi-fixtures.header-fixture.js";

type SchemaArm = {
  readonly title: string;
  readonly index: number;
  readonly fields: readonly { readonly title?: string }[];
};

const sdkArms = (
  SDK.SourceMembershipMismatchWitnessSchema as unknown as {
    readonly anyOf: readonly SchemaArm[];
  }
).anyOf.map((arm) => ({
  index: arm.index,
  title: arm.title,
  fields: arm.fields.map((field) => field.title ?? ""),
}));

describe("source-membership witness ABI", () => {
  it("encodes exactly the arms the on-chain witness accepts, in order", () => {
    const onChainArms = blueprintAbi
      .ref(
        "midgard/fraud_proofs/transition_trace/proof/SourceMembershipMismatchWitness",
      )
      .constructors()
      .map(({ index, title, fieldTitles }) => ({
        index,
        title,
        fields: [...fieldTitles],
      }));
    expect(sdkArms).toEqual(onChainArms);
  });
});
