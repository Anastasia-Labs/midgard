import {
  applyDoubleCborEncoding,
  CML,
  type Script,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { RUNTIME_FUNDING_TEST_PARAMETERS } from "./helpers/runtime-funding-policy-fixture.js";
import {
  fundingAddress,
  fundingReferenceOutRef,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";
import { prepareRuntimeFunding } from "./workflow-runtime.slash-funding-fixture.js";

const native: Script = {
  type: "Native",
  script: `8200581c${"a5".repeat(28)}`,
};
const plutus = (type: "PlutusV1" | "PlutusV2" | "PlutusV3"): Script => ({
  type,
  script: applyDoubleCborEncoding("4d01000033222220051200120011"),
});

/** CML decodes the carried program independently of the funding counter. */
const ledgerBytes = (script: Script): number => {
  switch (script.type) {
    case "Native":
      return CML.NativeScript.from_cbor_hex(script.script).to_cbor_bytes()
        .length;
    case "PlutusV1":
      return CML.PlutusV1Script.from_cbor_hex(script.script).to_raw_bytes()
        .length;
    case "PlutusV2":
      return CML.PlutusV2Script.from_cbor_hex(script.script).to_raw_bytes()
        .length;
    case "PlutusV3":
      return CML.PlutusV3Script.from_cbor_hex(script.script).to_raw_bytes()
        .length;
  }
};

const fixture = async (script: Script, count: 1 | 2) => {
  const secondOutRef = `${"75".repeat(32)}#0`;
  const runtime = await runtimeFunding("step-one", {
    governedReference: script,
    resolvedReference: script,
    referenceOutRefs:
      count === 1
        ? [fundingReferenceOutRef]
        : [fundingReferenceOutRef, secondOutRef],
    ...(count === 1
      ? {}
      : {
          additionalInputs: [
            {
              txHash: "75".repeat(32),
              outputIndex: 0,
              address: fundingAddress,
              assets: { lovelace: 2_000_000n },
              scriptRef: script,
            },
          ],
        }),
  });
  const referenceOutRefs =
    count === 1
      ? [fundingReferenceOutRef]
      : [fundingReferenceOutRef, secondOutRef];
  const total = runtime.snapshot.activeInputs
    .filter((input) => runtime.selected.fundingOutRefs.includes(input.outRef))
    .reduce((sum, input) => sum + BigInt(input.lovelace), 0n);
  const sign = (fee: bigint) =>
    signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: total - fee,
      referenceOutRefs,
      fee,
    });
  const parameters = RUNTIME_FUNDING_TEST_PARAMETERS;
  const linear = CML.LinearFee.new(
    BigInt(parameters.minFeeA),
    BigInt(parameters.minFeeB),
    15n,
  );
  const prices = CML.ExUnitPrices.new(
    CML.SubCoin.new(577n, 10_000n),
    CML.SubCoin.new(721n, 10_000_000n),
  );
  let fee = 200_000n;
  for (let iteration = 0; iteration < 3; iteration += 1)
    fee = CML.min_fee(
      sign(fee).toTransaction(),
      linear,
      prices,
      BigInt(ledgerBytes(script) * count),
    );
  expect(
    CML.min_fee(
      sign(fee).toTransaction(),
      linear,
      prices,
      BigInt(ledgerBytes(script) * count),
    ),
  ).toBe(fee);
  return { runtime, sign, fee };
};

it.each([native, plutus("PlutusV1"), plutus("PlutusV2"), plutus("PlutusV3")])(
  "admits a %s reference at the CML ledger minimum and refuses one lovelace below",
  async (script) => {
    const { runtime, sign, fee } = await fixture(script, 1);
    await expect(
      prepareRuntimeFunding(runtime, sign(fee - 1n)),
    ).rejects.toThrow(
      "funding transaction fee is outside the live protocol funding bounds",
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
    await prepareRuntimeFunding(runtime, sign(fee));
    expect(runtime.prepare).toHaveBeenCalledOnce();
  },
);

it.each([native, plutus("PlutusV3")])(
  "charges two physical occurrences of a %s reference independently",
  async (script) => {
    const { runtime, sign, fee } = await fixture(script, 2);
    await expect(
      prepareRuntimeFunding(
        runtime,
        sign(fee - BigInt(ledgerBytes(script)) * 15n),
      ),
    ).rejects.toThrow(
      "funding transaction fee is outside the live protocol funding bounds",
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
    await prepareRuntimeFunding(runtime, sign(fee));
    expect(runtime.prepare).toHaveBeenCalledOnce();
  },
);
