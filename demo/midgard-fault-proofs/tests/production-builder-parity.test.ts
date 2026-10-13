import { readFileSync } from "node:fs";

import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { applyFieldItemWidthIllegalScripts } from "../src/field-item-width-illegal/contracts.js";
import { applyMintDeclaredAssetLimitScripts } from "../src/mint-declared-asset-limit/contracts.js";
import { applyMintItemNonCanonicalScripts } from "../src/mint-item-non-canonical/contracts.js";
import { applyObserverOrderInvalidScripts } from "../src/observer-order-invalid/contracts.js";
import { applyObserversForbiddenScripts } from "../src/observers-forbidden-on-untagged-network/contracts.js";
import { applyOutputReferenceScriptDecodingScripts } from "../src/output-reference-script-decoding/contracts.js";
import { applyProtectedOutputSignerMissingScripts } from "../src/protected-output-signer-missing/contracts.js";
import { applyRedeemerCanonicityScripts } from "../src/redeemer-canonicity/contracts.js";
import { applyResolvedOutputNonCanonicalScripts } from "../src/resolved-output-non-canonical/contracts.js";
import { applySpendInputSignerMissingScripts } from "../src/spend-input-signer-missing/contracts.js";
import { applyTransactionOutputNonCanonicalScripts } from "../src/transaction-output-non-canonical/contracts.js";
import { applyWitnessScriptDecodingScripts } from "../src/witness-script-decoding/contracts.js";
import { applyZeroInputScripts } from "../src/zero-input/contracts.js";
const blueprint: {
  validators: {
    title: string;
    compiledCode: string;
    parameters?: { title: string; schema?: { $ref: string } }[];
  }[];
} = JSON.parse(
  readFileSync(
    new URL("../../../onchain/aiken/plutus.json", import.meta.url),
    "utf8",
  ),
);
const input = {
  blueprint,
  computationThreadPolicyId: "11".repeat(28),
  fraudProofPolicyId: "22".repeat(28),
  fraudProofTokenAddressData: new Constr(0, [
    new Constr(1, ["33".repeat(28)]),
    new Constr(1, []),
  ]),
  fieldPreimageCertificatePolicyId: "44".repeat(28),
  hubOracleScriptHash: "55".repeat(28),
};
const builders = [
  ["field-item-width-illegal", applyFieldItemWidthIllegalScripts],
  ["mint-declared-asset-limit", applyMintDeclaredAssetLimitScripts],
  ["mint-item-non-canonical", applyMintItemNonCanonicalScripts],
  ["observer-order-invalid", applyObserverOrderInvalidScripts],
  ["observers-forbidden-on-untagged-network", applyObserversForbiddenScripts],
  [
    "output-reference-script-decoding",
    applyOutputReferenceScriptDecodingScripts,
  ],
  ["protected-output-signer-missing", applyProtectedOutputSignerMissingScripts],
  ["redeemer-canonicity", applyRedeemerCanonicityScripts],
  ["resolved-output-non-canonical", applyResolvedOutputNonCanonicalScripts],
  ["spend-input-signer-missing", applySpendInputSignerMissingScripts],
  [
    "transaction-output-non-canonical",
    applyTransactionOutputNonCanonicalScripts,
  ],
  ["witness-script-decoding", applyWitnessScriptDecodingScripts],
  ["zero-input", applyZeroInputScripts],
] as const;
describe("production fault-proof construction refusals", () => {
  it.each(builders)(
    "refuses duplicate blueprint titles in %s",
    (_name, build) => {
      const malformed = {
        validators: [...blueprint.validators, ...blueprint.validators],
      };
      expect(() =>
        build({ ...input, blueprint: malformed, network: "Preprod" }),
      ).toThrow(/exactly one/);
    },
  );
  it.each(builders)("refuses under-application in %s", (_name, build) => {
    const malformed = {
      validators: blueprint.validators.map((entry) => ({
        ...entry,
        parameters: [...(entry.parameters ?? []), { title: "unexpected" }],
      })),
    };
    expect(() =>
      build({ ...input, blueprint: malformed, network: "Preprod" }),
    ).toThrow(/declares .* parameter/);
  });
});
