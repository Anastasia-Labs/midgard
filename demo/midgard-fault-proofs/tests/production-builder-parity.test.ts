import { readFileSync } from "node:fs";

import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { applyDistinctAssetAccumulationLimitScripts } from "../src/distinct-asset-accumulation-limit/contracts.js";
import { applyExecutionNativeScriptInvalidScripts } from "../src/execution-native-script-invalid/contracts.js";
import { applyExecutionSourceScriptDecodingScripts } from "../src/execution-source-script-decoding/contracts.js";
import { applyFieldItemWidthIllegalScripts } from "../src/field-item-width-illegal/contracts.js";
import { applyMintDeclaredAssetLimitScripts } from "../src/mint-declared-asset-limit/contracts.js";
import { applyMintItemNonCanonicalScripts } from "../src/mint-item-non-canonical/contracts.js";
import { applyMissingRedeemerScripts } from "../src/missing-redeemer/contracts.js";
import { applyMissingScriptSourceScripts } from "../src/missing-script-source/contracts.js";
import { applyObserverOrderInvalidScripts } from "../src/observer-order-invalid/contracts.js";
import { applyObserversForbiddenScripts } from "../src/observers-forbidden-on-untagged-network/contracts.js";
import { applyOutputReferenceScriptDecodingScripts } from "../src/output-reference-script-decoding/contracts.js";
import { applyProtectedOutputSignerMissingScripts } from "../src/protected-output-signer-missing/contracts.js";
import { applyReceivePurposeLanguageScripts } from "../src/receive-purpose-language/contracts.js";
import { applyRedeemerCanonicityScripts } from "../src/redeemer-canonicity/contracts.js";
import { applyResolvedOutputNonCanonicalScripts } from "../src/resolved-output-non-canonical/contracts.js";
import { applyScriptIntegrityHashMismatchScripts } from "../src/script-integrity-hash-mismatch/contracts.js";
import { applySpendInputSignerMissingScripts } from "../src/spend-input-signer-missing/contracts.js";
import { applyTransactionOutputNonCanonicalScripts } from "../src/transaction-output-non-canonical/contracts.js";
import { applyUnusedRedeemerScripts } from "../src/unused-redeemer/contracts.js";
import { applyUnusedScriptWitnessScripts } from "../src/unused-script-witness/contracts.js";
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
  [
    "distinct-asset-accumulation-limit",
    applyDistinctAssetAccumulationLimitScripts,
  ],
  ["execution-native-script-invalid", applyExecutionNativeScriptInvalidScripts],
  [
    "execution-source-script-decoding",
    applyExecutionSourceScriptDecodingScripts,
  ],
  ["field-item-width-illegal", applyFieldItemWidthIllegalScripts],
  ["mint-declared-asset-limit", applyMintDeclaredAssetLimitScripts],
  ["mint-item-non-canonical", applyMintItemNonCanonicalScripts],
  ["missing-redeemer", applyMissingRedeemerScripts],
  ["missing-script-source", applyMissingScriptSourceScripts],
  ["observer-order-invalid", applyObserverOrderInvalidScripts],
  ["observers-forbidden-on-untagged-network", applyObserversForbiddenScripts],
  [
    "output-reference-script-decoding",
    applyOutputReferenceScriptDecodingScripts,
  ],
  ["protected-output-signer-missing", applyProtectedOutputSignerMissingScripts],
  ["receive-purpose-language", applyReceivePurposeLanguageScripts],
  ["redeemer-canonicity", applyRedeemerCanonicityScripts],
  ["resolved-output-non-canonical", applyResolvedOutputNonCanonicalScripts],
  ["script-integrity-hash-mismatch", applyScriptIntegrityHashMismatchScripts],
  ["spend-input-signer-missing", applySpendInputSignerMissingScripts],
  [
    "transaction-output-non-canonical",
    applyTransactionOutputNonCanonicalScripts,
  ],
  ["unused-redeemer", applyUnusedRedeemerScripts],
  ["unused-script-witness", applyUnusedScriptWitnessScripts],
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
