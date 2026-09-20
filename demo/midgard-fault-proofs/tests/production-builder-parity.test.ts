import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { Constr, type Network } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { applyDistinctAssetAccumulationLimitScripts } from "../src/distinct-asset-accumulation-limit/contracts.js";
import { applyExecutionNativeScriptInvalidScripts } from "../src/execution-native-script-invalid/contracts.js";
import { applyExecutionSourceScriptDecodingScripts } from "../src/execution-source-script-decoding/contracts.js";
import { applyFieldItemWidthIllegalScripts } from "../src/field-item-width-illegal/contracts.js";
import { buildL2TxMistagChain } from "../src/l2-tx-mistag/contracts.js";
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
// Captured from production adapters at 2bc9bdbcdc21 before consolidation.
const baselines: Record<string, string> = {
  "distinct-asset-accumulation-limit":
    "3c1783e0a51d88db8ac8151296e1e2536975da69c9075e30be5d727c4bae548e",
  "execution-native-script-invalid":
    "05c8f71190645f1baba78d12f4e6754924a343c123900db206742b364a3371ef",
  "execution-source-script-decoding":
    "b39afd1da9d861f8a602e913551dd4528ebc9124a876f212fb728c71dfb4c477",
  "field-item-width-illegal":
    "b344d1bcaec0c680141917415a668e8d302653409647eef6e9eb5ffe0571daba",
  "mint-declared-asset-limit":
    "3679ee0fc063469172b084f096d6a8f688f60de5515bdb20d991055a0eca00d7",
  "mint-item-non-canonical":
    "c38c3cf343635d2c65119d431749ebffef23a56f845f87faa6bf31f51e09ecf2",
  "missing-redeemer":
    "62318f5120dba05ca5d4e06e06e524c20b500a8942e4698bf4d9d99b9c0777d2",
  "missing-script-source":
    "4905a3773de6aca896c9bfc09f187cd063225d439c287630951d224261e2820f",
  "observer-order-invalid":
    "1e1a0ac61c572a71740b25fb31aa3efe516253aa07c1c02c06eeaa8d9930626f",
  "observers-forbidden-on-untagged-network":
    "9646c20e9fa8bfd68dccb9ae6c755c3369d81da2b158a28239d3fea5575423fd",
  "output-reference-script-decoding":
    "b1bfef126ce6cd32654ef63bbd203bfa05eb07d6573c61e025187246434e835c",
  "protected-output-signer-missing":
    "b62f872fd755a572055337b6cad6bb653696b37f615b47c645d8c7ae9682462e",
  "receive-purpose-language":
    "199ee69cc7b6b859c8fdd67d7191e48081f119c11664dc162544d5c0fa874eac",
  "redeemer-canonicity":
    "5659ea938a5169e07ed1e43b6e26b89e923e12e416c50331edcf25ce7cd51b4d",
  "resolved-output-non-canonical":
    "16f239aaa016efe9b05aa62edd77e1e4789d50b899d953eeb03466168d038bf3",
  "script-integrity-hash-mismatch":
    "a9ffc998ecea0fe0d705e156390778ecc922635d59a138f9c5104df9532d0cba",
  "spend-input-signer-missing":
    "40bf2c05dc4e3766e4600a14f2011fa63e4f48e5520634837e8c4e09cfbe6c06",
  "transaction-output-non-canonical":
    "98e226d1eea479c9be0225221ba5ae468bf850404819d5a9d5387dfce967f6dc",
  "unused-redeemer":
    "4a0a8dc6bb7cd2715a71defb9ab494d0c3d9dc032c80db348af5d21ed89efa73",
  "unused-script-witness":
    "4280bb16702704fdc83813ee5e9b245421440b94ead7337792619d0612928265",
  "witness-script-decoding":
    "ef17819b9af94846c54074f265f47027de9929f2dcddc1a9ad3725492a56ebce",
  "zero-input":
    "04c0e6030e701090c9ea3aa51c925fc9622bd8f65259d8a000b83154f319d79d",
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
describe("production fault-proof construction parity", () => {
  it.each(builders)(
    "preserves %s bytes, identities and adapter metadata",
    (name, build) => {
      const values = (["Preprod", "Mainnet"] as const).map(
        (network: Network) => {
          const steps = build({ ...input, network });
          return {
            steps,
            acceptedPrelude:
              "acceptedPrelude" in steps ? steps.acceptedPrelude : undefined,
          };
        },
      );
      const digest = createHash("sha256")
        .update(JSON.stringify(values))
        .digest("hex");
      expect(digest).toBe(baselines[name]);
    },
  );
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

it("preserves the L2 mistag adapter identity", () => {
  const result = (["Preprod", "Mainnet"] as const).map((network) =>
    buildL2TxMistagChain({
      ...input,
      network,
      hubOraclePolicyId: input.hubOracleScriptHash,
    }),
  );
  const digest = createHash("sha256")
    .update(JSON.stringify(result))
    .digest("hex");
  // Captured from the L2 adapter at the same pre-consolidation commit.
  expect(digest).toBe(
    "bf8c1858c54bcac5980a799eb7e067ff2adced1e5040308bac97a6e6d709e22b",
  );
});
