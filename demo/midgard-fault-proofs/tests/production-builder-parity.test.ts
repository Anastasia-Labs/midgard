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
// Migrated for StateQueueNode.proven_fraud in the shared queue authenticator.
// Pinned testnet Aiken v1.1.23+5adf783 blueprint SHA-256:
// 3d04859765f10a154e7bfbc2a7daeb6063e2d216754d1cf39cd5981c8af2fb19.
// Existing full-byte, identity and adapter-metadata assertions remain intact.
const baselines: Record<string, string> = {
  "distinct-asset-accumulation-limit":
    "5db21fa765796f44754404319288ce635946a64387b966b246757d973b41faa0",
  "execution-native-script-invalid":
    "fe39f9475fd55a9f0bc7cfdbdc6b1ba1cca3a34ea7223d0c364868c61f02dac2",
  "execution-source-script-decoding":
    "7be112a12411c4a5d8edb7fbd64c454c252c3092c736aa723856150ffeda96f1",
  "field-item-width-illegal":
    "646ce5580702259782f1262cc5613204a25984b41066cfbc1f1bc7532c3253a8",
  "mint-declared-asset-limit":
    "9013434447e7b48fdc444517068c3c7430d2c53607c10fa300b0700b87b17da1",
  "mint-item-non-canonical":
    "a2f4a660bc3086f886cd6aa3d2224e0bafa81e9981b630535a5c7aaf0c882432",
  "missing-redeemer":
    "b2faa6365e62d955e139b8b9fd9c4bf60661340148d8b30a8cafdfef2aaafe65",
  "missing-script-source":
    "31c6996e515be139127ebeeb7f259a33c515c5b5ca09e4fe5967e2ce9ed7ab0c",
  "observer-order-invalid":
    "63b594b7365f05cbcc31011ebbeb9cace893931d5f58ddcd416548237ff9c1aa",
  "observers-forbidden-on-untagged-network":
    "54aee2fc8b8052578e22cfa14e457059aa8f597f9286fe075b1cda7938c77492",
  "output-reference-script-decoding":
    "8beccb1e8212ec2ea906ce621c8b96b7669c314f285f72dda5768a049520d0f8",
  "protected-output-signer-missing":
    "bb38b7afba8410b71ec64fcabee94b81f052d9663450e5685bf2fe53a34d831d",
  "receive-purpose-language":
    "e5ae3dfb1ea92cc28650058a18125ccbdc64bc5313bac8f600a0e12ff70bd31c",
  "redeemer-canonicity":
    "d14f2b6929ade6d2ee56ade87b946640d87ce6ddbb190aaf5b122baac934c65a",
  "resolved-output-non-canonical":
    "c991c7f79c395f0cd8186038d24121acdf61b3c7e1f898ecc0cbc583c7768d26",
  "script-integrity-hash-mismatch":
    "d5e271ea4d7675d4d89b2947e6022caff093de2f6091982e48ef493e5e6db439",
  "spend-input-signer-missing":
    "95194082e0b51f7a89bbe7ff67a33cbcef783f62cc0834208cc59a9cf1c8cd76",
  "transaction-output-non-canonical":
    "b5c59a9236fa34cfd2a9872cd6d751bee63c0485e4eada924c0226c3ea3e6424",
  "unused-redeemer":
    "cb72c551dd91fffce3f845a2e37f2a8553ddf2ec1cbb8531ac1b54ae2ec00adc",
  "unused-script-witness":
    "2ddfccd81f70ac3b7460ffc90a23e2517ec9ef0cf494df8e30412f3c5e5c1ba0",
  "witness-script-decoding":
    "b778a71e89e7b53b52a0de8f124ba2d309fa61143af4131b26a24babe70772ad",
  "zero-input":
    "d20bb8ae985a2b4cea8ccb95396c32a3fe8574a4add569c104c9f5b1461a42d1",
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
  // Same queue-schema/compiler migration as the production adapters above.
  expect(digest).toBe(
    "66e29e8a33f08e6125b27da894a56d0556232a5d0d03120b5534dd9e17ff7836",
  );
});
