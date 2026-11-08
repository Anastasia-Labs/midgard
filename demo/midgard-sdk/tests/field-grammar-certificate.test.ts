import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { computeHash32 } from "@al-ft/midgard-core";
import {
  deriveMidgardFieldGrammarCertificate,
  encodeMidgardFieldPreimage,
  walkMidgardFieldGrammar,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  Data,
  mintingPolicyToId,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceFieldGrammarCertificateRedeemer,
  buildFieldGrammarCertificateValidator,
  FIELD_GRAMMAR_CERTIFICATE_CONTRACT_TITLES,
  FieldGrammarCertificate,
  fieldGrammarCertificateDatumCbor,
  openFieldGrammarCertificateRedeemer,
  parseFaultProofBlueprint,
  reclaimFieldGrammarCertificateRedeemer,
  retireFieldGrammarCertificateRedeemer,
} from "../src/index.js";
import { applyExpectedScriptParams } from "./fault-proof.expected-parameter-application.js";

const blueprint = parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
        fileURLToPath(
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        ),
      "utf8",
    ),
  ) as unknown,
);

/**
 * The golden vectors of
 * `onchain/aiken/validators/field-grammar-certificate-handlers.test.ak`
 * (`field_grammar_certificate_handlers_datums_match_the_golden_vectors`): the validator
 * accepted both datums, and these are `blake2b_256(serialise_data(datum))`.
 */
const GOLDEN_TX_ID = Buffer.from(
  "752b97ec14d1474f7eb9e94550874b87f5de218887a8a15c6fa21cafcf46f62f",
  "hex",
);
const GOLDEN_IN_PROGRESS_DIGEST =
  "a5e89f141e62d1f052c6c5c3ebc3570e60957f118fe5c3ff6d29025845d46a5c";
const GOLDEN_COMPLETE_DIGEST =
  "722115e3e22a2b498e6128e9813951c5cfc2ec2babd51085598f8b35850debf5";
const OWNER = Buffer.alloc(28, 0x66);

const grammar = walkMidgardFieldGrammar(
  2,
  encodeMidgardFieldPreimage(
    Array.from({ length: 40 }, (_, k) =>
      Buffer.alloc(1 + ((7 * k) % 40), 0xc3),
    ),
  ),
);
const datumDigest = (next: number) =>
  computeHash32(
    Buffer.from(
      fieldGrammarCertificateDatumCbor(
        deriveMidgardFieldGrammarCertificate({
          owner: OWNER,
          txId: GOLDEN_TX_ID,
          grammar,
          next,
        }),
      ),
      "hex",
    ),
  ).toString("hex");

const source = {
  sourceKind: 0n as const,
  compactCbor: "a0",
  witnessSetCompactCbor: "",
};
const carriage = { Inline: { preimage: "80" } };

describe("field grammar certificate wire format", () => {
  it("encodes the datums the validator computes, byte for byte", () => {
    expect(datumDigest(16)).toBe(GOLDEN_IN_PROGRESS_DIGEST);
    expect(datumDigest(40)).toBe(GOLDEN_COMPLETE_DIGEST);
    const complete = Data.from(
      fieldGrammarCertificateDatumCbor(
        deriveMidgardFieldGrammarCertificate({
          owner: OWNER,
          txId: GOLDEN_TX_ID,
          grammar,
          next: 40,
        }),
      ),
      FieldGrammarCertificate,
    );
    expect(complete).toMatchObject({
      Complete: { item_count: 40n, waypoints: "00020140029e" },
    });
  });

  it("pins every redeemer constructor", () => {
    const open = openFieldGrammarCertificateRedeemer({
      source,
      carriage,
      batch: 16,
      outputIndex: 0,
    });
    const advance = advanceFieldGrammarCertificateRedeemer({
      source,
      checkpoint: "87",
      carriage,
      batch: 16,
      outputIndex: 1,
    });
    expect(open.startsWith("d8799f00")).toBe(true);
    expect(advance.startsWith("d8799f00")).toBe(true);
    expect(retireFieldGrammarCertificateRedeemer()).toBe("d87a80");
    expect(reclaimFieldGrammarCertificateRedeemer()).toBe("d87a80");
    expect(() =>
      openFieldGrammarCertificateRedeemer({
        source,
        carriage,
        batch: 0,
        outputIndex: 0,
      }),
    ).toThrow(/positive item count/u);
  });
});

describe("field grammar certificate deployment recipe", () => {
  const fieldPreimageCertificatePolicyId = "ab".repeat(28);

  it("applies exactly its one parameter, mint and spend as one script", () => {
    const validator = buildFieldGrammarCertificateValidator({
      blueprint,
      network: "Preprod",
      fieldPreimageCertificatePolicyId,
    });
    const entry = blueprint.validators.find(
      (candidate) =>
        candidate.title === FIELD_GRAMMAR_CERTIFICATE_CONTRACT_TITLES.mint,
    );
    expect(entry).toBeDefined();
    const applied = applyExpectedScriptParams(entry!.compiledCode, [
      fieldPreimageCertificatePolicyId,
    ]);
    const script = { type: "PlutusV3" as const, script: applied };
    expect(validator.mintingScriptCBOR).toBe(applied);
    expect(validator.policyId).toBe(mintingPolicyToId(script));
    expect(validator.spendingScriptHash).toBe(validator.policyId);
    expect(validator.spendingScriptAddress).toBe(
      validatorToAddress("Preprod", script),
    );
  });

  it("refuses a policy id that is not 28 bytes", () => {
    expect(() =>
      buildFieldGrammarCertificateValidator({
        blueprint,
        network: "Preprod",
        fieldPreimageCertificatePolicyId: "ab",
      }),
    ).toThrow(/28 bytes/u);
  });
});
